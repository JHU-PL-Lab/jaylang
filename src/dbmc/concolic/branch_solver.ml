(*
open Core

exception NoParentException

module Lookup_key = 
  struct
    include Lookup_key
    (* Core.Map.Key expects t_of_sexp, so provide failing implementation *)
    let t_of_sexp _ = failwith "Lookup_key.t_of_sexp needed and not implemented"
  end

module Parent =
  struct
    type t =
      | Global 
      | Local of Branch.Runtime.t
      [@@deriving compare, sexp]

    let of_runtime_branch (branch : Branch.Runtime.t) : t =
      Local branch

    (* TODO: think about making this option so no redundant `true` statements from Global *)
    let to_expr (parent : t) : Z3.Expr.expr =
      match parent with
      | Global -> Riddler.true_ (* Global scope is just a trivial parent *)
      | Local branch -> Branch.Runtime.to_expr branch
  end


module Formula_set =
  struct
    module Z3_expr =
      struct
        include Z3.Expr
        type t = Z3.Expr.expr

        (* Set.Make expects sexp conversions, but they aren't ever used. *)
        let t_of_sexp _ = failwith "fail t_of_sexp z3 expr"
        let sexp_of_t _ = failwith "fail sexp_of_t x3 expr" 
      end

    module S = Set.Make (Z3_expr)

    type t = S.t

    let add : t -> Z3_expr.t -> t = Set.add
    let union : t -> t -> t = Set.union
    let fold : t -> init:'a -> f:('a -> Z3_expr.t -> 'a) -> 'a = Set.fold
    let empty : t = S.empty
    let of_list : Z3_expr.t list -> t = S.of_list
    let to_list : t -> Z3_expr.t list = Set.to_list

    let collect (fset : t) : Z3_expr.t =
      match Set.to_list fset with
      | [] -> Riddler.true_
      | exp :: [] -> exp
      | exps -> Riddler.and_ exps
  end

module Env =
  struct
    type t =
      { parent        : Parent.t
      ; formulas      : Formula_set.t }

    let empty : t =
      { parent = Parent.Global ; formulas = Formula_set.empty }

    let create (branch : Branch.Runtime.t) : t =
      { empty with parent = Parent.of_runtime_branch branch }

    (** [collect x] is all formulas in [x.formulas] implied by [x.parent]. *)
    let collect ({ parent ; formulas } : t) : Z3.Expr.expr =
      match parent with
      | Global -> Formula_set.collect formulas
      | Local branch -> Riddler.(Branch.Runtime.to_expr branch @=> Formula_set.collect formulas)

    let add ({ formulas ; _ } as x : t) (formula : Z3.Expr.expr) : t =
      { x with formulas = Formula_set.add formulas formula }
  end

module Branch_map = Map.Make (Lookup_key)

(*
  There are a few additions we can make:
  * Store global formulas separately
  * Store the "current parent" and its formulas separately

  But these can be encapsulated in an environment stack. The global formulas are at
  the bottom of the stack. The current formulas are at the top of the stack.

  I do think it would be nicer to have a stack that is potentially empty, and then a 
  global formula store. This just leads to fewer fails, I think. It might be worse when
  exiting a branch, though, because it separates the logic into a consideration of these
  two cases: 1) exit to another local branch, or 2) exit to the global environment.
*)
type t =
  { stack : Env.t list
  ; pick_formulas : Z3.Expr.expr Branch_map.t } 

let empty : t =
  { stack = Env.empty :: []
  ; pick_formulas = Branch_map.empty }

let log_add_formula ({ stack ; _ } : t) (formula : Z3.Expr.expr) : unit =
  match stack with
  | { parent = Parent.Global ; _ } :: _ -> 
    Format.printf "ADD GLOBAL FORMULA %s\n" (Z3.Expr.to_string formula)
  | _ -> ()

let add_formula ({ stack ; _ } as x : t) (formula : Z3.Expr.expr) : t =
  (* log_add_formula x formula; *)
  let new_stack =
    match stack with
    | hd :: tl -> Env.add hd formula :: tl
    | _ -> raise NoParentException
  in
  { x with stack = new_stack }

let add_key_eq_val (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
  add_formula x @@ Riddler.eq_term_v key (Some v)

let add_alias (x : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
  add_formula x @@ Riddler.eq key1 key2

let add_binop (x : t) (key : Lookup_key.t) (op : Jayil.Ast.binary_operator) (left : Lookup_key.t) (right : Lookup_key.t) : t =
  add_formula x @@ Riddler.binop_without_picked key op left right

(* TODO: all other types of formulas, e.g. not, pattern, etc *)

let enter_branch ({ stack ; _ } as x : t) (branch : Branch.Runtime.t) : t =
  { x with stack = Env.create branch :: stack }

let exit_branch ({ stack ; pick_formulas ; _ } as x : t) : t =
  let gen_pick_formula (stack : Env.t list) : Z3.Expr.expr =
    match stack with
    | { parent = Local branch ; _ } :: tl ->
      let deps = List.map tl ~f:(fun { parent ; _ } -> Parent.to_expr parent) in
      Riddler.(picked branch.branch_key @=> and_ deps) (* hd implies all other parents that are below it on the stack *)
    | _ -> Riddler.true_ (* TODO: make option *)
  in
  match stack with
  | { parent = Local exited_branch ; _ } as old_hd :: new_hd :: tl ->
    (* Format.printf "exiting branch and collecting formulas. Formula is %s\n" (Env.collect old_hd |> Z3.Expr.to_string); *)
    { stack = Env.add new_hd (Env.collect old_hd) :: tl
    ; pick_formulas = Map.set pick_formulas ~key:exited_branch.branch_key ~data:(gen_pick_formula stack) }
  | _ -> raise NoParentException (* no parent to back up to because currently in global (or no) scope *)

let to_solver ({ stack ; pick_formulas } : t) (target_branch_key : Lookup_key.t) : Z3.Solver.solver option =
  match stack with
  | { parent = Global ; formulas } :: [] ->
    let new_solver = Z3.Solver.mk_solver Solver.SuduZ3.ctx None in
    Z3.Solver.add new_solver (Map.find_exn pick_formulas target_branch_key :: Formula_set.to_list formulas);
    Some new_solver
  | _ -> None

let get_model
  (x : t)
  (target : Branch.Runtime.t)
  : [ `Success of Z3.Model.model | `Unsatisfiable of Branch.t | `No_pick | `Not_global ]
  =
  let picked_branch_formula = Riddler.picked target.branch_key in (* TODO: check this is in the solver *)
  let condition_formula = Branch.Runtime.to_expr target in
  if Printer.print then
  begin
  Format.printf "Solving for target branch:\n";
  Format.printf "Branch to pick: %s\n" (Z3.Expr.to_string picked_branch_formula);
  Format.printf "Branch condition: %s\n" (Z3.Expr.to_string condition_formula)
  end;
  match to_solver x target.branch_key with
  | None -> `Not_global
  | Some z3_solver ->
    if Printer.print then Format.printf "Printing solver: \n%s\n" (Z3.Solver.to_string z3_solver);
    [ picked_branch_formula ; condition_formula ] (* will check that these formulas are consistent *)
    |> Z3.Solver.check z3_solver
    |> Solver.SuduZ3.get_model z3_solver
    |> function
      | Some model -> `Success model
      | None -> `Unsatisfiable (Branch.Runtime.to_ast_branch target)

let get_feeder
  (x : t)
  (target : Branch.Runtime.t)
  : (Concolic_feeder.t, Branch.t) result
  =
  match get_model x target with
  | `No_pick -> failwith "tried to solve for target that can't be picked"
  | `Not_global -> failwith "tried to solve for target before closing out of all branches"
  | `Unsatisfiable b -> Error b
  | `Success model -> Ok (Concolic_feeder.from_model model)

let get_cur_parent_exn ({ stack ; _ } : t) : Branch.Runtime.t =
  match stack with
  | { parent = Parent.Local branch ; _ } :: _ -> branch
  | _ -> failwith "no local parent to get"

let is_global ({ stack ; _ } : t) : bool =
  match stack with
  | { parent = Parent.Global ; _ } :: _ -> true
  | _ -> false

let add_formula_set (fset : Formula_set.t) (x : t) : t =
  Formula_set.fold fset ~init:x ~f:add_formula

let merge (a : t) (b : t) : t =
  let new_stack = 
    match a.stack, b.stack with
    | { parent = Global ; formulas = formulas1 } :: [], { parent = Global ; formulas = formulas2 } :: _ ->
      Env.{ parent = Global ; formulas = Formula_set.union formulas1 formulas2 } :: []
    | _ -> failwith "cannot merge non-global branch solvers"
  in
  let new_pick_formulas =
    Map.fold
      b.pick_formulas
      ~init:a.pick_formulas
      ~f:(fun ~key ~data acc -> Map.set ~key ~data acc)
  in
  { stack = new_stack ; pick_formulas = new_pick_formulas }

*)