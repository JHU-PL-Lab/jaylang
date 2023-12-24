(*
  TODO:
    Really, I should completely revamp this. I just need to have a stack of current parent and formula list.
    That is sufficient because we're under one parent at a time. And we can recurse up the parents to make
    a pick formula.
    And we leave a parent into the next one by creating the implies with the "and" of all formulas we're leaving.
    We need to track formulas along with the parent stack because we may enter and leave a parent as we hit
    branches in the clause list, and we ought to keep the formulas we've already seen.

    We can either have a global store separately (I think this is a good idea) or have the global be the last
    on the stack.
    The reason to keep it separate is to more easily merge solvers and to add the persistent formulas.
    The reason to keep it not separate is to make sure that we've properly collected the entire stack into the
    final global formulas before merging or solving, but this is a runtime assertion, so it's not necessarily
    safer programming.

    I'm not sure yet if the formula stacks should be in parallel (each their separate stack) or coupled (as a stack
    of tuples or records). To ensure safer programming and to let type-checking catch my coding mistakes, it's
    probably best to keep them coupled even if that is slightly clunky.

    Do separately keep the top value of the stack or always peek at the top value of the stack?
    If we store separately, it's still a bunch of copying pointers because all the record values need to be copied.
    If peeking the stack, we are constantly making a new stack and copying the back.
    So for efficiency, it doesn't matter. It only matters how I care to do the pattern matching: on a variant that
    helps track state, or just let the stack convey the state. This is to be decided after just a little bit of
    coding and designing.

    Then can wrap up the branch solver into a formula set when done, which makes for much easier and logically sound/safe
    merging of solvers. And we can delegate any persistent formula logic (ie anything that interacts with a Z3 expr) to
    the solver (which we can force to only happen when in global scope, or just let it happen any time by keeping global
    scope separate). So the difference between the branch solver and the concolic session is that the branch solver
    actually interacts directly with Z3 exprs (so should make `add_alias` and not let formulas get passed directly).

    The concolic session still tracks extra stuff like hit targets, abort information, target, etc, but the solver
    tracks current parent and actual expression.
*)


open Core

exception NoParentException

module Lookup_key = 
  struct
    include Lookup_key
    (* Core.Map.Key expects t_of_sexp, so provide failing implementation *)
    let t_of_sexp _ = failwith "Lookup_key.t_of_sexp needed and not implemented"
  end

(*
  I could no longer use this, and I just keep a global formula store when the stack is empty   .
  Currently forcing the bottom of the stack to be global feels a little messy.
*)
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

    let to_condition_key (parent : t) : Lookup_key.t option =
      match parent with
      | Global -> None
      | Local branch -> Some branch.condition_key

    let to_ast_branch_exn (parent : t) : Branch.Ast_branch.t =
      match parent with
      | Local x -> Branch.Runtime.to_ast_branch x
      | Global -> failwith "global ast branch undefined"

    let to_runtime_branch_exn = function
    | Local x -> x
    | Global -> failwith "global runtime branch undefined"
      
  end

module Z3_expr =
  struct
    include Z3.Expr
    type t = Z3.Expr.expr
    let t_of_sexp _ = failwith "fail t_of_sexp z3 expr"
    let sexp_of_t _ = failwith "fail sexp_of_t x3 expr" 
  end

module Formula_set =
  struct
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
      { parent : Parent.t
      ; formulas : Formula_set.t }

    let empty : t =
      { parent = Parent.Global ; formulas = Formula_set.empty }

    let create (branch : Branch.Runtime.t) : t =
      { empty with parent = Parent.of_runtime_branch branch }

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
  * Keep global formulas separate   
  * Store the "current parent" and its formulas separate

  But these can be encapsulated in an environment stack. The global formulas are at
  the bottom of the stack. The current formulas are at the top of the stack.
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
    Printf.printf "ADD GLOBAL FORMULA %s\n" (Z3.Expr.to_string formula)
  | _ -> ()

let add_formula ({ stack ; _ } as x : t) (formula : Z3.Expr.expr) : t =
  log_add_formula x formula;
  let new_stack =
    match stack with
    | { formulas ; _ } as hd :: tl ->
      { hd with formulas = Formula_set.add formulas formula } :: tl
    | _ -> raise NoParentException
  in
  { x with stack = new_stack }

let add_key_eq_val (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
  add_formula x @@ Riddler.eq_term_v key (Some v)

let add_alias (x : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
  add_formula x @@ Riddler.eq key1 key2

(* TODO: all other types of formulas, e.g. binop, not, pattern, etc *)

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
    { stack = Env.add new_hd (Env.collect old_hd) :: tl
    ; pick_formulas = Map.set pick_formulas ~key:exited_branch.branch_key ~data:(gen_pick_formula stack) }
  | _ -> raise NoParentException (* no parent to back up to because currently in global (or no) scope *)

let to_solver ({ stack ; pick_formulas } : t) (target_branch_key : Lookup_key.t) : Z3.Solver.solver option =
  match stack with
  | { parent = Global ; formulas } :: [] ->
    let new_solver = Z3.Solver.mk_solver Solver.SuduZ3.ctx None in
    Z3.Solver.add new_solver
    @@ Formula_set.to_list formulas;
    Z3.Solver.add new_solver [Map.find_exn pick_formulas target_branch_key];
    Some new_solver
  | _ -> None

let get_model
  (x : t)
  (target : Branch.Runtime.t)
  : [ `Success of Z3.Model.model | `Unsatisfiable of Branch.Ast_branch.t | `No_pick | `Not_global ]
  =
  let picked_branch_formula = Riddler.picked target.branch_key in (* TODO: check this is in the solver *)
  let condition_formula = Branch.Runtime.to_expr target in
  match to_solver x target.branch_key with
  | None -> `Not_global
  | Some z3_solver ->
    Format.printf "Solving for target branch:\n";
    Format.printf "Branch to pick: %s\n" (Z3.Expr.to_string picked_branch_formula);
    Format.printf "Branch condition: %s\n" (Z3.Expr.to_string condition_formula);
    [ picked_branch_formula ; condition_formula ] (* will check that these formulas are consistent *)
    |> Z3.Solver.check z3_solver
    |> Solver.SuduZ3.get_model z3_solver
    |> function
      | Some model -> `Success model
      | None -> `Unsatisfiable (Branch.Runtime.to_ast_branch target)

let get_feeder
  (x : t)
  (target : Branch.Runtime.t)
  : (Concolic_feeder.t, Branch.Ast_branch.t) result
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

