open Core

exception NoParentException

module Formula_set =
  struct
    module Z3_expr =
      struct
        include Z3.Expr
        type t = Z3.Expr.expr

        (* Set.Make expects sexp conversions, but we don't ever use them. *)
        let t_of_sexp _ = failwith "fail t_of_sexp z3 expr"
        let sexp_of_t _ = failwith "fail sexp_of_t x3 expr" 
      end

    module S = Set.Make (Z3_expr)

    type t = S.t

    let add : t -> Z3_expr.t -> t = Set.add
    let union : t -> t -> t = Set.union
    (* let fold : t -> init:'a -> f:('a -> Z3_expr.t -> 'a) -> 'a = Set.fold *)
    let empty : t = S.empty
    (* let of_list : Z3_expr.t list -> t = S.of_list *)
    let to_list : t -> Z3_expr.t list = Set.to_list

    let collect (fset : t) : Z3_expr.t =
      match Set.to_list fset with
      | [] -> Riddler.true_
      | exp :: [] -> exp
      | exps -> Riddler.and_ exps
  end

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

    let to_expr (parent : t) : Z3.Expr.expr option =
      match parent with
      | Global -> None (* Global scope is trivial parent. Could use `true` instead *)
      | Local branch -> Some (Branch.Runtime.to_expr branch)
  end

(* Parents are foced to be runtime branches *)
module Formula =
  struct
    module type S =
      sig
        type t
        val to_expr : t -> Z3.Expr.expr
        val exit_parent : Branch.Runtime.t -> t -> t
        val empty : Z3.Expr.expr -> t
        (** [empty expr] has no parent dependencies, but it does require [expr]. *)
      end

    module And : S =
      struct
        type t = Branch.Runtime.t list * Z3.Expr.expr (* will take the "and" of all parent dependencies and the expr *)

        let empty (expr : Z3.Expr.expr) : t =
          [], expr

        let exit_parent (parent : Branch.Runtime.t) (stack, expr : t) : t =
          parent :: stack, expr

        let to_expr (stack, expr : t) : Z3.Expr.expr =
          match stack with
          | [] -> expr
          | _ -> 
            stack
            |> List.map ~f:Branch.Runtime.to_expr
            |> List.cons expr
            |> Riddler.and_
      end

    module Implies : S =
      struct
        type t = Branch.Runtime.t list * Z3.Expr.expr (* list of parents implies for the formula down the line *)

        let empty (expr : Z3.Expr.expr) : t =
          [], expr

        (* most recently exited parent is on top of the stack *)
        let exit_parent (parent : Branch.Runtime.t) (stack, expr : t) : t =
          parent :: stack, expr

        (* need the least recently exited parent to imply the expr first, so fold right *)
        let to_expr (stack, expr : t) : Z3.Expr.expr =
          let implies (branch : Branch.Runtime.t) (expr : Z3.Expr.expr) : Z3.Expr.expr =
            Riddler.(Branch.Runtime.to_expr branch @=> expr)
          in
          List.fold_right stack ~init:expr ~f:implies
      end
  end

(* Note: I'm not actually using the "pick". I'm tracking them myself to reduce load on the solver *)
module Pick_formulas :
  sig
    (* TODO: have a "finished" variant where some formulas are now expr, so no recomputation needed *)
    type t

    val empty : t

    val exit_parent : t -> Branch.Runtime.t -> t
    (** [exit_parent t parent] steps the tracker [t] out of the [parent], adjusting all formulas
        as necessary and adding pick formulas for the exited parent. *)

    val union : t -> t -> t
    (** [union a b] contains all info from [a] and [b] *)

    val pick_target : t -> Branch.t -> Z3.Expr.expr
    val found_abort : t -> Branch.t -> Z3.Expr.expr
    val reach_max_step : t -> Branch.t -> Z3.Expr.expr
    
  end
  =
  struct
    (* Note that parents must be runtime branches only. Global is not allowed. *)
    module type S =
      sig
        type t
        (** [t] holds formulas to be picked by parent branches. *)
        val empty : t
        (** [empty] has no information about any branches yet. *)
        (* val add_pick : t -> Branch.Runtime.t -> t *) (* only used internally *)
        (** [add_pick t parent] is a new t that has formulas pickable by [parent]. *)
        val to_formula : t -> Branch.t -> Z3.Expr.expr
        (** [to_formulas t branch] is a combination of formulas in [t] that are picked by [branch]. *)
        val exit_parent : t -> Branch.Runtime.t -> t
        (** [exit_parent t parent] adds the [parent] to all formulas within, and calls [add_pick t parent] *)
        val merge : t -> t -> t
        (** [merge a b] contains all info from [a] and [b] *)
      end

    module M = Map.Make (Branch) (* key is AST branch *)
    
    let merge_m (m1 : 'a list M.t) (m2 : 'a list M.t) : 'a list M.t =
      Map.merge m1 m2 ~f:(fun ~key:_ -> function
        | `Both (left, right) -> Some (right @ left) (* assume that there are more formulas on the left *)
        | `Left ls | `Right ls -> Some ls
      )

    (*
      An implies pick formula can be picked such that the list of parents all imply
      some formula at the bottom.

      The logic is the same for max step and for aborts: whenever a parent is found,
      a pick key for that parent can set either this parent off limits or the other side
      off limits.

      It's intended that the `pick` is the same for any runtime instance of an AST branch.
      This way, all runtime instance can be set off limits using any runtime instance.
    *)
    module Implies : S =
      struct
        type t = Formula.Implies.t list M.t

        let empty = M.empty

        (* say that picking this side implies the other side must be true *)
        let add_pick (map : t) (branch : Branch.Runtime.t) : t =
          (* a formula that says branch is off limits *)
          let pick_formula (branch : Branch.Runtime.t) : Formula.Implies.t =
            branch
            |> Branch.Runtime.other_direction
            |> Branch.Runtime.to_expr (* the resulting expr is that the other direction must be satisfied *)
            |> Formula.Implies.empty (* note that this parent is not implied *)
          in
          let add (branch : Branch.Runtime.t) : t -> t =
            Map.add_multi ~key:(Branch.Runtime.to_ast_branch branch) ~data:(pick_formula branch)
          in
          map
          |> add branch
          |> add (Branch.Runtime.other_direction branch)

        let to_formula (map : t) (branch : Branch.t) : Z3.Expr.expr =
          match Map.find map branch with
          | Some formula_list ->
            formula_list
            |> List.map ~f:Formula.Implies.to_expr
            |> Riddler.and_
          | None -> failwith "no \"implies\" pick formulas found for parent branch" (* should be impossible if used correctly *)

        let exit_parent (map : t) (parent : Branch.Runtime.t) : t =
          map
          |> Map.map ~f:(List.map ~f:(Formula.Implies.exit_parent parent))
          |> Fn.flip add_pick parent

        let merge = merge_m
      end

    module Abort : S = Implies
    (* module Max_step : S = Implies *)

    (*
      Track targets by letting the condition be satisfied. This takes the "or" of all
      targets, so when getting formulas for any AST branch, all known runtime instances
      of that branch are considered.
    *)
    module Target : S =
      struct
        type t = Formula.And.t list M.t

        let empty = M.empty

        (* add formulas for both sides to be picked *)
        let add_pick (map : t) (branch : Branch.Runtime.t) : t =
          let pick_formula (branch : Branch.Runtime.t) : Formula.And.t =
            branch
            |> Branch.Runtime.to_expr (* say that this branch must be satisfied *)
            |> Formula.And.empty (* note that this parent is not implied *)
          in
          let add (branch : Branch.Runtime.t) : t -> t =
            Map.add_multi ~key:(Branch.Runtime.to_ast_branch branch) ~data:(pick_formula branch)
          in
          map
          |> add branch
          |> add (Branch.Runtime.other_direction branch)

        let to_formula (map : t) (branch : Branch.t) : Z3.Expr.expr =
          match Map.find map branch with
          | Some formula_list ->
            formula_list
            |> List.map ~f:Formula.And.to_expr (* the same parents are "anded" many times. We could make formulas smaller by being more intentional *)
            |> Solver.SuduZ3.or_
          | None -> failwith "no \"and\" pick formulas found for parent branch" (* should be impossible if used correctly *)

        let exit_parent (map : t) (parent : Branch.Runtime.t) : t =
          map
          |> Map.map ~f:(List.map ~f:(Formula.And.exit_parent parent))
          |> Fn.flip add_pick parent

        let merge = merge_m
      end

    type t =
      { abort    : Abort.t
      (* ; max_step : Max_step.t *) (* max step is treated precisely like abort right now, so it is redundant *)
      ; target   : Target.t }

    let empty =
      { abort    = Abort.empty 
      (* ; max_step = Max_step.empty *)
      ; target   = Target.empty }

    let union (a : t) (b : t) : t = 
      { abort    = Abort.merge a.abort b.abort
      (* ; max_step = Max_step.merge a.max_step b.max_step  *)
      ; target   = Target.merge a.target b.target }

    let exit_parent ({ abort ; (*max_step ;*) target } : t) (parent : Branch.Runtime.t) : t =
      { abort    = Abort.exit_parent abort parent
      (* ; max_step = Max_step.exit_parent max_step parent *)
      ; target   = Target.exit_parent target parent }

    let pick_target ({ target ; _ } : t) (branch : Branch.t) : Z3.Expr.expr =
      Target.to_formula target branch

    let found_abort ({ abort ; _ } : t) (branch : Branch.t) : Z3.Expr.expr =
      Abort.to_formula abort branch

    (* let reach_max_step ({ max_step ; _ } : t) (branch : Branch.t) : Z3.Expr.expr = *)
      (* Max_step.to_formula max_step branch *)
    let reach_max_step = found_abort (* treat exactly like abort *)

  end

module Env :
  sig
    type t =
      { parent        : Parent.t (* current "scope" *)
      ; formulas      : Formula_set.t (* always true formulas under this parent *)
      ; pick_formulas : Pick_formulas.t } (* pickable formulas *)

    val empty : t
    val create : Branch.Runtime.t -> t
    val add : t -> Z3.Expr.expr -> t
    val exit_to_env : t -> t -> t
    (** [exit_to_env exited new_env] wraps up all info in [exited] and appropriately puts it in [new_env],
        also adding any necessary pick formulas in case of aborts or max steps at any future point. *)
  end
  =
  struct
    type t =
      { parent        : Parent.t
      ; formulas      : Formula_set.t
      ; pick_formulas : Pick_formulas.t }

    let empty : t =
      { parent        = Parent.Global
      ; formulas      = Formula_set.empty
      ; pick_formulas = Pick_formulas.empty }

    let create (branch : Branch.Runtime.t) : t =
      { empty with parent = Parent.of_runtime_branch branch }

    (** [collect_formulas x] is all formulas in [x.formulas] implied by [x.parent]. *)
    let collect_formulas ({ parent ; formulas ; _ } : t) : Z3.Expr.expr =
      match parent with
      | Global -> Formula_set.collect formulas
      | Local branch -> Riddler.(Branch.Runtime.to_expr branch @=> Formula_set.collect formulas)

    let add ({ formulas ; _ } as x : t) (formula : Z3.Expr.expr) : t =
      { x with formulas = Formula_set.add formulas formula }

    let exit_to_env (exited : t) (new_env : t) : t =
      match exited with
      | { parent = Global ; _ } -> raise NoParentException
      | { parent = Local exited_branch ; _ } -> begin
        { parent = new_env.parent
        ; formulas = Formula_set.add new_env.formulas @@ collect_formulas exited
        ; pick_formulas =
          Pick_formulas.union new_env.pick_formulas
          @@ Pick_formulas.exit_parent exited.pick_formulas exited_branch
        }
      end

  end

(*
  There are a few additions we can make:
  * Store global formulas separately
  * Store the "current parent" and its formulas separately

  But these can be encapsulated in an environment stack. The global formulas are at
  the bottom of the stack. The current formulas are at the top of the stack.
*)
type t = { stack : Env.t list }

let empty : t = { stack = Env.empty :: [] }

let log_add_formula ({ stack  } : t) (formula : Z3.Expr.expr) : unit =
  match stack with
  | { parent = Parent.Global ; _ } :: _ -> 
    Format.printf "ADD GLOBAL FORMULA %s\n" (Z3.Expr.to_string formula)
  | _ -> ()

let add_formula ({ stack } : t) (formula : Z3.Expr.expr) : t =
  (* log_add_formula x formula; *)
  let new_stack =
    match stack with
    | hd :: tl -> Env.add hd formula :: tl
    | _ -> raise NoParentException
  in
  { stack = new_stack }
  
let add_key_eq_val (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
  add_formula x @@ Riddler.eq_term_v key (Some v)

let add_alias (x : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
  add_formula x @@ Riddler.eq key1 key2

let add_binop (x : t) (key : Lookup_key.t) (op : Jayil.Ast.binary_operator) (left : Lookup_key.t) (right : Lookup_key.t) : t =
  add_formula x @@ Riddler.binop_without_picked key op left right

(* We'd like to not choose this input anymore, so mark it off limits *)
(* TODO: how does this work for inputs in recursive functions that have different previous inputs? *)
let add_input (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
  Riddler.eq_term_v key (Some v)
  |> Solver.SuduZ3.not_
  |> add_formula x

(* TODO: all other types of formulas, e.g. not, pattern, etc, then hide `add_formula` *)

let enter_branch ({ stack } : t) (branch : Branch.Runtime.t) : t =
  { stack = Env.create branch :: stack }

let exit_branch ({ stack } : t) : t =
  match stack with
  | { parent = Local _ ; _ } as old_hd :: new_hd :: tl ->
    (* Format.printf "exiting branch and collecting formulas. Formula is %s\n" (Env.collect old_hd |> Z3.Expr.to_string); *)
    { stack = Env.exit_to_env old_hd new_hd :: tl }
  | _ -> raise NoParentException (* no parent to back up to because currently in global (or no) scope *)

let is_global ({ stack } : t) : bool =
  match stack with
  | { parent = Global ; _ } :: [] -> true
  | _ -> false

let hd_env_exn ({ stack } : t) : Env.t =
  match stack with
  | hd :: _ -> hd
  | _ -> failwith "no hd in `hd_env_exn`"

let rec exit_until_global (x : t) : t =
  if is_global x
  then x
  else exit_until_global (exit_branch x)

let union (a : t) (b : t) : t option =
  if is_global a && is_global b
  then
    let hd = 
      let a, b = hd_env_exn a, hd_env_exn b in
      Env.(
      { parent = Global
      ; formulas = Formula_set.union a.formulas b.formulas
      ; pick_formulas = Pick_formulas.union a.pick_formulas b.pick_formulas }
      )
    in
    Some { stack = [ hd ] }
  else
    None

let all_formulas
  ({ stack } : t)
  ~(target : Branch.t)
  ~(aborts : Branch.t list)
  ~(max_steps : Branch.t list)
  : Z3.Expr.expr list
  =
  match stack with
  | { parent = Local _ ; _ } :: _ -> failwith "cannot get formulas unless in global scope"
  | { parent = Global ; formulas ; pick_formulas } :: [] ->
    let abort_formulas = List.map aborts ~f:(Pick_formulas.found_abort pick_formulas) in
    let max_step_formulas = List.map max_steps ~f:(Pick_formulas.reach_max_step pick_formulas) in
    let target_formula = Pick_formulas.pick_target pick_formulas target in
    target_formula :: abort_formulas @ max_step_formulas @ Formula_set.to_list formulas
  | _ -> failwith "impossible global is not bottom of stack"

