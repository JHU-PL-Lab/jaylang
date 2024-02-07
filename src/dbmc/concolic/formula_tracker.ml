open Core

exception NoParentException

(*
  NOTE: the last commit containing "V1" with more wordy pick formulas (which were easier
    to logically follow but were slower) has the following hash:
      d99c763d9631f22873cfde17169298641dbbb988
    It is on the jaylang/concolic branch
*)

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


(* Note: I'm not actually using the "pick". I'm tracking them myself to reduce load on the solver *)
(* This is used to pick formulas for target branches. *)
module Pick_formulas :
  sig
    type t

    val empty : t

    val exit_parent : t -> Branch.Runtime.t -> t
    (** [exit_parent t parent] steps the tracker [t] out of the [parent], adjusting all formulas
        as necessary and adding pick formulas for the exited parent. *)

    val union : t -> t -> t
    (** [union a b] contains all info from [a] and [b] *)

    val pick_target : t -> Branch.t -> Z3.Expr.expr
    (** [pick_target t branch] is an expression that can be added to a solver from a formula tracker that owns this
        [t] in order to pick the [branch] as a target. *)
  end
  =
  struct
    (* we can pick an AST branch to get a formula *)
    module M = Map.Make (Branch)

    type t = Z3.Expr.expr M.t
    (** [t] maps AST branch to a formula (AND parents) AND (condition OR (AND PARENTS AND (condition OR ...))) *)
    (* When we exit a parent, we AND all the formulas with that parent, and we OR the whole thing with that condition. *)
    (* We only OR it with the formula corresponding to either direction of that AST branch. *)

    let empty = M.empty

    let union (m1 : t) (m2 : t) : t =
      Map.merge m1 m2 ~f:(fun ~key:_ -> function
        | `Both (left, right) -> Some (Solver.SuduZ3.or_ [left; right]) (* OR because both are valid ways to solve for target *)
        | `Left expr | `Right expr -> Some expr
      )

    let exit_parent (map : t) (parent : Branch.Runtime.t) : t =
      let parent_expr = Branch.Runtime.to_expr parent in
      let update_this_side branch map = (* add condition C *)
        branch
        |> Branch.Runtime.to_ast_branch
        |> Map.update map ~f:(function
          | Some expr -> Solver.SuduZ3.or_ [Branch.Runtime.to_expr branch; expr]
          | None -> Branch.Runtime.to_expr branch
        )
      in
      map
      |> Map.map ~f:(fun data -> Solver.SuduZ3.and_ [parent_expr ; data]) (* AND with the parent so that lower conditions must satisfy that parent *)
      |> update_this_side (Branch.Runtime.other_direction parent) (* OR with the condition on the other side to allow as a solvable target *)
      (* ^ note there is no need to add this side as a target because we clearly just hit this side, so it will never be solved for *)

    let pick_target (map : t) (branch : Branch.t) : Z3.Expr.expr =
      match Map.find map branch with
      | Some expr -> expr
      | None -> failwith "no target pick formula known for given branch"
  end

(* Represents all the inputs given in a single run of the interpreter. Naming clashes with Branch_tracker.Input *)
module Input :
  sig
    type t
    val none : t
    val add : t -> Lookup_key.t -> Jayil.Ast.value -> t
    val exit_parent : t -> Branch.Runtime.t -> t
    val merge_or : t -> t -> t
    val merge_and : t -> t -> t
    val to_expr : t -> Z3.Expr.expr
  end
  =
  struct
    type t = Z3.Expr.expr option
    let none : t = None
    
    let add (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
      let is_int_pattern = Riddler.is_pattern key (Jayil.Ast.Int_pattern) in
      let new_formula = (* new formula says that x cannot be the given value *)
        Riddler.eq_term_v key (Some v)
        |> Solver.SuduZ3.not_
        |> fun e -> Solver.SuduZ3.and_ [e; is_int_pattern]
      in
      match x with
      | None -> Some new_formula
      | Some expr -> Some (Solver.SuduZ3.or_ [expr; new_formula])

    let exit_parent (x : t) (parent : Branch.Runtime.t) : t =
      Option.map x ~f:(
        fun expr ->
          Riddler.(Branch.Runtime.to_expr parent @=> expr)
      )

    (* This is used within a single run because we want only one input to be different,
       "this input is different *or* that one is different." *)
    let merge_or (x : t) (y : t) : t =
      match x, y with
      | Some expr, None
      | None, Some expr -> Some expr
      | Some e1, Some e2 -> Some (Solver.SuduZ3.or_ [e1; e2])
      | _ -> None

    (* This is used between runs because we want input formulas from this run *and* the other
       runs to be satisfied. *)
    let merge_and (x : t) (y : t) : t =
      match x, y with
      | Some expr, None
      | None, Some expr -> Some expr
      | Some e1, Some e2 -> Some (Solver.SuduZ3.and_ [e1; e2])
      | _ -> None


    let to_expr (x : t) : Z3.Expr.expr =
      match x with
      | None -> Riddler.true_
      | Some expr -> expr
  end

module Env :
  sig
    type t =
      { parent        : Parent.t (* current "scope" *)
      ; formulas      : Formula_set.t (* always true formulas under this parent *)
      ; pick_formulas : Pick_formulas.t (* pickable formulas *)
      ; input         : Input.t }
    val empty : t
    val create : Branch.Runtime.t -> t
    val add : t -> Z3.Expr.expr -> t
    val add_input : t -> Lookup_key.t -> Jayil.Ast.value -> t
    val exit_to_env : t -> t -> t
  end
  =
  struct
    type t =
      { parent        : Parent.t
      ; formulas      : Formula_set.t
      ; pick_formulas : Pick_formulas.t
      ; input         : Input.t }

    let empty : t =
      { parent        = Parent.Global
      ; formulas      = Formula_set.empty
      ; pick_formulas = Pick_formulas.empty
      ; input         = Input.none }

    let create (branch : Branch.Runtime.t) : t =
      { empty with parent = Parent.of_runtime_branch branch }

    let collect_formulas ({ parent ; formulas ; _ } : t) : Z3.Expr.expr =
      match parent with
      | Global -> Formula_set.and_ formulas
      | Local branch -> Riddler.(Branch.Runtime.to_expr branch @=> Formula_set.and_ formulas)

    let add ({ formulas ; _ } as x : t) (formula : Z3.Expr.expr) : t =
      { x with formulas = Formula_set.add formulas formula }

    let get_picks (branch : Branch.Runtime.t) : Z3.Expr.expr list =
      List.cartesian_product
        [ Branch.Runtime.pick_abort ; Branch.Runtime.pick_max_step ]
        [ branch ; Branch.Runtime.other_direction branch ]
      |> List.map ~f:(fun (pick, branch) ->
          branch
          |> Branch.Runtime.other_direction
          |> Branch.Runtime.to_expr
          |> Riddler.(@=>) (pick branch)
        )

    let add_input ({ input ; _ } as x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
      { x with input = Input.add input key v }

    let exit_to_env (exited : t) (new_env : t) : t =
      match exited with
      | { parent = Global ; _ } -> raise NoParentException
      | { parent = Local exited_branch ; _ } -> begin
        { parent = new_env.parent
        ; formulas =
          exited
          |> collect_formulas
          |> Formula_set.add new_env.formulas
          |> Fn.flip Formula_set.add_multi (get_picks exited_branch)
        ; pick_formulas =
          Pick_formulas.union new_env.pick_formulas
          @@ Pick_formulas.exit_parent exited.pick_formulas exited_branch
        ; input =
          Input.merge_or
            new_env.input
            (Input.exit_parent (exited.input) exited_branch) }
      end

  end

(* Non-empty stack of environments *)
module Env_stack =
  struct
    type t =
      | Last of Env.t (* last is always global parent. Bjy types would be so nice here! *)
      | Cons of Env.t * t

    let empty : t = Last Env.empty

    let cons (hd : Env.t) (tl : t) : t =
      Cons (hd, tl)

    let map_hd (stack : t) ~(f : Env.t -> Env.t) : t =
      match stack with
      | Last last -> Last (f last)
      | Cons (hd, tl) -> Cons (f hd, tl)

    let is_last (stack : t) : bool =
      match stack with
      | Last _ -> true
      | _ -> false
  end

type t = { stack : Env_stack.t }

let empty : t = { stack = Env_stack.empty }

let log_add_formula ({ stack } : t) (formula : Z3.Expr.expr) : unit =
  match stack with
  | Last { parent = Parent.Global ; _ } ->
    Format.printf "ADD GLOBAL FORMULA %s\n" (Z3.Expr.to_string formula)
  | _ -> ()

let add_formula ({ stack } : t) (formula : Z3.Expr.expr) : t =
  (* log_add_formula x formula; *)
  { stack = Env_stack.map_hd stack ~f:(Fn.flip Env.add formula) }
  
let add_key_eq_val (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
  add_formula x @@ Riddler.eq_term_v key (Some v)

let add_alias (x : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
  add_formula x @@ Riddler.eq key1 key2

let add_binop (x : t) (key : Lookup_key.t) (op : Jayil.Ast.binary_operator) (left : Lookup_key.t) (right : Lookup_key.t) : t =
  add_formula x @@ Riddler.binop_without_picked key op left right

let add_input ({ stack } : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
  { stack = Env_stack.map_hd stack ~f:(fun e -> Env.add_input e key v) }

(* TODO: all other types of formulas, e.g. not, pattern, etc, then hide `add_formula` *)

let enter_branch ({ stack } : t) (branch : Branch.Runtime.t) : t =
  { stack = Env_stack.cons (Env.create branch) stack }

let exit_branch ({ stack } : t) : t =
  match stack with
  | Cons ({ parent = Local _ ; _ } as old_hd, Cons (new_hd, tl)) ->
    { stack = Cons (Env.exit_to_env old_hd new_hd, tl) }
  | Cons ({ parent = Local _ ; _ } as old_hd, Last new_hd ) ->
    { stack = Last (Env.exit_to_env old_hd new_hd) }
  | _ -> raise NoParentException (* no parent to back up to because currently in global (or no) scope *)

let is_global ({ stack } : t) : bool =
  Env_stack.is_last stack

let rec exit_until_global (x : t) : t =
  if is_global x
  then x
  else exit_until_global (exit_branch x)

let merge (a : t) (b : t) : t =
  match a.stack, b.stack with
  | Last a, Last b -> 
    { stack = Last
      { parent = Parent.Global
      ; formulas = Formula_set.union a.formulas b.formulas
      ; pick_formulas = Pick_formulas.union a.pick_formulas b.pick_formulas
      ; input = Input.merge_and a.input b.input }
    }
  | _ -> failwith "cannot merge non-global formula trackers"

let input_formula ({ stack } : t) : Z3.Expr.expr =
  match stack with
  | Last env -> Input.to_expr env.input
  | _ -> failwith "cannot get formulas unless in global scope"

let all_formulas
  ?(allow_repeat_inputs : bool = false)
  ({ stack } : t)
  ~(target : Branch.t)
  ~(aborts : Branch.t list)
  ~(max_steps : Branch.t list)
  : Z3.Expr.expr list
  =
  match stack with
  | Cons _ -> failwith "cannot get formulas unless in global scope"
  | Last { formulas ; pick_formulas ; _ } -> 
    let abort_formulas = List.map aborts ~f:Branch.pick_abort in
    let max_step_formulas = List.map max_steps ~f:Branch.pick_max_step in
    let target_formula = Pick_formulas.pick_target pick_formulas target in
    target_formula
    :: (if allow_repeat_inputs then [] else [ input_formula { stack } ])
    @ abort_formulas
    @ max_step_formulas
    @ Formula_set.to_list formulas

(* let abort_formulas ({ stack } : t) (aborts : Branch.t list) : Z3.Expr.expr list =
  match stack with
  | Cons _ -> failwith "cannot get abort formulas unless in global scope"
  | Last { formulas ; pick_formulas ; _ } -> List.map aborts ~f:Branch.pick_abort

let max_step_formulas ({ stack } : t) (max_steps : Branch.t list) : Z3.Expr.expr list =
  match stack with
  | Cons _ -> failwith "cannot get max step formulas unless in global scope"
  | Last { formulas ; pick_formulas ; _ } -> List.map max_steps ~f:Branch.pick_max_step *)
