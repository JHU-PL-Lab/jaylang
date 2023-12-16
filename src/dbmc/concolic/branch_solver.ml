open Core

module Lookup_key = 
  struct
    include Lookup_key
    (* Core.Map.Key expects t_of_sexp, so provide failing implementation *)
    let t_of_sexp _ = failwith "Lookup_key.t_of_sexp needed and not implemented"
  end

module Make_list_store (Key : Map.Key) (T : sig type t end) =
  struct
    module M = Map.Make (Key)
    type t = T.t list M.t
    let empty : t = M.empty
    let find_default (map : t) (key : Key.t) : T.t list =
      key
      |> Map.find map
      |> Option.value ~default:[]
    let add (map : t) (key : Key.t) (x : T.t) : t =
      Map.update
        map
        key
        ~f:(function
          | None -> [ x ]
          | Some ls -> x :: ls
        )
  end

(* module Runtime_branch =
  struct
    type t =
      { condition_key : Condition_key.t
      ; direction     : Ast_branch.Direction.t }
      [@@deriving compare, sexp]

    let to_expr ({condition_key ; direction} : t) : Z3.Expr.expr =
      Riddler.eqv condition_key (Ast_branch.Direction.to_value_bool direction)

    (* FIXME: this is wrong because an AST branch is the clause key, not the condition key *)
    (* let to_branch ({ condition_key ; direction} : t) : Ast_branch.t =
      Ast_branch.{ branch_ident = condition_key.x ; direction } *)

    let other_direction (x : t) : t =
      { x with direction = Ast_branch.Direction.other_direction x.direction }
  end *)

(* module Target =
  struct
    type t =
      { branch_key : Lookup_key.t
      ; branch     : Runtime_branch.t }

    let to_string ({ branch_key ; branch } : t) : string =
      Lookup_key.to_string branch_key
      ^ "; condition: "
      ^ Lookup_key.to_string branch.condition_key
      ^ " = "
      ^ Ast_branch.Direction.to_string branch.direction

    let print_option (x : t option) : unit =
      let target_branch_str = 
        match x with 
        | None -> "None"
        | Some target -> to_string target
      in 
      Format.printf "\nTarget branch: %s\n" target_branch_str

    let to_branch ({ branch_key ; branch = { direction ; _ } } : t) : Ast_branch.t =
      Ast_branch.{ branch_ident = branch_key.x ; direction }

    let other_direction (x : t) : t =
      { x with branch = Runtime_branch.other_direction x.branch }

  end *)

(*
  The parent is some condition key and the direction that it
  evaluates.

  The name of the condition variable and the call stack uniquely
  make a lookup key that identifies the parent.

  Usage:
    Every variable in the program that is not in the global scope or
    a function environment has a parent branch. The direction the parent
    evaluates to determines if the variable is reachable or not.

    e.g.
    c = true;
    x = c ? (
      y = 0
    ) : (
      z = 0
    )
    Here y has parent (c, true), and z has parent (c, false). z is only 0
    if c evaluates to false.
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
      
  end

(*
  The parent store tracks all the parent conditions that a variable depends on, but
  none greater than its immediate parent.

  e.g.
    cond = true;
    x = cond ? (
      inner_condition = false;
      inner_result = inner_condition ? (
        not_reached = 0;
      ) : (
        reached = 1;
      )
      w = 1;
      z = inner_result + w;
    )
    Then `reached` depends on `inner_condition` == false, and
    `z` depends on `inner_result`, which is an alias for `reached`.
    So `z` gains `inner_condition` == false as a parent because it
    depends on `inner_condition`, but it won't gain any parents that are
    parents on `cond`.
*)
module Parent_store = Make_list_store (Lookup_key) (Parent)

module Z3_expr =
  struct
    include Z3.Expr
    type t = Z3.Expr.expr
  end

(*
  This module stores formulas that are underneath some condition.
  Maps a parent to the formulas.
*)
module Formula_store = Make_list_store (Parent) (Z3_expr)

(*
  Keeps all the data about the program:
  * The formulas underneath each parent condition
  * The parents of each node   

  I modularize a few of the more simple functions under this.
*)
module Store =
  struct
    type t =
      { fstore : Formula_store.t
      ; pstore : Parent_store.t }

    let empty : t =
      { fstore = Formula_store.empty
      ; pstore = Parent_store.empty }

    let add_parent (child_key : Lookup_key.t) (parent : Parent.t) (store : t) : t =
      { store with pstore = Parent_store.add store.pstore child_key parent }

    let add_formula (key : Parent.t) (formula : Z3.Expr.expr) (store : t) : t =
      { store with fstore = Formula_store.add store.fstore key formula }

    let remove_formulas (key : Parent.t) (store : t) : t =
      { store with fstore = Map.remove store.fstore key }

    (* Puts all formulas in the global scope into a new solver and returns the solver *)
    (* TODO: maybe assert that all conditions are cleared to signify that evaluation is done. *)
    let to_solver (store : t) : Z3.Solver.solver =
      (* create a blank solver to add all the formulas *)
      let new_solver = Z3.Solver.mk_solver Solver.SuduZ3.ctx None in
      begin
      Parent.Global
      |> Formula_store.find_default store.fstore
      |> Z3.Solver.add new_solver 
      end;
      new_solver
  end

type t = Store.t
let empty = Store.empty

(*
  Generate the parent dependencies of all the children in the list, and return an
  expression list for the parents, and a list of the parents as keys.

  The returned lists are well-described by an example.
    Say a child key depends on two parents (P1, true), (P2, false), i.e.
    the parent key P1 must be true to reach the child key, and P2 must be false.
    So the expression list is
      [ (P1 == true) ; (P2 == false) ]
    and the parent key list is
      [ P1 ; P2 ]
    If multiple children are passed in, then the returned lists are the concatenation
    of all the lists for each child.

  A node might have multiple "parents" if it depends on some variable with a
  different parent. Then we just claim that node has as parents both its "real"
  parent and the parent(s) of the node it depends on. See the comment above `parent_store`
  above.
*)
let gen_parents
  (children : Lookup_key.t list)
  (store : Store.t)
  : (Z3.Expr.expr list) * (Lookup_key.t list)
  =
  let open List.Let_syntax in
  (* all parents of all children as found in the parent store *)
  let parents = children >>= Parent_store.find_default store.pstore
  in
  (parents >>| Parent.to_expr) (* make expression from key and direction *)
  , (parents >>= Fn.compose Option.to_list Parent.to_condition_key) (* keep only the key, and empty for Global *)

(*
  Given some children nodes, get a formula that says the parents
  imply the given formula.
  
  So if children have some list of expressions implied by their parents, then
  then that list is some formula, say f0. What this returns is f0 => formula,
  where formula is given.

  "Recursively create right associative chain of implications"

  We'll get this
    (root => (children_of_root => ... (grandparents => (parents => (children => given_formula)))))
  where "=>" literally means "implies", and root is the highest known parent
  that is tracked. It will rarely be the real root.

  This adds implications by the parents until there are no parents found (this is exactly
  when the parent of the current branch is reached.)

  Arguments:
    [dependencies] are any lookup keys that the formula depends on.
    [store] is the store thus far.
    [formula] is the formula to which we add any dependencies.
*)
let rec gen_implied_formula
  (dependencies : Lookup_key.t list)
  (store : Store.t)
  (formula : Z3.Expr.expr)
  : Z3.Expr.expr
  =
  match gen_parents dependencies store with
  | [], _ -> formula (* Logically, the parents "_" must be empty if no expressions *)
  | exps, parent_keys -> (* if no parent keys because is global, then next iteration does nothing *)
      Riddler.(and_ exps @=> formula) (* all the expressions imply the formula *)
      |> gen_implied_formula parent_keys store

(*
  Say that the parent implies the given formula. Use the children as
  context to find all the necessary parents that imply the formula.
  It's common that no children are provided because it's known that the
  formula is immediate and relies on no branches.
  
  See `gen_implied_formula`, and note that the added formula takes into
  account the truth of all the parents above it but below the given parent_opt.

  Arguments:
    [dependencies] are any lookup keys that the formula depends on.
    [parent] is the condition and direction of the current branch, or Global
    [formula] is the expression to add as a formula inside the current branch.
    [store] holds all the information in the evaluation thus far.
*)
let add_formula
  (dependencies : Lookup_key.t list)
  (parent : Parent.t)
  (formula : Z3.Expr.expr)
  (store : Store.t)
  : Store.t
  =
  (*
    Get a formula that says the parents of the children imply the given formula.
    This is necessary because if the formula includes anything that depends on an
    inner branch, then the branch direction matters.
  *)
  let implied_formula = gen_implied_formula dependencies store formula in
  begin
    match parent with
    | Global ->
      (* No parent condition, so this is a global formula and is always true. *)
      (* Print that a formula is getting added to global scope *)
      Printf.printf "ADD GLOBAL FORMULA %s\n" (Z3.Expr.to_string implied_formula)
    | Local branch -> ()
  end;
  Store.add_formula parent implied_formula store

(*
  Say that some variable (key) is just equal to a value (which necessarily depends on nothing).

  TODO: change `eq_term_v` so that doesn't need an option.
*)
let add_key_eq_val
  (parent : Parent.t)
  (key : Lookup_key.t)
  (v : Jayil.Ast.value)
  (store : Store.t)
  : Store.t
  =
  (* no children to consider because values have no dependencies *)
  add_formula [] parent (Riddler.eq_term_v key (Some v)) store
  
(*
  Say that the child now acquires all the same parents as the siblings.
  The siblings' parents are *added* to the parents that already exist; the
  new parents do not replace the existing ones.

  Sean has "add_parents2_"
*)
let add_siblings
  (child_key : Lookup_key.t)
  (siblings : Lookup_key.t list)
  (store : Store.t)
  : Store.t
  =
  let open List.Let_syntax in
  let new_parents = siblings >>= Parent_store.find_default store.pstore in
  let pstore =
    (* May prefer to loop with Store.add_parent for sake of modularism *)
    Map.update store.pstore child_key ~f:(function
      | None -> new_parents
      | Some cur_parents -> cur_parents @ new_parents
      )
  in
  { store with pstore }

(*
  Recursively gets the list of all parents for the children, where the parents
  are expressed as expressions (i.e. they "equal" the direction taken to the child).

  Sean has "get_dependencies" here.
*)
let get_all_parent_dependencies
  (children : Lookup_key.t list)
  (store : Store.t)
  : Z3.Expr.expr list
  =
  let rec loop acc = function
    | [] -> acc
    | children ->
      let exps, parents = gen_parents children store in
      loop (exps @ acc) parents
      (* ^ hopefully exps is small compared to acc, and this isn't slow *)
      (* Note that this might just run once if I flatten parents properly, so exps is large compared to acc, but still small on grand scale *)
  in
  loop [] children

(*
  Add a pickable branch such that we can later choose to solve for it.
  i.e.
    Add the formula that all the parents of the branch are implied by the branch
    key, thus forcing the solver to consider them true when finding a solution for
    the branch.

  Note: at the point of calling this function, the branch_key might have its associated
    condition key as a "parent", but this condition might not want to be satisfied in case
    we are solving for the other direction. We consequently use the parent to recursively
    find the parents.

  [branch_key] the key for the clause of the branch that can be picked.
  [parent] the parent of the branch that is to be picked.
*)
let add_pick_branch
  (branch_key : Lookup_key.t)
  (parent : Parent.t)
  (store : Store.t)
  : Store.t
  =
  let deps = 
    match parent with
    | Global -> []
    | Local branch -> Parent.to_expr parent :: get_all_parent_dependencies [branch.condition_key] store
  in
  (* let deps = get_all_parent_dependencies [branch_key] store in *)
  let pick_formula = Riddler.(picked branch_key @=> and_ deps) in
  Format.printf "ADD PICK FORMULA: %s\n" (Z3.Expr.to_string pick_formula);
  add_formula [] Global pick_formula store (* A picked formula will be true under global scope *)

(*
  Accumulates all the formulas under the branch that was just evaluated
  and assigns them under the parent; these formulas become one formula that
  is *implied* by the exited branch.

  Sets branch_key to be a sibling of result_key.

  Clears out all formulas under condition_key because it cannot be entered again and
  is encompassed under the first step.

  Arguments:
    [parent] the parent branch of the branch_key. This is NOT the condition and
      direction that evaluates to the branch result.
    [exited_branch] the branch (i.e. parent) that was just evaluated and is being exited.
    [result_key] the key of the last clause in the branch. Gets assigned to branch_key
    [store] ..
*)
let exit_branch
  (parent : Parent.t)
  (exited_branch : Branch.Runtime.t)
  (result_key : Lookup_key.t)
  (store : Store.t)
  : Store.t
  =
  let exited_parent = Parent.Local exited_branch in
  match Map.find store.fstore exited_parent with
  | None -> store (* nothing happened under the branch, so do nothing *) 
  | Some exps ->
    let antecedent = Branch.Runtime.to_expr exited_branch in
    (* The branch implies all the expressions within it *)
    let implication = Riddler.(antecedent @=> and_ exps) in
    add_formula [exited_branch.condition_key] parent implication store (* formula depends on exited branch's condition key *)
    |> Store.remove_formulas exited_parent (* clear out formulas under exited branch because they're not needed anymore *)
    |> Store.add_parent exited_branch.branch_key exited_parent (* add the exited branch as a parent *)
    |> add_siblings exited_branch.branch_key [result_key] (* branch_key now depends on everything the result depends on *)
    |> add_pick_branch exited_branch.branch_key parent

(* See https://github.com/Z3Prover/z3/blob/master/src/api/ml/z3.mli line 3290 *)
let solve_for_target
  (target : Branch.Runtime.t)
  (store : Store.t)
  : (Z3.Model.model, Branch.Ast_branch.t) result
  =
  (* Say that we're picking this branch, so all the parents that it was said to imply will now get implied. *)
  let picked_branch_formula = Riddler.picked target.branch_key in
  let condition_formula = Branch.Runtime.to_expr target in
  let solver =
    store
    (* |> pick_branch target.branch_key *) (* with parent commented out *)
    |> Store.to_solver
  in
  (* let solver = Store.to_solver store in *)
  Format.printf "Solving for target branch:\n";
  Format.printf "Branch to pick: %s\n" (Z3.Expr.to_string picked_branch_formula);
  Format.printf "Branch condition: %s\n" (Z3.Expr.to_string condition_formula);
  [ picked_branch_formula ; condition_formula ] (* will check if these are consistent with other formulas in the solver *)
  |> Z3.Solver.check solver (* returns status of "satisfiable" or not *)
  |> Solver.SuduZ3.get_model solver 
  |> function
    | Some model -> Ok model
    | None -> Error (Branch.Runtime.to_ast_branch target)

let get_feeder
  (target : Branch.Runtime.t)
  (store : Store.t)
  : (Concolic_feeder.t, Branch.Ast_branch.t) result
  =
  solve_for_target target store
  |> Result.map ~f:Concolic_feeder.from_model