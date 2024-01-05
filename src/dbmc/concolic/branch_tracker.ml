open Core

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

(*
  The runtime branch tracker is used alongside the interpreter to build up the relationships
  of runtime branches and formulas.

  The runtime tracker will keep a functional state that is its current parent (global program
  scope or a local branch), and when it exits that parent, it collects up all formulas it found
  into the next level.

  The user just needs to add formulas and enter/exit branches. The runtime branch tracker does
  the rest.
*)
module Runtime = 
  struct

    (* This will not be raised if the tracker is used properly *)
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

        let to_expr (parent : t) : Z3.Expr.expr option =
          match parent with
          | Global -> None (* Global scope is trivial parent. Could use `true` instead *)
          | Local branch -> Some (Branch.Runtime.to_expr branch)
      end

      module Pick_formula =
        struct
          module Formula :
            sig
              type t =
                | And of Parent.t list (* to "and" all the dependencies -- for picking a branch *)
                | Implies of Parent.t list * Z3.Expr.expr (* to imply a formula down the line -- for setting a branch off limits *)
              val to_expr : t -> Z3.Expr.expr
              val exit_parent : Parent.t -> t -> t
            end
            =
            struct
              type t =
                | And of Parent.t list (* order of parents doesn't matter *)
                | Implies of Parent.t list * Z3.Expr.expr (* deepest parent is last in list *)

              let exit_parent (parent : Parent.t) (x : t) : t =
                match x with
                | And ls -> And (x :: ls)
                | Implies (ls, expr) -> Expr (x :: ls, expr)

              let implies (parent : Parent.t) (expr : Z3.Expr.expr) : Z3.Expr.expr =
                match Parent.to_expr parent with
                | Some parent_expr -> Riddler.(parent_expr @=> expr)
                | None -> expr

              let rec to_expr (x : t) : Z3.Expr.expr =
                match x with 
                | And parents ->
                  parents
                  |> List.filter_map ~f:Parent.to_expr
                  |> Riddler.and_
                | Implies (ls, expr) -> 
                  List.fold_right ls ~init:expr ~f:implies
            end

          (* Maybe have a map from pick_key to multiple formulas so fewer are added *)
          type t =
            { pick_key : Lookup_key.t (* used to make the formula true *)
            ; formula  : Formula.t } 
          
          let empty_implies (pick_key : Lookup_key.t) (expr : Z3.Expr.expr) : t =
            { pick_key
            ; formula = Formula.Implies ([], expr) }

          let empty_and (pick_key : Lookup_key.t) : t =
            { pick_key
            ; formula = Formula.And [] }

          let to_expr ({ pick_key ; formula } : t) : Z3.Expr.expr =
            Riddler.(picked pick_key @=> Formula.to_expr formula)

          let exit_parent (parent : Parent.t) (x : t) : t =
            { x with formula = Formula.exit_parent parent x.formula }
        end

    module Env =
      struct
        type t =
          { parent            : Parent.t (* current "scope" *)
          ; formulas          : Formula_set.t (* always true formulas under this parent *)
          ; abort_formulas    : Pick_formula.t list (* to be picked to set branch as off limits *)
          ; max_step_formulas : Pick_formula.t list (* to be picked to set branch as off limits *)
          ; target_formulas   : Pick_formula.t list } (* to be picked to solve for target *)

        let empty : t =
          { parent            = Parent.Global
          ; formulas          = Formula_set.empty
          ; abort_formulas    = []
          ; max_step_formulas = []
          ; target_formulas   = [] }

        let create (branch : Branch.Runtime.t) : t =
          { empty with parent = Parent.of_runtime_branch branch }

        (** [collect x] is all formulas in [x.formulas] implied by [x.parent]. *)
        let collect ({ parent ; formulas ; _ } : t) : Z3.Expr.expr =
          match parent with
          | Global -> Formula_set.collect formulas
          | Local branch -> Riddler.(Branch.Runtime.to_expr branch @=> Formula_set.collect formulas)

        let add ({ formulas ; _ } as x : t) (formula : Z3.Expr.expr) : t =
          { x with formulas = Formula_set.add formulas formula }

        (* add formula to allow to pick this branch via branch key *)
        let add_pick_branch ({ parent ; target_formulas ; _ } as x : t) : t =
          match parent with
          | Global -> x (* no way to target global *)
          | Local branch ->
            { x with target_formulas =
              Pick_formula.empty_and branch.branch_key :: target_formulas
            }
            (* TODO: we don't actually want to exit the parent on this because then it implies this direction *)
            (* ^ to be clear, if when we exit this env up to the next one, fi we call Pick_formula.exit_parent, then
               that's wrong. *)
            (* One ugly patch is the Pick_formula has a parent to ignore that doesn't get added to the list, and we make that the immediate parent *)
            (* also this doesn't get called yet *)

        (* 
          Add formulas to the abort formulas that let some condition be picked for either direction
          depending on future aborts.

          This is to be called for any branches that are in the clause list for this environment.

          I need to think about this logic a little bit more because it's different from max
          step. If this branch contains an abort that is not in a sub branch, then we will always
          hit that abort, no matter the recursive depth. So if we ever go down the branch containing
          abort, we can be sure we're hitting it, and we just need to pick a formula that says
          all other runtime instances of that *condition* go do the other direction. 

          So if this branch has condition ident c, we need all condition keys for c to be the
          other direction. However, they still need to be implied by all the parents just like any
          other formula, so that's why we have the Pick_formula.Formula.Implies (parent_list, c = other dir).
          So basically every time we come across any key ever being used as a branch condition, we
          add the condition to be the other direction as pickable upon abort.

          [x] is current environment, and [branch] is what we're about to enter. We add formulas
          concerning [branch].
        *)
        let add_condition_for_abort_pick (x : t) (branch : Branch.Runtime.t) : t =
          (* say that picking this side to find an abort implies the other side must be true *)
          let abort_pick (branch : Branch.Runtime.t) : Pick_formula.t =
            branch.direction
            |> Branch.Direction.other_direction
            |> Branch.Direction.to_value_bool
            |> Branch.Runtime.other_direction
            |> Option.return
            |> Riddler.eq_term_v branch.condition_key
            |> Pick_formula.empty_implies (Branch.Runtime.to_abort_pick_key branch)
          in
          { x with abort_formulas =
            abort_pick branch :: abort_pick (Branch.Runtime.other_direction branch) :: x.abort_formulas
          }

        (* TODO max step *)

      end

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
    type t = { stack : Env.t list }

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

    let enter_branch ({ stack } : t) (branch : Branch.Runtime.t) : t =
      match stack with
      | hd :: tl ->
        { stack = Env.create branch :: Env.add_condition_for_abort_pick hd branch :: tl }
      | _ -> raise NoParentException

    let exit_branch ({ stack ; pick_formulas ; _ } as x : t) : t =
      match stack with
      | { parent = Local exited_branch as p ; _ } as old_hd :: new_hd :: tl ->
        (* Format.printf "exiting branch and collecting formulas. Formula is %s\n" (Env.collect old_hd |> Z3.Expr.to_string); *)
        { stack = Env.add new_hd (Env.collect old_hd) :: tl } (* definitely fails to type check right now *)
      | _ -> raise NoParentException (* no parent to back up to because currently in global (or no) scope *)



  end