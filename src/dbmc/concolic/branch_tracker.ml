open Core

[@@@warning "-32"]
[@@@warning "-27"]

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

  Note that pick formulas are effectively the same as adding some formulas to the solver with
  discretion. Instead of having the actual pick, I could just have a map of my own that lets
  me add in whatever formulas I want.
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
              (* TODO: maybe have these separate types instead of variant because it never bounces between the two *)
              (* ^ and then it would type check better *)
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
                | And ls -> And (parent :: ls)
                | Implies (ls, expr) -> Implies (parent :: ls, expr)

              let implies (parent : Parent.t) (expr : Z3.Expr.expr) : Z3.Expr.expr =
                match Parent.to_expr parent with
                | Some parent_expr -> Riddler.(parent_expr @=> expr)
                | None -> expr

              let to_expr (x : t) : Z3.Expr.expr =
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

    module Env :
      sig
        type t =
          { parent            : Parent.t (* current "scope" *)
          ; formulas          : Formula_set.t (* always true formulas under this parent *)
          ; abort_formulas    : Pick_formula.t list (* to be picked to set branch as off limits *)
          ; max_step_formulas : Pick_formula.t list (* to be picked to set branch as off limits *)
          ; target_formulas   : Pick_formula.t list } (* to be picked to solve for target *)

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
          { parent            : Parent.t
          ; formulas          : Formula_set.t
          ; abort_formulas    : Pick_formula.t list
          ; max_step_formulas : Pick_formula.t list
          ; target_formulas   : Pick_formula.t list }

        let empty : t =
          { parent            = Parent.Global
          ; formulas          = Formula_set.empty
          ; abort_formulas    = []
          ; max_step_formulas = []
          ; target_formulas   = [] }

        let create (branch : Branch.Runtime.t) : t =
          { empty with parent = Parent.of_runtime_branch branch }

        (** [collect_formulas x] is all formulas in [x.formulas] implied by [x.parent]. *)
        let collect_formulas ({ parent ; formulas ; _ } : t) : Z3.Expr.expr =
          match parent with
          | Global -> Formula_set.collect formulas
          | Local branch -> Riddler.(Branch.Runtime.to_expr branch @=> Formula_set.collect formulas)

        let add ({ formulas ; _ } as x : t) (formula : Z3.Expr.expr) : t =
          { x with formulas = Formula_set.add formulas formula }

        (* add formula to allow to pick this branch via branch key *)
        let get_branch_pick ({ parent ; _ } : t) : (Pick_formula.t, Parent.t) result =
          match parent with
          | Global -> Error Global (* no way to target global *)
          | Local branch -> Ok (Pick_formula.empty_and branch.branch_key)
            (* TODO: we don't actually want to exit the parent on this because then it implies this direction *)
            (* ^ to be clear, if when we exit this env up to the next one, fi we call Pick_formula.exit_parent, then
               that's wrong. *)
            (* One ugly patch is the Pick_formula has a parent to ignore that doesn't get added to the list, and we make that the immediate parent *)
            (* also this doesn't get called yet *)

        (* get an expression that effectively says "NOT branch". It is "condition = other direction" *)
        let off_limits_expr (branch : Branch.Runtime.t) : Z3.Expr.expr =
          branch.direction
          |> Branch.Direction.other_direction
          |> Branch.Direction.to_value_bool
          |> Option.return
          |> Riddler.eq_term_v branch.condition_key

        (* 
          Add formulas to the abort formulas that let some condition be picked for either direction
          depending on future aborts.

          This is called upon exiting because we are hitting some side of the branch. So acknowledge
          that the condition was used. We don't call upon creating because that would imply the parent
          incorrectly.

          Alternatively, we can accept an input branch to be called when entering that branch and add
          to this env.

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

          We may run into a problem that the stack for actually hitting the other side is different than
          the stack for hitting this side, so these formulas basically do nothing. I'll have to see.

          Just returns two pick formulas
        *)
        let get_abort_pick ({ parent ; _ } : t) : (Pick_formula.t * Pick_formula.t, Parent.t) result =
          (* say that picking this side to find an abort implies the other side must be true *)
          let abort_pick (branch : Branch.Runtime.t) : Pick_formula.t =
            branch
            |> off_limits_expr
            |> Pick_formula.empty_implies (Branch.Runtime.to_abort_pick_key branch)
          in
          match parent with
          | Global -> Error Global
          | Local branch -> Ok (abort_pick branch, abort_pick (Branch.Runtime.other_direction branch))

        let get_max_step_pick ({ parent ; _ } : t) : (Pick_formula.t * Pick_formula.t, Parent.t) result =
          let max_step_pick (branch : Branch.Runtime.t) : Pick_formula.t =
            branch
            |> off_limits_expr
            |> Pick_formula.empty_implies (Branch.Runtime.to_max_step_pick_key branch)
          in
          match parent with
          | Global -> Error Global
          | Local branch -> Ok (max_step_pick branch, max_step_pick (Branch.Runtime.other_direction branch))

        (* This is slow, I think. I should calculated worst case based on max step. *)
        (*
          Worst case is that every step is a branch. So we have two new formulas for every step. And then
          they all get copied and iterated over (but that doesn't increase complexity). And we have to exit
          as many as we entered. Seems just n^2 for entire program in worst case.
          i.e. this function is O(n) where n is size of program, but n is bounded by max step.
        *)
        let exit_to_env (exited : t) (new_env : t) : t =
          let exited_formulas = collect_formulas exited in
          let exit_picks = List.map ~f:(Pick_formula.exit_parent exited.parent) in
          let merge_picks get_picks old_env old_picks new_picks =
            match get_picks with
            | `Two f -> begin
              match f old_env with
              | Ok (a, b) -> a :: b :: exit_picks old_picks @ new_picks
              | _ -> raise NoParentException
            end
            | `One f -> begin
              match f old_env with
              | Ok a -> a :: exit_picks old_picks @ new_picks
              | _ -> raise NoParentException
            end
          in
          { parent            = new_env.parent
          ; formulas          = Formula_set.add new_env.formulas @@ collect_formulas exited
          ; abort_formulas    = merge_picks (`Two get_abort_pick) exited exited.abort_formulas new_env.abort_formulas
          ; max_step_formulas = merge_picks (`Two get_max_step_pick) exited exited.max_step_formulas new_env.max_step_formulas
          ; target_formulas   = merge_picks (`One get_branch_pick) exited exited.target_formulas new_env.target_formulas }

        (* TODO: if exiting to global scope, then finalize all pick formulas *)

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

    (* TODO: all other types of formulas, e.g. not, pattern, etc *)

    let enter_branch ({ stack } : t) (branch : Branch.Runtime.t) : t =
      { stack = Env.create branch :: stack }

    let exit_branch ({ stack } : t) : t =
      match stack with
      | { parent = Local _ ; _ } as old_hd :: new_hd :: tl ->
        (* Format.printf "exiting branch and collecting formulas. Formula is %s\n" (Env.collect old_hd |> Z3.Expr.to_string); *)
        { stack = Env.exit_to_env old_hd new_hd :: tl }
      | _ -> raise NoParentException (* no parent to back up to because currently in global (or no) scope *)

  end

(*
  The branch tracker will hold results of all runtime branches and send to solvers.

  See discussion above runtime on the pick vs map scenario
*)