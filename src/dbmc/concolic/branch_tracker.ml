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
        (* module type S =
          sig
            type t
          end

        module Abort : S
        module Max_step : S
        module Target : S *)

        (* TODO: have a "finished" variant where the formulas are now expr *)
        type t
          (* { abort    : Abort.t
          ; max_step : Max_step.t
          ; target   : Target.t } *)

        val empty : t

        val exit_parent : t -> Branch.Runtime.t -> t
        (** [exit_parent t parent] steps the tracker [t] out of the [parent], adjusting all formulas
            as necessary and adding pick formulas for the exited parent. *)

        val merge : t -> t -> t
        (** [merge a b] contains all info from [a] and [b] *)
        
        (*
          TODO: get formulas. It may be prefered that this is delegated to non runtime versions.
            But I can keep a runtime version that is always in global state, I guess, even though
            this doesn't track state.
        *)
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
            val add_pick : t -> Branch.Runtime.t -> t
            (** [add_pick t parent] is a new t that has formulas pickable by [parent]. *)
            val to_formula : t -> Branch.Runtime.t -> Z3.Expr.expr
            (** [to_formulas t parent] is a combination of formulas in [t] that are picked by [parent]. *)
            val exit_parent : t -> Branch.Runtime.t -> t
            (** [exit_parent t parent] adds the [parent] to all formulas within, and calls [add_pick t parent] *)
            val merge : t -> t -> t
            (** [merge a b] contains all info from [a] and [b] *)
          end

        module M = Map.Make (Lookup_key)
        
        let merge_m (m1 : 'a list M.t) (m2 : 'a list M.t) : 'a list M.t =
          Map.merge m1 m2 ~f:(fun ~key:_ -> function
            | `Both (left, right) -> Some (left @ right)
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
        module Make_implies (P : sig val pick : Branch.Runtime.t -> Lookup_key.t end) : S =
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
                Map.add_multi ~key:(P.pick branch) ~data:(pick_formula branch)
              in
              map
              |> add branch
              |> add (Branch.Runtime.other_direction branch)

            let to_formula (map : t) (branch : Branch.Runtime.t) : Z3.Expr.expr =
              match Map.find map (P.pick branch) with
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

        module Abort    : S = Make_implies (struct let pick = Branch.Runtime.to_abort_pick_key end)
        module Max_step : S = Make_implies (struct let pick = Branch.Runtime.to_max_step_pick_key end)

        (*
          Track targets by letting the condition be satisfied. This takes the "or" of all
          targets, so when getting formulas for any AST branch, all known runtime instances
          of that branch are considered.
        *)
        module Target : S =
          struct
            type t = Formula.And.t list M.t

            let empty = M.empty

            let pick = Branch.Runtime.to_target_pick_key

            (* add formulas for both sides to be picked *)
            let add_pick (map : t) (branch : Branch.Runtime.t) : t =
              let pick_formula (branch : Branch.Runtime.t) : Formula.And.t =
                branch
                |> Branch.Runtime.to_expr (* say that this branch must be satisfied *)
                |> Formula.And.empty (* note that this parent is not implied *)
              in
              let add (branch : Branch.Runtime.t) : t -> t =
                Map.add_multi ~key:(pick branch) ~data:(pick_formula branch)
              in
              map
              |> add branch
              |> add (Branch.Runtime.other_direction branch)

            let to_formula (map : t) (branch : Branch.Runtime.t) : Z3.Expr.expr =
              match Map.find map (pick branch) with
              | Some formula_list ->
                formula_list
                |> List.map ~f:Formula.And.to_expr
                |> Solver.SuduZ3.or_ (* TODO: add `or_` to `Riddler` *)
              | None -> failwith "no \"and\" pick formulas found for parent branch" (* should be impossible if used correctly *)

            let exit_parent (map : t) (parent : Branch.Runtime.t) : t =
              map
              |> Map.map ~f:(List.map ~f:(Formula.And.exit_parent parent))
              |> Fn.flip add_pick parent

            let merge = merge_m
          end

        type t =
          { abort    : Abort.t
          ; max_step : Max_step.t
          ; target   : Target.t }

        let empty =
          { abort    = Abort.empty 
          ; max_step = Max_step.empty
          ; target   = Target.empty }

        let merge (a : t) (b : t) : t = 
          { abort    = Abort.merge a.abort b.abort
          ; max_step = Max_step.merge a.max_step b.max_step 
          ; target   = Target.merge a.target b.target }

        let exit_parent ({ abort ; max_step ; target } : t) (parent : Branch.Runtime.t) : t =
          { abort    = Abort.exit_parent abort parent
          ; max_step = Max_step.exit_parent max_step parent
          ; target   = Target.exit_parent target parent }
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

        (* This is slow, I think. I should calculated worst case based on max step. *)
        (*
          Worst case is that every step is a branch. So we have two new formulas for every step. And then
          they all get copied and iterated over (but that doesn't increase complexity). And we have to exit
          as many as we entered. Seems just n^2 for entire program in worst case.
          i.e. this function is O(n) where n is size of program, but n is bounded by max step.
        *)
        let exit_to_env (exited : t) (new_env : t) : t =
          match exited with
          | { parent = Global ; _ } -> raise NoParentException
          | { parent = Local exited_branch ; _ } -> begin
            { parent = new_env.parent
            ; formulas = Formula_set.add new_env.formulas @@ collect_formulas exited
            ; pick_formulas =
              Pick_formulas.merge new_env.pick_formulas
              @@ Pick_formulas.exit_parent exited.pick_formulas exited_branch
            }
          end

        (* TODO: if exiting to global scope, then finalize all pick formulas ? *)

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