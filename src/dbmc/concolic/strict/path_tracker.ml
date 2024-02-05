open Core

(*
  We have a tree that holds formulas, and branches occur when the interpreter takes some side of a conditional.   
  At the top of every node is the formula "this parent condition = this direction"
*)

module Formula_set :
  sig
    type t
    val empty : t
    val singleton : Z3.Expr.expr -> t
    val add : t -> Z3.Expr.expr -> t
    val add_multi : t -> Z3.Expr.expr list -> t
    val union : t -> t -> t (* only used in V1 *)
    val to_list : t -> Z3.Expr.expr list
    val and_ : t -> Z3.Expr.expr
    (* val or_ : t -> Z3.Expr.expr *) (* only used in V1 *)
  end
  =
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

    let empty = S.empty
    let singleton = S.singleton
    let add = Set.add
    let add_multi (s : t) = List.fold ~init:s ~f:add
    let union = Set.union
    let to_list = Set.to_list

    let and_ (fset : t) : Z3_expr.t =
      match Set.to_list fset with
      | [] -> Riddler.true_
      | exp :: [] -> exp
      | exps -> Riddler.and_ exps

    (* let or_ (fset : t) : Z3_expr.t =
      match Set.to_list fset with
      | [] -> Riddler.true_
      | exp :: [] -> exp
      | exps -> Solver.SuduZ3.or_ exps *)
  end

module rec Node_base : (* serves as root node *)
  sig
    type t =
      { formulas : Formula_set.t
      ; children : Children.t }
    (** [t] is the root of the JIL program. *)
    val empty : t
    (** [empty] is the tree before the program has ever been run. It has no formulas or children. *)
    (* val add_child : t -> Branch.Runtime.t -> t *)
    val merge : t -> t -> t
    (** [merge a b] combines the trees [a] and [b] and throws an exception if there is a discrepancy. *)
    val add_formula : t -> Z3.Expr.expr -> t
    (** [add_formulas t expr] is [t] that has gained [expr] as a formula. *)
  end
  =
  struct
    type t =
      { formulas : Formula_set.t
      ; children : Children.t }

    let empty : t =
      { formulas = Formula_set.empty
      ; children = Children.empty }

    (* let add_child (x : t) (branch : Branch.Runtime.t) : t =
      { x with children = Children.add_child x.children branch } *)

    let merge (a : t) (b : t) : t =
      { formulas = Formula_set.union a.formulas b.formulas (* TODO: assert they are equal *)
      ; children = Children.merge a.children b.children }

    let add_formula (x : t) (expr : Z3.Expr.expr) : t =
      { x with formulas = Formula_set.add x.formulas expr }
  end
and Children :
  sig
    type t
    (** [t] represents the branches underneath some node. *)
    val empty : t
    (** [empty] is no children. *)
    (* val is_empty : t -> bool *)
    (* val of_branch : Branch.Runtime.t -> t *)
    (* val add_child : t -> Branch.Runtime.t -> t *)
    val of_node : Node.t -> t
    (** [of_node] makes a child out of the node. *)
    val merge : t -> t -> t
    (** [merge a b] merges all children in [a] and [b]. *)
  end
  =
  struct
    type t = 
      | No_children
      | Both of { true_side : Status.t ; false_side : Status.t }
      (* Could have chosen to have only true or only false, but Status.Unsolved takes care of that. *)
    
    let empty : t = No_children
    (* let is_empty (x : t) : bool =
      match x with
      | No_children -> true
      | _ -> false *)

    let of_node ({ this_branch ; base } as node : Node.t) : t =
      let new_node = Status.Hit node in
      match this_branch with
      | { direction = True_direction ; _ } -> Both { true_side = new_node ; false_side = Unsolved }
      | { direction = False_direction ; _ } -> Both { true_side = Unsolved ; false_side = new_node }

    (* let of_branch (branch : Branch.Runtime.t) : t =
      of_node
      @@ Node.of_parent_branch branch *)

    (* let add_child (x : t) (branch : Branch.Runtime.t) : t =
      match x with
      | No_children -> of_branch branch
      | _ -> failwith "unimplemented" *)

    let merge (a : t) (b : t) : t =
      match a, b with
      | No_children, x
      | x, No_children -> x
      | Both a, Both b ->
        Both
        { true_side = Status.merge a.true_side b.true_side
        ; false_side = Status.merge a.false_side b.false_side }

  end
and Status :
  sig
    type t =
      | Hit of Node.t
      | Unsatisfiable
      | Unknown (* for timeouts *)
      | Unsolved (* not yet tried *)
    (** [t] is a node during a solve. It has been hit, determined unsatisfiable,
        is not known if hittable or unsatisfiable, or has not been solved or seen yet.
        Unsatisfiable or Unknown nodes are status of the node before they've ever been
        hit during interpretation, so there is no existing node as a payload. *)
      
    val merge : t -> t -> t
    (** [merge a b] keeps the most information from [a] or [b] and merges the nodes if both are [Hit]. *)
  end
  =
  struct
    type t =
      | Hit of Node.t
      | Unsatisfiable
      | Unknown (* for timeouts *)
      | Unsolved (* not yet tried *)

    (*
      Merge by keeping the most info.
      * It is most information to know that we have hit a node. Merge the nodes if necessary.
      * Next is to have solved and determined unsatisfiable
      * After that is solved by timed out, so unknown
      * After that is completely unsolved, which is no information at all
    *)
    let merge (a : t) (b : t) : t =
      match a, b with
      | Hit n1, Hit n2 -> Hit (Node.merge n1 n2)
      | Hit node, _ | _, Hit node -> Hit node
      | Unsatisfiable, _ | _, Unsatisfiable -> Unsatisfiable
      | Unknown, _ | _, Unknown -> Unknown
      | Unsolved, _ -> Unsolved
  end
and Node :
  sig
    type t =
      { this_branch : Branch.Runtime.t
      ; base : Node_base.t }
    (** [t] is a node in the tree that is reached by taking [this_branch]. It has formulas
        and children as in [base]. *)
    
    val of_parent_branch : Branch.Runtime.t -> t
    (** [of_parent_branch branch] is an empty node with label [this_branch = branch]. *)
    val to_children : t -> Children.t
    (** [to_children t] is the children in [t]. *)
    val merge : t -> t -> t
    (** [merge a b] merges the nodes at [a] and [b] and merges the subtrees of their children.
        Throws an exception if [a.this_branch] and [b.this_branch] are unequal. *)
    val with_formulas : t -> Formula_set.t -> t
    (** [with_formulas t formulas] is [t] where [t.base] has its formulas overwritten with [formulas]. *)
  end
  =
  struct
    type t =
      { this_branch : Branch.Runtime.t
      ; base : Node_base.t }
    
    let of_parent_branch (this_branch : Branch.Runtime.t) : t =
      { this_branch ; base = Node_base.empty }

    let to_children (node : t) : Children.t =
      Children.of_node node

    let merge (a : t) (b : t) : t =
      if Branch.Runtime.compare a.this_branch b.this_branch <> 0 
      then failwith "trying to merge nodes of a different branch"
      else { this_branch = a.this_branch ; base = Node_base.merge a.base b.base }

    let with_formulas (x : t) (formulas : Formula_set.t) : t =
      { x with base = { x.base with formulas } }

  end

(* This is just for better naming *)
module Root = Node_base

(*
  I will want to solve for targets as they are on the node stack, updating their statuses inside
  the stack. Then, when we hit a satisfiable branch, we merge with the total tree and begin a new run.   

*)
module Node_stack :
  sig
    type t
    (** [t] is a nonempty stack of nodes where the bottom of the stack is a root node. *)
    val empty : t
    (** [empty] has an empty root. *)
    val map_hd : t -> f:(Node_base.t -> Node_base.t) -> t
    (** [map_hd t ~f] maps the head node base of [t] using [f]. *)
    val merge_with_tree : t -> Root.t -> Root.t
    (** [merge_with_tree t root] creates a tree from the stack [t] and merges with the tree given by [root]. *)
    val push : t -> Branch.Runtime.t -> t
    (** [push t branch] pushes the [branch] onto the stack [t] with a copy of all the formulas already on the stack. *)
    val add_formula : t -> Z3.Expr.expr -> t
    (** [add_formula t expr] is [t] where the top node on the stack has gained the formula [expr]. *)
    val add_key_eq_val : t -> Lookup_key.t -> Jayil.Ast.value -> t
    (** [add_key_eq_val t k v] adds the formula that [k] has value [v] in the top node of [t]. *)
    val add_alias : t -> Lookup_key.t -> Lookup_key.t -> t
    (** [add_alias t k k'] adds the formula that [k] and [k'] hold the same value in the top node of [t]. *)
    val add_binop : t -> Lookup_key.t -> Jayil.Ast.binary_operator -> Lookup_key.t -> Lookup_key.t -> t
    (** [add_binop t x op left right] adds the formula that [x = left op right] to the the top node of [t]. *)
    val add_input : t -> Lookup_key.t -> Jayil.Ast.value -> t
    (** [add_input t x v] is [t] that knows input [x = v] was given. *)
  end

  =
  struct
    type t =
      | Last of Root.t
      | Cons of Node.t * t 

    let empty : t = Last Root.empty

    let map_hd (stack : t) ~(f : Node_base.t -> Node_base.t) : t =
      match stack with
      | Last base -> Last (f base)
      | Cons (hd, tl) -> Cons ({ hd with base = f hd.base }, tl)

    let to_tree (stack : t) : Root.t =
      let rec to_tree acc = function
        | Last root -> { root with children = acc }
        | Cons ({ this_branch ; base }, tl) ->
          let acc = Children.of_node { this_branch ; base = { base with children = acc } } in
          to_tree acc tl
      in
      to_tree Children.empty stack

    let merge_with_tree (stack : t) (tree : Root.t) : Root.t =
      Root.merge tree
      @@ to_tree stack

    let hd_base : t -> Root.t = function
      | Last base
      | Cons ({ base ; _ }, _) -> base

    let push (stack : t) (branch : Branch.Runtime.t) : t =
      let formulas = (hd_base stack).formulas in
      Cons (Node.with_formulas (Node.of_parent_branch branch) formulas, stack)

    let add_formula (stack : t) (expr : Z3.Expr.expr) : t =
      map_hd stack ~f:(fun node_base -> Node_base.add_formula node_base expr)
      
    let add_key_eq_val (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
      add_formula x @@ Riddler.eq_term_v key (Some v)

    let add_alias (x : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
      add_formula x @@ Riddler.eq key1 key2

    let add_binop (x : t) (key : Lookup_key.t) (op : Jayil.Ast.binary_operator) (left : Lookup_key.t) (right : Lookup_key.t) : t =
      add_formula x @@ Riddler.binop_without_picked key op left right

    let add_input (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
      add_formula x @@ Riddler.is_pattern key Jayil.Ast.Int_pattern
  end

module Target =
  struct
    (* Notice that this is the same as a node. I wonder if I should have a Target status *)
    module T =  
      struct
        type t =
          { branch : Branch.Runtime.t 
          ; from : Node_base.t }
      end

    type t = T.t option

    let none : t = None

    let to_formulas (target : t) : Z3.Expr.expr list =
      match target with
      | Some { branch ; from } ->
        Branch.Runtime.to_expr branch :: Formula_set.to_list from.formulas
      | None -> []
  end


(*
  The user will keep a [t] and use it to enter branches. When the interpretation finishes,
  they will say so, and the stack is traversed to be included in total.   

  This way, we are not modifying the entire path in the tree with every step, and also we 
  don't have to use mutation to avoid it. It just takes one extra pass through the whole thing
  at the end.

  Possibly, if we are taking a known path already, we just pull the Single from the total
  to avoid creating the same formulas over and over again.
*)
type t =
  { tree   : Root.t
  ; stack  : Node_stack.t
  ; target : Target.t }

let empty : t =
  { tree   = Root.empty
  ; stack  = Node_stack.empty
  ; target = Target.none }