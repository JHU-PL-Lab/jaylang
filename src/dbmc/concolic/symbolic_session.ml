
open Core
open Path_tree


(* Node_stack is a stack of nodes that describes a path through the tree. This will be used by the symbolic session. *)
module Node_stack =
  struct
    type t =
      | Last of Root.t
      | Cons of Child.t * t 

    let empty : t = Last Root.empty

    (* To avoid extra children and to keep the stack a single path, begin with only the formulas (and discard the children) from root. *)
    let of_root (root : Root.t) : t =
      Last (Root.with_formulas Root.empty root.formulas)

    let hd_node : t -> Root.t = function
      | Last node -> node
      | Cons (child, _) -> Child.to_node_exn child

    let hd_branch_exn : t -> Branch.t = function
      | Last _ -> failwith "no hd branch"
      | Cons (child, _) -> Branch.Runtime.to_ast_branch child.branch

    let map_hd (stack : t) ~(f : Node.t -> Node.t) : t =
      match stack with
      | Last base -> Last (f base)
      | Cons (hd, tl) -> Cons (Child.map_node hd ~f, tl) (* might be a good choice to throw exception if node doesn't exist *)

    (* Creates a tree that is effectively the path down the node stack *)
    let to_tree (stack : t) : Root.t =
      let rec to_tree acc = function
        | Last root -> { root with children = acc }
        | Cons ({ status = Hit node ; _} as child, tl) -> (* branch was hit, and no failed assert or assume *)
          let acc = Children.set_node Children.empty child.branch { node with children = acc } in
          to_tree acc tl
        | Cons ({ status = Failed_assume ; _} as child, tl) -> (* branch was hit, and failed assert of assume *)
          assert (Children.is_empty acc);
          let acc = Children.set_child Children.empty child in (* has no node in which to set children *)
          to_tree acc tl
        | _ -> failwith "logically impossible" (* impossible if the code does as I expect it *) 
      in
      to_tree Children.empty stack

    (* Gives a root-to-leaf path by reversing the stack *)
    let to_path (x : t) : Path.t =
      let rec loop acc = function
        | Last _ -> acc
        | Cons (child, tl) -> loop (child.branch :: acc) tl
      in
      loop [] x

    (* Returns list of targets where deeper branches are at the front of the list *)
    let get_targets (stack : t) (tree : Root.t) : Target.t list =
      let total_path = to_path stack in (* this path leads to all possible targets *)
      let rec step
        (cur_targets : Target.t list)
        (cur_node : Node.t)
        (remaining_path : Path.t)
        (path : Path.t)
        : Target.t list
        =
        match remaining_path with
        | last_branch :: [] -> (* maybe last node hit should be target again in case it failed assume or assert *)
          push_target
            (push_target cur_targets cur_node last_branch path) (* push this direction *)
            cur_node
            (Branch.Runtime.other_direction last_branch) (* push other direction *)
            path
        | branch :: tl ->
          step
            (push_target cur_targets cur_node (Branch.Runtime.other_direction branch) path) (* push other direction as possible target *)
            (Child.to_node_exn @@ Node.get_child_exn cur_node branch) (* step down path *)
            tl (* continue down remainder of path *)
            (path @ [ branch ]) (* complexity of this is bad, but depth is so shallow that is not slow in practice *)
        | [] -> cur_targets
      and push_target
        (cur_targets : Target.t list)
        (cur_node : Node.t)
        (branch : Branch.Runtime.t)
        (path : Path.t)
        : Target.t list
        =
        if Node.is_valid_target_child cur_node branch
        then Target.create branch path :: cur_targets
        else cur_targets
      in
      step [] tree total_path []

    (* Note that merging two trees would have to visit every node in both of them in the worst case,
      but we know that the tree made from the stack is a single path, so it only has to merge down
      that single path because only one child is hit and needs to be merged (see Path_tree.Status.merge). *)
    let merge_with_tree (allowed_tree_depth : int) (stack : t) (tree : Root.t) : Root.t * Target.t list =
      let merged =
        Root.merge tree
        @@ to_tree stack
      in
      merged, get_targets stack merged

    (* pushes node reached by [branch] onto the stack *)
    let push (stack : t) (branch : Branch.Runtime.t) : t =
      Cons (Child.create Node.empty branch, stack)

    (* adds formula to top node of the stack *)
    let add_formula (stack : t) (expr : Z3.Expr.expr) : t =
      map_hd stack ~f:(fun node -> Node.add_formula node expr)
  end (* Node_stack *)

module Depth_logic =
  struct
    type t =
      { cur_depth    : int
      ; max_depth    : int
      ; is_below_max : bool } 
      (** [t] helps track if we've reached the max tree depth and thus should stop creating formulas *)

    let empty (max_depth : int) : t =
      { cur_depth = 0; max_depth ; is_below_max = true }

    let incr (x : t) : t =
      { x with cur_depth = x.cur_depth + 1 ; is_below_max = x.cur_depth < x.max_depth }
  end

(* Basic holds the information that is needed at any state of the session *)
module Basic =
  struct
    type t =
      { stack          : Node_stack.t
      ; target         : Target.t option
      ; branch_info    : Branch_info.t
      ; inputs         : Jil_input.t list
      ; depth          : Depth_logic.t
      ; reach_max_step : bool }

    let empty : t =
      { stack          = Node_stack.empty
      ; target         = None
      ; branch_info    = Branch_info.empty
      ; inputs         = []
      ; depth          = Depth_logic.empty Concolic_options.default.max_tree_depth
      ; reach_max_step = false }

    let with_options : (t -> t) Concolic_options.Fun.t =
      Concolic_options.Fun.make
      @@ fun (r : Concolic_options.t) -> fun (x : t) -> { x with depth = { x.depth with max_depth = r.max_tree_depth } }

    let found_abort (s : t) : t =
      { s with
        branch_info =
          Branch_info.set_branch_status
            ~new_status:(Found_abort s.inputs)
            s.branch_info @@ Node_stack.hd_branch_exn s.stack
      }

    (* require that cx is true by adding as formula *)
    let found_assume (s : t) (cx : Concolic_key.Lazy2.t) : t =
      match s.stack with
      | Last _ -> s (* `assume` found in global scope. We assume this is a test case that can't happen in real world translations to JIL *)
      | Cons (hd, tl) ->
        let new_hd = Child.map_node hd ~f:(fun node -> Node.add_formula node @@ Concolic_riddler.eqv (cx ()) (Jayil.Ast.Value_bool true)) in
        { s with stack = Cons (new_hd, tl) }

    let failed_assume (s : t) : t =
      match s.stack with
      | Last _ -> s (* `assume` found in global scope. We assume this is a test case that can't happen in real world translations to JIL *)
      | Cons (hd, tl) ->
        let new_hd =
          Child.{ status = Status.Failed_assume (* forget all formulas so that it is a possible target in future runs *)
                ; constraints = Formula_set.add_multi hd.constraints @@ Child.to_formulas hd (* constrain to passing assume/assert *)
                ; branch = hd.branch }
        in
        { s with stack = Cons (new_hd, tl) }

    let add_input (s : t) (i : Jil_input.t) : t =
      { s with inputs = i :: s.inputs }
  end

module At_max_depth =
  struct
    type t =
      { last_branch : Branch.t 
      ; base        : Basic.t }

    let of_basic (s : Basic.t) : t =
      { last_branch = Node_stack.hd_branch_exn s.stack
      ; base = s }

    let with_options : (t -> t) Concolic_options.Fun.t =
      Concolic_options.Fun.make
      @@ fun (r : Concolic_options.t) -> fun (x : t) -> { x with base = Concolic_options.Fun.appl Basic.with_options r x.base }

    let found_abort (a : t) : t =
      { a with
        base =
          { a.base with
            branch_info =
              Branch_info.set_branch_status
                ~new_status:(Found_abort a.base.inputs)
                a.base.branch_info
                a.last_branch
          }
      }
  end

module Finished =
  struct
    type t =
      { tree          : Root.t
      ; targets       : Target.t list
      ; prev          : Basic.t
      ; hit_max_depth : bool}

    let with_options : (t -> t) Concolic_options.Fun.t =
      Concolic_options.Fun.make
      @@ fun (r : Concolic_options.t) -> fun (x : t) -> { x with prev = Concolic_options.Fun.appl Basic.with_options r x.prev }

    let of_basic (s : Basic.t) (root : Root.t) : t =
      (* logically sound to have hit target if formulas are consistent with JIL program *)
      let tree, targets = Node_stack.merge_with_tree s.depth.max_depth s.stack root in
      assert (Option.is_none s.target || Target.is_hit (Option.value_exn s.target) tree); (* check that target was hit in new tree *)
      { tree ; targets ; prev = s ; hit_max_depth = false }
  end

type t =
  | Basic of Basic.t
  | At_max_depth of At_max_depth.t
  | Finished of Finished.t
  (* TODO: track a path in a tree and only add formulas if at a new node. TODO: add a "pruned" variant to path tree status *)

let empty : t = Basic Basic.empty

let with_options : (t -> t) Concolic_options.Fun.t =
  Concolic_options.Fun.make
  @@ fun (r : Concolic_options.t) -> fun (x : t) ->
    match x with
    | Basic s -> Basic (Concolic_options.Fun.appl Basic.with_options r s)
    | At_max_depth s -> At_max_depth (Concolic_options.Fun.appl At_max_depth.with_options r s)
    | Finished s -> Finished (Concolic_options.Fun.appl Finished.with_options r s)

(* [lazy_expr] does not get evaluated unless [x] is [Basic]. *)
let add_lazy_formula (x : t) (lazy_expr : unit -> Z3.Expr.expr) : t =
  match x with
  | Basic s -> Basic { s with stack = Node_stack.add_formula s.stack @@ lazy_expr () }
  | At_max_depth _ -> x
  | Finished _ -> failwith "adding formula to finished symbolic session"

let hit_branch (x : t) (branch : Branch.Runtime.t) : t =
  match x with
  | Basic s when (Depth_logic.incr s.depth).is_below_max ->
    Basic
    { s with branch_info = Branch_info.set_branch_status ~new_status:Hit s.branch_info @@ Branch.Runtime.to_ast_branch branch
    ; stack = Node_stack.push s.stack branch
    ; depth = Depth_logic.incr s.depth }
  | Basic s -> At_max_depth (At_max_depth.of_basic s)
  | At_max_depth a -> At_max_depth { a with last_branch = Branch.Runtime.to_ast_branch branch }
  | Finished _ -> failwith "using finished symbolic session to hit branch"

let found_assume (x : t) (cx : Concolic_key.Lazy2.t) : t =
  match x with
  | Basic s -> Basic (Basic.found_assume s cx)
  | At_max_depth _ -> x
  | Finished _ -> failwith "found assume with finished symbolic session"

let fail_assume (x : t) :  t =
  match x with
  | Basic s -> Basic (Basic.failed_assume s)
  | At_max_depth _ -> x
  | Finished _ -> failwith "failed assume with finished symbolic session"

let found_abort (x : t) : t =
  match x with
  | Basic s -> Basic (Basic.found_abort s)
  | At_max_depth a -> At_max_depth (At_max_depth.found_abort a)
  | Finished _ -> failwith "found abort with finished symbolic session"

let reach_max_step (x : t) : t =
  match x with
  | Basic s -> Basic ({ s with reach_max_step = true})
  | At_max_depth a -> At_max_depth ({ a with base = { a.base with reach_max_step = true } })
  | Finished _ -> failwith "reach max step with finished symbolic session"

let add_basic_input (x : t) (i : Jil_input.t) : t =
  match x with
  | Basic s -> Basic (Basic.add_input s i)
  | At_max_depth a -> At_max_depth ({ a with base = Basic.add_input a.base i })
  | Finished _ -> failwith "adding input to finished symbolic session"

(*
  ------------------------------
  FORMULAS FOR BASIC JIL CLAUSES
  ------------------------------
*)
let add_key_eq_val (x : t) (key : Concolic_key.Lazy2.t) (v : Jayil.Ast.value) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.eq_term_v (key ()) (Some v)

let add_alias (x : t) (key1 : Concolic_key.Lazy2.t) (key2 : Concolic_key.Lazy2.t) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.eq (key1 ()) (key2 ())

let add_binop (x : t) (key : Concolic_key.Lazy2.t) (op : Jayil.Ast.binary_operator) (left : Concolic_key.Lazy2.t) (right : Concolic_key.Lazy2.t) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.binop_without_picked (key ()) op (left ()) (right ())

let add_input (x : t) (key : Concolic_key.Lazy2.t) (v : Dvalue.t) : t =
  let key = key () in (* assume it's not that expensive to compute the key on inputs *)
  let n =
    match v with
    | Dvalue.Direct (Value_int n) -> n
    | _ -> failwith "non-int input" (* logically impossible *)
  in
  add_basic_input x { clause_id = Concolic_key.x key ; input_value = n }
  |> Fn.flip add_lazy_formula @@ fun () -> 
    let Ident s = Concolic_key.x key in
    Dj_common.Log.Export.CLog.app (fun m -> m "Feed %d to %s \n" n s);
    Concolic_riddler.if_pattern key Jayil.Ast.Int_pattern

let add_not (x : t) (key1 : Concolic_key.Lazy2.t) (key2 : Concolic_key.Lazy2.t) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.not_ (key1 ()) (key2 ())

let add_match (x : t) (k : Concolic_key.Lazy2.t) (m : Concolic_key.Lazy2.t) (pat : Jayil.Ast.pattern) : t =
  add_lazy_formula x
  @@ fun () ->
    let k_expr = Concolic_riddler.key_to_var (k ()) in
    Solver.SuduZ3.eq (Solver.SuduZ3.project_bool k_expr) (Concolic_riddler.if_pattern (m ()) pat)

(*
  -----------------
  BETWEEN-RUN LOGIC   
  -----------------
*)

(* Note that other side of all new targets are all the new hits *)
let[@landmarks] finish (x : t) (tree : Root.t) : t =
  match x with
  | Basic s -> Finished (Finished.of_basic s tree)
  | At_max_depth a -> Finished ({ (Finished.of_basic a.base tree) with hit_max_depth = true })
  | Finished r -> Finished ({ r with tree }) (* allow finishing twice by reseting the tree *)

let make (root : Root.t) (target : Target.t) : t =
  Basic { Basic.empty with stack = Node_stack.of_root root ; target = Some target }

let root_exn (x : t) : Root.t =
  match x with
  | Finished { tree ; _ } -> tree
  | _ -> failwith "no root"

let targets_exn (x : t) : Target.t list =
  match x with
  | Finished { targets ; _ } -> targets
  | _ -> failwith "no targets"

let branch_info (x : t) : Branch_info.t =
  match x with
  | Basic { branch_info ; _ }
  | At_max_depth { base = { branch_info ; _} ; _ }
  | Finished { prev = { branch_info ; _ } ; _ } -> branch_info

let hit_max_depth (x : t) : bool =
  match x with
  | Basic _ -> false
  | At_max_depth _ -> true
  | Finished { hit_max_depth ; _ } -> hit_max_depth

let is_reach_max_step (x : t) : bool =
  match x with
  | Basic { reach_max_step ; _ }
  | At_max_depth { base = { reach_max_step ; _ } ; _ }
  | Finished { prev = { reach_max_step ; _ } ; _ } -> reach_max_step