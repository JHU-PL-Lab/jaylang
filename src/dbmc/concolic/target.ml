open Core
open Path_tree

type t =
  { branch : Branch.Runtime.t
  ; path   : Path.t } (* The path just helps the solver find the node in the tree in order to gather formulas *)
  [@@deriving compare]  
  (* We do need to compare path because the branch key doesn't include exited branches, but the path does, so path is necessary to describe target completely *)

let create (branch : Branch.Runtime.t) (path : Path.t) : t =
  { branch ; path }

let is_hit ({ branch = target_branch ; path } : t) (root : Root.t) : bool =
  (* acc already contains all formulas pertaining to `node` *)
  let rec trace_path node = function
    | next_branch :: tl -> (*begin*)
      trace_path
        (Child.to_node_exn @@ Node.get_child_exn node next_branch)
        tl
    | [] -> (* necessarily has found the target at end of path *)
      Child.is_hit
      @@ Node.get_child_exn node target_branch 
  in
  trace_path root path

let[@landmarks] to_formulas ({ branch = target_branch ; path } : t) (root : Root.t) : Z3.Expr.expr list =
  (* acc already contains all formulas pertaining to `node` *)
  (* Format.printf "Printing path and target";
  List.iter path ~f:(fun b -> Format.printf "%s\n" (Branch.Runtime.to_string b));
  Format.printf "Target from that path is %s\n" (Branch.Runtime.to_string target_branch); *)

  let rec trace_path acc node = function
    | next_branch :: tl ->
      let next_child = Node.get_child_exn node next_branch in
      trace_path
        (Child.to_formulas next_child @ acc)
        (Child.to_node_exn next_child)
        tl
    | [] ->
      let target = Node.get_child_exn node target_branch in
      Child.to_formulas target @ acc (* necessarily has found the target at end of path *)
  in
  trace_path (Formula_set.to_list root.formulas) root path