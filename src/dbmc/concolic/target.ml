open Core
open Path_tree

type t =
  { child : Child.t
  ; npath : int (* npath is the length of the path needed to find the target. This is so we can copy the path instead of cutting it short *)
  ; path  : Path.t } (* The path just helps the solver find the node in the tree in order to gather formulas *)
  [@@deriving compare]  
  (* We do need to compare path because the child key doesn't include exited branches, but the path does, so path is necessary to describe target completely *)
  (* For now, npath is just used in compare and not in any logic later in this file. *)
  (* We only care about the path up to length n because two targets are equal even if the path after that target was aquired changes *)

let create (child : Child.t) (path : Path.t) (npath : int) : t =
  { child ; path ; npath } 

let is_hit ({ child ; path ; npath } : t) (root : Root.t) : bool =
  let target_branch = child.branch in
  (* acc already contains all formulas pertaining to `node` *)
  let rec trace_path n node = function
    | next_branch :: tl -> begin
      match Node.get_child node target_branch with
      | Some target -> 
        assert (n = npath); (* make sure it was found at the correct depth *)
        Format.printf "Found target on path with status %s\n"
        (
          match target.status with
          | Hit _ -> "HIT"
          | Unsatisfiable -> "UNSAT"
          | Failed_assume -> "FAILED ASSUME"
          | Unknown -> "UNKNOWN"
          | Unsolved -> "UNSOLVED"
        );
        Format.printf "Given target had status %s\n"
        (
          match child.status with
          | Hit _ -> "HIT"
          | Unsatisfiable -> "UNSAT"
          | Failed_assume -> "FAILED ASSUME"
          | Unknown -> "UNKNOWN"
          | Unsolved -> "UNSOLVED"
        );
        Child.is_hit target (* found target as a child on the path*)
      | None -> (* target is not a child of the node, so continue down the path *)
        trace_path
          (n + 1)
          (Child.to_node_exn @@ Node.get_child_exn node next_branch)
          tl
      end
    | [] -> (* necessarily has found the target at end of path *)
      assert (n = npath);
      Child.is_hit
      @@ Node.get_child_exn node target_branch 
  in
  trace_path 0 root path

let[@landmarks] to_formulas ({ child ; path ; _ } : t) (root : Root.t) : Z3.Expr.expr list =
  let target_branch = child.branch in
  (* acc already contains all formulas pertaining to `node` *)
  let rec trace_path acc node = function
    | next_branch :: tl -> begin
      match Node.get_child node target_branch with
      | Some target -> Child.to_formulas target @ acc (* found the target as a child of node. Stop here *)
      | None -> (* target is not a child of the node, so continue down the path *)
        let next_child = Node.get_child_exn node next_branch in
        trace_path
          (Child.to_formulas next_child @ acc)
          (Child.to_node_exn next_child)
          tl
      end
    | [] ->
      let target = Node.get_child_exn node target_branch in
      Child.to_formulas target @ acc (* necessarily has found the target. Stop here *)
  in
  trace_path (Formula_set.to_list root.formulas) root path