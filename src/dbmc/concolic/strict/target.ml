open Core
open Tree

type t =
  { child : Child.t
  ; path  : Path.t } (* The path just helps the solver find the node in the tree in order to gather formulas *)
  [@@deriving compare]
  (* We do need to compare path because the child key doesn't include exited branches, but the path does, so path is necessary to describe target completely *)

let create (child : Child.t) (path : Path.t) : t =
  { child ; path }

let to_formulas ({ child ; path } : t) (root : Root.t) : Z3.Expr.expr list =
  let target_branch = child.branch in
  let target_key = target_branch.branch_key in
  (* acc already contains all formulas pertaining to `node` *)
  let rec trace_path acc node = function
    | next_branch :: tl -> begin
      match Node.get_child node target_branch with
      | Some target -> (* found the target as a child of node *)
        Child.to_formulas target @ acc
      | None -> (* target is not a child of the node, so continue down the path *)
        let next_child = Node.get_child_exn node next_branch in
        trace_path
          (Child.to_formulas next_child @ acc)
          (Child.to_node_exn next_child)
          tl
      end
    | [] -> Child.to_formulas child @ acc (* necessarily has found the target *)
  in
  trace_path (Formula_set.to_list root.formulas) root path