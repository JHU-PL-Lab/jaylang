open Core
open Options.Fun.Infix

(* ignore warning about prev_res not used because it depends on current code for pop kind *)
type[@ocaml.warning "-69"] t =
  { tree         : Path_tree.t (* pointer to the root of the entire tree of paths *)
  ; run_num      : int
  ; options      : Options.t
  ; status       : Status.In_progress.t
  ; has_pruned   : bool
  ; prev_res     : Status.Eval.t option }

let empty : t =
  { tree         = Options.Fun.appl Path_tree.of_options Options.default ()
  ; run_num      = 1
  ; options      = Options.default
  ; status       = In_progress
  ; has_pruned   = false
  ; prev_res     = None }

let of_options : (unit, t * Eval_session.t) Options.Fun.a =
  (Options.Fun.make (fun r () -> r) &&& Path_tree.of_options) 
  ^>> (fun (r, tree) -> { empty with options = r ; tree })
  &&& (Eval_session.with_options <<^ fun () -> Eval_session.empty)

let accum_eval (x : t) (ev : Eval_session.t) : t =
  let ev_status = Eval_session.finish ev in
  ev_status
  |> begin function
    | (Status.Found_abort _ | Type_mismatch _) as res -> x.tree, x.has_pruned, res
    | Finished { pruned ; reached_max_step ; stem } ->
      Path_tree.add_stem x.tree stem
      , x.has_pruned || pruned || reached_max_step
      , x.status
  end
  |> fun (tree, new_pruned, new_status) ->
    { x with tree
    ; status = new_status
    ; has_pruned = new_pruned
    ; prev_res = Some ev_status }

let finish_status (x : t) : Status.Terminal.t =
  match x.status with
  | (Found_abort _ | Type_mismatch _) as res -> res
  | Diverge | In_progress -> (* TODO: fix the sloppiness here. Diverge is an artifact from eval session *)
    if x.has_pruned
    then Exhausted_pruned_tree
    else Exhausted_full_tree

let[@landmark] next (x : t) : [ `Done of Status.Terminal.t | `Next of (t * Eval_session.t) ] Lwt.t =
  let done_ () =
    Lwt.return
    @@ `Done (finish_status x)
  in
  if Status.is_terminal x.status then done_ () else
  let pop_kind =
    match x.prev_res with
    | Some (Finished { reached_max_step ; _ }) when reached_max_step -> Target_queue.Pop_kind.BFS (* only does BFS when last symbolic run reached max step *)
    | _ -> Random
  in
  let%lwt res = Path_tree.pop_sat_target ~kind:pop_kind x.tree in
  match res with
  | Some (tree, target, input_feeder) ->
    Lwt.return
    @@ `Next ( 
      { x with tree ; run_num = x.run_num + 1 }
      , Eval_session.make target input_feeder
        |> Options.Fun.appl Eval_session.with_options x.options
    )
  | None -> done_ ()

let run_num ({ run_num ; _ } : t) : int =
  run_num
