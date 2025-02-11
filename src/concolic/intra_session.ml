open Core

module New_context () = struct
  module Tree_make = Path_tree.New_context ()
  module Make (P : Pause.S) (O : Options.V) = struct
    module Tree = Tree_make.Make (Target_queue.All) (P) (O)

    (* ignore warning about prev_res not used *)
    type[@ocaml.warning "-69"] t =
      { tree         : Tree.t (* pointer to the root of the entire tree of paths *)
      ; run_num      : int
      ; status       : Status.In_progress.t
      ; has_pruned   : bool
      ; prev_res     : Status.Eval.t option }

    let empty : t =
      { tree         = Tree.empty
      ; run_num      = 1
      ; status       = In_progress
      ; has_pruned   = false
      ; prev_res     = None }

    let accum_eval (x : t) (ev : Status.Eval.t) : t =
      ev
      |> begin function
        | (Status.Found_abort _ | Type_mismatch _) as res -> x.tree, x.has_pruned, res
        | Finished { pruned ; reached_max_step ; stem } ->
          Tree.add_stem x.tree stem
          , x.has_pruned || pruned || reached_max_step
          , x.status
      end
      |> fun (tree, new_pruned, new_status) ->
        { x with tree
        ; status = new_status
        ; has_pruned = new_pruned
        ; prev_res = Some ev }

    let finish_status (x : t) : Status.Terminal.t =
      match x.status with
      | (Found_abort _ | Type_mismatch _) as res -> res
      | Diverge | In_progress -> (* TODO: fix the sloppiness here. Diverge is an artifact from eval session *)
        if x.has_pruned
        then Exhausted_pruned_tree
        else Exhausted_full_tree

    let[@landmark] next (x : t) : [ `Done of Status.Terminal.t | `Next of (t * Eval_session.t) ] P.t =
      let open P in
      let done_ () =
        return
        @@ `Done (finish_status x)
      in
      if Status.is_terminal x.status then done_ () else
      let* res = Tree.pop_sat_target x.tree in
      match res with
      | Some (tree, target, input_feeder) ->
        return
        @@ `Next ( 
          { x with tree ; run_num = x.run_num + 1 }
          , Eval_session.make target input_feeder
            |> Options.Arrow.appl Eval_session.with_options O.r
        )
      | None -> done_ ()

    let run_num ({ run_num ; _ } : t) : int =
      run_num

  end
end