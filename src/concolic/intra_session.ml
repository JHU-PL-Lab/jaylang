open Core

module Make (S : Solve.S) (P : Pause.S) (O : Options.V) = struct
  module TQ = Target_queue.All

  type t =
    { target_queue : TQ.t
    ; run_num      : int
    ; status       : Status.In_progress.t
    ; has_pruned   : bool }

  let empty : t =
    { target_queue = Options.Arrow.appl TQ.of_options O.r ()
    ; run_num      = 1
    ; status       = In_progress
    ; has_pruned   = false }

  let accum_eval (x : t) (ev : Status.Eval.t) : t =
    ev
    |> begin function
      | (Status.Found_abort _ | Type_mismatch _ | Unbound_variable _) as res -> x.target_queue, x.has_pruned, res
      | Finished { pruned ; reached_max_step ; stem } ->
        TQ.push_list x.target_queue (Stem.to_new_targets stem)
        , x.has_pruned || pruned || reached_max_step
        , x.status
    end
    |> fun (target_queue, new_pruned, new_status) ->
      { x with target_queue 
      ; status = new_status
      ; has_pruned = new_pruned }

  let finish_status (x : t) : Status.Terminal.t =
    match x.status with
    | (Found_abort _ | Type_mismatch _ | Unbound_variable _) as res -> res
    | Diverge | In_progress -> (* This here is a little sloppy. Diverge is an artifact from eval session *)
      if x.has_pruned
      then Exhausted_pruned_tree
      else Exhausted_full_tree

  let rec get_sat_target (target_queue : TQ.t) : (TQ.t * Target.t * Input_feeder.t) option P.t =
    let open P in
    let* () = pause () in
    match TQ.peek target_queue with
    | None -> return None
    | Some target ->
      let exprs = Target.to_expressions target in
      let tq = TQ.remove target_queue target in
      match S.solve exprs with
      | `Sat feeder -> P.return @@ Option.return (tq, target, feeder)
      | `Unsat -> get_sat_target tq

  let[@landmark] next (x : t) : [ `Done of Status.Terminal.t | `Next of (t * Eval_session.t) ] P.t =
    let open P in
    let done_ () =
      return
      @@ `Done (finish_status x)
    in
    if Status.is_terminal x.status then done_ () else
    let* res = get_sat_target x.target_queue in
    match res with
    | Some (target_queue, target, input_feeder) ->
      return
      @@ `Next ( 
        { x with target_queue ; run_num = x.run_num + 1 }
        , Eval_session.make target input_feeder
          |> Options.Arrow.appl Eval_session.with_options O.r
      )
    | None -> done_ ()

  let run_num ({ run_num ; _ } : t) : int =
    run_num
end
