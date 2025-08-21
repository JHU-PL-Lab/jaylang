
open Core
open Concolic_common

(*
  Evaluates an expression to a concolic path.

  Note:
  - If we want to collect a log from this evaluation, then we would want a functor that passes in the log
    we're using because that type would have to line up with the builder from `Log` in the `Make` functor
    below.
  - Or this type is parametrized by the log type, which would not be a monad but would be fully run and
    returned here. Then the loop just uses `tell` to collect that whole log in.
*)
type 'k eval = Lang.Ast.Embedded.t -> 'k Interp_common.Input_feeder.t -> max_step:Interp_common.Step.t -> Status.Eval.t * 'k Path.t

module type EVAL = sig
  type k
  val ceval : k eval
end

(*
  -------------
  CONCOLIC LOOP
  -------------
*)

let make_targets (target : 'k Target.t) (final_path : 'k Path.t) ~(max_tree_depth : int) : 'k Target.t list * [ `Pruned of bool ] =
  let stem = List.drop (Path.to_dirs final_path) (Target.path_n target) in
  List.fold_until stem ~init:([], target) ~f:(fun (acc, target) dir ->
      if Target.path_n target > max_tree_depth
      then Stop (acc, `Pruned true)
      else Continue (
          List.map (Direction.negations dir) ~f:(fun e -> Target.cons e target)
          @ acc
        , Target.cons (Direction.to_formula dir) target
        )
    ) ~finish:(fun (acc, _) -> acc, `Pruned false)

module Make (K : Smt.Symbol.KEY) (Make_tq : Target_queue.MAKE) (P : Pause.S) (Log : Utils.Logger.FULL with type B.a = Stat.t and type 'a M.m = 'a P.m) = struct
  module Tq = Make_tq (K)

  (*
    Falls back on all-zero input feeder on first run and default (random) feeder after that.
  *)
  let c_loop_body (e : Lang.Ast.Embedded.t) (eval : K.t eval) (tq : Tq.t) (solve : K.t Smt.Formula.solver)
    ~(max_tree_depth : int) ~(max_step : Interp_common.Step.t) : Status.Terminal.t Log.m =
    let open Log in
    let is_first_interp = ref true in
    let rec loop tq =
      let%bind () = upper @@ P.pause () in
      match Tq.pop tq with
      | Some (target, tq) -> begin
          let solve_span, solve_result = Stats.time solve (Target.to_formulas target) in
          let%bind () = log @@ Time (Solve_time, solve_span) in
          let%bind () = log @@ Count (N_solves, 1) in
          let%bind () = upper @@ P.pause () in
          match solve_result with
          | Sat model -> loop_on_model target tq model
          | Unknown -> let%bind a = loop tq in return (Status.min Unknown a)
          | Unsat -> loop tq
        end
      | None -> return Status.Exhausted_full_tree

    and loop_on_model target tq model =
      let feeder = 
        Interp_common.Input_feeder.of_smt_model 
          model 
          ~uid:K.uid 
          ~fallback_feeder:(
            if !is_first_interp
            then (is_first_interp := false; Interp_common.Input_feeder.zero)
            else Interp_common.Input_feeder.default
          )
      in
      let interp_span, (status, path) = Stats.time (eval e ~max_step) feeder in
      let%bind () = log @@ Time (Interp_time, interp_span) in
      let%bind () = log @@ Count (N_interps, 1) in
      let k ~reached_max_step = 
        let targets, `Pruned is_pruned = make_targets target path ~max_tree_depth in
        let%bind a = loop (Tq.push_list tq targets) in
        if is_pruned || reached_max_step
        then return (Status.min Exhausted_pruned_tree a)
        else return a
      in
      match status with
      | (Found_abort _ | Type_mismatch _ | Unbound_variable _) as s ->
        let%bind () = log @@ Depth (Target_depth, Target.path_n target) in
        let%bind () = log @@ Depth (Error_depth, Path.length path) in
        return s
      | Finished -> k ~reached_max_step:false (* TODO: track number of vanishes *)
      | Reached_max_step -> k ~reached_max_step:true

    in
    let t0 = Mtime_clock.now () in
    let%bind res = loop tq in
    let t1 = Mtime_clock.now () in
    let%bind () = log @@ Time (Total_time, Mtime.span t0 t1) in
    return res

  let c_loop ~(options : Options.t) (eval : K.t eval) (solve : K.t Smt.Formula.solver) (e : Lang.Ast.Embedded.t) : Status.Terminal.t Log.m =
    if not options.random then Interp_common.Rand.reset ();
    let lifted_timeout t f = Log.map_t 
      (fun m -> P.with_timeout t (fun () -> m)) (f ())
    in
    lifted_timeout options.global_timeout_sec @@ fun () ->
      let empty_tq = Tq.make options in
      c_loop_body
        e
        eval
        (Tq.push_list empty_tq [ Target.empty ]) 
        solve
        ~max_tree_depth:options.max_tree_depth
        ~max_step:(Step options.global_max_step)
end
