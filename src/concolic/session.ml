open Core
open Path_tree
open Dj_common

(*
  Mutable record that tracks a run through the evaluation. aka "interpreter session"
*)
module Concrete =
  struct

    type t =
      { input_feeder    : Concolic_feeder.t
      ; mutable step    : int
      ; max_step        : int }

    let create_default () =
      { input_feeder    = Fn.const 0
      ; step            = 0
      ; max_step        = Options.default.global_max_step }

    let create (input_feeder : Concolic_feeder.t) (global_max_step : int) : t =
      { (create_default ()) with 
        input_feeder
      ; max_step = global_max_step }

    let incr_step (x : t) : unit =
      x.step <- x.step + 1

    let is_max_step (x : t) : bool =
      x.step >= x.max_step
  end

module Symbolic = Symbolic_session

module Status =
  struct
    type t =
      | In_progress of { pruned : bool }
      | Found_abort of (Branch.t * Jil_input.t list [@compare.ignore])
      | Type_mismatch of (Jil_input.t list [@compare.ignore])
      | Exhausted of { pruned : bool }
      [@@deriving compare, sexp]

    let prune (x : t) : t =
      match x with
      | In_progress _ -> In_progress { pruned = true }
      | Exhausted _ -> Exhausted { pruned = true }
      | _ -> x

    let quit (x : t) : bool =
      match x with
      | Found_abort _
      | Type_mismatch _
      | Exhausted _ -> true
      | In_progress _ -> false

    let finish (x : t) : t =
      match x with
      | In_progress { pruned } -> Exhausted { pruned }
      | _ -> x

    let to_string (x : t) : string =
      match x with
      | Found_abort _                 -> "Found abort in interpretation"
      | Type_mismatch _               -> "Found type mismatch in interpretation"
      | In_progress { pruned = true } -> "In progress after interpretation (has pruned so far)"
      | In_progress _                 -> "In progress after interpretation"
      | Exhausted { pruned = true }   -> "Exhausted pruned true"
      | Exhausted _                   -> "Exhausted full tree"
  end

type t =
  { tree         : Root.t (* pointer to the root of the entire tree of paths *)
  ; target_queue : Target_queue.t
  ; run_num      : int
  ; options      : Options.t
  ; status       : Status.t
  ; last_sym     : Symbolic.Dead.t option }

let empty : t =
  { tree         = Root.empty
  ; target_queue = Target_queue.empty
  ; run_num      = 0
  ; options      = Options.default
  ; status       = Status.In_progress { pruned = false }
  ; last_sym     = None }

let with_options : (t, t) Options.Fun.t =
  Options.Fun.make
  @@ fun (r : Options.t) -> fun (x : t) ->
    { x with options = r
    ; target_queue = Options.Fun.run Target_queue.with_options r x.target_queue } 

let accum_symbolic (x : t) (sym : Symbolic.t) : t =
  let dead_sym = Symbolic.finish sym x.tree in
  let new_status =
    match Symbolic.Dead.get_status dead_sym with
    | Symbolic.Status.Found_abort (branch, inputs) -> Status.Found_abort (branch, inputs)
    | Type_mismatch inputs -> Type_mismatch inputs
    | Finished_interpretation { pruned = true } -> Status.prune x.status
    | _ -> x.status
  in
  { x with
    tree         = Symbolic.Dead.root dead_sym
  ; target_queue = Target_queue.push_list x.target_queue @@ Symbolic.Dead.targets dead_sym
  ; status       = new_status
  ; last_sym     = Some dead_sym }

let apply_options_symbolic (x : t) (sym : Symbolic.t) : Symbolic.t =
  Options.Fun.run Symbolic.with_options x.options sym

(* $ OCAML_LANDMARKS=on ./_build/... *)
let[@landmarks] next (x : t) : [ `Done of Status.t | `Next of (t * Symbolic.t * Concrete.t) ] Lwt.t =
  let pop_kind =
    match x.last_sym with
    | Some s when Symbolic.Dead.is_reach_max_step s -> Target_queue.Pop_kind.BFS (* only does BFS when last symbolic run reached max step *)
    | _ -> Random
  in
  let rec next (x : t) : [ `Done of Status.t | `Next of (t * Symbolic.t * Concrete.t) ] Lwt.t =
    let%lwt () = Lwt.pause () in
    if Status.quit x.status then done_ x else
    match Target_queue.pop ~kind:pop_kind x.target_queue with
    | Some (target, target_queue) -> 
      solve_for_target { x with target_queue } target
    | None when x.run_num = 0 ->
      Lwt.return
      @@ `Next (
          { x with run_num = 1 }
          , apply_options_symbolic x Symbolic.empty
          , Concrete.create Concolic_feeder.default x.options.global_max_step
        )
    | None -> done_ x (* no targets left, so done *)

  and solve_for_target (x : t) (target : Target.t) =
    let t0 = Caml_unix.gettimeofday () in
    Concolic_riddler.set_timeout (Core.Time_float.Span.of_sec x.options.solver_timeout_sec);
    Log.Export.CLog.debug (fun m -> m "Solving for target %s\n" (Branch.Runtime.to_string target.branch));
    match Concolic_riddler.solve (Target.to_formulas target x.tree) with
    | _, Z3.Solver.UNSATISFIABLE ->
      let t1 = Caml_unix.gettimeofday () in
      Log.Export.CLog.info (fun m -> m "FOUND UNSATISFIABLE in %fs\n" (t1 -. t0));
      next { x with tree = Root.set_status x.tree target.branch Unsatisfiable target.path }
    | _, Z3.Solver.UNKNOWN ->
      Log.Export.CLog.info (fun m -> m "FOUND UNKNOWN DUE TO SOLVER TIMEOUT\n");
      next { x with tree = Root.set_status x.tree target.branch Unknown target.path }
    | model, Z3.Solver.SATISFIABLE ->
      Log.Export.CLog.app (fun m -> m "FOUND SOLUTION FOR BRANCH: %s\n" (Branch.to_string @@ Branch.Runtime.to_ast_branch target.branch));
      Lwt.return
      @@ `Next (
            { x with run_num = x.run_num + 1 }
            , apply_options_symbolic x @@ Symbolic.make x.tree target
            , model
              |> Core.Option.value_exn
              |> Concolic_feeder.from_model
              |> fun feeder -> Concrete.create feeder x.options.global_max_step
          )

  and done_ (x : t) =
    Log.Export.CLog.info (fun m -> m "Done.\n");
    Lwt.return @@ `Done (Status.finish x.status)
    
  in next x

let run_num ({ run_num ; _ } : t) : int =
  run_num