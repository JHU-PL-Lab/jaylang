
open Core
open Concolic_common

(*
  Evaluates an expression to a concolic path.
*)
type 'k eval = Lang.Ast.Embedded.t -> 'k Interp_common.Input_feeder.t -> max_step:Interp_common.Step.t -> Status.Eval.t * 'k Path.t

(*
  -------------
  CONCOLIC LOOP
  -------------
*)

let global_runtime = Utils.Safe_cell.create 0.0
let global_solvetime = Utils.Safe_cell.create 0.0

let make_targets (target : 'k Target.t) (final_path : 'k Path.t) ~(max_tree_depth : int) : 'k Target.t list * [ `Pruned of bool ] =
  let stem = List.drop (Path.to_dirs final_path) (Target.path_n target) in
  List.fold_until stem ~init:([], target) ~f:(fun (acc, target) dir ->
      if Target.path_n target > max_tree_depth
      then Stop (acc, `Pruned true)
      else Continue (
          List.map (Direction.negations dir) ~f:(fun e -> Target.cons e target)
          @ acc
        , Target.cons (Direction.to_expression dir) target
        )
    ) ~finish:(fun (acc, _) -> acc, `Pruned false)

module Make (K : Smt.Symbol.KEY) (TQ : Target_queue.Make(K).S) (S : Smt.Formula.SOLVABLE) (P : Pause.S) = struct
  module Solve = Smt.Formula.Make_solver (S)

  (*
    Falls back on all-zero input feeder on first run and default (random) feeder after that.
  *)
  let c_loop_body : Lang.Ast.Embedded.t -> K.t eval -> TQ.t -> run_num:int -> max_tree_depth:int -> max_step:Interp_common.Step.t -> Status.Terminal.t P.t =
    fun e eval tq ~run_num ~max_tree_depth ~max_step ->
    let rec loop tq ~run_num =
      let open P in
      let* () = pause () in
      let t0 = Caml_unix.gettimeofday () in
      match TQ.pop tq with
      | Some (target, tq) -> begin
          let solve_result = Solve.solve (Target.to_expressions target) in
          let t1 = Caml_unix.gettimeofday () in
          let _ : float = Utils.Safe_cell.map (fun t -> t +. (t1 -. t0)) global_solvetime in
          let* () = pause () in
          match solve_result with
          | Sat model -> begin
              let feeder = 
                Interp_common.Input_feeder.of_smt_model 
                  model 
                  ~uid:K.uid 
                  ~fallback_feeder:(if run_num = 0 then Interp_common.Input_feeder.zero else Interp_common.Input_feeder.default)
              in
              let status, path = eval e feeder ~max_step in
              let t2 = Caml_unix.gettimeofday () in
              let _ : float = Utils.Safe_cell.map (fun t -> t +. (t2 -. t1)) global_runtime in
              let k ~reached_max_step = 
                let targets, `Pruned is_pruned = make_targets target path ~max_tree_depth in
                let* a = loop (TQ.push_list tq targets) ~run_num:(run_num + 1) in
                if is_pruned || reached_max_step
                then return (Status.min Exhausted_pruned_tree a)
                else return a
              in
              match status with
              | (Found_abort _ | Type_mismatch _ | Unbound_variable _) as s -> P.return s
              | Finished -> k ~reached_max_step:false
              | Reached_max_step -> k ~reached_max_step:true
            end
          | Unknown -> let* a = loop tq ~run_num:(run_num + 1) in return (Status.min Unknown a)
          | Unsat -> loop tq ~run_num:(run_num + 1)
        end
      | None ->
        let _ : float = Utils.Safe_cell.map (fun t -> t +. (Caml_unix.gettimeofday () -. t0)) global_solvetime in
        return Status.Exhausted_full_tree
    in
    loop tq ~run_num

  let c_loop : 
    options:Options.t ->
    K.t eval ->
    Lang.Ast.Embedded.t ->
    Concolic_common.Status.Terminal.t P.t =
    fun ~options eval program ->
    if not options.random then Interp_common.Rand.reset ();
    P.with_timeout options.global_timeout_sec @@ fun () ->
    let empty_tq = TQ.make options in
    c_loop_body
      program
      eval
      (TQ.push_list empty_tq [ Target.empty ]) 
      ~run_num:0
      ~max_tree_depth:options.max_tree_depth
      ~max_step:(Step options.global_max_step)
end

module TQ_Made = Target_queue.Make (Interp_common.Step)
module M = Make (Interp_common.Step) (TQ_Made.All) (Overlays.Typed_z3.Default) (Pause.Lwt)

let eager_c_loop :
  options:Options.t ->
  Lang.Ast.Embedded.t ->
  Concolic_common.Status.Terminal.t Lwt.t =
  M.c_loop Eager_concolic.Main.eager_eval 
