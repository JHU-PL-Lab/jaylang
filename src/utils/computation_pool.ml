
open Core
open Moonpool

module type Computation = sig
  module Compute_result : sig
    include Preface.Specs.MONOID
    val timeout_res : t
    val is_signal_to_quit : t -> bool
  end

  module Work : sig
    type t
    val run : t -> Compute_result.t
    (** [run t] does the work on [t] and promises that it will not
        invoke [Lwt].
        
        NOTE: This function is expected to be run in parallel on many
        threads, so it should be thread-safe. *)

    val run_with_internal_timeout : t -> Compute_result.t
    (** [run_with_internal_timeout t] does the work on [t] and has
        its own timer to stop the computation at timeout.
        
        NOTE: This function does not need to be thread-safe. *)
  end

  val timeout_sec : float
end

module Process (C : Computation) = struct
  let process_all (ls : C.Work.t Preface.Nonempty_list.t) =
    let open Preface.Nonempty_list in
    match ls with
    | Last item -> C.Work.run_with_internal_timeout item
    | _ ->
      let t0 = Caml_unix.gettimeofday () in
      let pool = Ws_pool.create ~num_threads:(Preface.Nonempty_list.length ls) () in
      let futures = 
        ls
        |> Preface.Nonempty_list.to_list
        |> List.map ~f:(fun item ->
          Fut.spawn ~on:pool (fun () -> C.Work.run item)
        )
      in
      let rec go acc unfinished futures =
        if Float.(Caml_unix.gettimeofday () - t0 > C.timeout_sec)
        then C.Compute_result.timeout_res
        else
          match futures with
          | [] ->
            if List.is_empty unfinished
            then acc (* totally done with work *)
            else go acc [] unfinished (* done with this pass, so begin new pass *)
          | promise :: tl ->
            if Fut.is_done promise
            then begin
              let res = Fut.await promise in
              if C.Compute_result.is_signal_to_quit res
              then res
              else go (C.Compute_result.combine res acc) unfinished tl
            end
            else go acc (promise :: unfinished) tl
      in
      let res = go C.Compute_result.neutral [] futures in
      Ws_pool.shutdown_without_waiting pool;
      res
end