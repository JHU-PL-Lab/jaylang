
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
  end

  val timeout_sec : float
end

module Process (C : Computation) = struct
  let process_all (ls : C.Work.t Preface.Nonempty_list.t) =
    let t0 = Caml_unix.gettimeofday () in
    (* We create one thread per work item, but it may be recommended to do fewer if this number is huge *)
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