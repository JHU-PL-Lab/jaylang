
(**
  Module [Computation_pool].

  This module provides an interface to the [Moonpool] library
  to allow parallel computation of items in a list until one of
  this items finishes and signals to quit.
*)

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
  (** [timeout_sec] is the number of seconds a computation is allowed to
      run before it is stopped short. *)
end

module Process (C : Computation) : sig
  val process_all : C.Work.t Preface.Nonempty_list.t -> C.Compute_result.t
  (** [process_all work_items] runs the computation on all items in
      [work_items] at once. If there is only one item, then no domains
      are spawned. Otherwise, each items gets its own domain. *)
end

