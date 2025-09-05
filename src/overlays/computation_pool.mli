
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
  end

  val timeout_sec : float
  (** [timeout_sec] is the number of seconds a computation is allowed to
      run before it is stopped short. *)
end

module Process (C : Computation) : sig
  val process_all : C.Work.t Preface.Nonempty_list.t -> C.Compute_result.t
  (** [process_all work_items] runs the computation on all items in
      [work_items] at once. Each item gets its own domain even if there is just
      one work item. *)
end

