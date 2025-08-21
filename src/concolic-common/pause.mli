(**
  Purpose: provide a common interface for a monad that allows pausing

  Detailed description:
    We like to use [Lwt] and [Lwt.pause] frequently allow the executable
    to quit on timeout. However, because this project supports parallelism,
    and that parallelism also uses [Lwt], but [Lwt] does not always nest
    nicely, we parametrize some modules with a [Pause] signature to allow
    them to optionally use [Lwt] or a no-op [Id] monad.
*)

module type S = sig
  include Utils.Types.MONAD
  val pause : unit -> unit m
  (** [pause ()] is a chance to check if timeout has been exceeded. *)

  val with_timeout : float -> (unit -> 'a m) -> 'a m

  val run : 'a m -> 'a
end

module Lwt : S with type 'a m = 'a Lwt.t
(** [Lwt] is a limited interface to the [Lwt] monad--everything the concolic
    evaluator happens to need. *)

module Id : S with type 'a m = 'a
(** [Id] is the identity monad. *)
