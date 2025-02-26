(**
  File: pause.mli
  Purpose: provide a common interface for a monad that allows pausing

  Detailed description:
    We like to use [Lwt] and [Lwt.pause] frequently allow the executable
    to quit on timeout. However, because this project supports parallelism,
    and that parallelism also uses [Lwt], but [Lwt] does not always nest
    nicely, we parametrize some modules with a [Pause] signature to allow
    them to optionally use [Lwt] or a no-op [Id] monad.
*)

module type S = sig
  type 'a t

  val pause : unit -> unit t
  (** [pause ()] is a chance to check if timeout has been exceeded. *)

  val with_timeout : float -> (unit -> 'a t) -> 'a t

  val run : 'a t -> 'a

  val return : 'a -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [let*] is the let-sugar for [bind] *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [(>>=)] is infix [bind]. *)
end

module Lwt : S with type 'a t = 'a Lwt.t
(** [Lwt] is a limited interface to the [Lwt] monad--everything the concolic
    evaluator happens to need. *)

module Id : S
(** [Id] is the identity monad. *)