module type S = sig
  type 'a t

  val pause : unit -> unit t

  val with_timeout : float -> (unit -> 'a t) -> 'a t

  val run : 'a t -> 'a

  val return : 'a -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Lwt : S with type 'a t = 'a Lwt.t = struct
  include Lwt
  let (let*) = (>>=)
  let with_timeout = Lwt_unix.with_timeout
  let run = Lwt_main.run
end

module Id : S = struct
  type 'a t = 'a

  let pause () = ()

  let with_timeout _ f = f ()

  let run a = a

  let return a = a

  let ( >>= ) a f = f a

  let (let*) = ( >>= )
end