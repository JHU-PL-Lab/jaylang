module type S = sig
  include Utils.Types.MONAD
  val pause : unit -> unit m

  val with_timeout : float -> (unit -> 'a m) -> 'a m

  val run : 'a m -> 'a
end

module Lwt : S with type 'a m = 'a Lwt.t = struct
  include Lwt
  type 'a m = 'a t
  let with_timeout = Lwt_unix.with_timeout
  let run = Lwt_main.run
end

module Id : S with type 'a m = 'a = struct
  include Utils.Identity.Monad
  let pause () = ()

  let with_timeout _ f = f ()

  let run a = a
end
