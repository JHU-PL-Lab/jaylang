(**
  Module [Counter].

  Sometimes it's helpful to increment a counter,
  but we may run into trouble if parallel computations
  share a counter. We ease this problem by locking the
  counter with a mutex.
*)

type t =
  { mutex : Mutex.t
  ; cell  : int ref }

let create () =
  { mutex = Mutex.create ()
  ; cell  = ref 0 }

let next { mutex ; cell } =
  Mutex.lock mutex;
  incr cell;
  let r = !cell in
  Mutex.unlock mutex;
  r 