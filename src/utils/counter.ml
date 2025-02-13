
module Make () = struct
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
end