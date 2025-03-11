(**
  Module [Safe_cell].

  For benchmarking purposes, we use reference cells
  to accumulate values. However, because we use parallelism
  at times, this would may not be safe with native ref cells.

  So we lock the cell with a mutex before affecting it.
*)

open Core

type 'a t =
  { mutex : Caml_threads.Mutex.t
  ; cell  : 'a ref }

let create default =
  { mutex = Caml_threads.Mutex.create ()
  ; cell  = ref default }

let map f { mutex ; cell } =
  Caml_threads.Mutex.lock mutex;
  cell := f !cell;
  Caml_threads.Mutex.unlock mutex;
  !cell

let get { cell ; mutex } =
  Caml_threads.Mutex.lock mutex;
  let r = !cell in
  Caml_threads.Mutex.unlock mutex;
  r
  
module Make (X : T) = struct
  type nonrec t = X.t t
  let create = create
  let map = map
end
