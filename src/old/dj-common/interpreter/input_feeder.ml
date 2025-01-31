open Core

type t = Id.t * Concrete_stack.t -> int

let from_list ?(default = 42) inputs : t =
  let cell = ref inputs in
  fun _ ->
    let r = List.hd_exn !cell in
    cell := List.tl_exn !cell ;
    match r with Some i -> i | None -> default

let memorized_from_list ?(default = 42)
    (history : (Id.t * Concrete_stack.t * int option) list ref) input_spec : t =
  let cell = ref input_spec in
  fun (x, stk) ->
    let r, next_spec = Input_spec.next !cell in
    cell := next_spec ;
    history := !history @ [ (x, stk, r) ] ;
    match r with Some i -> i | None -> default
