open Core
open Dj_common

type t = Id.t * Concrete_stack.t -> int

let from_list ?(default = 42) inputs : t =
  let cell = ref inputs in
  fun _ ->
    let r = List.hd_exn !cell in
    cell := List.tl_exn !cell ;
    match r with Some i -> i | None -> default

let memorized_from_list ?(default = 42)
    (history : (Id.t * Concrete_stack.t * int option) list ref) inputs : t =
  let cell = ref inputs in
  fun (x, stk) ->
    let r = List.hd_exn !cell in
    cell := List.tl_exn !cell ;
    history := !history @ [ (x, stk, r) ] ;
    match r with Some i -> i | None -> default

let query_model model target_stack (x, call_stack) : int option =
  let stk = Rstack.relativize target_stack call_stack in
  let name = Lookup_key.to_str2 x stk in
  Solver.SuduZ3.get_int_s model name

let from_model ?(history = ref []) model target_stack : t =
  let input_feeder = query_model model target_stack in
  fun query ->
    let answer = input_feeder query in
    history := answer :: !history ;
    Option.value ~default:42 answer
