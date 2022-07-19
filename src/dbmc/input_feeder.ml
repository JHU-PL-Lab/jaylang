open Core

type t = Id.t * Concrete_stack.t -> int option

let from_list inputs =
  let history = ref inputs in
  fun _ ->
    let r = List.hd_exn !history in
    history := List.tl_exn !history ;
    Some r

let from_model model target_stack (x, call_stack) : int option =
  let stk = Rstack.relativize target_stack call_stack in
  let name = Lookup_key.to_str2 x stk in
  Solver.SuduZ3.get_int_s model name

let memorized_from_model mem model target_stack =
  let input_feeder = from_model model target_stack in
  fun query ->
    let answer = input_feeder query in
    mem := answer :: !mem ;
    answer
