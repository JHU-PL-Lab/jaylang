open Core
open Dj_common

type t = Id.t * Concrete_stack.t -> int

let query_model model (x, call_stack) : int option =
  let name = Lookup_key.to_str2 x (call_stack |> Rstack.from_concrete) in 
  Solver.SuduZ3.get_int_s model name 

let from_model ?(history = ref []) model : t =
  let input_feeder = query_model model in
  fun query ->
    let answer = input_feeder query in
    history := answer :: !history ;
    Option.value ~default:42 answer
    