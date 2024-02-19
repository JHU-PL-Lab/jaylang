open Core
open Dj_common

type t = Input_feeder.t

let query_model model (x, call_stack) : int option =
  let name = Lookup_key.to_str2 x (call_stack |> Rstack.from_concrete) in 
  Solver.SuduZ3.get_int_s model name 

let default : Input_feeder.t =
  fun _ -> Quickcheck.random_value ~seed:`Nondeterministic (Int.gen_incl (-10) 10)

let from_model ?(history = ref []) model : t =
  let input_feeder = query_model model in
  fun query ->
    let answer = input_feeder query in
    history := answer :: !history ;
    Option.value ~default:(default query) answer
    