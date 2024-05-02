open Core
open Dj_common

type t = Input_feeder.t

let query_model model (x, call_stack) : int option =
  let key = Concolic_key.generate x call_stack in
  Solver.SuduZ3.get_int_expr model (Concolic_riddler.key_to_var key)

let default : Input_feeder.t =
  fun _ -> Random.int 21 - 10 (* random int between -10 and 10 inclusive *)

let from_model ?(history = ref []) model : t =
  let input_feeder = query_model model in
  fun query ->
    let answer = input_feeder query in
    history := answer :: !history ;
    Option.value ~default:(default query) answer
    