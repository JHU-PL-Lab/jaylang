open Core

type t = Concolic_key.t -> int

let query_model model x_key : int option =
  From_dbmc.Solver.SuduZ3.get_int_expr model (Concolic_riddler.key_to_var x_key)
let default : t =
  fun _ -> Random.int 21 - 10 (* random int between -10 and 10 inclusive *)

let from_model ?(history = ref []) model : t =
  let input_feeder = query_model model in
  fun query ->
    let answer = input_feeder query in
    history := answer :: !history ;
    Option.value ~default:(default query) answer
    