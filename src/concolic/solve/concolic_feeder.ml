
type t = Concolic_key.t -> int

let zero : t =
  fun _ -> 0

let default : t =
  fun _ -> C_random.int_incl (-10) 10

let from_model (model : Z3.Model.model) : t =
  fun key ->
    match C_sudu.get_int_expr model key with
    | Some i -> i
    | None -> default key
    