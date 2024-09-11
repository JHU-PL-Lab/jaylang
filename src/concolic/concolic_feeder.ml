
type t = Concolic_key.t -> int

let default : t =
  fun _ -> 0

let from_model (model : Z3.Model.model) : t =
  fun key ->
    match Concolic_riddler.get_int_expr model key with
    | Some i -> i
    | None -> default key
    