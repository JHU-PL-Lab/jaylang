
type t = Concolic_key.t -> int

let zero : t =
  fun _ -> 0

let default : t =
  fun _ -> C_random.int_incl (-10) 10

(* TODO: have interface for getting int from key *)
let from_model (model : Z3.Model.model) : t =
  fun key ->
    match C_sudu.int_of_expr model @@ C_sudu.int_var @@ Concolic_key.uniq_id key with
    | Some i -> i
    | None -> default key
    