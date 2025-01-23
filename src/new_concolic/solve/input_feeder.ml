
open Core

module Input = struct
  type t =
    | Int of int
    | Bool of bool
    [@@deriving compare, sexp]
end

type t = 
  { get_int : Concolic_key.t -> int
  ; get_bool : Concolic_key.t -> bool }

let zero : t =
  { get_int = (fun _ -> 0)
  ; get_bool = fun _ -> false }

let default : t =
  { get_int = (fun _ -> C_random.int_incl (-10) 10)
  ; get_bool = fun _ -> C_random.bool () }

let from_model (model : Z3.Model.model) : t =
  { get_int = (fun key ->
      match C_sudu.get_int_expr model key with
      | Some i -> i
      | None -> default.get_int key
    )
  ; get_bool = fun key -> 
      match C_sudu.get_bool_expr model key with
      | Some b-> b
      | None -> default.get_bool key
    }
