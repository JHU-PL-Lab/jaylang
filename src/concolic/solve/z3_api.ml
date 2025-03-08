
open Core

module type S = sig
  include module type of Overlays.Typed_z3.Make (struct let ctx = Z3.mk_context [] end)

  val var_of_key : 'a Stepkey.t -> 'a t
  val value_of_key : model -> 'a Stepkey.t -> 'a option
end

module Make () : S = struct
  include Overlays.Typed_z3.Make (struct let ctx = Z3.mk_context [] end) 

  let var_of_key (type a) (key : a Stepkey.t) : a t =
    match key with
    | I id -> int_var id
    | B id -> bool_var id

  let value_of_key model key =
    key
    |> var_of_key
    |> value_of_expr model
end