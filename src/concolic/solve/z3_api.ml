
open Core

module type S = sig
  type model
  include Overlays.Typed_z3.S with type model := model

  val var_of_key : 'a Interp_common.Key.Stepkey.t -> 'a t
  val value_of_key : model -> 'a Interp_common.Key.Stepkey.t -> 'a option
end

module Make () : S = struct
  include Overlays.Typed_z3.New_context ()

  let var_of_key (type a) (key : a Interp_common.Key.Stepkey.t) : a t =
    match key with
    | I id -> int_var id
    | B id -> bool_var id

  let value_of_key model key =
    key
    |> var_of_key
    |> value_of_expr model
end