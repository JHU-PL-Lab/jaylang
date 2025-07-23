
include Overlays.Typed_smt

(* TODO: parametrize with the key. This is hard coding just for now *)
type 'a t = ('a, Interp_common.Step.t) Overlays.Typed_smt.T.t

module Key = struct
  include Interp_common.Step
  let uid = to_int
end

module Symbol = Make_symbol (Key)
