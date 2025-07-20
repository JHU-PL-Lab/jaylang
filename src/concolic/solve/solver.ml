
module type S = sig
  val solve : bool Formula.t list -> Formula.Key.t Overlays.Typed_smt.solution
end

module Make () = Overlays.Typed_smt.Z3 ()

include Make ()
