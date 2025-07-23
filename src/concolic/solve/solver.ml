
module type S = sig
  val solve : bool Formula.t list -> Formula.Key.t Overlays.Typed_smt.solution
end

module Make () = Overlays.Typed_smt.Solve (Overlays.Typed_smt.Make_Z3 (struct let ctx = Z3.mk_context [] end))

include Make ()
