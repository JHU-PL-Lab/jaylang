
(* This functor is generative because it makes a new Z3 context *)
module Make () : sig
  include Z3_intf.S

  module Expression : sig
    val t_to_formula : 'a Expression.t -> 'a t
  end

  module Input_feeder : sig
    val from_model : Z3.Model.model -> Input_feeder.t
  end
end