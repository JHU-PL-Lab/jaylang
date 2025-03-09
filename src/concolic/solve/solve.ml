
module type S = sig
  include Z3_api.S

  module Expression : sig
    val to_formula : 'a Expression.t -> 'a t
  end

  module Input_feeder : sig
    val from_model : model -> Input_feeder.t
  end
end

module Make () = struct
  module Sudu = Z3_api.Make ()
  include Sudu
  
  module Expression = Expression.Solve (Sudu)

  module Input_feeder = Input_feeder.Make (Sudu)
end

module Default = Make ()
