
module Make () = struct
  module Sudu = Z3_intf.Make ()
  include Sudu
  
  module Expression = Expression.Solve (Sudu)

  module Input_feeder = Input_feeder.Make (Sudu)
end