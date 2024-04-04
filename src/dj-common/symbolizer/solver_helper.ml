open Core

let ctx = Z3.mk_context []

module C = struct
  let ctx = ctx
end

module Make (C : Sudu.Z3_helper.Context) = struct
  include C
  include Sudu.Z3_helper.Make_helper (C)
  include Sudu.Z3_helper.Contextless_functions
end

include Make (C)
