
module SuduZ3 = Sudu.Gadt_z3_api.Make (struct
  let ctx = Z3.mk_context []
end)

include SuduZ3
