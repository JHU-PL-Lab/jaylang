
include Interp_common.Input_feeder.Make (Interp_common.Step)

let of_model m = Interp_common.Input_feeder.of_smt_model m ~uid:Interp_common.Step.to_int
