open Core

type one_case = {
  inputs : Input_spec.t;
  target : string; [@default "target"]
  max_step : int option; [@sexp.option]
}

and t = one_case list [@@deriving sexp, show { with_path = false }]

let default =
  { inputs = Input_spec.no_spec; target = Id.target_s; max_step = None }
