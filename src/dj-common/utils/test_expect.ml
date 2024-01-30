open Core

(* A test_expect file contains a list of expect, usually one.

   If a test contains no expect file, we set the list with only one
   default element `default`. It doesn't specify any input with `Input_spec.any_input`.
   The default target is the variable `target`.
   That is to say, we expect the program should reach `target` with any input

   Expected unreachable is set by an `Input_spec.unreachable`.
*)

type one_case = {
  inputs : Input_spec.t;
  target : string; [@default "target"]
  max_step : int option; [@sexp.option]
}

and t = one_case list [@@deriving sexp, show { with_path = false }]

let default =
  { inputs = Input_spec.any_input; target = Id.target_s; max_step = None }
