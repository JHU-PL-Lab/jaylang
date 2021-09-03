open Core

type t = {
  inputs : one_run list;
  target : string [@default "target"];
  strict_match : bool [@default true];
  max_step: int option [@sexp.option];
} 
and 
one_run = int list
[@@deriving sexp]

(* let t1 : t = {
  runs = [[1]];
  strict_match = true;
  max_step = None;
}

let ss = sexp_of_t t1

let sss = Sexp.to_string ss

let t1v = sss |> Sexp.of_string |> t_of_sexp *)