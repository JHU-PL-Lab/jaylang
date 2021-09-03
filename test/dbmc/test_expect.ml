open Core

type t = {
  inputs : one_run list;
  target : string; [@default "target"]
  strict_match : bool; [@default true]
  max_step : int option; [@sexp.option]
}

and one_run = int list [@@deriving sexp, show { with_path = false }]

let load_sexp_expectation_for testpath =
  let expect_path = Filename.chop_extension testpath ^ ".expect.s" in
  if Sys.is_file_exn expect_path then
    Some (Sexp.load_sexp_conv_exn expect_path t_of_sexp)
  else
    None

(* let t1 : t = {
     runs = [[1]];
     strict_match = true;
     max_step = None;
   }

   let ss = sexp_of_t t1

   let sss = Sexp.to_string ss

   let t1v = sss |> Sexp.of_string |> t_of_sexp *)
