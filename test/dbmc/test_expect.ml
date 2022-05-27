open Core

(* TODO: sepx_of and of_sexp cannot cancel out *)
type one_run = int option list [@@deriving sexp_of, show { with_path = false }]

let one_run_of_sexp s =
  List.t_of_sexp
    (fun a ->
      match a with Sexp.List _ -> None | Sexp.Atom ns -> int_of_string_opt ns)
    s

type one_case = {
  inputs : one_run list;
  target : string; [@default "target"]
  strict_match : bool; [@default true]
  max_step : int option; [@sexp.option]
}

and t = one_case list [@@deriving sexp, show { with_path = false }]

let load_sexp_expectation_for testpath =
  let expect_path = Filename.chop_extension testpath ^ ".expect.s" in
  if Sys_unix.is_file_exn expect_path
  then Some (Sexp.load_sexp_conv_exn expect_path t_of_sexp)
  else None

(* let t1 : t = {
     runs = [[1]];
     strict_match = true;
     max_step = None;
   }

   let ss = sexp_of_t t1

   let sss = Sexp.to_string ss

   let t1v = sss |> Sexp.of_string |> t_of_sexp *)
