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
  max_step : int option; [@sexp.option]
}

and t = one_case list [@@deriving sexp, show { with_path = false }]
