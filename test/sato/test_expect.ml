open Core

(* TODO: sepx_of and of_sexp cannot cancel out *)
(* type one_run = int option list [@@deriving sexp_of, show { with_path = false }]

   let one_run_of_sexp s =
     List.t_of_sexp
       (fun a ->
         match a with Sexp.List _ -> None | Sexp.Atom ns -> int_of_string_opt ns)
       s *)
type type_error = {
  t_var : string;
  t_expected_type : string;
  t_actual_type : string;
}

and match_error = {
  m_value : string list * string;
  expected_type : string;
  actual_type : string;
}

and value_error = { v_value : string list * string }

and error =
  | Match_error of match_error
  | Value_error of value_error
  | Type_error of type_error

and t = {
  found_at_clause : string;
  number_of_errors : int;
  error_list : error list;
}
[@@deriving sexp, equal, show { with_path = false }]
(* }[@@deriving sexp, equal] *)

let load_sexp_expectation_for testpath =
  let expect_path = Filename.chop_extension testpath ^ ".expect.s" in
  if Sys_unix.is_file_exn expect_path
  then Some (Sexp.load_sexp_conv_exn expect_path t_of_sexp)
  else None

(*
   ** NatOdefa Type Errors **
   - Input sequence  : [0]
   - Found at clause : let (id : (bool -> bool)) x
    = 1 + 1 in id
   - Found in steps  : 1175
   --------------------
   * Value    : id
   * Expected : (bool -> bool)
   * Actual   : (bool -> int)
   1 error found.
   No further control flows exist.
*)

(* let t1 : t = {
        found_at_clause = "let (id : (bool -> bool)) x = 1 + 1 in id";
        number_of_errors = 1;
        error_list =
        [
         (Type_error {
           t_var = "id";
           t_expected_type = "(bool -> bool)";
           t_actual_type = "(bool -> int)";
         });
        ]
      }

   let ss = sexp_of_t t1

   let sss = Sexp.to_string_hum ss *)

(* let t1v = sss |> Sexp.of_string |> t_of_sexp *)
