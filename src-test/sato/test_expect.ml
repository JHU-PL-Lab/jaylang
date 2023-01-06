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
(* [@@deriving sexp, equal] *)

let dedup_whitespace s =
  s
  |> String.substr_replace_all ~pattern:"\n" ~with_:" "
  |> String.split ~on:' '
  |> List.filter ~f:(fun s -> not @@ String.is_empty s)
  |> String.concat ~sep:" "

let clean_up_error_str err =
  match err with
  | Match_error { m_value = xs, v; expected_type = t1; actual_type = t2 } ->
      let xs' = xs |> List.map ~f:dedup_whitespace in
      let v' = v |> dedup_whitespace in
      let t1' = t1 |> dedup_whitespace in
      let t2' = t2 |> dedup_whitespace in
      Match_error
        { m_value = (xs', v'); expected_type = t1'; actual_type = t2' }
  | Value_error { v_value = xs, v } ->
      let xs' = xs |> List.map ~f:dedup_whitespace in
      let v' = v |> dedup_whitespace in
      Value_error { v_value = (xs', v') }
  | Type_error { t_var = x; t_expected_type = t1; t_actual_type = t2 } ->
      let x' = x |> dedup_whitespace in
      let t1' = t1 |> dedup_whitespace in
      let t2' = t2 |> dedup_whitespace in
      Type_error { t_var = x'; t_expected_type = t1'; t_actual_type = t2' }

let clean_up_t t =
  match t with
  | { found_at_clause = cls; number_of_errors = n; error_list = errs } ->
      let cls' = cls |> dedup_whitespace in
      let errs' = errs |> List.map ~f:clean_up_error_str in
      { found_at_clause = cls'; number_of_errors = n; error_list = errs' }

let load_sexp_expectation_for testpath =
  let expect_path = Filename.chop_extension testpath ^ ".expect.s" in
  if Sys_unix.is_file_exn expect_path
  then Some (Sexp.load_sexp_conv_exn expect_path t_of_sexp)
  else None

(*
** Bluejay Type Errors **
- Input sequence  : 0,1,-1
- Found at clause : let rec 
          prepend (l1 : [int])  (l2 : [bool])
           : [int] = match l2 with 
                                     | [] -> l1
                                     | hd :: tl -> prepend (hd :: l1) tl
                                    end
 in prepend
--------------------
* Value    : prepend
* Expected : ([int] -> ([bool] -> [int]))
* Actual   : ([int] -> ([bool] -> [bool]))
*)

(* let t1 : t = {
           found_at_clause = "let rec prepend (l1 : [int])  (l2 : [bool]) : [int] = match l2 with | [] -> l1 | hd :: tl -> prepend (hd :: l1) tl end";
           number_of_errors = 1;
           error_list =
           [
            (Type_error {
              t_var = "prepend";
              t_expected_type = "([int] -> ([bool] -> [int]))";
              t_actual_type = "([int] -> ([bool] -> [bool]))";
            });
           ]
         }
      let ss = sexp_of_t t1

      let sss = Sexp.to_string_hum ss *)

(* let t1v = sss |> Sexp.of_string |> t_of_sexp *)
