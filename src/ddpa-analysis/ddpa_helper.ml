open Batteries
open Ddpa_abstract_ast
open Ddpa_graph

let has_condition_clause clauses =
  List.exists
    (function
      (* main condition exp *)
      | Unannotated_clause (Abs_clause (Abs_var _, Abs_conditional_body _)) ->
          true
      | _ -> false)
    clauses

let has_application_clause clauses =
  List.exists
    (function
      (* callsite exp *)
      | Unannotated_clause (Abs_clause (Abs_var _, Abs_appl_body _)) -> true
      | _ -> false)
    clauses

let log_acl acl prevs =
  print_endline
  @@ Printf.sprintf "%s \t\t[prev: %s]"
       (Jhupllib.Pp_utils.pp_to_string pp_brief_annotated_clause acl)
       (Jhupllib.Pp_utils.pp_to_string
          (Jhupllib.Pp_utils.pp_list pp_brief_annotated_clause)
          prevs)

let find_cond_choice cond_btm cfg =
  let rec loop c =
    match c with
    | Nonbinding_enter_clause (Abs_value_bool cond, _) -> cond
    | _ ->
        let principle_pred_acl = preds c cfg |> List.of_enum |> List.hd in
        loop principle_pred_acl
  in
  loop cond_btm

let preds_l acl g = preds acl g |> List.of_enum
let succs_l acl g = succs acl g |> List.of_enum
