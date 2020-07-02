open Batteries
open Odefa_ddpa
open Ddpa_abstract_ast
open Ddpa_graph

let find_cond_top cond_btm cfg =
  let rec loop c =
    match c with
    | Nonbinding_enter_clause (Abs_value_bool cond, _) ->
      cond
    | _ ->
      let principle_pred_acl = 
        preds c cfg
        |> List.of_enum 
        |> List.hd
      in
      loop principle_pred_acl
  in
  loop cond_btm

