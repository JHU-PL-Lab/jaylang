open Batteries
open Odefa_ddpa
open Ddpa_abstract_ast
open Ddpa_graph
open Jhupllib
open Logger_utils

let lazy_logger = make_lazy_logger "Symbolic_interpreter.Interpreter"

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

let log_constraints constraints =
  lazy_logger `info (fun () ->
      Printf.sprintf "phis: %s\n"
        (Jhupllib.Pp_utils.pp_to_string
           (Jhupllib.Pp_utils.pp_list Constraint.pp) constraints)
    )