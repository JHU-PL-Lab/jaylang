(**
   This module contains a definition of the DDSE symbolic interpreter.
*)

open Batteries;;
open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;
open Ddpa_abstract_ast;;
open Ddpa_graph;;
open Interpreter_types;;
open Relative_stack;;
open Sat_types;;
open Symbolic_monad;;
(* open Symbolic_monad_types;; *)

type lookup_environment = {
  le_cfg : ddpa_graph;
  le_clause_mapping : clause Ident_map.t;
  le_first_var : Ident.t;
};;

let rec lookup
    (env : lookup_environment)
    (lookup_stack : Ident.t list)
    (cl0 : clause)
    (relstack : relative_stack)
  : Symbol.t m =
  (* *** Main lookup logic ***)
  let acl0 = lift_clause cl0 in
  let%bind acl1 = pick @@ preds (Unannotated_clause acl0) env.le_cfg in
  match lookup_stack with
  | [] ->
    (* No rule handles an empty variable stack. *)
    zero ()
  | lookup_var :: lookup_stack' ->
    begin
      match acl1 with
      | Unannotated_clause uacl1 ->
        let Abs_clause(Abs_var x,_) = uacl1 in
        let cl1 = Ident_map.find x env.le_clause_mapping in
        let Clause(_,b) = cl1 in
        if not @@ equal_ident lookup_var x then
          let%bind _ = lookup env [x] cl1 relstack in
          lookup env lookup_stack cl1 relstack
        else begin
          match b with
          | Value_body v ->
            if List.is_empty lookup_stack' then
              (* ## Value Discovery rule ## *)
              let%bind _ =
                if equal_ident lookup_var env.le_first_var then
                  return @@ Symbol(env.le_first_var, relstack)
                else
                  lookup env [env.le_first_var] cl1 relstack
              in
              let symbol = Symbol(lookup_var, relstack) in
              let formula = Formula(symbol, Formula_expression_value(v)) in
              let%bind () = record_formula formula in
              return symbol
            else
              (* ## Value Discard rule ## *)
              lookup env lookup_stack' cl1 relstack
          | Var_body(Var(x',_)) ->
            let%bind found_symbol =
              lookup env (x' :: lookup_stack') cl1 relstack
            in
            let symbol = Symbol(lookup_var, relstack) in
            let formula =
              Formula(symbol, Formula_expression_alias(found_symbol))
            in
            let%bind () = record_formula formula in
            return symbol
          | Input_body ->
            if List.is_empty lookup_stack' then
              (* ## Value Discovery rule ## *)
              let%bind _ =
                if equal_ident lookup_var env.le_first_var then
                  return @@ Symbol(env.le_first_var, relstack)
                else
                  lookup env [env.le_first_var] cl1 relstack
              in
              let symbol = Symbol(lookup_var, relstack) in
              let formula = Formula(symbol, Formula_expression_alias(symbol)) in
              let%bind () = record_formula formula in
              return symbol
            else
              (* ## Value Discard rule ## *)
              lookup env lookup_stack' cl1 relstack
          | Appl_body (_, _) ->
            failwith "TODO"
          | Conditional_body (_, _, _) ->
            failwith "TODO"
          | Binary_operation_body (_, _, _) ->
            failwith "TODO"
        end
      | Binding_enter_clause (_, _, _) ->
        failwith "TODO"
      | Binding_exit_clause (_, _, _) ->
        failwith "TODO"
      | Nonbinding_enter_clause _ ->
        failwith "TODO"
      | Start_clause _ ->
        failwith "TODO"
      | End_clause _ ->
        failwith "TODO"
    end
;;
