(**
   This module contains a definition of the DDSE symbolic interpreter.
*)

(*
open Batteries;;
open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;
open Ddpa_abstract_ast;;
open Ddpa_graph;;
open Ddpa_utils;;
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
    (acl0 : annotated_clause)
    (relstack : relative_stack)
  : Symbol.t m =
  let%bind acl1 = pick @@ preds acl0 env.le_cfg in
  match lookup_stack with
  | [] ->
    (* No rule handles an empty variable stack. *)
    zero ()
  | lookup_var :: lookup_stack' ->
    begin
      match acl1 with
      | Unannotated_clause uacl1 ->
        let Abs_clause(Abs_var x,ab) = uacl1 in
        if not @@ equal_ident lookup_var x then
          (* ## Skip rule ## *)
          let%bind _ = lookup env [x] acl1 relstack in
          lookup env lookup_stack acl1 relstack
        else begin
          (* In all cases of this match, we already know that the top variable
             of the lookup stack is the variable defined by this clause. *)
          match ab with
          | Abs_value_body _ ->
            let cl1 = Ident_map.find x env.le_clause_mapping in
            let%orzero Clause(_,Value_body(v)) = cl1 in
            if List.is_empty lookup_stack' then
              (* ## Value Discovery rule ## *)
              let%bind _ =
                if equal_ident lookup_var env.le_first_var then
                  return @@ Symbol(env.le_first_var, relstack)
                else
                  lookup env [env.le_first_var] acl1 relstack
              in
              let symbol = Symbol(lookup_var, relstack) in
              let formula = Formula(symbol, Formula_expression_value(v)) in
              let%bind () = record_formula formula in
              return symbol
            else
              (* ## Value Discard rule ## *)
              lookup env lookup_stack' acl1 relstack
          | Abs_var_body(Abs_var(x')) ->
            (* ## Alias rule ## *)
            let%bind found_symbol =
              lookup env (x' :: lookup_stack') acl1 relstack
            in
            let symbol = Symbol(lookup_var, relstack) in
            let formula =
              Formula(symbol, Formula_expression_alias(found_symbol))
            in
            let%bind () = record_formula formula in
            return symbol
          | Abs_input_body ->
            if List.is_empty lookup_stack' then
              (* ## Value Discovery rule ## *)
              let%bind _ =
                if equal_ident lookup_var env.le_first_var then
                  return @@ Symbol(env.le_first_var, relstack)
                else
                  lookup env [env.le_first_var] acl1 relstack
              in
              let symbol = Symbol(lookup_var, relstack) in
              let formula = Formula(symbol, Formula_expression_alias(symbol)) in
              let%bind () = record_formula formula in
              return symbol
            else
              (* ## Value Discard rule ## *)
              lookup env lookup_stack' acl1 relstack
          | Abs_appl_body (_, _) ->
            (* No rules apply to Appl_body when the lookup variable matches.
               (The function rules apply to wiring nodes and the skip rule
               applies when the lookup variable does not match. *)
            zero ()
          | Abs_conditional_body (_, _, _) ->
            (* No rules apply to Conditional_body when the lookup variable
               matches.  (The conditional rules apply to wiring nodes and the
               skip rule applies when the lookup variable does not match. *)
            zero ()
          | Abs_binary_operation_body (Abs_var(x1), op, Abs_var(x2)) ->
            (* ## Binop rule ## *)
            let%bind symbol1 = lookup env (x1 :: lookup_stack) acl1 relstack in
            let%bind symbol2 = lookup env (x2 :: lookup_stack) acl1 relstack in
            let symbol = Symbol(lookup_var, relstack) in
            let formula =
              Formula(symbol, Formula_expression_binop(symbol1, op, symbol2))
            in
            let%bind () = record_formula formula in
            return symbol
        end
      | Binding_enter_clause (Abs_var x, Abs_var x', c) ->
        [%guard equal_ident x lookup_var];
        (* The only rules which apply to binding enter clauses are Function
           Enter Parameter and Function Enter Non-Local.  They have a lot in
           common, so let's do that part first. *)
        let%orzero Abs_clause(Abs_var xr,
                              Abs_appl_body(Abs_var xf, Abs_var xa)) = c in
        (* Find the concrete clause corresponding to c.*)
        let cc = Ident_map.find xr env.le_clause_mapping in
        (* MayBeTop check *)
        let%orzero true = may_be_top relstack cc in
        (* Decision check *)
        let symbol = Symbol(x, relstack) in
        let%bind () = record_decision symbol x cc x' in
        (* Real function check *)
        (* TODO: find the functions corresponding to xf and ensure one of them
           is the function with parameter x. *)
        (* The only difference in the rules is the stack that we use to continue
           lookup, which varies based upon whether we're looking for a parameter
           or a non-local. *)
        if equal_ident lookup_var x then begin
          (* ## Function Enter Parameter rule ## *)
          failwith "TODO"
        end else begin
          (* ## Function Enter Non-Local rule ## *)
          failwith "TODO"
        end
      | Binding_exit_clause (Abs_var x, Abs_var x', c) ->
        [%guard equal_ident x lookup_var];
        (* The rules applying to binding exit clauses are the Function Bottom
           and Conditional Bottom rules. *)
        begin
          match c with
          | Abs_clause(Abs_var xr, Abs_appl_body(Abs_var xf, Abs_var xa)) ->
            (* ## Function Exit rule ## *)
            failwith "TODO"
          | Abs_clause(Abs_var x_, Abs_conditional_body(Abs_var x1, e1, e2)) ->
            (* ## Conditional Bottom - True AND Conditional Bottom - False ## *)
            [%guard equal_ident x x_];
            (* These rules have exactly the same preconditions with two
               exceptions: whether the return variable of e1 or e2 is used and
               whether true or false is used.  Decide that immediately and then
               generalize the rest of the code. *)
            let target_value =
              let Abs_var rve1 = retv e1 in
              if equal_ident rve1 x' then true else false
            in
            (*
            failwith "TODO"
          | _ ->
            failwith "Non-application, non-conditional body found in binding exit clause!"
        end
      | Nonbinding_enter_clause(v,c) ->
        (* ## Conditional Top rule ## *)
        let%orzero Abs_clause(Abs_var x,
                              Abs_conditional_body(Abs_var x1, e1, e2)) = c
        in
        (* We're obliged to do a subordinate lookup here to determine if the
           value from the non-binding enter clause (a boolean) is in the set of
           values returned.  Rather than actually inspecting the formulae,
           though, we can just *require* that this holds after the lookup.  If
           it's not true, we should get an immediate failure to add the formulae
           leading the computation to zero itself (since it's a boolean). *)
        let%bind symbol_x' = lookup env [x1] (Unannotated_clause c) relstack in
        let v' =
          match v with
          | Abs_value_bool(b) -> Value_bool b
          | _ -> raise @@ Jhupllib.Utils.Invariant_failure
              "non-boolean in non-binding enter clause"
        in
        let formula =
          Formula(symbol_x', Formula_expression_value v')
        in
        let%bind () = record_formula formula in
        lookup env lookup_stack acl1 relstack
      | Start_clause _
      | End_clause _ ->
        (* Although the formal specification does not list these clauses, they
           exist in the implementation to produce more readable and meaningful
           graphs.  They can be ignored. *)
        lookup env lookup_stack acl1 relstack
    end
;;
