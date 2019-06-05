(**
   This module contains a definition of the DDSE symbolic interpreter.
*)

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

(** This type describes the information which must be in context during lookup. *)
type lookup_environment = {
  le_cfg : ddpa_graph;
  (** The DDPA CFG to use during lookup. *)

  le_clause_mapping : clause Ident_map.t;
  (** A mapping from identifiers to the concrete clauses which define them. *)

  le_function_parameter_mapping : function_value Ident_map.t;
  (** A mapping from function parameter variables to the functions which declare
      them. *)

  le_function_return_mapping : function_value Ident_map.t;
  (** A mapping from function return variables to the functions which declare
      them. *)

  le_first_var : Ident.t;
  (** The identifier which represents the first defined variable in the
      program. *)
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
                              Abs_appl_body(Abs_var xf, Abs_var _)) = c in
        (* Find the concrete clause corresponding to c.*)
        let cc = Ident_map.find xr env.le_clause_mapping in
        (* MayBeTop check *)
        let%orzero true = may_be_top relstack cc in
        (* Decision check *)
        let symbol = Symbol(x, relstack) in
        let%bind () = record_decision symbol x cc x' in
        (* Real function check *)
        (* Rather than trying to find the functions which could reach xf, we'll
           just find the function to which this wiring node corresponds and
           require it to equal xf in the formulae. *)
        let fnval = Ident_map.find x env.le_function_parameter_mapping in
        let lookup_symbol = Symbol(lookup_var, relstack) in
        let fun_symbol = Symbol(xf, relstack) in
        let%bind fun_definition_symbol = lookup env [xf] acl1 relstack in
        let%bind () = record_formula @@
          Formula(fun_definition_symbol,
                  Formula_expression_value(Value_function(fnval)))
        in
        (* NOTE: we probably don't actually need this formula? *)
        let%bind () = record_formula @@
          Formula(fun_definition_symbol,
                  Formula_expression_alias(fun_symbol))
        in
        (* Record our decision to use this wiring node. *)
        let cc = Ident_map.find xr env.le_clause_mapping in
        let%bind () = record_decision lookup_symbol x cc x' in
        (* The only difference in the rules is the stack that we use to continue
           lookup, which varies based upon whether we're looking for a parameter
           or a non-local. *)
        let%orzero Some popped_stack = pop relstack cc in
        if equal_ident lookup_var x then begin
          (* ## Function Enter Parameter rule ## *)
          lookup env (x' :: lookup_stack') acl1 popped_stack
        end else begin
          (* ## Function Enter Non-Local rule ## *)
          lookup env (xf :: lookup_var :: lookup_stack') acl1 popped_stack
        end
      | Binding_exit_clause (Abs_var x, Abs_var x', c) ->
        [%guard equal_ident x lookup_var];
        (* The rules applying to binding exit clauses are the Function Bottom
           and Conditional Bottom rules. *)
        begin
          match c with
          | Abs_clause(Abs_var xr, Abs_appl_body(Abs_var xf, Abs_var _)) ->
            (* ## Function Exit rule ## *)
            [%guard may_be_top relstack @@
              Ident_map.find xr env.le_clause_mapping];
            (* Based upon the exit clause, we can figure out which function
               would have to have been called for us to use it. *)
            let fnval = Ident_map.find x' env.le_function_return_mapping in
            (* Rather than try to find the functions that may appear in xf, we
               can just look up xf (to induce equations) and then insist that
               xf is equal to this function value. *)
            let fun_symbol = Symbol(xf, relstack) in
            let%bind fun_definition_symbol = lookup env [xf] acl1 relstack in
            let%bind () = record_formula @@
              Formula(fun_definition_symbol,
                      Formula_expression_value(Value_function(fnval)))
            in
            (* NOTE: we probably don't actually need this formula? *)
            let%bind () = record_formula @@
              Formula(fun_definition_symbol,
                      Formula_expression_alias(fun_symbol))
            in
            lookup env (x' :: lookup_stack') acl1 relstack
          | Abs_clause(Abs_var x_, Abs_conditional_body(Abs_var x1, e1, _)) ->
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
            let subject_symbol = Symbol(x1, relstack) in
            let%bind subject_definition_symbol =
              lookup env [x1] acl1 relstack
            in
            let%bind () = record_formula @@
              Formula(subject_definition_symbol,
                      Formula_expression_value(Value_bool target_value))
            in
            (* NOTE: We probably don't actually need this formula. *)
            let%bind () = record_formula @@
              Formula(subject_symbol,
                      Formula_expression_alias(subject_definition_symbol))
            in
            lookup env (x' :: lookup_stack) acl1 relstack
          | _ ->
            raise @@ Jhupllib.Utils.Invariant_failure
              "Non-application, non-conditional body found in binding exit clause!"
        end
      | Nonbinding_enter_clause(v,c) ->
        (* ## Conditional Top rule ## *)
        let%orzero Abs_clause(Abs_var x,
                              Abs_conditional_body(Abs_var x1, _, _)) = c
        in
        [%guard equal_ident x lookup_var];
        (* We have to verify that any lookups of the subject variable match the
           value of the non-binding enter clause.  This can be accomplished by
           looking up the *variable* and then adding a formula to the set which
           constrains it.  This won't stop computation until the solver is
           called, but it will guarantee that bad universes are halted. *)
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
