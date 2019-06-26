open Batteries;;
open Jhupllib;;
open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;
open Ast_pp;;
open Ddpa_abstract_ast;;
open Ddpa_graph;;
open Ddpa_utils;;
open Interpreter_types;;
open Relative_stack;;
open Sat_types;;

let lazy_logger =
  Logger_utils.make_lazy_logger "Symbolic_interpreter.Interpreter"
;;

module Cache_key = struct
  type 'a t =
    | Cache_lookup :
        Ident.t list * annotated_clause * relative_stack ->
        (symbol * concrete_stack) t
  ;;

  let compare : type a b. a t -> b t -> (a, b) Gmap.Order.t = fun k1 k2 ->
    match k1, k2 with
    | Cache_lookup(lookup_stack_1, acl1, relative_stack_1),
      Cache_lookup(lookup_stack_2, acl2, relative_stack_2) ->
      let c = List.compare Ident.compare lookup_stack_1 lookup_stack_2 in
      if c < 0 then Lt else
      if c > 0 then Gt else
        let c = Annotated_clause.compare acl1 acl2 in
        if c < 0 then Lt else
        if c > 0 then Gt else
          let c =
            Relative_stack.compare_relative_stack
              relative_stack_1 relative_stack_2
          in
          if c < 0 then Lt else
          if c > 0 then Gt else
            Eq
  ;;
end;;

module M = Symbolic_monad.Make(
  struct
    module Cache_key = Cache_key;;
    module Work_collection = Symbolic_monad.QueueWorkCollection;;
  end);;

open M;;

(** This type describes the information which must be in context during lookup. *)
type lookup_environment = {
  le_cfg : ddpa_graph;
  (** The DDPA CFG to use during lookup. *)

  le_clause_mapping : clause Ident_map.t;
  (** A mapping from identifiers to the concrete clauses which define them. *)

  le_clause_predecessor_mapping : clause Ident_map.t;
  (** A mapping from clauses (represented by the identifier they define) to the
      concrete clauses which appear immediately *before* them.  The first clause
      in each expression does not appear in this map. *)

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

(**
   Given a program and its corresponding CFG, constructs an appropriate lookup
   environment.
*)
let prepare_environment (e : expr) (cfg : ddpa_graph)
  : lookup_environment =
  let rec enum_all_functions_in_expr expr : function_value Enum.t =
    let Expr(clauses) = expr in
    Enum.concat @@ Enum.map enum_all_functions_in_clause @@ List.enum clauses
  and enum_all_functions_in_clause clause : function_value Enum.t =
    let Clause(_,body) = clause in
    enum_all_functions_in_body body
  and enum_all_functions_in_body body : function_value Enum.t =
    match body with
    | Value_body v ->
      enum_all_functions_in_value v
    | Var_body _
    | Input_body
    | Appl_body _
    | Binary_operation_body (_, _, _) ->
      Enum.empty ()
    | Conditional_body (_, e1, e2) ->
      Enum.append
        (enum_all_functions_in_expr e1) (enum_all_functions_in_expr e2)
  and enum_all_functions_in_value value : function_value Enum.t =
    match value with
    | Value_function(Function_value(_,e) as f) ->
      Enum.append (Enum.singleton f) @@ enum_all_functions_in_expr e
    | Value_int _
    | Value_bool _ -> Enum.empty ()
  in
  let function_parameter_mapping, function_return_mapping =
    enum_all_functions_in_expr e
    |> Enum.fold
      (fun (pm,rm) (Function_value(Var(p,_),Expr(clss)) as f) ->
         let pm' = Ident_map.add p f pm in
         let retvar =
           clss
           |> List.last
           |> (fun (Clause(Var(r,_),_)) -> r)
         in
         let rm' = Ident_map.add retvar f rm in
         (pm', rm')
      )
      (Ident_map.empty, Ident_map.empty)
  in
  let clause_mapping =
    Ast_tools.flatten e
    |> List.enum
    |> Enum.fold
      (fun map (Clause(Var(x,_),_) as c) ->
         Ident_map.add x c map
      )
      Ident_map.empty
  in
  let rec expr_flatten ((Expr clauses) as expr) : expr list =
    expr ::
    (clauses
     |>
     List.map
       (fun (Clause(_,b)) ->
          match b with
          | Value_body (Value_function(Function_value(_,e))) -> expr_flatten e
          | Value_body _
          | Var_body _
          | Input_body
          | Appl_body (_, _)
          | Binary_operation_body (_, _, _) -> []
          | Conditional_body (_, e1, e2) ->
            e1 :: e2 :: expr_flatten e1 @ expr_flatten e2
       )
     |> List.concat
    )
  in
  let clause_predecessor_mapping =
    expr_flatten e
    |> List.enum
    |> Enum.map
      (fun (Expr clauses) ->
         let c1 = List.enum clauses in
         let c2 = List.enum clauses in
         Enum.drop 1 c1;
         Enum.combine (c1,c2)
         |> Enum.map
           (fun (Clause(Var(x,_),_),clause) -> (x,clause))
      )
    |> Enum.concat
    |> Ident_map.of_enum
  in
  let first_var =
    e
    |> (fun (Expr cls) -> cls)
    |> List.first
    |> (fun (Clause(Var(x,_),_)) -> x)
  in
  { le_cfg = cfg;
    le_clause_mapping = clause_mapping;
    le_clause_predecessor_mapping = clause_predecessor_mapping;
    le_function_parameter_mapping = function_parameter_mapping;
    le_function_return_mapping = function_return_mapping;
    le_first_var = first_var;
  }
;;

(* NOTE 1: rather than introducing a "stack=" SAT formula, we instead have
   lookup return a pair.  The "stack=" formula in the spec is a hack to avoid
   dealing with pairs in the lookup definition since the mathematical notation
   is... unpleasant.

   This incurs an obligation: we technically need to show that all of the stacks
   are the same.  In the "stack=" variation, two distinct stacks would lead to
   unsatisfiable formulae; here, if two different stacks are returned, we must
   zero the computation.  Conveniently, however, this never occurs: all lookups
   respect the stack discipline and the function call decision set is shared
   between them.  Therefore, all stacks *will* be the same; we needn't verify,
   which is good because it'd make the code quite a lot sloppier.
*)
let rec lookup
    (env : lookup_environment)
    (lookup_stack : Ident.t list)
    (acl0 : annotated_clause)
    (relstack : relative_stack)
  : (symbol * concrete_stack) m =
  let%bind acl1 = pick @@ preds acl0 env.le_cfg in
  let zeromsg msg () =
    lazy_logger `trace
      (fun () ->
         Printf.sprintf "ZERO (%s) for lookup of\n  %s\n  with stack %s\n  at %s\n  after %s\n"
           msg
           (Jhupllib.Pp_utils.pp_to_string
              (Jhupllib.Pp_utils.pp_list pp_ident) lookup_stack)
           (Jhupllib.Pp_utils.pp_to_string pp_relative_stack relstack)
           (Jhupllib.Pp_utils.pp_to_string
              pp_brief_annotated_clause acl0)
           (Jhupllib.Pp_utils.pp_to_string
              pp_brief_annotated_clause acl1)
      );
    zero ()
  in
  let%bind () = pause () in
  let recurse lookup_stack' acl0' relstack' =
    lazy_logger `trace
      (fun () ->
         Printf.sprintf
           "From lookup of\n  %s\n  with stack %s\n  at %s\n  after %s\nRecursively looking up\n  %s\n  with stack %s\n  at %s\n"
           (Jhupllib.Pp_utils.pp_to_string
              (Jhupllib.Pp_utils.pp_list pp_ident) lookup_stack)
           (Jhupllib.Pp_utils.pp_to_string pp_relative_stack relstack)
           (Jhupllib.Pp_utils.pp_to_string
              pp_brief_annotated_clause acl0)
           (Jhupllib.Pp_utils.pp_to_string
              pp_brief_annotated_clause acl1)
           (Jhupllib.Pp_utils.pp_to_string
              (Jhupllib.Pp_utils.pp_list pp_ident) lookup_stack')
           (Jhupllib.Pp_utils.pp_to_string pp_relative_stack relstack')
           (Jhupllib.Pp_utils.pp_to_string
              pp_brief_annotated_clause acl0')
      );
    cache (Cache_lookup(lookup_stack', acl0', relstack')) @@
    lookup env lookup_stack' acl0' relstack'
  in
  lazy_logger `trace
    (fun () ->
       Printf.sprintf
         "Looking up\n  %s\n  with stack %s\n  at %s\n  after %s\n"
         (Jhupllib.Pp_utils.pp_to_string
            (Jhupllib.Pp_utils.pp_list pp_ident) lookup_stack)
         (Jhupllib.Pp_utils.pp_to_string pp_relative_stack relstack)
         (Jhupllib.Pp_utils.pp_to_string pp_brief_annotated_clause acl0)
         (Jhupllib.Pp_utils.pp_to_string pp_brief_annotated_clause acl1)
    );
  match lookup_stack with
  | [] ->
    (* No rule handles an empty variable stack. *)
    zeromsg "empty variable stack" ()
  | lookup_var :: lookup_stack' ->
    begin
      match acl1 with
      | Unannotated_clause uacl1 ->
        let Abs_clause(Abs_var x,ab) = uacl1 in
        if not @@ equal_ident lookup_var x then
          (* ## Skip rule ## *)
          let%bind _ = recurse [x] acl0 relstack in
          recurse lookup_stack acl1 relstack
        else begin
          (* In all cases of this match, we already know that the top variable
             of the lookup stack is the variable defined by this clause. *)
          match ab with
          | Abs_value_body _ ->
            let zero = zeromsg "Abs_value_body" in
            let cl1 = Ident_map.find x env.le_clause_mapping in
            let%orzero Clause(_,Value_body(v)) = cl1 in
            lazy_logger `trace
              (fun () -> "Discovered value: " ^ Ast_pp_brief.show_value v);
            if List.is_empty lookup_stack' then
              (* ## Value Discovery rule ## *)
              let lookup_symbol = Symbol(lookup_var, relstack) in
              let%bind () = record_formula @@
                Formula(lookup_symbol, Formula_expression_value(v))
              in
              if equal_ident lookup_var env.le_first_var then begin
                (* Then we've found the start of the program!  Build an
                   appropriate concrete stack. *)
                lazy_logger `trace
                  (fun () -> "This lookup complete; top of program reached.");
                return (lookup_symbol, stackize relstack)
              end else begin
                lazy_logger `trace
                  (fun () ->
                     "This lookup complete; resuming to top of program.");
                let%bind (_, stack) =
                  recurse [env.le_first_var] acl1 relstack
                in
                return (lookup_symbol, stack)
              end
            else
              (* ## Value Discard rule ## *)
              recurse lookup_stack' acl1 relstack
          | Abs_var_body(Abs_var(x')) ->
            (* ## Alias rule ## *)
            recurse (x' :: lookup_stack') acl1 relstack
          | Abs_input_body ->
            (* ## Input rule ## *)
            let lookup_symbol = Symbol(lookup_var, relstack) in
            let formula =
              Formula(SpecialSymbol SSymTrue,
                      Formula_expression_binop(
                        lookup_symbol,
                        Binary_operator_equal_to,
                        lookup_symbol))
            in
            let%bind () = record_formula formula in
            (* The lookup definition would technically imply that a
               non-singleton lookup stack is okay here, but we made that
               decision because we believed it was impossible for a
               non-singleton to appear.  Defensive programming FTW. *)
            if not @@ List.is_empty lookup_stack' then begin
              raise @@ Jhupllib.Utils.Invariant_failure
                "Non-singleton lookup stack in Input rule!"
            end;
            if equal_ident lookup_var env.le_first_var then
              (* Then we've found the start of the program!  Get the concrete
                 stack together. *)
              return (lookup_symbol, stackize relstack)
            else
              let%bind (_, stack) =
                recurse [env.le_first_var] acl1 relstack
              in
              return (lookup_symbol, stack)
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
            (* We ignore the stacks here intentionally; see note 1 above. *)
            let%bind (symbol1, _) = recurse [x1] acl1 relstack in
            let%bind (symbol2, _) = recurse [x2] acl1 relstack in
            let lookup_symbol = Symbol(lookup_var, relstack) in
            let%bind () = record_formula @@
              Formula(lookup_symbol,
                      Formula_expression_binop(symbol1, op, symbol2))
            in
            (* The "further" clause in the Binop rule says that, if the lookup
               stack is non-empty, we have to look that stuff up too.  That
               should never happen because none of our operators operate on
               functions and functions are the only non-bottom elements in the
               lookup stack.  So instead, we'll just skip the check here and
               play defensively; it saves us a bind. *)
            if not @@ List.is_empty lookup_stack' then begin
              raise @@ Jhupllib.Utils.Not_yet_implemented
                "Non-singleton lookup stack in Binop rule!"
            end;
            let%bind (_, stack) =
              recurse [env.le_first_var] acl1 relstack
            in
            return (lookup_symbol, stack)
        end
      | Binding_enter_clause (Abs_var x, Abs_var x', c) ->
        (* The only rules which apply to binding enter clauses are Function
           Enter Parameter and Function Enter Non-Local.  They have a lot in
           common, so let's do that part first. *)
        let%orzero Abs_clause(Abs_var xr,
                              Abs_appl_body(Abs_var xf, Abs_var _)) = c in
        (* Find the concrete clause corresponding to c.*)
        let cc = Ident_map.find xr env.le_clause_mapping in
        let zero = zeromsg "Binding_enter_clause" in
        lazy_logger `trace(fun () ->
            Printf.sprintf "BEC: relstack = %s, xr = %s"
              (Relative_stack.show_relative_stack relstack) (show_ident xr));
        let%orzero Some relstack' = pop relstack xr in
        lazy_logger `trace(fun () ->
            "BEC: relstack' = " ^
            Relative_stack.show_relative_stack relstack');
        (* Decision check *)
        let symbol = Symbol(x, relstack) in
        let%bind () = record_decision symbol x cc x' in
        (* Function lookup *)
        (* We ignore the stack here intentionally; see note 1 above. *)
        let%bind (function_symbol, _) =
          recurse [xf] (Unannotated_clause c) relstack'
        in
        (* Record our decision to use this wiring node. *)
        let%bind () = record_decision (Symbol(x,relstack)) x cc x' in
        (* Real function check *)
        (* Rather than trying to find the functions which could reach xf, we'll
           just find the function to which this wiring node corresponds and
           require it to equal xf in the formulae. *)
        let fnval = Ident_map.find x env.le_function_parameter_mapping in
        let%bind () = record_formula @@
          Formula(function_symbol,
                  Formula_expression_value(Value_function(fnval)))
        in
        (* The only difference in the rules is the stack that we use to continue
           lookup, which varies based upon whether we're looking for a parameter
           or a non-local. *)
        if equal_ident lookup_var x then begin
          (* ## Function Enter Parameter rule ## *)
          recurse (x' :: lookup_stack') acl1 relstack'
        end else begin
          (* ## Function Enter Non-Local rule ## *)
          recurse (xf :: lookup_var :: lookup_stack') acl1 relstack'
        end
      | Binding_exit_clause (Abs_var x, Abs_var x', c) ->
        (* The rules applying to binding exit clauses are the Function Bottom
           and Conditional Bottom rules.  These rules all require that the
           exit clause binds the variable we are looking up. *)
        [%guard equal_ident x lookup_var];
        let zero = zeromsg "Binding_exit_clause" in
        begin
          match c with
          | Abs_clause(Abs_var xr, Abs_appl_body(Abs_var xf, Abs_var _)) ->
            (* ## Function Exit rule ## *)
            (* Based upon the exit clause, we can figure out which function
               would have to have been called for us to use it. *)
            let fnval = Ident_map.find x' env.le_function_return_mapping in
            (* Look up function *)
            let%bind (formula_symbol, _) =
              recurse [xf] (Unannotated_clause c) relstack
            in
            (* Induce formula *)
            let%bind () = record_formula @@
              Formula(formula_symbol,
                      Formula_expression_value(Value_function(fnval)))
            in
            (* Produce result *)
            let%orzero Some relstack' = push relstack xr in
            recurse (x' :: lookup_stack') acl1 relstack'
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
            (* Acquire subject formulae *)
            (* We ignore the returned stack here; see note 1 above. *)
            let%bind (subject_symbol, _) =
              recurse [x1] (Unannotated_clause(c)) relstack
            in
            (* Induce formulae *)
            let%bind () = record_formula @@
              Formula(subject_symbol,
                      Formula_expression_value(Value_bool target_value))
            in
            (* Produce result. *)
            recurse (x' :: lookup_stack') acl1 relstack
          | _ ->
            raise @@ Jhupllib.Utils.Invariant_failure
              "Non-application, non-conditional body found in binding exit clause!"
        end
      | Nonbinding_enter_clause(v,c) ->
        (* ## Conditional Top rule ## *)
        let zero = zeromsg "Nonbinding_enter_clause" in
        let%orzero Abs_clause(_, Abs_conditional_body(Abs_var x1, _, _)) = c in
        (* Since conditionals are the only kind of thing we see here, we can
           insist that the value is a boolean.  This is important, since we have
           to be able to map the abstract value from the AST into a concrete
           value form for the solver. *)
        let%orzero Abs_value_bool b = v in
        (* Learn about subject *)
        (* Intentionally ignoring stack; see note 1 above for details. *)
        let%bind (subject_symbol, _) =
          recurse [x1] (Unannotated_clause c) relstack
        in
        (* Induce formula *)
        let%bind () = record_formula @@
          Formula(subject_symbol, Formula_expression_value(Value_bool b))
        in
        (* Produce result *)
        recurse lookup_stack acl1 relstack
      | Start_clause _
      | End_clause _ ->
        (* Although the formal specification does not list these clauses, they
           exist in the implementation to produce more readable and meaningful
           graphs.  They can be ignored. *)
        recurse lookup_stack acl1 relstack
    end
;;

type evaluation = Evaluation of concrete_stack M.evaluation;;

type evaluation_result = {
  er_formulae : Formulae.t;
  er_stack : concrete_stack;
  er_solution : (symbol -> value option);
};;

exception Invalid_query of string;;

let start (cfg : ddpa_graph) (e : expr) (program_point : ident) : evaluation =
  let env = prepare_environment e cfg in
  let initial_lookup_var =
    let clause =
      match Ident_map.Exceptionless.find
              program_point env.le_clause_predecessor_mapping with
      | None ->
        raise @@ Invalid_query("Variable " ^ show_ident program_point ^
                               " is not defined or is first in its expression")
      | Some cl -> cl
    in
    let Clause(Var(ident,_),_) = clause in
    ident
  in
  let acl =
    Unannotated_clause(
      lift_clause @@ Ident_map.find program_point env.le_clause_mapping)
  in
  let m : concrete_stack m =
    (* At top level, we don't actually need the returned symbol; we just want
       the concrete stack it produces.  The symbol is only used to generate
       formulae, which we'll get from the completed computations. *)
    let%bind _, stack =
      lookup env [initial_lookup_var] acl Relative_stack.empty
    in
    return stack
  in
  let m_eval = start m in
  Evaluation(m_eval)
;;

let step (x : evaluation) : evaluation_result list * evaluation option =
  let Evaluation(evaluation) = x in
  let results, evaluation' = M.step evaluation in
  let results' =
    results
    |> Enum.filter_map
      (fun evaluation_result ->
         match Solver.solve evaluation_result.M.er_formulae with
         | Some f ->
           begin
             lazy_logger `trace (fun () ->
                 Printf.sprintf
                   "Discovered answer of stack %s and formulae:\n%s"
                   (show_concrete_stack evaluation_result.M.er_value)
                   (Formulae.show evaluation_result.M.er_formulae)
               )
           end;
           Some {er_formulae = evaluation_result.M.er_formulae;
                 er_stack = evaluation_result.M.er_value;
                 er_solution = f
                }
         | None ->
           begin
             lazy_logger `trace (fun () ->
                 Printf.sprintf
                   "Dismissed answer of stack %s with unsolvable formulae:\n%s"
                   (show_concrete_stack evaluation_result.M.er_value)
                   (Formulae.show evaluation_result.M.er_formulae)
               )
           end;
           None
      )
  in
  (List.of_enum results',
   if M.is_complete evaluation' then None else Some(Evaluation(evaluation'))
  )
;;
