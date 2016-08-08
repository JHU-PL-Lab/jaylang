open Batteries;;
open Jhupllib;;

open Core_ast;;

(** Returns a list of all clauses that occur in expression, deeply traversing
    the syntax tree. *)
let rec flatten (Expr clauses) =
  match clauses with
  | [] ->
    []
  | ((Clause (_, Value_body (Value_function (Function_value (_, function_body))))) as clause) :: rest_clauses ->
    clause :: flatten function_body @ flatten (Expr rest_clauses)
  | ((Clause (_, Conditional_body(_, _,
                                  Function_value (_, match_body),
                                  Function_value (_, antimatch_body)))) as clause) :: rest_clauses ->
    clause :: flatten match_body @ flatten antimatch_body @ flatten (Expr rest_clauses)
  | clause :: rest_clauses ->
    clause :: flatten (Expr rest_clauses)
;;

(** Returns a list of clauses that occur in the immediate block, shallowly
    traversing the syntax tree and inlining conditionals only. *)
let rec flatten_immediate_block (Expr clauses) =
  match clauses with
  | [] ->
    []
  | ((Clause (_, Conditional_body(_, _,
                                  Function_value (_, match_body),
                                  Function_value (_, antimatch_body)))) as clause) :: rest_clauses ->
    clause :: flatten_immediate_block match_body @ flatten_immediate_block antimatch_body @ flatten_immediate_block (Expr rest_clauses)
  | clause :: rest_clauses ->
    clause :: flatten_immediate_block (Expr rest_clauses)
;;

(** Returns the set of immediate variable bindings that occur in expression,
    shallowly traversing the syntax tree. *)
let defined_variables (Expr clauses) =
  clauses
  |> List.map (fun (Clause (bound_variable, _)) -> bound_variable)
  |> Var_set.of_list
;;

(** Returns a list of all variable bindings that occur in expression, including
    repeated ones, deeply traversing the syntax tree. *)
let bindings_with_repetition expression =
  flatten expression
  |> List.map
    (
      function
      | Clause (bound_variable, Value_body (Value_function (Function_value (formal_parameter, _)))) ->
        [bound_variable; formal_parameter]
      | Clause (bound_variable, Conditional_body(_, _,
                                                 Function_value (match_formal_parameter, _),
                                                 Function_value (antimatch_formal_parameter, _))) ->
        [bound_variable; match_formal_parameter; antimatch_formal_parameter]
      | Clause (bound_variable, _) ->
        [bound_variable]
    )
  |> List.flatten
;;

(** Returns the set of variable bindings that occur in expression, deeply
    traversing the syntax tree. *)
let bindings expression =
  Var_set.of_list @@ bindings_with_repetition expression
;;

(** Returns the set of variables that have use occurrences in expression, deeply
    traversing the syntax tree. *)
let use_occurrences expression =
  flatten expression
  |> List.map (
    fun (Clause (_, clause_body)) ->
      match clause_body with
      | Value_body value ->
        begin
          match value with
          | Value_record (Record_value fields) ->
            Ident_map.values fields
            |> Var_set.of_enum
          | Value_ref (Ref_value variable) ->
            Var_set.singleton variable
          | Value_function _
          | Value_int _
          | Value_bool _
          | Value_string _ ->
            Var_set.empty
        end
      | Var_body variable ->
        Var_set.singleton variable
      | Appl_body (function_, actual_parameter) ->
        Var_set.of_list [function_; actual_parameter]
      | Conditional_body (subject, _, _, _) ->
        Var_set.singleton subject
      | Projection_body (subject, _) ->
        Var_set.singleton subject
      | Deref_body cell ->
        Var_set.singleton cell
      | Update_body (cell, value) ->
        Var_set.of_list [cell; value]
      | Binary_operation_body (left_operand, _, right_operand) ->
        Var_set.of_list [left_operand; right_operand]
      | Unary_operation_body (_, operand) ->
        Var_set.singleton operand
      | Indexing_body (subject, index) ->
        Var_set.of_list [subject; index]
  )
  |> List.fold_left Var_set.union Var_set.empty
;;

(** Returns the set of bindings repeated in expression, deeply traversing the
    syntax tree. *)
let non_unique_bindings expression =
  bindings_with_repetition expression
  |> List.group compare_var
  |> List.filter_map (
    fun group ->
      if List.length group > 1 then
        Some (List.first group)
      else
        None
  )
  |> Var_set.of_list
;;

(** Returns the set of variables upon which the clause depends. Those are the
    variables immediately used, i.e., not frozen; they require verification of
    being in scope. *)
let dependencies (Clause (bound_variable, _)) dependency_analysis =
  Var_map.find bound_variable dependency_analysis
;;

(** Return the set of variable requirements to satisfy a given variable
    dependency. *)
let requirements dependency requirement_analysis =
  if Var_map.mem dependency requirement_analysis then
    Var_map.find dependency requirement_analysis
  else
    Var_set.empty
;;

(** Return the set of variable immediate requirements to satisfy a given variable
    dependency. *)
let immediate_requirements dependency immediate_requirement_analysis =
  if Var_map.mem dependency immediate_requirement_analysis then
    Var_map.find dependency immediate_requirement_analysis
  else
    Var_set.empty
;;

(** Returns the set of variables available for dependency resolution at a given
    clause. *)
let available_for_dependency (Clause (bound_variable, _)) availability_for_dependency_analysis =
  Var_map.find bound_variable availability_for_dependency_analysis
;;

(** Returns the set of variables available for requirement resolution at a given
    clause. *)
let available_for_requirement (Clause (bound_variable, _)) availability_for_requirement_analysis =
  Var_map.find bound_variable availability_for_requirement_analysis
;;

(** Returns the clauses before a given clause in a list (not inclusive). Do not
    call this function on a list of clauses that have been flattened. *)
let clauses_up_to clause clauses =
  clauses
  |> List.nsplit (equal_clause clause)
  |> List.first
;;

(** Returns the clauses before the first dependent of a given variable (not
    inclusive). Do not call this function on a list of clauses that have been
    flattened. *)
let clauses_up_to_first_dependent variable clauses dependency_analysis =
  clauses
  |> List.nsplit (
    fun clause ->
      Var_set.mem variable @@ dependencies clause dependency_analysis
  )
  |> List.first
;;

(** Pre-computes the immediate requirement relation between variables. Don't
    rely on this directly unless you know what you are doing;
    `analyze_requirements` is suited for public consumption. This does not
    change during the check of the program, so it can be calculated once and
    cached in the form of a multimap (maps of sets) of variables. *)
let analyze_immediate_requirements expression =
  flatten expression
  |> List.map (
    fun ((Clause (bound_variable, _)) as clause) ->
      (
        bound_variable,
        Var_set.diff
          (use_occurrences (Expr [clause]))
          (Var_set.remove bound_variable (bindings (Expr [clause])))
      )
  )
  |> List.enum
  |> Var_map.of_enum
;;

(** Pre-computes the requirement relation between variables, by performing a
    transitive closure on the result of `analyze_immediate_requirements'. This
    does not change during the check of the program, so it can be calculated
    once and cached in the form of a multimap (maps of sets) of variables. *)
let rec requirement_analysis_closure partial_requirement_analysis =
  let augmeted_requirement_analysis =
    partial_requirement_analysis
    |> Var_map.map (
      fun this_requirements ->
        this_requirements
        |> Var_set.enum
        |> Enum.map (
          fun requirement ->
            requirements requirement partial_requirement_analysis
        )
        |> Enum.fold Var_set.union this_requirements
    )
  in

  if Var_map.equal Var_set.equal partial_requirement_analysis augmeted_requirement_analysis then
    augmeted_requirement_analysis
  else
    requirement_analysis_closure augmeted_requirement_analysis
;;

(** Pre-computes set of variables upon which each clause depends. Those are the
    variables immediately used, i.e., not frozen; they require verification of
    being in scope. This does not change during the check of the program, so it
    can be calculated once and cached in the form of a multimap (maps of sets)
    of variables. Instead of having a map of clauses onto sets of variables, we
    pun on the variables bound at those clauses -- program points. *)
let analyze_dependencies expression immediate_requirement_analysis =
  flatten expression
  |> List.map (
    fun (Clause (bound_variable, clause_body)) ->
      (
        bound_variable,
        match clause_body with
        | Value_body (Value_function (Function_value (_, _))) ->
          Var_set.empty
        | _ ->
          immediate_requirements bound_variable immediate_requirement_analysis
      )
  )
  |> List.enum
  |> Var_map.of_enum
;;

(** Pre-computes the set of variables available for requirement resolution at a
    given clause in the expression. This does not change during the check of the
    program, so it can be calculated once and cached in the form of a multimap
    (maps of sets) of variables. Instead of having a map of clauses onto sets of
    variables, we pun on the variables bound at those clauses -- program
    points. *)
let analyze_availability_for_requirement expression dependency_analysis =
  let rec variables_available_for_requirement_at clause (Expr (clauses)) =
    if List.mem clause clauses then
      defined_variables (Expr (clauses_up_to clause clauses))
    else
      let containing_clause =
        clauses
        |> List.find (
          fun candidate_containing_clause ->
            List.mem clause (flatten (Expr [candidate_containing_clause]))
        )
      in
      match containing_clause with
      | Clause (bound_variable, Value_body (Value_function (Function_value (formal_parameter, function_body)))) ->
        Var_set.add formal_parameter @@
        Var_set.union
          (defined_variables (Expr (clauses_up_to_first_dependent bound_variable clauses dependency_analysis)))
          (variables_available_for_requirement_at clause function_body)
      | Clause (_, Conditional_body(_, _, Function_value (formal_parameter, branch_body), _))
        when List.mem clause (flatten branch_body) ->
        Var_set.add formal_parameter @@
        Var_set.union
          (defined_variables (Expr (clauses_up_to containing_clause clauses)))
          (variables_available_for_requirement_at clause branch_body)
      | Clause (_, Conditional_body(_, _, _, Function_value (formal_parameter, branch_body)))
        when List.mem clause (flatten branch_body) ->
        Var_set.add formal_parameter @@
        Var_set.union
          (defined_variables (Expr (clauses_up_to containing_clause clauses)))
          (variables_available_for_requirement_at clause branch_body)
      | _ ->
        raise @@ Utils.Invariant_failure "A clause appears within an expression in an unknown location."
  in

  flatten expression
  |> List.map
    (
      fun (Clause (bound_variable, _) as clause) ->
        (bound_variable, variables_available_for_requirement_at clause expression)
    )
  |> List.enum
  |> Var_map.of_enum
;;

(** Pre-computes the set of variables available for dependency resolution at a
    given clause in the expression. This does not change during the check of the
    program, so it can be calculated once and cached in the form of a multimap
    (maps of sets) of variables. Instead of having a map of clauses onto sets of
    variables, we pun on the variables bound at those clauses -- program
    points. *)
let analyze_availability_for_dependency expression dependency_analysis =
  let rec variables_available_for_dependency_at clause (Expr (clauses)) =
    if List.mem clause clauses then
      defined_variables (Expr (clauses_up_to clause clauses))
    else
      let containing_clause =
        clauses
        |> List.find (
          fun candidate_containing_clause ->
            List.mem clause (flatten (Expr [candidate_containing_clause]))
        )
      in
      match containing_clause with
      | Clause (_, Value_body (Value_function (Function_value (formal_parameter, function_body))))
        when not (List.mem clause (flatten_immediate_block function_body)) ->
        Var_set.add formal_parameter @@
        Var_set.union
          (defined_variables (Expr (clauses_up_to containing_clause clauses)))
          (variables_available_for_dependency_at clause function_body)
      | Clause (bound_variable, Value_body (Value_function (Function_value (formal_parameter, function_body))))
        when List.mem clause (flatten_immediate_block function_body) ->
        Var_set.add formal_parameter @@
        Var_set.union
          (defined_variables (Expr (clauses_up_to_first_dependent bound_variable clauses dependency_analysis)))
          (variables_available_for_dependency_at clause function_body)
      | Clause (_, Conditional_body(_, _, Function_value (formal_parameter, branch_body), _))
        when List.mem clause (flatten branch_body) ->
        Var_set.add formal_parameter @@
        Var_set.union
          (defined_variables (Expr (clauses_up_to containing_clause clauses)))
          (variables_available_for_dependency_at clause branch_body)
      | Clause (_, Conditional_body(_, _, _, Function_value (formal_parameter, branch_body)))
        when List.mem clause (flatten branch_body) ->
        Var_set.add formal_parameter @@
        Var_set.union
          (defined_variables (Expr (clauses_up_to containing_clause clauses)))
          (variables_available_for_dependency_at clause branch_body)
      | _ ->
        raise @@ Utils.Invariant_failure "A clause appears within an expression in an unknown location."
  in

  flatten expression
  |> List.map
    (
      fun (Clause (bound_variable, _) as clause) ->
        (bound_variable, variables_available_for_dependency_at clause expression)
    )
  |> List.enum
  |> Var_map.of_enum
;;

(** Predicate that holds when the variable dependency is in scope at the given
    clause of the expression. *)
let in_scope clause dependency requirement_analysis availability_for_dependency_analysis availability_for_requirement_analysis =
  let availability_for_dependency = available_for_dependency clause availability_for_dependency_analysis in
  let availability_for_requirement = available_for_dependency clause availability_for_requirement_analysis in
  Var_set.mem dependency availability_for_dependency &&
  requirements dependency requirement_analysis
  |> Var_set.for_all (fun requirement -> Var_set.mem requirement availability_for_requirement)
;;

(** Returns a list of pairs of variables. The pair represents a violation on the
    concept of scope, i.e., a variable used that was not in scope. The first
    variable is the program point in which the violation occurred, the second
    variable is the one that was not in scope. *)
let scope_violations expression =
  let immediate_requirement_analysis = analyze_immediate_requirements expression in
  let requirement_analysis = requirement_analysis_closure immediate_requirement_analysis in
  let dependency_analysis = analyze_dependencies expression immediate_requirement_analysis in
  let availability_for_dependency_analysis = analyze_availability_for_dependency expression dependency_analysis in
  let availability_for_requirement_analysis = analyze_availability_for_requirement expression dependency_analysis in

  flatten expression
  |> List.map
    (
      fun ((Clause (program_point, _)) as clause) ->
        dependencies clause dependency_analysis
        |> Var_set.enum
        |> Enum.filter_map (
          fun dependency ->
            if
              in_scope
                clause dependency
                requirement_analysis
                availability_for_dependency_analysis
                availability_for_requirement_analysis
            then
              None
            else
              Some (program_point, dependency)
        )
        |> List.of_enum
    )
  |> List.flatten
;;
