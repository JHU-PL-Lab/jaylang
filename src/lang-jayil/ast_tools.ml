open Batteries
open Ast

(** Returns a list of all clauses that occur in expression, deeply traversing
    the syntax tree. *)
let rec flatten (Expr clauses) =
  match clauses with
  | [] -> []
  | (Clause (_, Value_body (Value_function (Function_value (_, function_body))))
    as clause)
    :: rest_clauses ->
      (clause :: flatten function_body) @ flatten (Expr rest_clauses)
  | (Clause (_, Conditional_body (_, match_body, antimatch_body)) as clause)
    :: rest_clauses ->
      (clause :: flatten match_body)
      @ flatten antimatch_body
      @ flatten (Expr rest_clauses)
  | clause :: rest_clauses -> clause :: flatten (Expr rest_clauses)

(** Returns a list of clauses that occur in the immediate block, shallowly
    traversing the syntax tree and inlining conditionals only. *)
let rec flatten_immediate_block (Expr clauses) =
  match clauses with
  | [] -> []
  | (Clause (_, Conditional_body (_, match_body, antimatch_body)) as clause)
    :: rest_clauses ->
      (clause :: flatten_immediate_block match_body)
      @ flatten_immediate_block antimatch_body
      @ flatten_immediate_block (Expr rest_clauses)
  | clause :: rest_clauses ->
      clause :: flatten_immediate_block (Expr rest_clauses)

(** Returns the set of immediate variable bindings that occur in expression,
    shallowly traversing the syntax tree. *)
let defined_variables (Expr clauses) =
  clauses
  |> List.map (fun (Clause (bound_variable, _)) -> bound_variable)
  |> Var_set.of_list

(** Returns a list of all variable bindings that occur in expression, including
    repeated ones, deeply traversing the syntax tree. *)
let bindings_with_repetition expression =
  flatten expression
  |> List.map (function
       | Clause
           ( bound_variable,
             Value_body (Value_function (Function_value (formal_parameter, _)))
           ) ->
           [ bound_variable; formal_parameter ]
       | Clause (bound_variable, _) -> [ bound_variable ])
  |> List.flatten

(** Returns the set of variable bindings that occur in expression, deeply
    traversing the syntax tree. *)
let bindings expression = Var_set.of_list @@ bindings_with_repetition expression

(** Returns the set of variables that have use occurrences in expression, deeply
    traversing the syntax tree. *)
(* let use_occurrences expression =
     flatten expression
     |> List.map (
       fun (Clause (_, clause_body)) ->
         match clause_body with
         | Value_body _
         | Input_body ->
           Var_set.empty
         | Var_body variable ->
           Var_set.singleton variable
         | Appl_body (function_, actual_parameter) ->
           Var_set.of_list [function_; actual_parameter]
         | Conditional_body (subject, _, _) ->
           Var_set.singleton subject
         | Match_body (subject, _) ->
           Var_set.singleton subject
         | Projection_body(subject, _) ->
           Var_set.singleton subject
         | Binary_operation_body (left_operand, _, right_operand) ->
           Var_set.of_list [left_operand; right_operand]
     )
     |> List.fold_left Var_set.union Var_set.empty
   ;; *)

(** Returns the set of bindings repeated in expression, deeply traversing the
    syntax tree. *)
let non_unique_bindings expression =
  bindings_with_repetition expression
  |> List.group compare_var
  |> List.filter_map (fun group ->
         if List.length group > 1 then Some (List.first group) else None)
  |> Var_set.of_list

let _bind_filt bound site_x vars =
  vars
  |> List.filter (fun x -> not @@ Ident_set.mem x bound)
  |> List.map (fun x -> (site_x, x))

let rec check_scope_expr (bound : Ident_set.t) (e : expr) : (ident * ident) list
    =
  let (Expr cls) = e in
  snd
  @@ List.fold_left
       (fun (bound', result) clause ->
         let result' = result @ check_scope_clause bound' clause in
         let (Clause (Var (x, _), _)) = clause in
         let bound'' = Ident_set.add x bound' in
         (bound'', result'))
       (bound, []) cls

and check_scope_clause (bound : Ident_set.t) (c : clause) : (ident * ident) list
    =
  let (Clause (Var (site_x, _), b)) = c in
  check_scope_clause_body bound site_x b

and check_scope_clause_body (bound : Ident_set.t) (site_x : ident)
    (b : clause_body) : (ident * ident) list =
  match b with
  | Value_body v -> (
      match v with
      | Value_function (Function_value (Var (x', _), e)) ->
          check_scope_expr (Ident_set.add x' bound) e
      | _ -> [])
  | Var_body (Var (x, _)) -> _bind_filt bound site_x [ x ]
  | Input_body -> []
  | Appl_body (Var (x1, _), Var (x2, _)) -> _bind_filt bound site_x [ x1; x2 ]
  | Conditional_body (Var (x, _), e1, e2) ->
      _bind_filt bound site_x [ x ]
      @ check_scope_expr bound e1 @ check_scope_expr bound e2
  | Match_body (Var (x, _), _) -> _bind_filt bound site_x [ x ]
  | Projection_body (Var (x, _), _) -> _bind_filt bound site_x [ x ]
  | Not_body (Var (x, _)) -> _bind_filt bound site_x [ x ]
  | Binary_operation_body (Var (x1, _), _, Var (x2, _)) ->
      _bind_filt bound site_x [ x1; x2 ]
  | Abort_body -> []
  | Assume_body (Var (x, _)) -> _bind_filt bound site_x [ x ]
  | Assert_body (Var (x, _)) -> _bind_filt bound site_x [ x ]

(** Returns a list of pairs of variables. The pair represents a violation on the
    concept of scope, i.e., a variable used that was not in scope. The first
    variable is the program point in which the violation occurred, the second
    variable is the one that was not in scope. *)
let scope_violations expression =
  check_scope_expr Ident_set.empty expression
  |> List.map (fun (i1, i2) -> (Var (i1, None), Var (i2, None)))

(** Returns the last defined variable in a list of clauses. *)
let rv (cs : clause list) : Var.t =
  let (Clause (x, _)) = List.last cs in
  x

(** Returns the last defined variable in an expression. *)
let retv (e : expr) : Var.t =
  let (Expr cs) = e in
  rv cs

(** Homomorphically maps all variables in an expression. *)
let rec map_expr_vars (fn : Var.t -> Var.t) (e : expr) : expr =
  let (Expr cls) = e in
  Expr (List.map (map_clause_vars fn) cls)

and map_clause_vars (fn : Var.t -> Var.t) (c : clause) : clause =
  let (Clause (x, b)) = c in
  Clause (fn x, map_clause_body_vars fn b)

and map_clause_body_vars (fn : Var.t -> Var.t) (b : clause_body) : clause_body =
  match (b : clause_body) with
  | Value_body v -> Value_body (map_value_vars fn v)
  | Var_body x -> Var_body (fn x)
  | Input_body -> Input_body
  | Appl_body (x1, x2) -> Appl_body (fn x1, fn x2)
  | Conditional_body (x, e1, e2) ->
      Conditional_body (fn x, map_expr_vars fn e1, map_expr_vars fn e2)
  | Match_body (x, p) -> Match_body (fn x, p)
  | Projection_body (x, l) -> Projection_body (fn x, l)
  | Not_body x -> Not_body (fn x)
  | Binary_operation_body (x1, op, x2) ->
      Binary_operation_body (fn x1, op, fn x2)
  | Abort_body -> Abort_body
  | Assume_body x -> Assume_body (fn x)
  | Assert_body x -> Assert_body (fn x)

and map_value_vars (fn : Var.t -> Var.t) (v : value) : value =
  match (v : value) with
  | Value_record (Record_value m) ->
      Value_record (Record_value (Ident_map.map fn m))
  | Value_function f -> Value_function (map_function_vars fn f)
  | Value_int _ -> v
  | Value_bool _ -> v

and map_function_vars (fn : Var.t -> Var.t) (f : function_value) :
    function_value =
  let (Function_value (x, e)) = f in
  Function_value (fn x, map_expr_vars fn e)

(** Mostly-homomorphically operates on every subexpression of an expression.
    Expressions are modified in a bottom-up fashion. *)
let rec transform_exprs_in_expr (fn : expr -> expr) (e : expr) : expr =
  let (Expr cls) = e in
  fn @@ Expr (List.map (transform_exprs_in_clause fn) cls)

and transform_exprs_in_clause (fn : expr -> expr) (c : clause) : clause =
  let (Clause (x, b)) = c in
  Clause (x, transform_exprs_in_clause_body fn b)

and transform_exprs_in_clause_body (fn : expr -> expr) (b : clause_body) :
    clause_body =
  match (b : clause_body) with
  | Value_body v -> Value_body (transform_exprs_in_value fn v)
  | Var_body _ -> b
  | Input_body -> Input_body
  | Appl_body (_, _) -> b
  | Conditional_body (x, e1, e2) ->
      Conditional_body
        (x, transform_exprs_in_expr fn e1, transform_exprs_in_expr fn e2)
  | Match_body (x, p) -> Match_body (x, p)
  | Projection_body (_, _) -> b
  | Not_body _ -> b
  | Binary_operation_body (_, _, _) -> b
  | Abort_body -> b
  | Assume_body _x -> b
  | Assert_body _x -> b

and transform_exprs_in_value (fn : expr -> expr) (v : value) : value =
  match (v : value) with
  | Value_record _ -> v
  | Value_function f -> Value_function (transform_exprs_in_function fn f)
  | Value_int _ -> v
  | Value_bool _ -> v

and transform_exprs_in_function (fn : expr -> expr) (fv : function_value) :
    function_value =
  let (Function_value (x, e)) = fv in
  Function_value (x, transform_exprs_in_expr fn e)

let first_id e =
  e |> (fun (Expr cls) -> cls) |> List.first |> fun (Clause (Var (x, _), _)) ->
  x

let label_sep = "~~~"

let record_of_clause_body = function
  | Value_body (Value_record (Record_value r)) -> r
  | _ -> failwith "not record body`"

let rec defined_vars_of_expr (e : expr) : Var_set.t =
  let (Expr cls) = e in
  List.fold
    (fun acc cls ->
      let res = defined_vars_of_clause cls in
      Var_set.union acc res)
    Var_set.empty cls

and defined_vars_of_clause (c : clause) : Var_set.t =
  let (Clause (x, b)) = c in
  Var_set.add x (defined_vars_of_clause_body b)

and defined_vars_of_clause_body (b : clause_body) : Var_set.t =
  match (b : clause_body) with
  | Var_body _ | Input_body | Appl_body _ | Match_body _ | Projection_body _
  | Not_body _ | Binary_operation_body _ | Abort_body | Assert_body _
  | Assume_body _ ->
      Var_set.empty
  | Value_body v -> defined_vars_of_value v
  | Conditional_body (_, e1, e2) ->
      let s1 = defined_vars_of_expr e1 in
      let s2 = defined_vars_of_expr e2 in
      Var_set.union s1 s2

and defined_vars_of_value (v : value) : Var_set.t =
  match (v : value) with
  | Value_record _ | Value_int _ | Value_bool _ -> Var_set.empty
  | Value_function f -> defined_vars_of_function f

and defined_vars_of_function (f : function_value) : Var_set.t =
  let (Function_value (_, e)) = f in
  defined_vars_of_expr e
