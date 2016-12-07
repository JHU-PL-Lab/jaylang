open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;
open Ddpa_abstract_ast;;
open Ddpa_graph;;
open Nondeterminism;;

exception Non_record_projection of string;;

let rv body =
  match body with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Abs_clause(x,_) = List.last body in x
;;

let negative_pattern_set_selection record_type pattern_set =
  let (Record_value m) = record_type in
  let record_labels = Ident_set.of_enum @@ Ident_map.keys m in
  let relevant_patterns = pattern_set
                          |> Pattern_set.enum
                          |> Enum.filter
                            (fun pattern ->
                               match pattern with
                               | Record_pattern m' ->
                                 Ident_set.subset (Ident_set.of_enum @@ Ident_map.keys m')
                                   record_labels
                               | Any_pattern ->
                                 raise @@ Utils.Invariant_failure "Shouldn't call `negative_pattern_set_selection' with a pattern set that contains `any' patterns."
                               | _ -> false)
  in
  (* This function selects a single label from a given pattern and constructs
     a pattern from it. *)
  let pick_pattern pattern =
    match pattern with
    | Record_pattern m' ->
      let open Nondeterminism_monad in
      let%bind (k,v) = pick_enum @@ Ident_map.enum m' in
      return @@ Record_pattern(Ident_map.singleton k v)
    | _ -> raise @@ Utils.Invariant_failure (
        Printf.sprintf "The non-record pattern `%s' ended up on `analysis.ml:negative_pattern_set_selection:pick_pattern'."
          (show_pattern pattern))
  in
  let open Nondeterminism_monad in
  let%bind selected_patterns =
    Nondeterminism_monad.mapM pick_pattern relevant_patterns
  in
  return @@ Pattern_set.of_enum selected_patterns
;;

(* `pattern' /must/ satisfy the `is_record_pattern_set' predicate. Note that
   the results of `negative_pattern_set_selection' already satisfy it. *)
let pattern_projection pattern label =
  match pattern with
  | Record_pattern m ->
    begin
      try
        Some (Ident_map.find label m)
      with
      | Not_found -> None
    end
  | Any_pattern -> None
  | _ -> raise @@ Non_record_projection (
      Printf.sprintf "Tried to project out of a non-record pattern `%s' in `analysis.ml:pattern_projection'."
        (show_pattern pattern))
;;

let pattern_set_projection set label =
  set
  |> Pattern_set.enum
  |> Enum.map (flip pattern_projection label)
  |> Enum.filter_map identity
  |> Pattern_set.of_enum
;;

let is_record_pattern_set set =
  set
  |> Pattern_set.enum
  |> Enum.for_all
    (
      fun pattern ->
        match pattern with
        | Record_pattern _ | Any_pattern -> true
        | _ -> false
    )
;;

let labels_in_record (Record_value m) =
  Ident_set.of_enum @@ Ident_map.keys m
;;

(* `pattern' /must/ satisfy the `is_record_pattern_set' predicate. Note that
   the results of `negative_pattern_set_selection' already satisfy it. *)
let labels_in_pattern pattern =
  match pattern with
  | Record_pattern m ->
    Ident_set.of_enum @@ Ident_map.keys m
  | Any_pattern ->
    Ident_set.empty
  | _ -> raise @@ Non_record_projection (
      Printf.sprintf "Tried to enumerate labels out of a non-record pattern `%s' in `analysis.ml:labels_in_pattern'."
        (show_pattern pattern))
;;

let labels_in_pattern_set set =
  set
  |> Pattern_set.enum
  |> Enum.map labels_in_pattern
  |> Enum.fold Ident_set.union Ident_set.empty
;;

let is_immediate acl =
  match acl with
  | Unannotated_clause(abs_clause) -> is_abstract_clause_immediate abs_clause
  | Enter_clause _
  | Exit_clause _
  | Start_clause _
  | End_clause _ -> true
;;

let wire site_cl func x1 x2 graph =
  let site_acl = Unannotated_clause(site_cl) in
  let Abs_function_value(x0, Abs_expr(body)) = func in
  let wire_in_acl = Enter_clause(x0,x1,site_cl) in
  let start_acl = Start_clause (rv body) in
  let end_acl = End_clause (rv body) in
  let wire_out_acl = Exit_clause(x2,rv body,site_cl) in
  let pred_edges =
    Ddpa_graph.preds site_acl graph
    |> Enum.map (fun acl' -> Ddpa_edge(acl',wire_in_acl))
  in
  let succ_edges =
    Ddpa_graph.succs site_acl graph
    |> Enum.map (fun acl' -> Ddpa_edge(wire_out_acl,acl'))
  in
  let inner_edges =
    List.enum body
    |> Enum.map (fun cl -> Unannotated_clause(cl))
    |> Enum.append (Enum.singleton start_acl)
    |> Enum.append (Enum.singleton wire_in_acl)
    |> flip Enum.append (Enum.singleton end_acl)
    |> flip Enum.append (Enum.singleton wire_out_acl)
    |> Utils.pairwise_enum_fold
      (fun acl1 acl2 -> Ddpa_edge(acl1,acl2))
  in
  Enum.append pred_edges @@ Enum.append inner_edges succ_edges
;;

module End_of_block_map =
struct
  type t = annotated_clause Annotated_clause_map.t
  let pp = Annotated_clause_map.pp pp_annotated_clause
end;;


(**
   This function generates a dictionary mapping each annotated clause to the
   end of its block.
*)
let rec create_end_of_block_map (acls : abstract_clause list)
  : End_of_block_map.t =
  (* This primary function produces a dictionary containing a mapping for each
     clause in the provided list.  The recursively-defined helper functions
     (starting with underscores) are used to find recursive blocks on which the
     primary function is invoked.  This is why e.g. the function
     _create_end_of_block_map_for_annotated_clause frequently returns empty
     dictionaries: the clause itself has already been mapped in a dictionary
     in the primary function. *)
  if List.length acls = 0
  then
    raise (Utils.Invariant_failure "attempted to create EoS map for empty list")
  else
    let last_var = rv acls in
    let clause_map =
      acls
      |> List.enum
      |> Enum.map (fun acl -> (Unannotated_clause(acl), End_clause last_var))
      |> Enum.append
        (List.enum
           [ (End_clause last_var, End_clause last_var)
           ; (Start_clause last_var, End_clause last_var)
           ]
        )
      |> Annotated_clause_map.of_enum
    in
    (* Collect EoS maps recursively. *)
    let recursive_maps =
      acls
      |> List.enum
      |> Enum.map _create_end_of_block_map_for_abstract_clause
    in
    (* Merge all maps *)
    recursive_maps
    |> Enum.fold _merge_maps clause_map

and _merge_maps m1 m2 =
  let join _ opt1 opt2 =
    match opt1, opt2 with
    | None, None -> None
    | _, Some _ -> opt2 (* prefers second value when both are present *)
    | Some _ , _ -> opt1
  in Annotated_clause_map.merge join m1 m2

and _create_end_of_block_map_for_abstract_clause (cl : abstract_clause) =
  let Abs_clause(_, b) = cl in
  _create_end_of_block_map_for_body b

and _create_end_of_block_map_for_body (b : abstract_clause_body) =
  match b with
  | Abs_value_body v ->
    begin
      match v with
      | Abs_value_record _ -> Annotated_clause_map.empty
      | Abs_value_function f -> _create_end_of_block_map_for_function f
      | Abs_value_ref _ -> Annotated_clause_map.empty
      | Abs_value_int -> Annotated_clause_map.empty
      | Abs_value_bool _ -> Annotated_clause_map.empty
      | Abs_value_string -> Annotated_clause_map.empty
    end
  | Abs_var_body _ -> Annotated_clause_map.empty
  | Abs_appl_body (_,_) -> Annotated_clause_map.empty
  | Abs_conditional_body (_,_,f1,f2) ->
    _merge_maps
      (_create_end_of_block_map_for_function f1)
      (_create_end_of_block_map_for_function f2)
  | Abs_projection_body (_,_) -> Annotated_clause_map.empty
  | Abs_deref_body _ -> Annotated_clause_map.empty
  | Abs_update_body (_,_) -> Annotated_clause_map.empty
  | Abs_binary_operation_body (_,_,_) -> Annotated_clause_map.empty
  | Abs_unary_operation_body (_,_) -> Annotated_clause_map.empty
  | Abs_indexing_body (_,_) -> Annotated_clause_map.empty

and _create_end_of_block_map_for_function (f : abstract_function_value) =
  let Abs_function_value(_, Abs_expr(body)) = f in
  create_end_of_block_map body
;;
