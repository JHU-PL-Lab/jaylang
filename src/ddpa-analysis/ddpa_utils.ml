open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;
open Ddpa_abstract_ast;;
open Nondeterminism;;

exception Non_record_projection of string;;

let rv body =
  match body with
  | [] -> raise @@
    Utils.Invariant_failure "empty function body provided to rv"
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
  | Exit_clause _ -> true
  | Start_clause
  | End_clause -> false
;;
