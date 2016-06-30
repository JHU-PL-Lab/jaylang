(** Testing to see if inconsistencies are translated from core to nested properly *)
open Batteries;;
open OUnit2;;

module TLA = Toploop_ddpa.Make(Ddpa_analysis.Make(Ddpa_single_element_stack.Stack))
;;

let placeholder_fv = Ddpa_graph.Abs_filtered_value(Ddpa_graph.Abs_value_int, Ddpa_graph.Pattern_set.empty, Ddpa_graph.Pattern_set.empty)
;;

let rec inconsistencies_check generated expected =
  if ((List.length generated) != (List.length expected))
  then assert_failure "Generated and expected not of same length"
  else
    match generated with
    | [] -> ()
    | gh::gt ->
      let inconsistency_check generated expected =
        match (generated, expected) with
        | (Nested_sugaring.Application_of_non_function(guid1, guid2, _),
           Nested_sugaring.Application_of_non_function(euid1, euid2, _)) ->
          assert_bool "Appl_of_non_function check"
            ((Uid.equal_uid guid1 euid1)
             && (Uid.equal_uid guid2 euid2))
        | (Nested_sugaring.Projection_of_non_record(guid1, guid2, _),
           Nested_sugaring.Projection_of_non_record(euid1, euid2, _)) ->
          assert_bool "Projection_of_non_record check"
            ((Uid.equal_uid guid1 euid1)
             && (Uid.equal_uid guid2 euid2))
        | (Nested_sugaring.Projection_of_absent_label(guid1, guid2, _, _),
           Nested_sugaring.Projection_of_absent_label(euid1, euid2, _, _)) ->
          assert_bool "Projection_of_absent_label check"
            ((Uid.equal_uid guid1 euid1)
             && (Uid.equal_uid guid2 euid2))
        | (Nested_sugaring.Deref_of_non_ref(guid1, guid2, _),
           Nested_sugaring.Deref_of_non_ref(euid1, euid2, _)) ->
          assert_bool "Deref_of_non_ref check"
            ((Uid.equal_uid guid1 euid1)
             && (Uid.equal_uid guid2 euid2))
        | (Nested_sugaring.Update_of_non_ref(guid1, guid2, _),
           Nested_sugaring.Update_of_non_ref(euid1, euid2, _)) ->
          assert_bool "Update_of_non_ref check"
            ((Uid.equal_uid guid1 euid1)
             && (Uid.equal_uid guid2 euid2))
        | (Nested_sugaring.Invalid_binary_operation(guid1, _, guid2, _, guid3, _),
           Nested_sugaring.Invalid_binary_operation(euid1, _, euid2, _, euid3, _)) ->
          assert_bool "Invalid_binary_operation check"
            ((Uid.equal_uid guid1 euid1)
             && (Uid.equal_uid guid2 euid2)
             && (Uid.equal_uid guid3 euid3))
        | (Nested_sugaring.Invalid_unary_operation(guid1, _, guid2, _),
           Nested_sugaring.Invalid_unary_operation(euid1, _, euid2, _)) ->
          assert_bool "Invalid_unary_operation check"
            ((Uid.equal_uid guid1 euid1)
             && (Uid.equal_uid guid2 euid2))
        | (Nested_sugaring.Invalid_indexing_subject(guid1, guid2, _),
           Nested_sugaring.Invalid_indexing_subject(euid1, euid2, _)) ->
          assert_bool "Invalid_indexing_subject check"
            ((Uid.equal_uid guid1 euid1)
             && (Uid.equal_uid guid2 euid2))
        | (Nested_sugaring.Invalid_indexing_argument(guid1, guid2, _),
           Nested_sugaring.Invalid_indexing_argument(euid1, euid2, _)) ->
          assert_bool "Invalid_indexing_argument check"
            ((Uid.equal_uid guid1 euid1)
             && (Uid.equal_uid guid2 euid2))
        | _ -> assert_failure("Generated head != Expected head")
      in
      inconsistency_check gh (List.hd expected);
      inconsistencies_check gt (List.tl expected)
;;

let update_test =
  "update_test" >:: fun _ ->
    let update_uid = Uid.next_uid () in
    let int0_uid = Uid.next_uid () in
    let int1_uid = Uid.next_uid () in
    let e = Nested_ast.Update_expr(
        update_uid,
        Nested_ast.Int_expr(int0_uid, 0),
        Nested_ast.Int_expr(int1_uid, 1))
    in
    let (core_e, map) = A_translator.a_translate_nested_expr e in
    let analysis = TLA.create_analysis core_e in
    let core_inconsistencies = TLA.check_inconsistencies analysis in
    let nested_inconsistencies = Nested_sugaring.batch_translation map core_inconsistencies in
    let generated = List.of_enum nested_inconsistencies in
    let expected = [
      Nested_sugaring.Update_of_non_ref(
        update_uid,
        int0_uid,
        placeholder_fv)
    ]
    in
    inconsistencies_check generated expected
;;

let deref_appl_test =
  "deref_appl_test" >:: fun _ ->
    let bin_op_uid = Uid.next_uid () in
    let deref_uid = Uid.next_uid () in
    let int_uid = Uid.next_uid () in
    let appl_uid = Uid.next_uid () in
    let string1_uid = Uid.next_uid () in
    let string2_uid = Uid.next_uid () in
    let e =
      Nested_ast.Binary_operation_expr(
        bin_op_uid,
        Nested_ast.Deref_expr(
          deref_uid,
          Nested_ast.Int_expr(int_uid, 0)
        ),
        Core_ast.Binary_operator_plus,
        Nested_ast.Appl_expr(
          appl_uid,
          Nested_ast.String_expr(string1_uid, "1"),
          Nested_ast.String_expr(string2_uid, "2")
        )
      )
    in
    let (core_e, map) = A_translator.a_translate_nested_expr e in
    let analysis = TLA.create_analysis core_e in
    let core_inconsistencies = TLA.check_inconsistencies analysis in
    let nested_inconsistencies = Nested_sugaring.batch_translation map core_inconsistencies in
    let generated = List.of_enum nested_inconsistencies in
    let expected = [
      Nested_sugaring.Deref_of_non_ref(
        deref_uid,
        int_uid,
        placeholder_fv
      );
      Nested_sugaring.Application_of_non_function(
        appl_uid,
        string1_uid,
        placeholder_fv
      )
    ]
    in
    inconsistencies_check
      (List.sort Nested_sugaring.compare_inconsistency generated)
      (List.sort Nested_sugaring.compare_inconsistency expected)
;;

let projection_non_record_test =
  "projection_non_record_test" >:: fun _ ->
    let proj_uid = Uid.next_uid () in
    let int_uid = Uid.next_uid () in
    let e =
      Nested_ast.Projection_expr(
        proj_uid,
        Nested_ast.Int_expr(int_uid, 0),
        Core_ast.Ident("none")
      )
    in
    let (core_e, map) = A_translator.a_translate_nested_expr e in
    let analysis = TLA.create_analysis core_e in
    let core_inconsistencies = TLA.check_inconsistencies analysis in
    let nested_inconsistencies = Nested_sugaring.batch_translation map core_inconsistencies in
    let generated = List.of_enum nested_inconsistencies in
    let expected = [
      Nested_sugaring.Projection_of_non_record(
        proj_uid,
        int_uid,
        placeholder_fv
      )
    ]
    in
    inconsistencies_check generated expected
;;

let projection_bad_label_test =
  "projection_bad_label_test" >:: fun _ ->
    let proj_uid = Uid.next_uid () in
    let rec_uid = Uid.next_uid () in
    let id = Core_ast.Ident("bad") in
    let e =
      Nested_ast.Projection_expr(
        proj_uid,
        Nested_ast.Record_expr(rec_uid, Core_ast.Ident_map.empty),
        id
      )
    in
    let (core_e, map) = A_translator.a_translate_nested_expr e in
    let analysis = TLA.create_analysis core_e in
    let core_inconsistencies = TLA.check_inconsistencies analysis in
    let nested_inconsistencies = Nested_sugaring.batch_translation map core_inconsistencies in
    let generated = List.of_enum nested_inconsistencies in
    let expected = [
      Nested_sugaring.Projection_of_absent_label(
        proj_uid,
        rec_uid,
        placeholder_fv,
        id
      )
    ]
    in
    inconsistencies_check generated expected
;;

let bin_op_test =
  "bin_op_test" >:: fun _ ->
    let bin_op_uid = Uid.next_uid () in
    let int_uid = Uid.next_uid () in
    let string_uid = Uid.next_uid () in
    let e =
      Nested_ast.Binary_operation_expr(
        bin_op_uid,
        Nested_ast.Int_expr(int_uid, 0),
        Core_ast.Binary_operator_int_minus,
        Nested_ast.String_expr(string_uid, "0")
      )
    in
    let (core_e, map) = A_translator.a_translate_nested_expr e in
    let analysis = TLA.create_analysis core_e in
    let core_inconsistencies = TLA.check_inconsistencies analysis in
    let nested_inconsistencies = Nested_sugaring.batch_translation map core_inconsistencies in
    let generated = List.of_enum nested_inconsistencies in
    let expected = [
      Nested_sugaring.Invalid_binary_operation(
        bin_op_uid,
        Core_ast.Binary_operator_int_minus,
        int_uid,
        placeholder_fv,
        string_uid,
        placeholder_fv
      )
    ]
    in
    inconsistencies_check generated expected
;;

let un_op_test =
  "un_op_test" >:: fun _ ->
    let un_op_uid = Uid.next_uid () in
    let string_uid = Uid.next_uid () in
    let e =
      Nested_ast.Unary_operation_expr(
        un_op_uid,
        Core_ast.Unary_operator_bool_not,
        Nested_ast.String_expr(string_uid, "0")
      )
    in
    let (core_e, map) = A_translator.a_translate_nested_expr e in
    let analysis = TLA.create_analysis core_e in
    let core_inconsistencies = TLA.check_inconsistencies analysis in
    let nested_inconsistencies = Nested_sugaring.batch_translation map core_inconsistencies in
    let generated = List.of_enum nested_inconsistencies in
    let expected = [
      Nested_sugaring.Invalid_unary_operation(
        un_op_uid,
        Core_ast.Unary_operator_bool_not,
        string_uid,
        placeholder_fv
      )
    ]
    in
    inconsistencies_check generated expected
;;


let tests = "Tests_resugar" >:::
            [ update_test
            ; deref_appl_test
            ; projection_non_record_test
            ; projection_bad_label_test
            ; bin_op_test
            ; un_op_test
            ]
;;
