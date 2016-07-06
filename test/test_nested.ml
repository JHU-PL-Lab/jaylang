(** These tests test the desugaring of the Nested ASTS into basic ASTs *)

open OUnit2;;
open Uid;;
open A_translator;;
(* open Core_ast.Ident_map;; *)
(* open Pp_utils;; *)

let zero () = assert_failure("Expected Ident not found in map, or proof of the wrong form.")

let basic_test =
  "basic_test" >:: fun _ ->
    let int_uid = next_uid () in
    let nested = Nested_ast.Int_expr(int_uid, 1) in
    let expr, ids = a_translate_nested_expr nested in
    match expr with
    | Core_ast.Expr([Core_ast.Clause(Core_ast.Var(int_id,_), Core_ast.Value_body(Core_ast.Value_int(1)))]) ->
      let%orzero Some (Proof_rule(_,mapped_int_uid)) =
        Ident_map.Exceptionless.find int_id ids
      in
      assert_bool "Int_uid matches proof_rule" (equal_uid int_uid mapped_int_uid)
    | _ -> assert_failure("Conditional Test: Nested translation did not match AST control")
;;

let conditional_test =
  "conditional_test" >:: fun _ ->
    let cond_uid = next_uid () in
    let expr_uid = next_uid () in
    let func1_uid = next_uid () in
    let func2_uid = next_uid () in
    let nested = Nested_ast.Conditional_expr(
        cond_uid,
        Nested_ast.Int_expr(expr_uid,1),
        Nested_ast.Int_pattern(next_uid ()),
        Nested_ast.Function(func1_uid, Nested_ast.Nested_var(next_uid(), Core_ast.Ident("testv1")), Nested_ast.Int_expr(next_uid(), 1)),
        Nested_ast.Function(func2_uid, Nested_ast.Nested_var(next_uid(), Core_ast.Ident("testv2")), Nested_ast.Int_expr(next_uid(), 0))
      )
    in
    let expr, ids = a_translate_nested_expr nested in
    match expr with
    (* print_newline();
       print_endline(
       expr
       |> pp_to_string Core_ast_pp.pp_expr
       );
       print_newline() *)
    | Core_ast.Expr(
        [ Core_ast.Clause(Core_ast.Var(expr_id,_), Core_ast.Value_body(Core_ast.Value_int(1)));
          Core_ast.Clause(Core_ast.Var(cond_id,_), Core_ast.Conditional_body(
              Core_ast.Var(expr2_id,_),
              _,
              Core_ast.Function_value(Core_ast.Var(func1_id,_), _),
              Core_ast.Function_value(Core_ast.Var(func2_id,_), _)
            ))
        ]) ->
      let%orzero Some (Proof_rule(_,mapped_expr_uid)) =
        Ident_map.Exceptionless.find expr_id ids
      in
      let%orzero Some (Proof_rule(_,mapped_cond_uid)) =
        Ident_map.Exceptionless.find cond_id ids
      in
      let%orzero Some (Proof_rule(_,mapped_func1_uid)) =
        Ident_map.Exceptionless.find func1_id ids
      in
      let%orzero Some (Proof_rule(_,mapped_func2_uid)) =
        Ident_map.Exceptionless.find func2_id ids
      in
      assert_bool "Expr_uid matches proof_rule" (equal_uid expr_uid mapped_expr_uid);
      assert_bool "Cond_uid matches proof_rule" (equal_uid cond_uid mapped_cond_uid);
      assert_bool "Cond uses correct expr" (Core_ast.equal_ident expr_id expr2_id);
      assert_bool "Func1_uid matches proof_rule" (equal_uid func1_uid mapped_func1_uid);
      assert_bool "Func2_uid matches proof_rule" (equal_uid func2_uid mapped_func2_uid);
    | _ -> assert_failure("Conditional Test: Nested translation did not match AST control")
;;

let appl_test =
  "appl_test" >:: fun _ ->
    let appl_uid = next_uid () in
    let func_uid = next_uid () in
    let int_uid = next_uid () in
    let nested = Nested_ast.Appl_expr(
        appl_uid,
        Nested_ast.Function_expr(
          func_uid,
          Nested_ast.Function(
            next_uid (),
            Nested_ast.Nested_var(next_uid (), Core_ast.Ident("testv")),
            Nested_ast.Int_expr(next_uid(), 1))),
        Nested_ast.Int_expr(int_uid, 0)
      ) in
    let expr, ids = a_translate_nested_expr nested in
    match expr with
    | Core_ast.Expr(
        [ Core_ast.Clause(Core_ast.Var(func_id,_), _);
          Core_ast.Clause(Core_ast.Var(int_id,_), Core_ast.Value_body(Core_ast.Value_int(0)));
          Core_ast.Clause(Core_ast.Var(appl_id,_), Core_ast.Appl_body(_,_))
        ]) ->
      let%orzero Some (Proof_rule(_,mapped_appl_uid)) =
        Ident_map.Exceptionless.find appl_id ids
      in
      let%orzero Some (Proof_rule(_,mapped_int_uid)) =
        Ident_map.Exceptionless.find int_id ids
      in
      let%orzero Some (Proof_rule(_,mapped_func_uid)) =
        Ident_map.Exceptionless.find func_id ids
      in
      assert_bool "Appl_uid matches proof_rule" (equal_uid appl_uid mapped_appl_uid);
      assert_bool "Func_uid matches proof_rule" (equal_uid func_uid mapped_func_uid);
      assert_bool "Int_uid matches proof_rule" (equal_uid int_uid mapped_int_uid)
    | _ -> assert_failure("Conditional Test: Nested translation did not match AST control")
;;

let tests = "Test_nested" >:::
            [ conditional_test;
              basic_test;
              appl_test
            ]
;;
