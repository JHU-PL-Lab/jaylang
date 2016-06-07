(* These tests test the desugaring of the Swan ASTs into Nested ASTs *)

(* open Batteries;; *)
open OUnit2;;
module Ident_map = Ast.Ident_map

let if_then_else_test_1 =
  "if_then_else_test" >:: fun _ ->
    let swan = Swan_ast.If_expr(Swan_ast.Bool_expr(true), Swan_ast.Int_expr(0), Swan_ast.Int_expr(1)) in
    let expr = (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Conditional_expr(Nested_ast.Bool_expr(true),
                                  Nested_ast.Bool_pattern(true),
                                  Nested_ast.Function(_, Nested_ast.Int_expr(0)),
                                  Nested_ast.Function(_, Nested_ast.Int_expr(1))) -> ()
    | _ -> assert_failure("If Test 1: Swan translation did not match Nested control")
;;

let if_then_else_test_2 =
  "if_then_else_test" >:: fun _ ->
    let swan = Swan_ast.If_expr(Swan_ast.Bool_expr(false), Swan_ast.Int_expr(0), Swan_ast.Int_expr(1)) in
    let expr = (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Conditional_expr(Nested_ast.Bool_expr(false),
                                  Nested_ast.Bool_pattern(true),
                                  Nested_ast.Function(_, Nested_ast.Int_expr(0)),
                                  Nested_ast.Function(_, Nested_ast.Int_expr(1))) -> ()
    | _ -> assert_failure("If Test 2: Swan translation did not match Nested control")
;;

let if_in_if_test =
  "if_in_if_test" >:: fun _ ->
    let swan = Swan_ast.If_expr(Swan_ast.Bool_expr(true), Swan_ast.If_expr(Swan_ast.Bool_expr(false), Swan_ast.Int_expr(0), Swan_ast.Int_expr(1)), Swan_ast.Int_expr(1)) in
    let expr = (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Conditional_expr(Nested_ast.Bool_expr(true),
                                  Nested_ast.Bool_pattern(true),
                                  Nested_ast.Function(_, Nested_ast.Conditional_expr(Nested_ast.Bool_expr(false),
                                                                                     Nested_ast.Bool_pattern(true),
                                                                                     Nested_ast.Function(_, Nested_ast.Int_expr(0)),
                                                                                     Nested_ast.Function(_, Nested_ast.Int_expr(1)))),
                                  Nested_ast.Function(_, Nested_ast.Int_expr(1))) -> assert_string("")
    | _ -> assert_failure("If in if test: Swan translation did not match Nested control")
;;

let match_test_1 =
  "match_test_1" >:: fun _ ->
    let swan = Swan_ast.Match_expr(Swan_ast.Int_expr(0), [Swan_ast.Match_pair(Swan_ast.Bool_pattern(true), Swan_ast.Int_expr(1));
                                                          Swan_ast.Match_pair(Swan_ast.Fun_pattern, Swan_ast.Int_expr(2));
                                                          Swan_ast.Match_pair(Swan_ast.Int_pattern, Swan_ast.Int_expr(0))]) in
    let expr = (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Let_expr(_, Nested_ast.Int_expr(0),
                          Nested_ast.Conditional_expr(
                            Nested_ast.Int_expr(0),
                            Nested_ast.Bool_pattern(true),
                            (Nested_ast.Function(_, Nested_ast.Int_expr(1))),
                            (Nested_ast.Function(_,
                                                 Nested_ast.Conditional_expr(
                                                   Nested_ast.Int_expr(0),
                                                   Nested_ast.Fun_pattern,
                                                   (Nested_ast.Function(_, Nested_ast.Int_expr(2))),
                                                   (Nested_ast.Function(_,
                                                                        Nested_ast.Conditional_expr(
                                                                          Nested_ast.Int_expr(0),
                                                                          Nested_ast.Int_pattern,
                                                                          (Nested_ast.Function(_, Nested_ast.Int_expr(0))),
                                                                          (Nested_ast.Function(_, Nested_ast.Appl_expr(
                                                                               Nested_ast.Record_expr(m1),
                                                                               Nested_ast.Record_expr(m2)
                                                                             )
                                                                             )
                                                                          )
                                                                        )
                                                                       )
                                                   )
                                                 )
                                                )
                            )
                          )
                         ) when Ident_map.is_empty m1 && Ident_map.is_empty m2 -> ()
    | _ -> assert_failure("Match test 1: Swan translation did not match Nested control")

let match_test_2 =
  "match_test_2" >:: fun _ ->
    let swan = Swan_ast.Match_expr(Swan_ast.Bool_expr(true), [Swan_ast.Match_pair(Swan_ast.Bool_pattern(true), Swan_ast.Int_expr(1));
                                                              Swan_ast.Match_pair(Swan_ast.Fun_pattern, Swan_ast.Int_expr(2));
                                                              Swan_ast.Match_pair(Swan_ast.Int_pattern, Swan_ast.Int_expr(0))]) in
    let expr = (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Let_expr(_, Nested_ast.Bool_expr(true),
                          Nested_ast.Conditional_expr(
                            Nested_ast.Bool_expr(true),
                            Nested_ast.Bool_pattern(true),
                            (Nested_ast.Function(_, Nested_ast.Int_expr(1))),
                            (Nested_ast.Function(_,
                                                 Nested_ast.Conditional_expr(
                                                   Nested_ast.Bool_expr(true),
                                                   Nested_ast.Fun_pattern,
                                                   (Nested_ast.Function(_, Nested_ast.Int_expr(2))),
                                                   (Nested_ast.Function(_,
                                                                        Nested_ast.Conditional_expr(
                                                                          Nested_ast.Bool_expr(true),
                                                                          Nested_ast.Int_pattern,
                                                                          (Nested_ast.Function(_, Nested_ast.Int_expr(0))),
                                                                          (Nested_ast.Function(_, Nested_ast.Appl_expr(
                                                                               Nested_ast.Record_expr(m1),
                                                                               Nested_ast.Record_expr(m2)
                                                                             )
                                                                             )
                                                                          )
                                                                        )
                                                                       )
                                                   )
                                                 )
                                                )
                            )
                          )
                         ) when Ident_map.is_empty m1 && Ident_map.is_empty m2 -> ()
    | _ -> assert_failure("Match test 2: Swan translation did not match Nested control")

let match_test_none =
  "match_test_none" >:: fun _ ->
    let swan = Swan_ast.Match_expr(Swan_ast.String_expr("test"), [Swan_ast.Match_pair(Swan_ast.Bool_pattern(true), Swan_ast.Int_expr(1));
                                                                  Swan_ast.Match_pair(Swan_ast.Fun_pattern, Swan_ast.Int_expr(2));
                                                                  Swan_ast.Match_pair(Swan_ast.Int_pattern, Swan_ast.Int_expr(0))]) in
    let expr = (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Let_expr(_, Nested_ast.String_expr("test"),
                          Nested_ast.Conditional_expr(
                            Nested_ast.String_expr("test"),
                            Nested_ast.Bool_pattern(true),
                            (Nested_ast.Function(_, Nested_ast.Int_expr(1))),
                            (Nested_ast.Function(_,
                                                 Nested_ast.Conditional_expr(
                                                   Nested_ast.String_expr("test"),
                                                   Nested_ast.Fun_pattern,
                                                   (Nested_ast.Function(_, Nested_ast.Int_expr(2))),
                                                   (Nested_ast.Function(_,
                                                                        Nested_ast.Conditional_expr(
                                                                          Nested_ast.String_expr("test"),
                                                                          Nested_ast.Int_pattern,
                                                                          (Nested_ast.Function(_, Nested_ast.Int_expr(0))),
                                                                          (Nested_ast.Function(_, Nested_ast.Appl_expr(
                                                                               Nested_ast.Record_expr(m1),
                                                                               Nested_ast.Record_expr(m2)
                                                                             )
                                                                             )
                                                                          )
                                                                        )
                                                                       )
                                                   )
                                                 )
                                                )
                            )
                          )
                         ) when Ident_map.is_empty m1 && Ident_map.is_empty m2 -> ()
    | _ -> assert_failure("Match test none: Swan translation did not match Nested control")

let if_in_match_test =
  "if_in_match_test" >:: fun _ ->
    let swan = Swan_ast.Match_expr(Swan_ast.Int_expr(0), [Swan_ast.Match_pair(Swan_ast.Bool_pattern(true), Swan_ast.Int_expr(1));
                                                          Swan_ast.Match_pair(Swan_ast.Fun_pattern, Swan_ast.Int_expr(2));
                                                          Swan_ast.Match_pair(Swan_ast.Int_pattern, Swan_ast.If_expr(Swan_ast.Bool_expr(true), Swan_ast.Int_expr(0), Swan_ast.Int_expr(1)))]) in
    let expr = (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Let_expr(_, Nested_ast.Int_expr(0),
                          Nested_ast.Conditional_expr(
                            Nested_ast.Int_expr(0),
                            Nested_ast.Bool_pattern(true),
                            (Nested_ast.Function(_, Nested_ast.Int_expr(1))),
                            (Nested_ast.Function(_,
                                                 Nested_ast.Conditional_expr(
                                                   Nested_ast.Int_expr(0),
                                                   Nested_ast.Fun_pattern,
                                                   (Nested_ast.Function(_, Nested_ast.Int_expr(2))),
                                                   (Nested_ast.Function(_,
                                                                        Nested_ast.Conditional_expr(
                                                                          Nested_ast.Int_expr(0),
                                                                          Nested_ast.Int_pattern,
                                                                          (Nested_ast.Function(_, Nested_ast.Conditional_expr(Nested_ast.Bool_expr(true),
                                                                                                                              Nested_ast.Bool_pattern(true),
                                                                                                                              Nested_ast.Function(_, Nested_ast.Int_expr(0)),
                                                                                                                              Nested_ast.Function(_, Nested_ast.Int_expr(1))))),
                                                                          (Nested_ast.Function(_, Nested_ast.Appl_expr(
                                                                               Nested_ast.Record_expr(m1),
                                                                               Nested_ast.Record_expr(m2)
                                                                             )
                                                                             )
                                                                          )
                                                                        )
                                                                       )
                                                   )
                                                 )
                                                )
                            )
                          )
                         ) when Ident_map.is_empty m1 && Ident_map.is_empty m2 -> ()
    | _ -> assert_failure("If in match test: Swan translation did not match Nested control")


let tests = "Test_swan" >:::
            [ if_then_else_test_1 ;
              if_then_else_test_2 ;
              match_test_1;
              match_test_2;
              match_test_none;
              if_in_if_test;
              if_in_match_test ]
;;
