(* These tests test the desugaring of the Swan ASTs into Nested ASTs *)

(* open Batteries;; *)
open OUnit2;;
open Uid;;
open Swan_translator;;
(* open Pp_utils;; *)
module Ident_map = Ast.Ident_map

let zero () = assert_failure("Expected UID not found in map, or proof of the wrong form.")

let if_then_else_test_1 =
  "if_then_else_test" >:: fun _ ->
    let if_uid = next_uid () in
    let orig_bool_uid = next_uid () in
    let int_0_uid = next_uid () in
    let int_1_uid = next_uid () in
    let swan = Swan_ast.If_expr(
        if_uid
      , Swan_ast.Bool_expr(orig_bool_uid,true)
      , Swan_ast.Int_expr(int_0_uid,0)
      , Swan_ast.Int_expr(int_1_uid,1))
    in
    let (expr, uids) = Swan_translator.nested_expr_of_swan_expr swan in
    match expr with
    | Nested_ast.Conditional_expr(
        nested_cond_uid,Nested_ast.Bool_expr(nested_bool_uid,true),
        Nested_ast.Bool_pattern(_,true),
        Nested_ast.Function(_, _, Nested_ast.Int_expr(nested_int_0_uid, 0)),
        Nested_ast.Function(_, _, Nested_ast.Int_expr(nested_int_1_uid, 1))) ->
      let%orzero Some (If_expr_rule(_, swan_cond_uid)) =
        Uid_map.Exceptionless.find nested_cond_uid uids
      in
      let%orzero Some (Bool_expr_rule(_, swan_bool_uid)) =
        Uid_map.Exceptionless.find nested_bool_uid uids
      in
      let%orzero Some (Int_expr_rule(_, swan_int_0_uid)) =
        Uid_map.Exceptionless.find nested_int_0_uid uids
      in
      let%orzero Some (Int_expr_rule(_, swan_int_1_uid)) =
        Uid_map.Exceptionless.find nested_int_1_uid uids
      in
      assert_bool "If_uid matches proof_rule" (equal_uid if_uid swan_cond_uid);
      assert_bool "Bool_uid matches proof_rule" (equal_uid orig_bool_uid swan_bool_uid);
      assert_bool "Int_0_uid matches proof_rule" (equal_uid int_0_uid swan_int_0_uid);
      assert_bool "Int_1_uid matches proof_rule" (equal_uid int_1_uid swan_int_1_uid)
    | _ -> assert_failure("If Test 1: Swan translation did not match Nested control")
;;

let if_then_else_test_2 =
  "if_then_else_test" >:: fun _ ->
    let swan = Swan_ast.If_expr(
        next_uid ()
      , Swan_ast.Bool_expr(next_uid (),false)
      , Swan_ast.Int_expr(next_uid (),0)
      , Swan_ast.Int_expr(next_uid (),1))
    in
    let expr = fst (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Conditional_expr(_,Nested_ast.Bool_expr(_,false),
                                  Nested_ast.Bool_pattern(_,true),
                                  Nested_ast.Function(_, _, Nested_ast.Int_expr(_,0)),
                                  Nested_ast.Function(_, _, Nested_ast.Int_expr(_,1))) -> ()
    | _ -> assert_failure("If Test 2: Swan translation did not match Nested control")
;;

let if_in_if_test =
  "if_in_if_test" >:: fun _ ->
    let swan = Swan_ast.If_expr(
        next_uid ()
      , Swan_ast.Bool_expr(next_uid (),true)
      , Swan_ast.If_expr(
          next_uid ()
        , Swan_ast.Bool_expr(next_uid (),false)
        , Swan_ast.Int_expr(next_uid (),0)
        , Swan_ast.Int_expr(next_uid (),1))
      , Swan_ast.Int_expr(next_uid (),1))
    in
    let expr = fst (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Conditional_expr
        ( _
        , Nested_ast.Bool_expr(_,true)
        , Nested_ast.Bool_pattern(_,true)
        , Nested_ast.Function(
            _
          , _
          , Nested_ast.Conditional_expr(
              _
            , Nested_ast.Bool_expr(_,false),
            Nested_ast.Bool_pattern(_,true),
            Nested_ast.Function(_, _, Nested_ast.Int_expr(_,0)),
            Nested_ast.Function(_,_, Nested_ast.Int_expr(_,1))))
        , Nested_ast.Function(_,_, Nested_ast.Int_expr(_,1))
        ) -> assert_string("")
    | _ -> assert_failure("If in if test: Swan translation did not match Nested control")
;;

let match_test_1 =
  "match_test_1" >:: fun _ ->
    let match_uid = next_uid () in
    let fst_mp_uid = next_uid () in
    let snd_mp_uid = next_uid () in
    let thd_mp_uid = next_uid () in
    let swan = Swan_ast.Match_expr(
        match_uid
      , Swan_ast.Int_expr(next_uid (),0)
      , [Swan_ast.Match_pair(fst_mp_uid, Swan_ast.Bool_pattern(next_uid (),true), Swan_ast.Int_expr(next_uid (),1));
         Swan_ast.Match_pair(snd_mp_uid, Swan_ast.Fun_pattern(next_uid ()), Swan_ast.Int_expr(next_uid (),2));
         Swan_ast.Match_pair(thd_mp_uid, Swan_ast.Int_pattern(next_uid ()), Swan_ast.Int_expr(next_uid (),0))])
    in
    let (expr, uids) = (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Let_expr(
        let_uid,
        _,
        Nested_ast.Int_expr(_,0),
        Nested_ast.Conditional_expr(
          nested_fst_cond_uid,
          Nested_ast.Int_expr(_,0),
          Nested_ast.Bool_pattern(_,true),
          (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,1))),
          (Nested_ast.Function(_,
                               _,
                               Nested_ast.Conditional_expr(
                                 nested_snd_cond_uid,
                                 Nested_ast.Int_expr(_,0),
                                 Nested_ast.Fun_pattern(_),
                                 (Nested_ast.Function(_,_, Nested_ast.Int_expr(_,2))),
                                 (Nested_ast.Function(
                                     _,
                                     _,
                                     Nested_ast.Conditional_expr(
                                       nested_thd_cond_uid,
                                       Nested_ast.Int_expr(_,0),
                                       Nested_ast.Int_pattern(_),
                                       (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,0))),
                                       (Nested_ast.Function(
                                           _,
                                           _,
                                           Nested_ast.Appl_expr(
                                             _,
                                             Nested_ast.Record_expr(_,m1),
                                             Nested_ast.Record_expr(_,m2)
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
      ) when Ident_map.is_empty m1 && Ident_map.is_empty m2 ->
      let%orzero Some (Match_expr_rule(_, swan_match_uid)) =
        Uid_map.Exceptionless.find let_uid uids
      in
      let%orzero Some (Match_pair_rule(_, swan_fst_cond_uid, _, _)) =
        Uid_map.Exceptionless.find nested_fst_cond_uid uids
      in
      (* print_endline (
         uids
         |> pp_to_string
          (pp_map pp_uid pp_proof Uid_map.enum)
         );
         print_newline ();
         print_endline (
         swan
         |> pp_to_string Swan_ast.pp_expr
         );
         print_newline ();
         print_endline (
         expr
         |> pp_to_string Nested_ast.pp_expr
         ); *)
      let%orzero Some (Match_pair_rule(_, swan_snd_cond_uid, _, _)) =
        Uid_map.Exceptionless.find nested_snd_cond_uid uids
      in
      let%orzero Some (Match_pair_rule(_, swan_thd_cond_uid, _, _)) =
        Uid_map.Exceptionless.find nested_thd_cond_uid uids
      in
      assert_bool "Match_uid matches proof_rule" (equal_uid match_uid swan_match_uid);
      assert_bool "First Match_pair_uid matches proof_rule" (equal_uid fst_mp_uid swan_fst_cond_uid);
      assert_bool "Second Match_pair_uid matches proof_rule" (equal_uid snd_mp_uid swan_snd_cond_uid);
      assert_bool "Third Match_pair_uid matches proof_rule" (equal_uid thd_mp_uid swan_thd_cond_uid);
    | _ -> assert_failure("Match test 1: Swan translation did not match Nested control")

let match_test_2 =
  "match_test_2" >:: fun _ ->
    let swan = Swan_ast.Match_expr(
        next_uid (),Swan_ast.Bool_expr(next_uid (),true),
        [Swan_ast.Match_pair(next_uid (),Swan_ast.Bool_pattern(next_uid (),true), Swan_ast.Int_expr(next_uid (),1));
         Swan_ast.Match_pair(next_uid (),Swan_ast.Fun_pattern(next_uid ()), Swan_ast.Int_expr(next_uid (),2));
         Swan_ast.Match_pair(next_uid (),Swan_ast.Int_pattern(next_uid ()), Swan_ast.Int_expr(next_uid (),0))])
    in
    let expr = fst (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Let_expr(
        _,
        _,
        Nested_ast.Bool_expr(_,true),
        Nested_ast.Conditional_expr(
          _,
          Nested_ast.Bool_expr(_,true),
          Nested_ast.Bool_pattern(_,true),
          (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,1))),
          (Nested_ast.Function(
              _,
              _,
              Nested_ast.Conditional_expr(
                _,
                Nested_ast.Bool_expr(_,true),
                Nested_ast.Fun_pattern(_),
                (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,2))),
                (Nested_ast.Function(
                    _,
                    _,
                    Nested_ast.Conditional_expr(
                      _,
                      Nested_ast.Bool_expr(_,true),
                      Nested_ast.Int_pattern(_),
                      (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,0))),
                      (Nested_ast.Function(
                          _,
                          _,
                          Nested_ast.Appl_expr(
                            _,
                            Nested_ast.Record_expr(_,m1),
                            Nested_ast.Record_expr(_,m2)
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
    let swan = Swan_ast.Match_expr(
        next_uid (),
        Swan_ast.String_expr(next_uid (),"test"),
        [Swan_ast.Match_pair(next_uid (),Swan_ast.Bool_pattern(next_uid (),true), Swan_ast.Int_expr(next_uid (),1));
         Swan_ast.Match_pair(next_uid (),Swan_ast.Fun_pattern(next_uid ()), Swan_ast.Int_expr(next_uid (),2));
         Swan_ast.Match_pair(next_uid (),Swan_ast.Int_pattern(next_uid ()), Swan_ast.Int_expr(next_uid (),0))])
    in
    let expr = fst (Swan_translator.nested_expr_of_swan_expr swan) in
    match expr with
    | Nested_ast.Let_expr(
        _,
        _,
        Nested_ast.String_expr(_,"test"),
        Nested_ast.Conditional_expr(
          _,
          Nested_ast.String_expr(_,"test"),
          Nested_ast.Bool_pattern(_,true),
          (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,1))),
          (Nested_ast.Function(
              _,
              _,
              Nested_ast.Conditional_expr(
                _,
                Nested_ast.String_expr(_,"test"),
                Nested_ast.Fun_pattern(_),
                (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,2))),
                (Nested_ast.Function(
                    _,
                    _,
                    Nested_ast.Conditional_expr(
                      _,
                      Nested_ast.String_expr(_,"test"),
                      Nested_ast.Int_pattern(_),
                      (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,0))),
                      (Nested_ast.Function(
                          _,
                          _,
                          Nested_ast.Appl_expr(
                            _,
                            Nested_ast.Record_expr(_,m1),
                            Nested_ast.Record_expr(_,m2)
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
    let match_uid = next_uid () in
    let mp_3_uid = next_uid () in
    let if_uid = next_uid () in
    let int_1_uid = next_uid () in
    let swan = Swan_ast.Match_expr(
        match_uid,
        Swan_ast.Int_expr(next_uid (),0),
        [Swan_ast.Match_pair(next_uid (),Swan_ast.Bool_pattern(next_uid (),true), Swan_ast.Int_expr(next_uid (),1));
         Swan_ast.Match_pair(next_uid (),Swan_ast.Fun_pattern(next_uid ()), Swan_ast.Int_expr(next_uid (),2));
         Swan_ast.Match_pair(mp_3_uid,Swan_ast.Int_pattern(next_uid ()), Swan_ast.If_expr(if_uid, Swan_ast.Bool_expr(next_uid (),true), Swan_ast.Int_expr(next_uid (),0), Swan_ast.Int_expr(int_1_uid,1)))]) in
    let (expr, uids) = Swan_translator.nested_expr_of_swan_expr swan in
    match expr with
    | Nested_ast.Let_expr(
        let_uid,
        _,
        Nested_ast.Int_expr(_,0),
        Nested_ast.Conditional_expr(
          _,
          Nested_ast.Int_expr(_,0),
          Nested_ast.Bool_pattern(_,true),
          (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,1))),
          (Nested_ast.Function(
              _,
              _,
              Nested_ast.Conditional_expr(
                _,
                Nested_ast.Int_expr(_,0),
                Nested_ast.Fun_pattern(_),
                (Nested_ast.Function(_, _, Nested_ast.Int_expr(_,2))),
                (Nested_ast.Function(
                    _,
                    _,
                    Nested_ast.Conditional_expr(
                      cond_uid,
                      Nested_ast.Int_expr(_,0),
                      Nested_ast.Int_pattern(_),
                      (Nested_ast.Function(
                          _,
                          _,
                          Nested_ast.Conditional_expr(
                            if_cond_uid,
                            Nested_ast.Bool_expr(_,true),
                            Nested_ast.Bool_pattern(_,true),
                            Nested_ast.Function(_, _, Nested_ast.Int_expr(_,0)),
                            Nested_ast.Function(_, _,Nested_ast.Int_expr(nested_int_1_uid,1))))),
                      (Nested_ast.Function(
                          _,
                          _,
                          Nested_ast.Appl_expr(
                            _,
                            Nested_ast.Record_expr(_,m1),
                            Nested_ast.Record_expr(_,m2)
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
      ) when Ident_map.is_empty m1 && Ident_map.is_empty m2 ->
      let%orzero Some (Match_expr_rule(_, swan_match_uid)) =
        Uid_map.Exceptionless.find let_uid uids
      in
      let%orzero Some (Match_pair_rule(_, swan_mp_3_uid, _, _)) =
        Uid_map.Exceptionless.find cond_uid uids
      in
      let%orzero Some (If_expr_rule(_, swan_if_uid)) =
        Uid_map.Exceptionless.find if_cond_uid uids
      in
      let%orzero Some (Int_expr_rule(_, swan_int_1_uid)) =
        Uid_map.Exceptionless.find nested_int_1_uid uids
      in
      assert_bool "Match_uid matches proof_rule" (equal_uid match_uid swan_match_uid);
      assert_bool "3rd Match_pair matches proof_rule" (equal_uid mp_3_uid swan_mp_3_uid);
      assert_bool "If_uid matches proof_rule" (equal_uid if_uid swan_if_uid);
      assert_bool "Int_expr matches proof_rule" (equal_uid int_1_uid swan_int_1_uid);
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
