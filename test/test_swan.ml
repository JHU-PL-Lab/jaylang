(* These tests test the desugaring of the Swan ASTs into Nested ASTs *)

(* open Batteries;; *)
open OUnit2;;
open Uid;;
open Swan_translator;;
open Egg_translator;;
(* open Pp_utils;; *)
module Ident_map = Core_ast.Ident_map

let zero () = assert_failure("Expected UID not found in map, or proof of the wrong form.")

let if_then_else_test_1 =
  "if_then_else_test" >:: fun _ ->
    let if_uid = next_uid () in
    let int_0_uid = next_uid () in
    let int_1_uid = next_uid () in
    let swan = Swan_ast.If_expr(
        if_uid
      , Swan_ast.Bool_expr(next_uid (),true)
      , Swan_ast.Int_expr(int_0_uid,0)
      , Swan_ast.Int_expr(int_1_uid,1))
    in
    let (expr, uids) = Swan_translator.swan_to_nested_translation swan in
    match expr with
    | Nested_ast.Conditional_expr(
        nested_cond_uid,Nested_ast.Bool_expr(_,true),
        Nested_ast.Bool_pattern(_,true),
        Nested_ast.Function(f0_uid, _, Nested_ast.Int_expr(_, 0)),
        Nested_ast.Function(f1_uid, _, Nested_ast.Int_expr(_, 1))) ->
      let%orzero Some (If_to_conditional(mapped_cond_uid, if_if_uid)) =
        Uid_map.Exceptionless.find nested_cond_uid uids
      in
      let%orzero Some (If_true_branch_to_function(_, true_branch_if_uid)) =
        Uid_map.Exceptionless.find f0_uid uids
      in
      let%orzero Some (If_false_branch_to_function(_, false_branch_if_uid)) =
        Uid_map.Exceptionless.find f1_uid uids
      in
      assert_bool "If_uid is second element in all log entries" (
        equal_uid if_uid if_if_uid &&
        equal_uid if_uid true_branch_if_uid &&
        equal_uid if_uid false_branch_if_uid
      );
      assert_bool "If_uid maps to conditional_uid" (
        equal_uid nested_cond_uid mapped_cond_uid
      );
    | _ -> assert_failure("If Test 1: Swan translation did not match Nested control")
;;


let if_in_if_test =
  "if_in_if_test" >:: fun _ ->
    let if1_uid = next_uid () in
    let if2_uid = next_uid () in
    let swan = Swan_ast.If_expr(
        if1_uid
      , Swan_ast.Bool_expr(next_uid (),true)
      , Swan_ast.If_expr(
          if2_uid
        , Swan_ast.Bool_expr(next_uid (),false)
        , Swan_ast.Int_expr(next_uid (),0)
        , Swan_ast.Int_expr(next_uid (),1))
      , Swan_ast.Int_expr(next_uid (),1))
    in
    let (expr, uids) = swan_to_nested_translation swan in
    match expr with
    | Nested_ast.Conditional_expr
        ( cond1_uid
        , Nested_ast.Bool_expr(_,true)
        , Nested_ast.Bool_pattern(_,true)
        , Nested_ast.Function(
            f1_uid
          , _
          , Nested_ast.Conditional_expr(
              cond2_uid
            , Nested_ast.Bool_expr(_,false),
            Nested_ast.Bool_pattern(_,true),
            Nested_ast.Function(f3_uid, _, Nested_ast.Int_expr(_,0)),
            Nested_ast.Function(f4_uid,_, Nested_ast.Int_expr(_,1))))
        , Nested_ast.Function(f2_uid,_, Nested_ast.Int_expr(_,1))
        ) ->
      let%orzero Some (If_to_conditional(_, mapped_if1_uid)) =
        Uid_map.Exceptionless.find cond1_uid uids
      in
      let%orzero Some (If_to_conditional(_, mapped_if2_uid)) =
        Uid_map.Exceptionless.find cond2_uid uids
      in
      let%orzero Some (If_true_branch_to_function(_, true_branch_if1_uid)) =
        Uid_map.Exceptionless.find f1_uid uids
      in
      let%orzero Some (If_true_branch_to_function(_, true_branch_if2_uid)) =
        Uid_map.Exceptionless.find f3_uid uids
      in
      let%orzero Some (If_false_branch_to_function(_, false_branch_if2_uid)) =
        Uid_map.Exceptionless.find f4_uid uids
      in
      let%orzero Some (If_false_branch_to_function(_, false_branch_if1_uid)) =
        Uid_map.Exceptionless.find f2_uid uids
      in
      assert_bool "If1_uid appears in second element of all If1 log entries" (
        equal_uid if1_uid mapped_if1_uid &&
        equal_uid if1_uid true_branch_if1_uid &&
        equal_uid if1_uid false_branch_if1_uid
      );
      assert_bool "If2_uid appears in second element of all If2 log entries" (
        equal_uid if2_uid mapped_if2_uid &&
        equal_uid if2_uid true_branch_if2_uid &&
        equal_uid if2_uid false_branch_if2_uid
      );
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
    let (expr, uids) = (swan_to_nested_translation swan) in
    match expr with
    | Nested_ast.Let_expr(
        _,
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
                                             appl_uid,
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
      let%orzero Some (Inexhaustive_match_branch(_, swan_match_uid)) =
        Uid_map.Exceptionless.find appl_uid uids
      in
      let%orzero Some (Match_branch(fst_branch_uid, fst_match_uid)) =
        Uid_map.Exceptionless.find nested_fst_cond_uid uids
      in
      let%orzero Some (Match_branch(snd_branch_uid, snd_match_uid)) =
        Uid_map.Exceptionless.find nested_snd_cond_uid uids
      in
      let%orzero Some (Match_branch(thd_branch_uid, thd_match_uid)) =
        Uid_map.Exceptionless.find nested_thd_cond_uid uids
      in
      assert_bool "Match_uid matches all second elements"
        ( equal_uid swan_match_uid match_uid &&
          equal_uid fst_match_uid match_uid &&
          equal_uid snd_match_uid match_uid &&
          equal_uid thd_match_uid match_uid
        );
      assert_bool "First Match_pair_uid matches branch" (equal_uid fst_mp_uid fst_branch_uid);
      assert_bool "Second Match_pair_uid matches branch" (equal_uid snd_mp_uid snd_branch_uid);
      assert_bool "Third Match_pair_uid matches branch" (equal_uid thd_mp_uid thd_branch_uid);
    | _ -> assert_failure("Match test 1: Swan translation did not match Nested control")
;;



let if_in_match_test =
  "if_in_match_test" >:: fun _ ->
    let match_uid = next_uid () in
    let fst_mp_uid = next_uid () in
    let snd_mp_uid = next_uid () in
    let thd_mp_uid = next_uid () in
    let if_uid = next_uid () in
    let int_0_uid = next_uid () in
    let int_1_uid = next_uid () in
    let swan = Swan_ast.Match_expr(
        match_uid
      , Swan_ast.Int_expr(next_uid (),0)
      , [Swan_ast.Match_pair(fst_mp_uid, Swan_ast.Bool_pattern(next_uid (),true), Swan_ast.Int_expr(next_uid (),1));
         Swan_ast.Match_pair(snd_mp_uid, Swan_ast.Fun_pattern(next_uid ()), Swan_ast.Int_expr(next_uid (),2));
         Swan_ast.Match_pair(thd_mp_uid, Swan_ast.Int_pattern(next_uid ()), Swan_ast.If_expr(
             if_uid
           , Swan_ast.Bool_expr(next_uid (),true)
           , Swan_ast.Int_expr(int_0_uid,0)
           , Swan_ast.Int_expr(int_1_uid,1)))])
    in
    let (expr, uids) = (swan_to_nested_translation swan) in
    match expr with
    | Nested_ast.Let_expr(
        _,
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
                                       (Nested_ast.Function(_, _,
                                                            Nested_ast.Conditional_expr(
                                                              nested_cond_uid,Nested_ast.Bool_expr(_,true),
                                                              Nested_ast.Bool_pattern(_,true),
                                                              Nested_ast.Function(f0_uid, _, Nested_ast.Int_expr(_, 0)),
                                                              Nested_ast.Function(f1_uid, _, Nested_ast.Int_expr(_, 1))))),
                                       (Nested_ast.Function(
                                           _,
                                           _,
                                           Nested_ast.Appl_expr(
                                             appl_uid,
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
      )
when Ident_map.is_empty m1 && Ident_map.is_empty m2 ->
  let%orzero Some (Inexhaustive_match_branch(_, swan_match_uid)) =
    Uid_map.Exceptionless.find appl_uid uids
  in
  let%orzero Some (Match_branch(fst_branch_uid, fst_match_uid)) =
    Uid_map.Exceptionless.find nested_fst_cond_uid uids
  in
  let%orzero Some (Match_branch(snd_branch_uid, snd_match_uid)) =
    Uid_map.Exceptionless.find nested_snd_cond_uid uids
  in
  let%orzero Some (Match_branch(thd_branch_uid, thd_match_uid)) =
    Uid_map.Exceptionless.find nested_thd_cond_uid uids
  in
  let%orzero Some (If_to_conditional(mapped_cond_uid, if_if_uid)) =
    Uid_map.Exceptionless.find nested_cond_uid uids
  in
  let%orzero Some (If_true_branch_to_function(_, true_branch_if_uid)) =
    Uid_map.Exceptionless.find f0_uid uids
  in
  let%orzero Some (If_false_branch_to_function(_, false_branch_if_uid)) =
    Uid_map.Exceptionless.find f1_uid uids
  in
  assert_bool "Match_uid matches all second elements"
    ( equal_uid swan_match_uid match_uid &&
      equal_uid fst_match_uid match_uid &&
      equal_uid snd_match_uid match_uid &&
      equal_uid thd_match_uid match_uid
    );
  assert_bool "First Match_pair_uid matches branch" (equal_uid fst_mp_uid fst_branch_uid);
  assert_bool "Second Match_pair_uid matches branch" (equal_uid snd_mp_uid snd_branch_uid);
  assert_bool "Third Match_pair_uid matches branch" (equal_uid thd_mp_uid thd_branch_uid);
  assert_bool "If_uid is second element in all log entries" (
    equal_uid if_uid if_if_uid &&
    equal_uid if_uid true_branch_if_uid &&
    equal_uid if_uid false_branch_if_uid
  );
  assert_bool "If_uid maps to conditional_uid" (
    equal_uid nested_cond_uid mapped_cond_uid
  );
  | _ -> assert_failure("Match test 1: Swan translation did not match Nested control")
;;


let tests = "Test_swan" >:::
            [ if_then_else_test_1 ;
              if_in_if_test ;
              match_test_1 ;
              if_in_match_test
            ]
;;
