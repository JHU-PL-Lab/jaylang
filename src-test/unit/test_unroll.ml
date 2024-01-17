open Core
open Unroll.Stream_helper

(* let history = ref [] in *)
(* U_int.iter u 1 (fun v ->
    history := v :: !history ;
    Lwt.return_unit) *)

let ascending xs = List.sort xs ~compare:Int.compare
let pair_add (x, y) = x + y
let pairwire_add xs = List.map xs ~f:pair_add

module Test_stream = struct
  let two_streams_from_list () =
    let msgs1 = List.init 3 ~f:Fn.id in
    let msgs2 = List.init 3 ~f:Fn.id in
    let s1 = Lwt_stream.of_list msgs1 in
    let s2 = Lwt_stream.of_list msgs2 in
    let s12 = product_stream s1 s2 in
    let ans = Lwt_stream.get_available s12 in
    Alcotest.(check @@ list (pair int int))
      "equal"
      [ (0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1); (0, 2); (1, 2); (2, 2) ]
      ans

  let one_stepped_one_list () =
    let msgs1 = List.init 3 ~f:Fn.id in
    let msgs2 = List.init 3 ~f:Fn.id in
    let s1, p1 = Lwt_stream.create () in
    List.iter msgs1 ~f:(fun m1 -> p1 (Some m1)) ;
    p1 None ;
    (* p2 (Some 1) ; *)
    let s2 = Lwt_stream.of_list msgs2 in
    let s12 = product_stream s1 s2 in
    Alcotest.(check @@ list (pair int int))
      "equal"
      [ (0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1); (0, 2); (1, 2); (2, 2) ]
      (Lwt_stream.get_available s12) ;
    ()

  let two_stepped () =
    let msgs1 = List.init 3 ~f:Fn.id in
    let msgs2 = List.init 3 ~f:Fn.id in
    let s1, p1 = Lwt_stream.create () in
    List.iter msgs1 ~f:(fun m -> p1 (Some m)) ;
    p1 None ;
    let s2, p2 = Lwt_stream.create () in
    List.iter msgs2 ~f:(fun m -> p2 (Some m)) ;
    p2 None ;
    let s12 = product_stream s1 s2 in
    Alcotest.(check @@ list (pair int int))
      "equal"
      [ (0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1); (0, 2); (1, 2); (2, 2) ]
      (Lwt_stream.get_available s12) ;
    ()

  let two_stepped_interleaved () =
    let msgs = List.init 3 ~f:Fn.id in
    let s1, p1 = Lwt_stream.create () in
    let s2, p2 = Lwt_stream.create () in
    List.iter msgs ~f:(fun m ->
        p1 (Some m) ;
        p2 (Some m)) ;
    p1 None ;
    p2 None ;
    let s12 = product_stream s1 s2 in
    Alcotest.(check @@ list (pair int int))
      "equal"
      [ (0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1); (0, 2); (1, 2); (2, 2) ]
      (Lwt_stream.get_available s12) ;
    ()

  (* This test can pass when the `product_stream` uses `iter_p` rather than `iter_s` *)
  let two_opened () =
    let msgs = List.init 3 ~f:Fn.id in
    let s1, p1 = Lwt_stream.create () in
    let s2, p2 = Lwt_stream.create () in
    List.iter msgs ~f:(fun m ->
        p1 (Some m) ;
        p2 (Some m)) ;
    let s12 = product_stream s1 s2 in
    Alcotest.(check @@ list (pair int int))
      "equal"
      [ (0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1); (0, 2); (1, 2); (2, 2) ]
      (Lwt_stream.get_available s12) ;
    ()

  let two_closed () =
    let msgs1 = List.init 3 ~f:Fn.id in
    let msgs2 = List.init 3 ~f:Fn.id in
    let s1, p1 = Lwt_stream.create () in
    List.iter msgs1 ~f:(fun m -> p1 (Some m)) ;
    p1 None ;
    let s2, p2 = Lwt_stream.create () in
    List.iter msgs2 ~f:(fun m -> p2 (Some m)) ;
    p2 None ;
    let s12 = product_stream s1 s2 in
    let ans = Lwt_main.run @@ Lwt_stream.to_list s12 in
    Alcotest.(check @@ list (pair int int))
      "equal"
      [ (0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1); (0, 2); (1, 2); (2, 2) ]
      ans ;
    ()

  let all_tests =
    [
      Alcotest.test_case "two_closed_streams" `Quick two_streams_from_list;
      Alcotest.test_case "one_stepped_one_list" `Quick one_stepped_one_list;
      Alcotest.test_case "two_stepped" `Quick two_stepped;
      Alcotest.test_case "two_stepped_interleaved" `Quick
        two_stepped_interleaved;
      Alcotest.test_case "two_opened" `Quick two_opened;
      Alcotest.test_case "two_closed" `Quick two_closed;
    ]
end

module Int_payload = struct
  type payload = int [@@deriving equal]
end

module type U_bg_ints = Unroll.U_bg with type key = int and type payload = int

module type U_bg_ints_pipe =
  Unroll.U_bg with type key = int and type payload = int and type pipe = int

module type U_fg_ints =
  Unroll.U with type key = int and type payload = int and type 'a act = 'a Lwt.t

module type U2_fg_ints =
  Unroll.U2
    with type key = int
     and type payload = int
     and type 'a act = 'a Lwt.t

module U_to_test_bg_ints (U : U_bg_ints) = struct
  let one_msg () =
    let msg = 42 in
    let u = U.create () in
    U.one_shot u 1 [ msg ] ;
    let ans = Lwt_main.run @@ U.get_payloads u 1 in
    Alcotest.(check @@ list int) "equal" [ msg ] ans

  let one_shot () =
    let nums = List.init 10 ~f:Fn.id in
    let u = U.create () in
    U.one_shot u 2 nums ;
    let ans = Lwt_main.run @@ U.get_payloads u 2 in
    Alcotest.(check @@ list int) "equal" nums ans

  let available_ongoing () =
    let nums = List.init 10 ~f:Fn.id in
    let u = U.create () in
    List.iter nums ~f:(fun msg -> U.push u 2 (Some msg)) ;
    let ans = U.get_available_payloads u 2 in
    Alcotest.(check @@ list int) "equal" nums ans

  let all_tests =
    [
      Alcotest.test_case "one_msg" `Quick one_msg;
      Alcotest.test_case "one_shot" `Quick one_shot;
      Alcotest.test_case "available_ongoing" `Quick available_ongoing;
    ]
end

module U_to_test_bg_key_int (U : U_bg_ints_pipe) = struct
  let test_join () =
    let k = 5 in
    let nums1 = List.init k ~f:Fn.id in
    let nums2 = List.init k ~f:(fun x -> x + k) in
    let u = U.create () in
    U.one_shot u 3 nums1 ;
    U.one_shot u 4 nums2 ;
    U.join u [ 3; 4 ] 5 ;
    let ans = U.get_available_payloads u 5 in
    Alcotest.(check @@ list int) "equal" (nums1 @ nums2) ans

  let test_map () =
    let msgs_in = List.init 10 ~f:Fn.id in
    let plus_10 m = m + 10 in
    let msgs_out = List.map msgs_in ~f:plus_10 in
    let u = U.create () in
    U.one_shot u 1 msgs_in ;
    U.map u 1 42 plus_10 ;
    let ans = U.get_available_payloads u 42 in
    Alcotest.(check @@ list int) "equal" msgs_out ans

  let test_filter_map () =
    let msgs_in = List.init 10 ~f:Fn.id in
    let plus_10_even m =
      let ans = m + 10 in
      if ans mod 2 = 0 then Some ans else None
    in
    let msgs_out = List.filter_map msgs_in ~f:plus_10_even in
    let u = U.create () in
    U.one_shot u 1 msgs_in ;
    U.filter_map u 1 42 plus_10_even ;
    let ans = U.get_available_payloads u 42 in
    Alcotest.(check @@ list int) "equal" msgs_out ans

  let test_combine () =
    let msgs1 = List.init 5 ~f:Fn.id in
    let msgs2 = List.init 5 ~f:Fn.id in
    let s1 = Lwt_stream.of_list msgs1 in
    let s2 = Lwt_stream.of_list msgs2 in
    let s12 = Lwt_stream.combine s1 s2 in
    let anss = Lwt_stream.map pair_add s12 in
    let ans = Lwt_stream.get_available anss in
    Alcotest.(check @@ list int) "equal" [ 0; 2; 4; 6; 8 ] ans

  let test_map2_naive () =
    let msgs1 = List.init 5 ~f:Fn.id in
    let msgs2 = [ 42 ] in
    let msgs_out = List.map msgs1 ~f:(fun x -> x + 42) in
    let u = U.create () in
    U.one_shot u 1 msgs1 ;
    U.one_shot u 2 msgs2 ;
    U.map2 u 1 2 5 (fun (x, y) -> x + y) ;
    let ans = U.get_available_payloads u 5 in
    Alcotest.(check @@ list int) "equal" msgs_out ans

  let test_map2_product_seq () =
    let msgs1 = List.init 5 ~f:Fn.id in
    let msgs2 = [ 10; 20 ] in
    let expected =
      List.cartesian_product msgs1 msgs2 |> pairwire_add |> ascending
    in
    let u = U.create () in
    U.one_shot u 1 msgs1 ;
    U.one_shot u 2 msgs2 ;
    U.map2 u 1 2 5 pair_add ;
    let ans = U.get_available_payloads u 5 |> ascending in
    Alcotest.(check @@ list int) "equal" expected ans

  let test_map2_product_interleaved () =
    let msgs = List.init 3 ~f:Fn.id in
    let expected =
      List.cartesian_product msgs msgs |> pairwire_add |> ascending
    in
    let u = U.create ~is_dedup:false () in
    List.iter msgs ~f:(fun n1 ->
        U.push u 1 (Some n1) ;
        U.push u 2 (Some n1)) ;

    (* U.push u 1 None ;
       U.push u 2 None ; *)
    U.map2 u 1 2 5 pair_add ;
    Alcotest.(check @@ list int)
      "s12" expected
      (U.get_available_payloads u 5 |> ascending) ;
    ()

  let test_map2_product_interleaved_open () =
    let msgs = List.init 3 ~f:Fn.id in
    let expected =
      List.cartesian_product msgs msgs |> pairwire_add |> ascending
    in
    let u = U.create ~is_dedup:false () in
    List.iter msgs ~f:(fun n1 ->
        U.push u 1 (Some n1) ;
        U.push u 2 (Some n1)) ;
    U.push u 1 None ;
    U.push u 2 None ;

    U.map2 u 1 2 5 pair_add ;
    Alcotest.(check @@ list int)
      "s12" expected
      (Lwt_main.run @@ U.get_payloads u 5 |> ascending) ;

    ()

  let all_tests =
    [
      Alcotest.test_case "join_two" `Quick test_join;
      Alcotest.test_case "map_one" `Quick test_map;
      Alcotest.test_case "filter_map_one" `Quick test_filter_map;
      Alcotest.test_case "combine" `Quick test_combine;
      Alcotest.test_case "map2_naive" `Quick test_map2_naive;
      Alcotest.test_case "map2_product_seq" `Quick test_map2_product_seq;
      Alcotest.test_case "map2_product_interleaved" `Quick
        test_map2_product_interleaved;
      Alcotest.test_case "map2_product_interleaved_open" `Quick
        test_map2_product_interleaved_open;
    ]
end

module U_to_test_fg_key_int (U : U_fg_ints) = struct
  let test_join () =
    let k = 5 in
    let nums1 = List.init k ~f:Fn.id in
    let nums2 = List.init k ~f:(fun x -> x + k) in
    let u = U.create () in
    let all_work =
      let%lwt p3 = U.one_shot u 3 nums1 in
      let%lwt p4 = U.one_shot u 4 nums2 in
      let%lwt _p5 = U.join u [ p3; p4 ] 5 in
      (* U.get_payload_stream u 5 |> Lwt_stream.to_list *)
      Lwt.return @@ U.get_available_payloads u 5
    in
    let ans = Lwt_main.run all_work in
    Alcotest.(check @@ list int) "equal" (nums1 @ nums2) ans

  let all_tests = [ Alcotest.test_case "join_two" `Quick test_join ]
end

module N = Unroll.Naive_state_machine

module U_to_test_state (U : U2_fg_ints) = struct
  let id_fail () =
    let u = U.create () in
    U.push u 1 (Some 1) ;
    U.push u 1 (Some 2) ;
    ignore @@ U.id u (U.find_detail u 1) 2 ;
    U.push_state u 1 N.Fail ;
    U.push u 1 (Some 3) ;
    (* let all_work = *)
    Alcotest.(check @@ list int) "s1" [ 1; 2 ] (U.get_available_payloads u 1) ;
    Alcotest.(check @@ list int) "s2" [ 1; 2 ] (U.get_available_payloads u 2)

  let map_fail () =
    let u = U.create () in
    U.push u 1 (Some 1) ;
    U.push u 1 (Some 2) ;
    ignore @@ U.map u (U.find_detail u 1) 2 (fun x -> 10 * x) ;
    U.push_state u 1 N.Fail ;
    U.push u 1 (Some 3) ;
    Alcotest.(check @@ list int) "s1" [ 1; 2 ] (U.get_available_payloads u 1) ;
    Alcotest.(check @@ list int) "s2" [ 10; 20 ] (U.get_available_payloads u 2)

  let map2_fail () =
    let u = U.create () in
    U.push u 1 (Some 1) ;
    U.push u 1 (Some 2) ;
    U.push_state u 1 N.Fail ;
    U.push u 1 (Some 3) ;
    U.push u 2 (Some 10) ;
    U.push u 2 (Some 20) ;
    U.push_state u 2 N.Fail ;
    U.push u 2 (Some 30) ;
    ignore
    @@ U.map2 u (U.find_detail u 1) (U.find_detail u 2) 5 (fun (x, y) -> x + y) ;
    Alcotest.(check @@ list int) "s1" [ 1; 2 ] (U.get_available_payloads u 1) ;
    Alcotest.(check @@ list int) "s2" [ 10; 20 ] (U.get_available_payloads u 2) ;
    Alcotest.(check @@ list int)
      "s12" [ 11; 12; 21; 22 ]
      (U.get_available_payloads u 5)

  let map2_fail_extra () =
    let u = U.create () in
    U.push u 1 (Some 1) ;
    U.push u 1 (Some 2) ;
    U.push_state u 1 N.Fail ;
    U.push u 1 (Some 3) ;
    U.push u 2 (Some 10) ;
    U.push u 2 (Some 20) ;
    (* U.push_state u 2 N.Fail ;
       U.push u 2 (Some 30) ; *)
    ignore
    @@ U.map2 u (U.find_detail u 1) (U.find_detail u 2) 5 (fun (x, y) -> x + y) ;
    (* U.push u 5 (Some 42) ; *)
    (* we cannot push to 5 anymore because the only task is done then stream 5 is closed automatically *)
    Alcotest.(check @@ list int)
      "s12" [ 11; 12; 21; 22 ]
      (U.get_available_payloads u 5)

  let join () =
    let u = U.create () in
    U.push u 1 (Some 1) ;
    U.push u 1 (Some 2) ;
    U.push_state u 1 N.Fail ;
    U.push u 1 (Some 3) ;
    U.push u 2 (Some 10) ;
    U.push u 2 (Some 20) ;
    U.push_state u 2 N.Fail ;
    U.push u 2 (Some 30) ;
    U.push u 3 (Some 100) ;
    U.push u 3 (Some 200) ;
    U.push_state u 3 N.Fail ;
    U.push u 3 (Some 300) ;
    ignore
    @@ U.join u [ U.find_detail u 1; U.find_detail u 2; U.find_detail u 3 ] 5 ;
    (* U.push u 5 (Some 42) ; *)
    (* we cannot push to 5 anymore because the only task is done then stream 5 is closed automatically *)
    Alcotest.(check @@ list int)
      "s5" [ 1; 2; 10; 20; 100; 200 ]
      (U.get_available_payloads u 5)

  let bind_like () =
    let u = U.create () in
    U.push u 1 (Some 1) ;
    U.push u 1 (Some 2) ;
    U.push_state u 1 N.Fail ;
    U.push u 1 (Some 3) ;
    U.push u 10 (Some 10) ;
    U.push u 10 (Some 20) ;
    U.push_state u 10 N.Fail ;
    U.push u 10 (Some 30) ;
    U.push u 20 (Some 100) ;
    U.push u 20 (Some 200) ;
    U.push_state u 20 N.Fail ;
    U.push u 20 (Some 300) ;
    ignore @@ U.bind_like u (U.find_detail u 1) (fun x -> 10 * x) 5 ;
    Alcotest.(check @@ list int)
      "s5" [ 10; 20; 100; 200 ]
      (U.get_available_payloads u 5)

  let all_tests =
    [
      Alcotest.test_case "id_fail" `Quick id_fail;
      Alcotest.test_case "map_fail" `Quick map_fail;
      Alcotest.test_case "map2_fail" `Quick map2_fail;
      Alcotest.test_case "map2_fail_extra" `Quick map2_fail_extra;
      Alcotest.test_case "join" `Quick join;
      Alcotest.test_case "bind_like" `Quick bind_like;
    ]
end

let make_test_common (module U : U_bg_ints) =
  let module UA = U_to_test_bg_ints (U) in
  UA.all_tests

let make_test_bg (module U : U_bg_ints_pipe) =
  let module UA = U_to_test_bg_key_int (U) in
  UA.all_tests

let make_test_fg (module U : U_fg_ints) =
  let module UA = U_to_test_fg_key_int (U) in
  UA.all_tests

let make_test_state (module U : U2_fg_ints) =
  let module UA = U_to_test_state (U) in
  UA.all_tests

module U1 = Unroll.Make_use_key (Int) (Int_payload)
module U2 = Unroll.Make_dummy_control (Int) (Int_payload)
module U3 = Unroll.Make_control (Int) (Int_payload)
module U4 = Unroll.Make_control_bg (Int) (Int_payload)

let () =
  Alcotest.run "Interpreter"
    [
      ("lwt_stream", Test_stream.all_tests);
      ("basic_bg", make_test_common (module U1));
      ("basic_bg_control", make_test_common (module U4));
      ("basic_pipe_bg", make_test_common (module U2.Bg));
      ("basic_pipe_bg_control", make_test_common (module U4));
      ("join_bg_alt", make_test_bg (module U1));
      ("join_pipe_fg", make_test_fg (module U2));
      ("state", make_test_state (module U3));
    ]

(* let test_bg_ints =
   let module U = Unroll.Make_payload_bg (Int) (Int_payload) in
   make_test_list (module U : U_bg_ints) *)
