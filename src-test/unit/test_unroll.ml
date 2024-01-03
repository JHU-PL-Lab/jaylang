open Core

(* let history = ref [] in *)
(* U_int.iter u 1 (fun v ->
    history := v :: !history ;
    Lwt.return_unit) *)

module Int_payload = struct
  type payload = int [@@deriving equal]
end

module type U_bg_ints = Unroll.S_bg with type key = int and type payload = int

module type U_bg_ints_pipe =
  Unroll.S_bg with type key = int and type payload = int and type pipe = int

module type U_fg_ints_pipe =
  Unroll.S with type key = int and type payload = int and type 'a act = 'a Lwt.t

module U_to_test_bg_ints (U : U_bg_ints) = struct
  let get_msgs u k = U.get_payload_stream u k |> Lwt_stream.to_list

  let get_available_msgs u k =
    U.get_payload_stream u k |> Lwt_stream.get_available

  let one_msg () =
    let msg = 42 in
    let u = U.create () in
    U.one_shot u 1 [ msg ] ;
    let ans = Lwt_main.run @@ get_msgs u 1 in
    Alcotest.(check @@ list int) "equal" [ msg ] ans

  let one_shot () =
    let nums = List.init 10 ~f:Fn.id in
    let u = U.create () in
    U.one_shot u 2 nums ;
    let ans = Lwt_main.run @@ get_msgs u 2 in
    Alcotest.(check @@ list int) "equal" nums ans

  let available_ongoing () =
    let nums = List.init 10 ~f:Fn.id in
    let u = U.create () in
    List.iter nums ~f:(fun msg -> U.push u 2 (Some msg)) ;
    let ans = get_available_msgs u 2 in
    Alcotest.(check @@ list int) "equal" nums ans

  let all_tests =
    [
      Alcotest.test_case "one_msg" `Quick one_msg;
      Alcotest.test_case "one_shot" `Quick one_shot;
      Alcotest.test_case "available_ongoing" `Quick available_ongoing;
      (* Alcotest.test_case "get_all_on_blocked" `Quick get_all_on_blocked; *)
      (* Alcotest.test_case "join_two" `Quick join_two; *)
    ]
end

module U_to_test_bg_key_int (U : U_bg_ints_pipe) = struct
  (* let get_msgs u k = U.get_payload_stream u k |> Lwt_stream.to_list *)

  let get_available_msgs u k =
    U.get_payload_stream u k |> Lwt_stream.get_available

  (* let get_msgs u k = U.get_payload_stream u k |> Lwt_stream.to_list *)
  let ascending xs = List.sort xs ~compare:Int.compare
  let pair_add (x, y) = x + y
  let pairwire_add xs = List.map xs ~f:pair_add

  let test_join () =
    let k = 5 in
    let nums1 = List.init k ~f:Fn.id in
    let nums2 = List.init k ~f:(fun x -> x + k) in
    let u = U.create () in
    U.one_shot u 3 nums1 ;
    U.one_shot u 4 nums2 ;
    U.join u [ 3; 4 ] 5 ;
    let ans = get_available_msgs u 5 in
    Alcotest.(check @@ list int) "equal" (nums1 @ nums2) ans

  let test_map () =
    let msgs_in = List.init 10 ~f:Fn.id in
    let plus_10 m = m + 10 in
    let msgs_out = List.map msgs_in ~f:plus_10 in
    let u = U.create () in
    U.one_shot u 1 msgs_in ;
    U.map u 1 42 plus_10 ;
    let ans = get_available_msgs u 42 in
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
    let ans = get_available_msgs u 42 in
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

  let _mk_product_stream s1 s2 =
    Lwt_stream.map_list_s
      (fun v2 -> Lwt_stream.(clone s1 |> map (fun v1 -> (v1, v2)) |> to_list))
      (Lwt_stream.clone s2)

  let _mk_product_stream s1 s2 =
    Lwt_stream.map
      (* _list_s *)
        (fun v2 -> Lwt_stream.(clone s1 |> map (fun v1 -> (v1, v2))))
      (Lwt_stream.clone s2)
    |> Lwt_stream.concat

  let product_stream_bg s1 s2 =
    let s, f = Lwt_stream.create () in
    Lwt.async (fun () ->
        Lwt_stream.iter_p
          (fun v2 ->
            Lwt_stream.iter_p
              (fun v1 ->
                f (Some (v1, v2)) ;
                Lwt.return_unit)
              (Lwt_stream.clone s1))
          (Lwt_stream.clone s2)) ;
    s

  let test_product_stream () =
    let msgs1 = List.init 3 ~f:Fn.id in
    let msgs2 = List.init 3 ~f:Fn.id in
    let s1 = Lwt_stream.of_list msgs1 in
    let s2 = Lwt_stream.of_list msgs2 in
    let s12 = product_stream_bg s1 s2 in
    let ans = Lwt_stream.get_available s12 in
    Alcotest.(check @@ list (pair int int))
      "equal"
      [ (0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1); (0, 2); (1, 2); (2, 2) ]
      ans

  let test_product_stream_2 () =
    let _msgs1 = List.init 3 ~f:Fn.id in
    let _msgs2 = List.init 3 ~f:Fn.id in
    let s1, p1 = Lwt_stream.create () in
    let s2, p2 = Lwt_stream.create () in
    let s12 = product_stream_bg s1 s2 in
    Alcotest.(check @@ list (pair int int))
      "equal" []
      (Lwt_stream.get_available s12) ;
    p1 (Some 1) ;
    p1 (Some 2) ;
    (* p1 None ; *)
    p2 (Some 1) ;
    Alcotest.(check @@ list (pair int int))
      "equal"
      [ (1, 1); (2, 1) ]
      (Lwt_stream.get_available s12) ;
    ()

  let test_map2_naive () =
    let msgs1 = List.init 5 ~f:Fn.id in
    let msgs2 = [ 42 ] in
    let msgs_out = List.map msgs1 ~f:(fun x -> x + 42) in
    let u = U.create () in
    U.one_shot u 1 msgs1 ;
    U.one_shot u 2 msgs2 ;
    U.map2 u 1 2 5 (fun (x, y) -> x + y) ;
    let ans = get_available_msgs u 5 in
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
    let ans = get_available_msgs u 5 |> ascending in
    Alcotest.(check @@ list int) "equal" expected ans

  let test_map2_product_random () =
    let msgs = List.init 3 ~f:Fn.id in
    let expected =
      List.cartesian_product msgs msgs |> pairwire_add |> ascending
    in
    let u = U.create () in
    List.iter msgs ~f:(fun n1 ->
        U.push u 1 (Some n1) ;
        U.push u 2 (Some n1)) ;
    U.push u 1 None ;
    U.push u 2 None ;

    U.map2 u 1 2 5 pair_add ;
    Alcotest.(check @@ list int) "s1" msgs (get_available_msgs u 1) ;
    Alcotest.(check @@ list int) "s2" msgs (get_available_msgs u 2) ;
    Alcotest.(check @@ list int) "s12" expected (get_available_msgs u 5) ;
    (* let ans = Lwt_main.run @@ get_msgs u 5 in
       Alcotest.(check @@ list int) "equal" expected ans *)
    ()

  let all_tests =
    [
      Alcotest.test_case "join_two" `Quick test_join;
      Alcotest.test_case "map_one" `Quick test_map;
      Alcotest.test_case "filter_map_one" `Quick test_filter_map;
      Alcotest.test_case "combine" `Quick test_combine;
      Alcotest.test_case "product_stream" `Quick test_product_stream;
      Alcotest.test_case "product_stream_2" `Quick test_product_stream_2;
      Alcotest.test_case "map2_naive" `Quick test_map2_naive;
      Alcotest.test_case "map2_product_seq" `Quick test_map2_product_seq;
      Alcotest.test_case "map2_product_random" `Quick test_map2_product_random;
    ]
end

module U_to_test_fg_key_int (U : U_fg_ints_pipe) = struct
  (* let get_msgs u k = U.get_payload_stream u k |> Lwt_stream.to_list *)

  let get_available_msgs u k =
    U.get_payload_stream u k |> Lwt_stream.get_available

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
      Lwt.return @@ get_available_msgs u 5
    in
    let ans = Lwt_main.run all_work in
    Alcotest.(check @@ list int) "equal" (nums1 @ nums2) ans

  let all_tests = [ Alcotest.test_case "join_two" `Quick test_join ]
end

let make_test_common (module U : U_bg_ints) =
  let module UA = U_to_test_bg_ints (U) in
  UA.all_tests

let make_test_bg (module U : U_bg_ints_pipe) =
  let module UA = U_to_test_bg_key_int (U) in
  UA.all_tests

let make_test_fg (module U : U_fg_ints_pipe) =
  let module UA = U_to_test_fg_key_int (U) in
  UA.all_tests

module U1 = Unroll.Make_payload_bg (Int) (Int_payload)
module U2 = Unroll.Make_pipe_bg (Int) (Int_payload)
module U3 = Unroll.Make_pipe (Int) (Int_payload)

let () =
  Alcotest.run "Interpreter"
    [
      ("basic_bg", make_test_common (module U1));
      ("basic_pipe_bg", make_test_common (module U2));
      ("join_fg", make_test_bg (module U1));
      ("join_pipe_fg", make_test_fg (module U3));
    ]

(* let test_bg_ints =
   let module U = Unroll.Make_payload_bg (Int) (Int_payload) in
   make_test_list (module U : U_bg_ints) *)
