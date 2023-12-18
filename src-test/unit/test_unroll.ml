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

  let one_msg msg =
    let u = U.create () in
    U.one_shot u 1 [ msg ] ;
    Lwt_main.run @@ get_msgs u 1

  let one_shot msgs =
    let u = U.create () in
    U.one_shot u 2 msgs ;
    Lwt_main.run @@ get_msgs u 2

  let available_ongoing msgs =
    let u = U.create () in
    List.iter msgs ~f:(fun msg -> U.push u 2 (Some msg)) ;
    get_available_msgs u 2

  module To_test = struct
    let one_msg () =
      let msg = 42 in
      Alcotest.(check @@ list int) "equal" [ msg ] (one_msg msg)

    let one_shot () =
      let nums = List.init 10 ~f:Fn.id in
      Alcotest.(check @@ list int) "equal" nums (one_shot nums)

    let available_ongoing () =
      let nums = List.init 10 ~f:Fn.id in
      Alcotest.(check @@ list int) "equal" nums (available_ongoing nums)

    let all_tests =
      [
        Alcotest.test_case "one_msg" `Quick one_msg;
        Alcotest.test_case "one_shot" `Quick one_shot;
        Alcotest.test_case "available_ongoing" `Quick available_ongoing;
        (* Alcotest.test_case "get_all_on_blocked" `Quick get_all_on_blocked; *)
        (* Alcotest.test_case "join_two" `Quick join_two; *)
      ]
  end
end

module U_to_test_bg_key_int (U : U_bg_ints_pipe) = struct
  (* let get_msgs u k = U.get_payload_stream u k |> Lwt_stream.to_list *)

  let get_available_msgs u k =
    U.get_payload_stream u k |> Lwt_stream.get_available

  let join_two msgs1 msgs2 =
    let u = U.create () in
    U.one_shot u 3 msgs1 ;
    U.one_shot u 4 msgs2 ;
    U.join u [ 3; 4 ] 5 ;
    get_available_msgs u 5

  module To_test = struct
    let join_two () =
      let k = 5 in
      let nums1 = List.init k ~f:Fn.id in
      let nums2 = List.init k ~f:(fun x -> x + k) in
      Alcotest.(check @@ list int)
        "equal" (nums1 @ nums2) (join_two nums1 nums2)

    let all_tests = [ Alcotest.test_case "join_two" `Quick join_two ]
  end
end

module U_to_test_fg_key_int (U : U_fg_ints_pipe) = struct
  (* let get_msgs u k = U.get_payload_stream u k |> Lwt_stream.to_list *)

  let get_available_msgs u k =
    U.get_payload_stream u k |> Lwt_stream.get_available

  (* open Lwt.Infix *)

  let join_two msgs1 msgs2 =
    let u = U.create () in
    let all_work =
      let%lwt p3 = U.one_shot u 3 msgs1 in
      let%lwt p4 = U.one_shot u 4 msgs2 in
      let%lwt _p5 = U.join u [ p3; p4 ] 5 in
      (* U.get_payload_stream u 5 |> Lwt_stream.to_list *)
      Lwt.return @@ get_available_msgs u 5
    in
    Lwt_main.run all_work

  module To_test = struct
    let join_two () =
      let k = 5 in
      let nums1 = List.init k ~f:Fn.id in
      let nums2 = List.init k ~f:(fun x -> x + k) in
      Alcotest.(check @@ list int)
        "equal" (nums1 @ nums2) (join_two nums1 nums2)

    let all_tests = [ Alcotest.test_case "join_two" `Quick join_two ]
  end
end

let make_test_common (module U : U_bg_ints) =
  let module UA = U_to_test_bg_ints (U) in
  UA.To_test.all_tests

let make_test_bg (module U : U_bg_ints_pipe) =
  let module UA = U_to_test_bg_key_int (U) in
  UA.To_test.all_tests

let make_test_fg (module U : U_fg_ints_pipe) =
  let module UA = U_to_test_fg_key_int (U) in
  UA.To_test.all_tests

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
