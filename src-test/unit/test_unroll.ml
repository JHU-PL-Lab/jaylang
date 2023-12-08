open Core
(* open Lwt.Infix *)

module To_test = struct
  (* module Int_message = struct
       type message = int [@@deriving equal]
       type payload = int
     end

     module U0 = Unroll.Make (Int) (Int_message)
     module U = U0.No_wait *)

  module Int_message = struct
    type payload = int [@@deriving equal]
  end

  module U = Unroll.Make_just_payload_no_wait (Int) (Int_message)

  let one_msg msg =
    let u = U.create () in
    U.one_shot u 1 msg ;
    let all_work = U.get_stream u 1 |> Lwt_stream.to_list in
    Lwt_main.run all_work

  let one_to_one msgs =
    let u = U.create () in
    U.push_all u 2 msgs ;
    U.just_push u 2 None ;
    let all_work = U.get_stream u 2 |> Lwt_stream.to_list in
    Lwt_main.run all_work

  let one_to_one_nonstop msgs =
    let u = U.create () in
    U.push_all u 2 msgs ;
    U.get_stream u 2 |> Lwt_stream.get_available
  (* Lwt_main.run all_work *)

  let join_two msgs1 msgs2 =
    let u = U.create () in
    U.push_all u 3 msgs1 ;
    U.push_all u 4 msgs2 ;
    U.join u [ 3; 4 ] 5 ;
    U.get_stream u 5 |> Lwt_stream.get_available
end

(* let history = ref [] in *)
(* U_int.iter u 1 (fun v ->
    history := v :: !history ;
    Lwt.return_unit) *)

let simple () =
  let msg = 42 in
  Alcotest.(check @@ list int) "equal" [ msg ] (To_test.one_msg msg)

let one_to_one () =
  let nums = List.init 10 ~f:Fn.id in
  Alcotest.(check @@ list int) "equal" nums (To_test.one_to_one nums)

let one_to_one_nonstop () =
  let nums = List.init 10 ~f:Fn.id in
  Alcotest.(check @@ list int) "equal" nums (To_test.one_to_one_nonstop nums)

let join_two () =
  let k = 5 in
  let nums1 = List.init k ~f:Fn.id in
  let nums2 = List.init k ~f:(fun x -> x + k) in
  Alcotest.(check @@ list int)
    "equal" (nums1 @ nums2)
    (To_test.join_two nums1 nums2)

let () =
  Alcotest.run "Interpreter"
    [
      ( "basic",
        [
          Alcotest.test_case "one" `Quick simple;
          Alcotest.test_case "one_to_one" `Quick one_to_one;
          Alcotest.test_case "one_to_one_nonstop" `Quick one_to_one_nonstop;
          Alcotest.test_case "join_two" `Quick join_two;
        ] );
    ]
