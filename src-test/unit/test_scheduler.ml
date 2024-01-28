open Core
open Dj_common
open Lwt.Infix

module To_test = struct
  let add_all_then_run nums =
    let q = Scheduler.create ~cmp:Int.compare () in
    let history = ref [] in
    List.iter nums ~f:(fun x ->
        Scheduler.push q x (fun () ->
            history := x :: !history ;
            Lwt.return_unit)) ;
    let q_main = Scheduler.run q in
    ignore @@ Lwt_main.run q_main ;
    List.rev !history

  let add_in_task k =
    let q = Scheduler.create ~cmp:Int.compare () in
    let history = ref [] in
    let rec mk_task x =
      Scheduler.push q x (fun () ->
          history := x :: !history ;
          if x < k then mk_task (x + 1) ;
          Lwt.return_unit)
    in
    mk_task 0 ;
    let q_main = Scheduler.run q in
    ignore @@ Lwt_main.run q_main ;
    List.rev !history

  let add_in_task_exit_half k =
    let s = Scheduler.create ~cmp:Int.compare () in
    let history = ref [] in
    let counter = ref 0 in
    let rec mk_task x =
      Scheduler.push s x (fun () ->
          prerr_endline ("->" ^ string_of_int x) ;
          Int.incr counter ;
          (if x > k then Lwt.pause () else Lwt.return_unit) >>= fun () ->
          history := x :: !history ;
          prerr_endline ("<-" ^ string_of_int x) ;
          if x < k
          then (
            mk_task (x + 1) ;
            mk_task (x + k + 2) ;
            Lwt.return_unit)
          else if x = k
          then (
            Scheduler.set_complete s ;
            Lwt.fail Exit)
          else Lwt.return_unit)
    in
    mk_task 0 ;
    let q_main =
      (Lwt.async_exception_hook := function _ -> prerr_endline "async exn") ;
      Scheduler.run s
    in

    ignore @@ Lwt_main.run q_main ;
    prerr_endline (string_of_int !counter) ;
    List.rev !history
end

let add_all_then_run () =
  let nums = List.init 10000 ~f:Fn.id in
  Alcotest.(check @@ list int) "equal" nums (To_test.add_all_then_run nums)

let add_all_rev_then_run () =
  let nums = List.init 10000 ~f:Fn.id in
  Alcotest.(check @@ list int)
    "equal" nums
    (To_test.add_all_then_run @@ List.rev nums)

let add_in_task () =
  let n = 10000 in
  let nums = List.init n ~f:Fn.id in
  Alcotest.(check @@ list int) "equal" nums (To_test.add_in_task (n - 1))

let add_in_task_exit_half () =
  let n = 10 in
  let nums = List.init n ~f:Fn.id in
  Alcotest.(check @@ list int)
    "equal" nums
    (To_test.add_in_task_exit_half (n - 1))

let () =
  Alcotest.run "Interpreter"
    [
      ( "test_order",
        [
          Alcotest.test_case "ascending" `Quick add_all_then_run;
          Alcotest.test_case "descending" `Quick add_all_rev_then_run;
        ] );
      ( "in-place",
        [
          Alcotest.test_case "ascending" `Quick add_in_task;
          Alcotest.test_case "ascending" `Quick add_in_task_exit_half;
        ] );
    ]
