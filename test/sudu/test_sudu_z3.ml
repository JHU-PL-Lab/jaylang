open Core

let ctx = Z3.mk_context []

let solver = Z3.Solver.mk_solver ctx None

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

module To_test = struct
  open SuduZ3

  (* Test Z3 expression, no model *)
  let box_and_unbox_int i = i |> box_int |> unbox_int

  let box_and_unbox_bool b = b |> box_bool |> unbox_bool

  let box_and_unbox_string s = s |> box_string |> unbox_string

  let inj_and_prj_int i =
    Out_channel.newline stdout;
    let i_inj = int_ i in
    dump i_inj;
    let i_inj_prj = project_int i_inj in
    dump i_inj_prj;
    let i_z3_again = simplify i_inj_prj in
    dump i_z3_again;
    let plain_int = unbox_int i_z3_again in
    print_endline @@ string_of_int plain_int;
    plain_int

  let inj_and_prj_bool b = b |> bool_ |> project_bool |> simplify |> unbox_bool

  let inj_and_prj_string b =
    b |> string_ |> project_string |> simplify |> unbox_string

  (* open Fact *)
  let bool_binop (b1, b2) =
    let e1 = box_bool b1 in
    let e2 = box_bool b2 in
    let e3 = and2 e1 e2 in
    let e4 = simplify e3 in
    unbox_bool e4

  let bool_binop_inj (b1, b2) =
    let e1 = box_bool b1 in
    let e2 = box_bool b2 in
    let e3 = and2 e1 e2 in
    let e4 = inject_bool e3 in
    let e5 = project_bool e4 in
    let e6 = simplify e5 in
    unbox_bool e6
end

let invariant_int i f () = Alcotest.(check int) "same int" i (f i)

let invariant_bool b f () = Alcotest.(check bool) "same bool" b (f b)

let same_bool b1 b2 () = Alcotest.(check bool) "same bool" b1 b2

let invariant_string s f () = Alcotest.(check string) "same bool" s (f s)

let invariant_bools s f =
  List.map [ true; false ] ~f:(fun b ->
      Alcotest.test_case s `Quick (invariant_bool b f))

let test_inj_and_prj_int () =
  Alcotest.(check int) "same int" 1 (To_test.inj_and_prj_int 1)

open Fact

let test_bool_binop s f =
  List.map Bool_fact.and_facts ~f:(fun (bp, br) ->
      Alcotest.test_case s `Quick (same_bool br (f bp)))

let () =
  let open Alcotest in
  run "Sudu"
    [
      ( "box",
        [
          test_case "box invariant on int" `Quick
            (invariant_int 1 To_test.box_and_unbox_int);
          test_case "box invariant on string" `Quick
            (invariant_string "world" To_test.box_and_unbox_string);
        ]
        @ invariant_bools "box invariant on bools" To_test.box_and_unbox_bool );
      ( "projection",
        [
          test_case "projection invariant on int" `Quick
            (invariant_int 1 To_test.inj_and_prj_int);
          test_case "projection invariant on string" `Quick
            (invariant_string "world" To_test.inj_and_prj_string);
        ]
        @ invariant_bools "projection invariant on bools"
            To_test.inj_and_prj_bool );
      ( "binop",
        test_bool_binop "Bool.(&&) on bools" To_test.bool_binop
        @ test_bool_binop "Bool.(&&) on projected bools" To_test.bool_binop_inj
      );
    ]

(*
module Sudu = Sudu.Z3_gadt.Make (struct
  let ctx = Z3.mk_context []
end)

   let gadt_in'n'out () =
   let i_z3 = Sudu.int_ 2 in
   Sudu.dump i_z3;
   Alcotest.(check unit) "pass" () ()
*)
