open Core

let ctx = Z3.mk_context []

let solver = Z3.Solver.mk_solver ctx None

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

module To_test = struct
  open SuduZ3

  let solver = Z3.Solver.mk_solver SuduZ3.ctx None

  let reset () = Z3.Solver.reset solver

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
  let binop_bool (b1, b2) =
    let e1 = box_bool b1 in
    let e2 = box_bool b2 in
    let e3 = and2 e1 e2 in
    let e4 = simplify e3 in
    unbox_bool e4

  let binop_bool_inj (b1, b2) =
    let e1 = box_bool b1 in
    let e2 = box_bool b2 in
    let e3 = and2 e1 e2 in
    let e4 = inject_bool e3 in
    let e5 = project_bool e4 in
    let e6 = simplify e5 in
    unbox_bool e6

  let put_get_eval_int i =
    let x = var_s "x" in
    let e1 = int_ i in
    let eq_x_e1 = eq x e1 in
    let status = Z3.Solver.check solver [ eq_x_e1 ] in
    let model = get_model_exn solver status in
    (* dump_model model; *)
    let x' = eval_exn model (project_int x) false in
    let i' = x' |> unbox_int in
    i'

  let put_get_const_interp_int i =
    let x = var_s "x" in
    let e1 = int_ i in
    let model = get_model_exn solver @@ Z3.Solver.check solver [ eq x e1 ] in
    (* dump_model model; *)
    let v = Option.value_exn (Z3.Model.get_const_interp_e model x) in
    v |> project_int |> simplify |> unbox_int

  let not_unique_model_assumption i =
    let x = var_s "x" in
    let e1 = int_ i in
    let model =
      get_model_exn solver @@ Z3.Solver.check solver [ not_ @@ eq x e1 ]
    in
    (* dump_model model; *)
    let v = Option.value_exn (Z3.Model.get_const_interp_e model x) in
    v |> project_int |> simplify |> unbox_int

  let plus i1 i2 =
    let x = var_s "x" in
    let e1 = int_ i1 in
    let e2 = int_ i2 in
    let phi = fn_plus x e1 e2 in
    let model = get_model_exn solver @@ Z3.Solver.check solver [ phi ] in
    let x' = project_int x in
    let v = eval_exn model x' false in
    unbox_int v
end

let invariant_int i f () = Alcotest.(check int) "same int" i (f i)

let same_int_f i f () = Alcotest.(check int) "same int" i (f ())

let invariant_bool b f () = Alcotest.(check bool) "same bool" b (f b)

let same_bool b1 b2 () = Alcotest.(check bool) "same bool" b1 b2

let invariant_string s f () = Alcotest.(check string) "same bool" s (f s)

let invariant_bools s f =
  List.map [ true; false ] ~f:(fun b ->
      Alcotest.test_case s `Quick (invariant_bool b f))

let test_inj_and_prj_int () =
  Alcotest.(check int) "same int" 1 (To_test.inj_and_prj_int 1)

open Fact

let test_binop_bool s f =
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
      ( "binop z3",
        test_binop_bool "Bool.(&&) on bools" To_test.binop_bool
        @ test_binop_bool "Bool.(&&) on projected bools" To_test.binop_bool_inj
      );
      ( "solver",
        [
          test_case "solver invariant on int" `Slow
            (invariant_int 1 To_test.put_get_eval_int);
          test_case "solver invariant on int, const" `Slow
            (invariant_int 1 To_test.put_get_const_interp_int);
          test_case "not unique model" `Slow
            (same_int_f 2 (fun () -> To_test.not_unique_model_assumption 1));
        ] );
      ( "binop+solver",
        [ test_case "1+2=3" `Slow (same_int_f 3 (fun () -> To_test.plus 1 2)) ]
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
