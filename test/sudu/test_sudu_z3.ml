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

  let binop_inj op i1 i2 =
    let x = var_s "x" in
    let e1 = int_ i1 in
    let e2 = int_ i2 in
    let phi = op x e1 e2 in
    let model = get_model_exn solver @@ Z3.Solver.check solver [ phi ] in
    let x' = project_int x in
    let v = eval_exn model x' false in
    unbox_int v

  let test_lt_gt i =
    let x = var_s "x" in
    let e1, e2 = (int_ (i - 1), int_ (i + 1)) in
    let b1, b2 = (var_s "b1", var_s "b2") in
    let phi1, phi2 = (fn_lt b1 e1 x, fn_lt b2 x e2) in
    let model =
      get_model_exn solver
      @@ Z3.Solver.check solver [ phi1; phi2; project_bool b1; project_bool b2 ]
    in
    dump_model model;
    let x' = project_int x in
    let v = eval_exn model x' false in
    unbox_int v

  let test_lt_gt_no_binding i =
    let x = var_s "x" in
    let e1, e2 = (int_ (i - 1), int_ (i + 1)) in
    let phi1 = Z3.Arithmetic.mk_lt ctx (project_int e1) (project_int x) in
    let phi2 = Z3.Arithmetic.mk_lt ctx (project_int x) (project_int e2) in
    let model = get_model_exn solver @@ Z3.Solver.check solver [ phi1; phi2 ] in
    let x' = project_int x in
    let v = eval_exn model x' false in
    unbox_int v

  let test_lt_gt_no_binding_no_inj_int i =
    let x = var_s "x" in
    let e1, e2 = (box_int (i - 1), box_int (i + 1)) in
    let phi1 = Z3.Arithmetic.mk_lt ctx e1 (project_int x) in
    let phi2 = Z3.Arithmetic.mk_lt ctx (project_int x) e2 in
    let model = get_model_exn solver @@ Z3.Solver.check solver [ phi1; phi2 ] in
    let x' = project_int x in
    let v = eval_exn model x' false in
    unbox_int v

  let test_lt_gt_no_inj_int i =
    let x = var_s "x" in
    let e1, e2 = (box_int (i - 1), box_int (i + 1)) in
    let phi1 = Z3.Arithmetic.mk_lt ctx e1 (project_int x) in
    let phi2 = Z3.Arithmetic.mk_lt ctx (project_int x) e2 in
    let b1, b2 = (var_s "b1", var_s "b2") in
    let bp1, bp2 = (project_bool b1, project_bool b2) in
    let model =
      get_model_exn solver @@ Z3.Solver.check solver [ phi1; phi2; bp1; bp2 ]
    in
    let x' = project_int x in
    let v = eval_exn model x' false in
    unbox_int v

  let incremental_plus i1 i2 i3 =
    let x = var_s "x" in
    let t = var_s "t" in
    let e1, e2, e3 = (int_ i1, int_ i2, int_ i3) in
    let phi_e1_e2 = fn_plus t e1 e2 in
    Z3.Solver.add solver [ phi_e1_e2 ];
    let phi_t_e3 = fn_plus x t e3 in
    Z3.Solver.add solver [ phi_t_e3 ];
    let model = get_model_exn solver @@ Z3.Solver.check solver [] in
    let x' = project_int x in
    let v = eval_exn model x' false in
    unbox_int v

  let add_int i =
    let x = Z3.Arithmetic.Integer.mk_const_s ctx "x" in
    let e1 = box_int i in
    let phi = eq x e1 in
    Z3.Solver.add solver [ phi ];
    let _status = Z3.Solver.check solver [] in
    Z3.Solver.reset solver;
    1

  let add_bool b =
    let x = Z3.Boolean.mk_const_s ctx "x" in
    let e1 = box_bool b in
    let phi = eq x e1 in
    Z3.Solver.add solver [ phi ];
    let _status = Z3.Solver.check solver [] in
    Z3.Solver.reset solver;
    true

  let reset_once _i1 _i2 _i3 _i4 =
    (* let _ = incremental_plus i1 i2 i3 in
       Z3.Solver.reset solver; *)
    Z3.Solver.reset solver;
    let r = test_lt_gt_no_inj_int 6 in
    (* let r = incremental_plus i2 i3 i4 in *)
    Z3.Solver.reset solver;
    r
end

let invariant_int i f () = Alcotest.(check int) "same int" i (f i)

let same_int_f i f () = Alcotest.(check int) "same int" i (f ())

let diff_int_f i f () = Alcotest.(check (neg int)) "same int" i (f ())

let invariant_bool b f () = Alcotest.(check bool) "same bool" b (f b)

let same_bool b1 b2 () = Alcotest.(check bool) "same bool" b1 b2

let same_bool_f b f () = Alcotest.(check bool) "same bool" b (f ())

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
      ( "solve int",
        [
          test_case "solver invariant on int" `Slow
            (invariant_int 1 To_test.put_get_eval_int);
          test_case "solver invariant on int, const" `Slow
            (invariant_int 1 To_test.put_get_const_interp_int);
          test_case "not unique model" `Slow
            (diff_int_f 1 (fun () -> To_test.not_unique_model_assumption 1));
        ] );
      ( "solve int plus",
        [
          test_case "1+2=3" `Slow
            (same_int_f 3 (fun () -> To_test.binop_inj SuduZ3.fn_plus 1 2));
          test_case "6-4=2" `Slow
            (same_int_f 2 (fun () -> To_test.binop_inj SuduZ3.fn_minus 6 4));
          test_case "3*4=12" `Slow
            (same_int_f 12 (fun () -> To_test.binop_inj SuduZ3.fn_times 3 4));
          test_case "100/20=5" `Slow
            (same_int_f 5 (fun () -> To_test.binop_inj SuduZ3.fn_divide 100 20));
          test_case "25%3=1" `Slow
            (same_int_f 1 (fun () -> To_test.binop_inj SuduZ3.fn_modulus 25 3));
          (* test_case "1+2+3=6" `Slow
             (same_int_f 6 (fun () -> To_test.incremental_plus 1 2 3)); *)
          (* test_case "reset;2+3+4=9" `Slow
             (same_int_f 6 (fun () -> To_test.reset_once 1 1 2 3)); *)
        ] );
      ( "solve int relation",
        [
          test_case "5 < x < 7 (int inj, phi with var)" `Slow
            (same_int_f 6 (fun () -> To_test.test_lt_gt 6));
          test_case "5 < x < 7 (int inj, phi noname)" `Slow
            (same_int_f 6 (fun () -> To_test.test_lt_gt_no_binding 6));
          test_case "5 < x < 7 (int literal, phi with var)" `Slow
            (same_int_f 6 (fun () -> To_test.test_lt_gt_no_inj_int 6));
          test_case "5 < x < 7 (int literal, phi noname)" `Slow
            (same_int_f 6 (fun () -> To_test.test_lt_gt_no_binding_no_inj_int 6));
        ] );
      ( "debug-mac",
        [
          test_case "true => true" `Slow
            (same_bool_f true (fun () -> To_test.add_bool true));
          test_case "1 => 1" `Slow (same_int_f 1 (fun () -> To_test.add_int 1));
        ] );
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
