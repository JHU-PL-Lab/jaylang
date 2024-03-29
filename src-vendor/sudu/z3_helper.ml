open Core
open Z3

module type Context = sig
  val ctx : context
end

module type Jil_z3_datatye = sig
  type t
  type case

  (* sort *)
  val the_sort : Sort.sort
  val mk_int : FuncDecl.func_decl
  val mk_bool : FuncDecl.func_decl
  val mk_fun : FuncDecl.func_decl
  val mk_record : FuncDecl.func_decl
  val is_int : Expr.expr -> Expr.expr
  val is_bool : Expr.expr -> Expr.expr
  val is_fun : Expr.expr -> Expr.expr
  val is_record : Expr.expr -> Expr.expr
  val inject_int : Expr.expr -> Expr.expr
  val inject_bool : Expr.expr -> Expr.expr
  val inject_string : Expr.expr -> Expr.expr
  val inject_record : Expr.expr -> Expr.expr
  val project_int : Expr.expr -> Expr.expr
  val project_bool : Expr.expr -> Expr.expr
  val project_string : Expr.expr -> Expr.expr
  val project_record : Expr.expr -> Expr.expr
  val case_to_recognizer : case -> Expr.expr -> Expr.expr
  val case_to_injector : case -> Expr.expr -> Expr.expr
  val case_to_projecter : case -> Expr.expr -> Expr.expr
end

module Make_helper (C : Context) = struct
  open C

  let set_timeout_sec sec =
    match sec with
    | None -> ()
    | Some sec ->
        let time_s =
          sec |> Time_float.Span.to_sec |> Float.iround_up_exn |> fun t ->
          t * 1000 |> string_of_int
        in
        Params.update_param_value ctx "timeout" time_s

  let reset_solver solver = Solver.reset solver

  (* clone by translating to the same context *)
  let clone_solver solver = Solver.translate solver ctx
  let string_of_solver solver = Solver.to_string solver
  let get_assertion_count solver = List.length (Solver.get_assertions solver)

  let get_rlimit solver =
    let stat = Z3.Solver.get_statistics solver in
    let r = Z3.Statistics.get stat "rlimit count" in
    let rv = Option.value_exn r in
    Z3.Statistics.Entry.get_int rv

  let dump e =
    let es = Expr.to_string e in
    let ss = e |> Expr.get_sort |> Sort.to_string in
    let s = Printf.sprintf "E:%s; Sort:%s" es ss in
    print_endline s

  let dump_model model =
    let open Model in
    Fmt.(
      pr
        ("\nModel: %s\n" ^^ "Const[%n]: @[%a@]\n" ^^ "Decl[%n]: @[%a@]\n"
       ^^ "Sort[%n]: @[%a@]\n")
        (to_string model) (get_num_consts model) (list string ~sep:cut)
        (List.map (get_const_decls model) ~f:FuncDecl.to_string)
        (get_num_funcs model) (list string ~sep:cut)
        (List.map (get_func_decls model) ~f:FuncDecl.to_string)
        (get_num_sorts model) (list string ~sep:cut)
        (List.map (get_sorts model) ~f:Sort.to_string))

  let get_model solver status =
    match status with
    | Solver.SATISFIABLE -> Solver.get_model solver
    | Solver.UNSATISFIABLE -> None
    | Solver.UNKNOWN -> None

  let get_model_exn solver status =
    match status with
    | Solver.SATISFIABLE -> (
        match Solver.get_model solver with
        | None -> failwith "should have model1"
        | Some model -> model)
    | Solver.UNSATISFIABLE -> failwith "should not UNSAT"
    | Solver.UNKNOWN -> failwith (Solver.get_reason_unknown solver)

  let is_unsat status = Poly.equal status Solver.UNSATISFIABLE
  let is_sat status = Poly.equal status Solver.SATISFIABLE
  let eval model e flag = Model.eval model e flag
  let eval_ model e = Model.eval model e false
  let eval_exn model e flag = Option.value_exn (eval model e flag)
  let eval_exn_ model e = Option.value_exn (eval_ model e)
  let simplify e = Expr.simplify e None

  let check_with_assumption solver assumptions =
    (* for _ = 1 to 100 do
         Solver.push solver;
         ignore @@ Solver.check solver assumptions;
         Solver.pop solver 1
       done; *)
    match Solver.check solver assumptions with
    | Solver.SATISFIABLE -> (
        match Solver.get_model solver with
        | None ->
            failwith
              ("check is not invoked before; " ^ "the result is not SAT; "
             ^ " the model production is not enabled")
        | Some model -> Result.Ok model)
    | Solver.UNSATISFIABLE -> Result.Error None
    (* TODO *)
    (* (Solver.get_unsat_core solver) *)
    | Solver.UNKNOWN ->
        failwith
        @@ Printf.sprintf "[check_and_get_model] Unknown result in solve: %s"
             (Solver.get_reason_unknown solver)

  let check ?(verbose = true) solver phis phi_used_once =
    let _ = verbose in
    Solver.add solver phis ;
    check_with_assumption solver phi_used_once
end

module Make_basic_to_z3_basic (C : Context) = struct
  open C

  let mk_bool_s s = Boolean.mk_const_s ctx s
  let string_sort = Seq.mk_string_sort ctx
  let mk_string_s s = Expr.mk_const_s ctx s string_sort
  let add2 e1 e2 = Arithmetic.mk_add ctx [ e1; e2 ]
  let sub2 e1 e2 = Arithmetic.mk_sub ctx [ e1; e2 ]
  let mul2 e1 e2 = Arithmetic.mk_mul ctx [ e1; e2 ]
  let div = Arithmetic.mk_div ctx
  let mod_ = Arithmetic.Integer.mk_mod ctx
  let lt = Arithmetic.mk_lt ctx
  let le = Arithmetic.mk_le ctx
  let and_ = Boolean.mk_and ctx
  let join = and_
  let or_ = Boolean.mk_or ctx
  let not_ = Boolean.mk_not ctx
  let implies = Boolean.mk_implies ctx
  let ( @=> ) = implies
  let and2 e1 e2 = Boolean.mk_and ctx [ e1; e2 ]
  let or2 e1 e2 = Boolean.mk_or ctx [ e1; e2 ]
  let eq = Boolean.mk_eq ctx
  let neq e1 e2 = not_ @@ eq e1 e2
  let ite = Boolean.mk_ite ctx

  (* box to Z3 expression *)
  let box_int i = Arithmetic.Integer.mk_numeral_i ctx i
  let box_bool b = Boolean.mk_val ctx b
  let box_string s = Seq.mk_string ctx s

  let box_bitvector i =
    BitVector.mk_numeral ctx (Int.to_string i)
      63 (* if we want the bits for 0b011, we give argument i = 3 *)

  (* unbox from Z3 expression *)
  let unbox_bool_exn v =
    match Boolean.get_bool_value v with
    | L_TRUE -> true
    | L_FALSE -> false
    | L_UNDEF -> failwith "pass_if_true"

  let unbox_bool v =
    match Boolean.get_bool_value v with
    | L_TRUE -> true
    | L_FALSE -> false
    | L_UNDEF -> false

  let unbox_int e =
    e |> Arithmetic.Integer.get_big_int |> Big_int_Z.int_of_big_int

  let unbox_string e = Seq.get_string ctx e

  let unbox_bitvector e =
    BitVector.numeral_to_string e |> Int.of_string (* allow only 63 bits *)
end
