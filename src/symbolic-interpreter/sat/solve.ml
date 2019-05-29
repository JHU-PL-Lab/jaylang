open Odefa_ast;;

open Z3;;
open Z3.Arithmetic;;
open Z3.Boolean;;

open Ast;;
open Formula_typer;;
open Interpreter_types;;
open Sat_types;;
open Symbol_cache;;

let add_formula
    (ctx : Z3.context)
    (solver : Z3.Solver.solver)
    (symbol_cache : symbol_cache)
    (symbol_types : symbol_type Symbol_map.t)
    (formula : Formula.t)
  : unit =
  let translate (symbol : Symbol.t) : Expr.expr =
    let symbol_type = Symbol_map.find symbol symbol_types in
    let z3symbol = define_symbol symbol_cache symbol in
    match symbol_type with
    | IntSymbol -> Integer.mk_const ctx z3symbol
    | BoolSymbol -> Boolean.mk_const ctx z3symbol
  in
  try
    let Formula(symbol0, expr) = formula in
    let e0 = translate symbol0 in
    match expr with
    | Formula_expression_value v ->
      let e1o =
        match v with
        | Value_function _ ->
          None
        | Value_int n ->
          Some (Integer.mk_numeral_i ctx n)
        | Value_bool b ->
          Some (Boolean.mk_val ctx b)
      in
      begin
        match e1o with
        | None -> ()
        | Some e1 ->
          let c = mk_eq ctx e0 e1 in
          Solver.add solver [c]
      end
    | Formula_expression_alias symbol1 ->
      let e1 = translate symbol1 in
      let c = mk_eq ctx e0 e1 in
      Solver.add solver [c]
    | Formula_expression_binop (symbol1, op, symbol2) ->
      let e1 = translate symbol1 in
      let e2 = translate symbol2 in
      let mk_op =
        match op with
        | Binary_operator_plus -> mk_add ctx
        | Binary_operator_minus -> failwith "unimplemented"
        | Binary_operator_less_than -> failwith "unimplemented"
        | Binary_operator_less_than_or_equal_to -> failwith "unimplemented"
        | Binary_operator_equal_to -> failwith "unimplemented"
        | Binary_operator_and -> failwith "unimplemented"
        | Binary_operator_or -> failwith "unimplemented"
        | Binary_operator_xor -> failwith "unimplemented"
      in
      let c = mk_eq ctx e0 (mk_op [e1; e2]) in
      Solver.add solver [c]
  with
  | Not_found ->
    (* Someone's type wasn't found in the type map.  This means either that the
       symbol was unconstrained or the type was something the solver can't
       reason about.  Ignore this case. *)
    ()
;;

let add_formulae
    (ctx : Z3.context)
    (solver : Z3.Solver.solver)
    (symbol_cache : symbol_cache)
    (symbol_types : symbol_type Symbol_map.t)
    (formulae : Formula_set.t)
  : unit =
  formulae
  |> Formula_set.iter (add_formula ctx solver symbol_cache symbol_types)
;;

(**
   Determines whether a set of formulae is solvable.
*)
let solve (formulae : Formula_set.t) : bool =
  let ctx = Z3.mk_context [] in
  let solver = Solver.mk_solver ctx None in
  let symbol_cache = new_symbol_cache ctx in
  let symbol_types = infer_types formulae in
  add_formulae ctx solver symbol_cache symbol_types formulae;
  match Solver.check solver [] with
  | Solver.SATISFIABLE ->
    true
  (* (
     match Solver.get_model solver with
     | None -> failwith "failure: impossible none model"
     | Some model ->
      Printf.printf "!!! %s\n"
        (Model.to_string model)
     ) *)
  | Solver.UNSATISFIABLE ->
    false
  | Solver.UNKNOWN ->
    failwith @@ Printf.sprintf "Unknown result in solve.check: %s"
      (Solver.get_reason_unknown solver)
;;
