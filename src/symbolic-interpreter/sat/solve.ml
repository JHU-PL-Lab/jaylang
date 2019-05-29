(* for testing when developping
   it will be moved to a separate test target for symbolic interpreter
*)

open Z3
open Z3.Arithmetic
open Z3.Boolean
open Sat_types
open Odefa_ast
open Ast
open Interpreter_types
open Relative_stack

module Phi_set = struct
  let eq_int id v = 
    let stk = Relative_stack([], [])
    in let sid = Symbol(Ident(id), stk)
    in (Formula(sid, Formula_expression_value(Value_int(v))))

  let eq_symbol id1 id2 =
    let stk = Relative_stack([], [])
    in let sid1 = Symbol(Ident(id1), stk)
    and sid2 = Symbol(Ident(id2), stk)
    in (Formula(sid1, Formula_expression_alias(sid2)))

  let s1 = Formula_set.empty
           |> Formula_set.add @@ eq_int "a" 4

  let s2 = s1
           |> Formula_set.add @@ eq_int "b" 5

  let s3 = s2
           |> Formula_set.add @@ eq_symbol "a" "b"
end

exception Program_type_error of string

let string_of_symbol (s : symbol) =
  let Symbol(Ident id, _) = s in id

type binop_type = Binop_int | Binop_bool

let get_binop_type = function
  | Binary_operator_plus | Binary_operator_minus
  | Binary_operator_less_than
  | Binary_operator_less_than_or_equal_to 
  | Binary_operator_equal_to 
    -> Binop_int
  | Binary_operator_and | Binary_operator_or | Binary_operator_xor 
    -> Binop_bool

let add_to_z3 ctx solver (phi : Formula_set.t) (symbol_map :  (Expr.expr Symbol_map.t) ref) : unit =
  let rec add_to_z3_rec phi symbol_map = 
    if Formula_set.is_empty phi
    then ()
    else begin
      let later_phi = ref Formula_set.empty in
      (* condition on a formula,
         when x = 1, we know the type of x
          add this formula to z3,
          and add the type of x to symbol_map
         when x = y, x = y bop z
          add this formula when the types of all symbols are define
          postpone this formula when either of the symbols are not defined yet
      *)
      let add_now_or_later (formula : formula) = 
        let Formula(fs, fexp) = formula
        in
        match fexp with
        | Formula_expression_value (Value_int i) -> 
          let Symbol(id, _) = fs
          in let zs = Integer.mk_const_s ctx @@ string_of_symbol fs
          in symbol_map := Symbol_map.add id zs !symbol_map;
          let ze = mk_eq ctx zs (Integer.mk_numeral_i ctx i)
          in Solver.add solver [ze]
        | Formula_expression_value (Value_bool b) -> 
          let Symbol(id, _) = fs
          in let zs = Boolean.mk_const_s ctx @@ string_of_symbol fs
          in symbol_map := Symbol_map.add id zs !symbol_map;
          let ze = mk_eq ctx zs (Boolean.mk_val ctx b)
          in Solver.add solver [ze]
        | Formula_expression_value (Value_function _) -> () 
        | Formula_expression_alias s2 -> (
            try
              let Symbol(id2, _) = s2
              in let zs2 = Symbol_map.find id2 !symbol_map
              in let zs1 =  
                   if is_int zs2
                   then Integer.mk_const_s ctx @@ string_of_symbol s2
                   else Boolean.mk_const_s ctx @@ string_of_symbol s2
              in let ze = mk_eq ctx zs1 zs2
              in Solver.add solver [ze]
            with Not_found ->
              later_phi := Formula_set.add formula !later_phi
          )
        | Formula_expression_binop (s1, op, s2) -> 
          let zs1, zs2 =
            match get_binop_type op with
            | Binop_bool ->
              Boolean.mk_const_s ctx @@ string_of_symbol s1,
              Boolean.mk_const_s ctx @@ string_of_symbol s2
            | Binop_int ->
              Integer.mk_const_s ctx @@ string_of_symbol s1,
              Integer.mk_const_s ctx @@ string_of_symbol s2
          in let e_right = match op with
              | Binary_operator_plus -> mk_add ctx [zs1; zs2]
              | _ -> failwith "for commit"
          in Solver.add solver [e_right]
      in Formula_set.iter add_now_or_later phi;
      add_to_z3_rec !later_phi symbol_map
    end
  in 
  add_to_z3_rec phi symbol_map
;;

let solve (phi : Formula_set.t) = 
  let _ = Formula_set.show phi
  in let ctx = Z3.mk_context []
  in let solver = Solver.mk_solver ctx None
  in let _ = add_to_z3 ctx solver phi (ref Symbol_map.empty)
  in match Solver.check solver [] with
  | Solver.SATISFIABLE -> (
      match Solver.get_model solver with
      | None -> failwith "failure: impossible none model"
      | Some model ->
        Printf.printf "!!! %s\n"
          (Model.to_string model))
  | Solver.UNSATISFIABLE -> (
      failwith "failure: unsat in solve.check"
    )
  | Solver.UNKNOWN -> (
      print_endline (Solver.get_reason_unknown solver);
      failwith "failure: unknown in solve.check"
    )
let main () = solve Phi_set.s1