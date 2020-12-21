open Batteries
open Odefa_ast
open Ast

type stack_frame = Ident.t * Ident.t

type env_value = value * Ident.t

let rec eval_exp ~input_feeder stk env e : value = 
  let Expr(clauses) = e in
  let _, vs' = List.fold_left_map (eval_clause ~input_feeder stk) env clauses in
  List.last vs'

and eval_clause ~input_feeder stk env clause : env_value Ident_map.t * value =
  let Clause(Var(x, _), cbody) = clause in
  let v = match cbody with
    | Value_body vb -> vb
    | Var_body (vx) -> eval_val env vx
    | Conditional_body (x2, e1, e2) ->
      let e = if eval_val_to_bool env x2 then e1 else e2 in
      eval_exp ~input_feeder stk env e
    | Input_body ->
      let n = input_feeder (x,stk) in
      Value_int n
    | Appl_body (vx1, vx2) -> (
        let v1, xf = eval_val_with_def env vx1
        and v2 = eval_val env vx2 in
        match v1 with
        | Value_function (Function_value (Var (arg,_), body)) -> 
          let stk2 = (x,xf) :: stk in
          let env2 = Ident_map.add arg (v2,x) env in
          eval_exp ~input_feeder stk2 env2 body
        | _ -> failwith "app to a non fun"
      )
    | Match_body (vx, p) -> 
      Value_bool (check_pattern env vx p)
    | Projection_body (v, key) -> (
        match eval_val env v with
        | Value_record (Record_value record) ->
          let vv = Ident_map.find key record in
          eval_val env vv
        | _ -> failwith "project on a non record"
      )
    | Binary_operation_body (vx1, op, vx2) -> (
        let v1 = eval_val env vx1
        and v2 = eval_val env vx2 in
        match op, v1, v2 with
        | Binary_operator_plus, Value_int(n1), Value_int(n2) ->  Value_int(n1+n2)
        | Binary_operator_minus, Value_int(n1), Value_int(n2) ->  Value_int(n1-n2)
        | Binary_operator_times, Value_int(n1), Value_int(n2) ->  Value_int(n1*n2)
        | Binary_operator_divide, Value_int(n1), Value_int(n2) ->  Value_int(n1/n2)
        | Binary_operator_modulus, Value_int(n1), Value_int(n2) ->  Value_int(n1 mod n2)
        | Binary_operator_less_than, Value_int(n1), Value_int(n2) ->  Value_bool(n1<n2)
        | Binary_operator_less_than_or_equal_to, Value_int(n1), Value_int(n2) -> Value_bool(n1<=n2)
        | Binary_operator_equal_to, Value_int(n1), Value_int(n2) -> Value_bool(n1=n2)

        | Binary_operator_equal_to, Value_bool(b1), Value_bool(b2) -> Value_bool(b1=b2)
        | Binary_operator_and, Value_bool(b1), Value_bool(b2) -> Value_bool(b1&&b2)
        | Binary_operator_or, Value_bool(b1), Value_bool(b2) -> Value_bool(b1||b2)
        | Binary_operator_xor, Value_bool(b1), Value_bool(b2) -> Value_bool(b1<>b2)
        | _,_,_ -> failwith "incorrect binop"
      )
  in
  (Ident_map.add x (v,x) env), v

and eval_val env (Var(x, _)) : value =
  let v, _ = Ident_map.find x env in
  v

and eval_val_with_def env (Var(x, _)) : value * Ident.t =
  Ident_map.find x env

and eval_val_to_bool env vx : bool = 
  match eval_val env vx with
  | Value_bool b -> b
  | _ -> failwith "eval to non bool"

and check_pattern env vx pattern : bool =
  let is_pass = 
    match (eval_val env vx), pattern with
    | Value_int _, Int_pattern
    | Value_function _, Fun_pattern 
    | _, Any_pattern
      -> true
    | Value_bool b1, Bool_pattern b2 ->
      b1 = b2
    | Value_record (Record_value record), Record_pattern key_map ->
      Ident_map.for_all (fun k pv -> 
          match Ident_map.Exceptionless.find k record with
          | Some field -> check_pattern env field pv
          | None -> false
        ) key_map
    | _, _ -> false
  in
  is_pass

;;
let eval ?(input_feeder = Input_feeder.dummy0) e = 
  let empty_stk = [] in
  let empty_env = Ident_map.empty in
  eval_exp ~input_feeder empty_stk empty_env e