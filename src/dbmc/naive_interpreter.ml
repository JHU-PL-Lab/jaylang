open Core
open Odefa_ast
open Ast

exception Found_target of value

type dvalue = Direct of value | Closure of Ident.t * function_value * denv

and denv = dvalue Ident_map.t

let cond_fid b = if b then Ident "$tt" else Ident "$ff"

let value_of_dvalue = function
  | Direct v -> v
  | Closure (_fid, fv, _env) -> Value_function fv

let rec same_stack s1 s2 =
  match (s1, s2) with
  | (cs1, fid1) :: ss1, (cs2, fid2) :: ss2 ->
      Ident.equal cs1 cs2 && Ident.equal fid1 fid2 && same_stack ss1 ss2
  | [], [] -> true
  | _, _ -> false

let rec eval_exp ~input_feeder ~target stk env e : dvalue =
  let (Expr clauses) = e in
  let _, vs' =
    (* List.fold_left_map (eval_clause ~input_feeder ~target stk) env clauses *)
    List.fold_map ~f:(eval_clause ~input_feeder ~target stk) ~init:env clauses
  in
  List.last_exn vs'

and eval_clause ~input_feeder ~target stk env clause : denv * dvalue =
  let (Clause (Var (x, _), cbody)) = clause in
  let (v : dvalue) =
    match cbody with
    | Value_body (Value_function vf) -> Closure (x, vf, env)
    | Value_body v -> Direct v
    | Var_body vx -> eval_val env vx
    | Conditional_body (x2, e1, e2) ->
        let e, stk' =
          if eval_val_to_bool env x2 then
            (e1, (x, cond_fid true) :: stk)
          else
            (e2, (x, cond_fid false) :: stk)
        in
        eval_exp ~input_feeder ~target stk' env e
    | Input_body ->
        (* TODO: the interpreter may propagate the dummy value (through the value should never be used in any control flow)  *)
        let n = Option.value ~default:0 (input_feeder (x, stk)) in
        Direct (Value_int n)
    | Appl_body (vx1, vx2) -> (
        match eval_val env vx1 with
        | Closure (fid, Function_value (Var (arg, _), body), fenv) ->
            let v2 = eval_val env vx2 in
            let stk2 = (x, fid) :: stk in
            let env2 = Ident_map.add arg v2 fenv in
            eval_exp ~input_feeder ~target stk2 env2 body
        | _ -> failwith "app to a non fun")
    | Match_body (vx, p) -> Direct (Value_bool (check_pattern env vx p))
    | Projection_body (v, key) -> (
        match eval_val env v with
        | Direct (Value_record (Record_value record)) ->
            let vv = Ident_map.find key record in
            eval_val env vv
        | _ -> failwith "project on a non record")
    | Binary_operation_body (vx1, op, vx2) ->
        let v1 = eval_val_to_direct env vx1
        and v2 = eval_val_to_direct env vx2 in
        let v =
          match (op, v1, v2) with
          | Binary_operator_plus, Value_int n1, Value_int n2 ->
              Value_int (n1 + n2)
          | Binary_operator_minus, Value_int n1, Value_int n2 ->
              Value_int (n1 - n2)
          | Binary_operator_times, Value_int n1, Value_int n2 ->
              Value_int (n1 * n2)
          | Binary_operator_divide, Value_int n1, Value_int n2 ->
              Value_int (n1 / n2)
          | Binary_operator_modulus, Value_int n1, Value_int n2 ->
              Value_int (n1 mod n2)
          | Binary_operator_less_than, Value_int n1, Value_int n2 ->
              Value_bool (n1 < n2)
          | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 ->
              Value_bool (n1 <= n2)
          | Binary_operator_equal_to, Value_int n1, Value_int n2 ->
              Value_bool (n1 = n2)
          | Binary_operator_equal_to, Value_bool b1, Value_bool b2 ->
              Value_bool (Bool.( = ) b1 b2)
          | Binary_operator_and, Value_bool b1, Value_bool b2 ->
              Value_bool (b1 && b2)
          | Binary_operator_or, Value_bool b1, Value_bool b2 ->
              Value_bool (b1 || b2)
          | Binary_operator_xor, Value_bool b1, Value_bool b2 ->
              Value_bool (Bool.( <> ) b1 b2)
          | _, _, _ -> failwith "incorrect binop"
        in
        Direct v
  in
  let target_x, target_stk = target in
  if Ident.equal target_x x then
    if same_stack (List.rev target_stk) stk then
      raise (Found_target (value_of_dvalue v))
    else
      (* Fmt.(
         pr "found %a at stack %a, expect %a" pp_ident x
           Dump.(list (pair pp_ident pp_ident))
           target_stk
           Dump.(list (pair pp_ident pp_ident))
           stk) *)
      ()
  else
    ();

  (Ident_map.add x v env, v)

and eval_val env (Var (x, _)) : dvalue = Ident_map.find x env

and eval_val_to_direct env vx : value =
  match eval_val env vx with
  | Direct v -> v
  | _ -> failwith "eval to non direct value"

and eval_val_to_bool env vx : bool =
  match eval_val env vx with
  | Direct (Value_bool b) -> b
  | _ -> failwith "eval to non bool"

and check_pattern env vx pattern : bool =
  let is_pass =
    match (eval_val env vx, pattern) with
    | Direct (Value_int _), Int_pattern -> true
    | Direct (Value_bool b1), Bool_pattern b2 -> Bool.( = ) b1 b2
    | Direct (Value_function _), _ -> failwith "must be a closure"
    | Direct (Value_record (Record_value record)), Record_pattern key_map ->
        Ident_map.for_all
          (fun k pv ->
            match Ident_map.Exceptionless.find k record with
            | Some field -> check_pattern env field pv
            | None -> false)
          key_map
    | Closure (_, _, _), Fun_pattern -> true
    | _, Any_pattern -> true
    | _, _ -> false
  in
  is_pass

let eval ?(input_feeder = Fn.const None) ~target e =
  let empty_stk = [] in
  let empty_env = Ident_map.empty in
  try
    let _ = eval_exp ~input_feeder ~target empty_stk empty_env e in
    failwith "no target val"
  with Found_target v -> v
