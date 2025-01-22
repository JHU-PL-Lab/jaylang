
open Core
open Lang
open Ast
open Expr

open Value

type symbolic_session = [ `Symbolic_session ] (* TODO *)
let session : symbolic_session = `Symbolic_session
let get_input_feeder : symbolic_session -> Input_feeder.t = fun _ -> Input_feeder.default (* TODO *)

module C_result = struct
  type r = { v : Value.t ; step : int ; symb_session : symbolic_session } (* TODO: use this in eval *)
end

module CPS_Result_M = struct
  module Err = struct
    type t = Abort | Diverge | Type_mismatch | Reach_max_step
  end

  module C = Monadlib.Continuation.Make (struct type r = Value.t end)

  module T = struct
    type 'a m = ('a, Err.t * symbolic_session) result C.m

    let[@inline_always] bind (x : 'a m) (f : 'a -> 'b m) : 'b m = 
      C.bind x (function
        | Ok r -> f r
        | Error e -> C.return (Error e)
      )

    let[@inline_always] return (a : 'a) : 'a m =
      C.return
      @@ Result.return a
  end

  (* include Monadlib.Monad.Make (T) *) (* will replace the line below with this when I need any helpers *)
  include T

  let fail (e : Err.t * symbolic_session) : 'a m =
    C.return
    @@ Result.fail e

  (* TODO: with actual session module, we would call the appropriate functions *)
  let abort (symb_session : symbolic_session) : 'a m =
    fail (Err.Abort, symb_session)

  let diverge (symb_session : symbolic_session) : 'a m =
    fail (Err.Diverge, symb_session)

  let type_mismatch (symb_session : symbolic_session) : 'a m =
    fail (Err.Type_mismatch, symb_session)

  let reach_max_step (symb_session : symbolic_session) : 'a m =
    fail (Err.Reach_max_step, symb_session)
end

let eval_exp (expr : Embedded.t) : Value.t =
  let open CPS_Result_M in
  (* TODO: step count and symbolic session. Note that session needs to be passed back up, so return type changes *)
  let rec eval (expr : Embedded.t) (env : Env.t) : Value.t m =
    match expr with
    (* Ints and bools -- constant expressions *)
    | EInt i -> 
      return @@ VInt (i, Expression.const_int i)
    | EBool b -> 
      return @@ VBool (b, Expression.const_bool b)
    (* Simple -- no different than interpreter *)
    | EVar id -> 
      return @@ Env.fetch env id
    | EFunction { param ; body } ->
      return @@ VFunClosure { param ; body = { expr = body ; env } }
    | EId -> 
      return VId
    | EFreeze e_freeze_body -> 
      return @@ VFrozen { expr = e_freeze_body ; env }
    | EVariant { label ; payload = e_payload } -> 
      let%bind payload = eval e_payload env in
      return @@ VVariant { label ; payload }
    | EProject { record = e_record ; label } -> begin
      let%bind v = eval e_record env in
      match v with
      | VRecord record_body -> begin
        match Map.find record_body label with
        | Some v -> return v
        | None -> type_mismatch session
      end
      | _ -> type_mismatch session
    end
    | EThaw e_frozen -> begin
      let%bind v = eval e_frozen env in
      match v with
      | VFrozen { expr ; env } -> eval expr env
      | _ -> type_mismatch session
    end
    | ERecord record_body ->
      let%bind value_record_body = 
        Map.fold record_body ~init:(return RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
          let%bind acc = acc_m in
          let%bind v = eval e env in
          return @@ Map.set acc ~key ~data:v
        )
      in
      return @@ VRecord value_record_body
    | EIgnore { ignored ; cont } ->
      let%bind _ = eval ignored env in (* TODO: make sure to not ignore the returned session when we have that done *)
      eval cont env
    | EMatch { subject ; patterns } -> begin (* Note: there cannot be symbolic branching on match *)
      let%bind v = eval subject env in
      match
        (* find the matching pattern and add to env any values capture by the pattern *)
        List.find_map patterns ~f:(fun (pat, body) ->
          match pat, v with
          | PAny, _ -> Some (body, env)
          | PVariable id, _ -> Some (body, Env.add env id v)
          | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
              when VariantLabel.equal variant_label label ->
            Some (body, Env.add env payload_id payload)
          | _ -> None
          )
      with
      | Some (e, env) -> eval e env
      | None -> type_mismatch session
    end
    | ELet { var ; body ; cont } ->
      let%bind v = eval body env in
      eval cont (Env.add env var v)
    | EAppl { func ; arg } -> begin
      let%bind vfunc = eval func env in
      let%bind varg = eval arg env in
      match vfunc with
      | VId -> return varg
      | VFunClosure { param ; body } ->
        eval body.expr (Env.add body.env param varg)
      | _ -> type_mismatch session
    end
    (* Operations -- build new expressions *)
    | EBinop { left ; binop ; right } -> begin
      let%bind vleft = eval left env in
      let%bind vright = eval right env in
      let k f e1 e2 op =
        return (f (Expression.op e1 e2 op)) (* TODO: use symbolic session op to shortcut this if not needed (when depth is exceeded) *)
      in
      let open Expression.Typed_binop in
      let v_int n = fun e -> VInt (n, e) in
      let v_bool b = fun e -> VBool (b, e) in
      match binop, vleft, vright with
      | BPlus        , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 + n2)) e1 e2 Plus
      | BMinus       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 - n2)) e1 e2 Minus
      | BTimes       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 * n2)) e1 e2 Times
      | BDivide      , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 / n2)) e1 e2 Divide
      | BModulus     , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 mod n2)) e1 e2 Modulus
      | BEqual       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 = n2)) e1 e2 Equal_int
      | BEqual       , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool Bool.(b1 = b2)) e1 e2 Equal_bool
      | BNeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <> n2)) e1 e2 Not_equal
      | BLessThan    , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 < n2)) e1 e2 Less_than
      | BLeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <= n2)) e1 e2 Less_than_eq
      | BGreaterThan , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 > n2)) e1 e2 Greater_than
      | BGeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 >= n2)) e1 e2 Greater_than_eq
      | BAnd         , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool (b1 && b2)) e1 e2 And
      | BOr          , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool (b1 || b2)) e1 e2 And
      | _ -> type_mismatch session (* includes mod or divide by 0 *)
    end
    | ENot e_not_body -> begin
      let%bind v = eval e_not_body env in
      match v with
      | VBool (b, e_b) ->
        return @@ VBool (not b, Expression.not_ e_b) 
      | _ -> type_mismatch session
    end
    (* Branching *)
    | EIf _
    | ECase _ -> failwith "unimplemented"
    (* Inputs *)
    | EPick_i ->
      let key = Concolic_key.create 0 (* TODO: use real step count *) in
      return @@ VInt (get_input_feeder session key, Expression.int_key key)
    | EPick_b
    (* Failure cases *)
    | EAbort
    | EDiverge -> failwith "unimplemented"

  in
  eval expr Value.Env.empty (function
    | Ok r -> r
    | Error _ -> failwith "todo"
    )

