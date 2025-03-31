(**
  Module [Interp].

  This module interpretes any language defined in this system.
  It uses GADTs and type constraints to define the interpreter
  for all languages in one function.

  Note that all input clauses get the default `0` or `false`.
  There is currently not support for custom inputs.
*)

open Core
open Ast
open Expr

exception InvariantFailure of string

module V = Value.Make (Value.Map_store) (Lazy) (Utils.Identity)
open V

(* 
  Notes:
    * With CPS is definitely much faster on very long computations
    * However, on somewhat small computations on the order of ms and tens of ms, non-CPS is faster by the same order
      * With [@inline always], most of that performance gap goes away, so CPS is generally a fine choice
 *)
module CPS_Error_M (Env : T) = struct
  module Err = struct
    type t = Abort | Diverge | Type_mismatch | Unbound_variable of Ident.t
    (* we might consider adding an Assert_false and Assume_false construct *)
  end

  (* No state. Reader on Env. Err is error type *)
  include Utils.State_read_result.Make (Unit) (Env) (Err)

  (* unit is needed to surmount the value restriction *)
  let abort (type a) (() : unit) : a m =
    fail Err.Abort

  let diverge (type a) (() : unit) : a m =
    fail Err.Diverge

  let type_mismatch (type a) (() : unit) : a m =
    fail Err.Type_mismatch

  let unbound_variable (type a) (id : Ident.t) : a m =
    fail @@ Err.Unbound_variable id

  let list_map (f : 'a -> 'b m) (ls : 'a list) : 'b list m =
    List.fold_right ls ~init:(return []) ~f:(fun a acc_m ->
      let%bind acc = acc_m in
      let%bind b = f a in
      return (b :: acc)
    )

  let read_env : Env.t m =
    let%bind (), env = read in
    return env

  let using_env (f : Env.t -> 'a) : 'a m =
    let%bind (), env = read in
    return (f env)
end

let eval_exp (type a) (e : a Expr.t) : a V.t =
  let module E = struct type t = a Env.t end in
  let open CPS_Error_M (E) in
  let zero () = type_mismatch () in
  let rec eval (e : a Expr.t) : a V.t m =
    match e with
    (* direct values *)
    | EInt i -> return (VInt i)
    | EBool b -> return (VBool b)
    | EVar id -> begin
      let%bind env = read_env in
      match Env.fetch id env with
      | None -> unbound_variable id
      | Some v -> return v
    end
    | ETypeInt -> return VTypeInt
    | ETypeBool -> return VTypeBool
    | ETypeTop -> return VTypeTop
    | ETypeBottom -> return VTypeBottom
    | ETypeForall { type_variables ; tau } -> 
      using_env @@ fun env ->
      VTypeForall { type_variables ; tau = { expr = tau ; env = lazy env } }
    | EType -> return VType
    | EAbort -> abort ()
    | EDiverge -> diverge ()
    | EFunction { param ; body } -> 
      using_env @@ fun env ->
      VFunClosure { param ; body = { expr = body ; env = lazy env } }
    | EMultiArgFunction { params ; body } -> 
      using_env @@ fun env ->
      VMultiArgFunClosure { params ; body = { expr = body ; env = lazy env } }
    | EFreeze expr ->
      using_env @@ fun env ->
      VFrozen { expr ; env = lazy env }
    | EId -> return VId
    (* inputs *) (* Consider: use an input stream to allow user to provide inputs *)
    | EPick_i -> return (VInt 0)
    | EPick_b -> return (VBool false)
    (* simple propogation *)
    | EVariant { label ; payload } ->
      let%bind payload = eval payload in
      return (VVariant { label ; payload = payload })
    | EList e_list ->
      let%bind ls = list_map eval e_list in
      return (VList ls)
    | ETypeList tau ->
      let%bind vtau = eval tau in
      return (VTypeList vtau)
    | ETypeSingle tau ->
      let%bind vtau = eval tau in
      return (VTypeSingle vtau)
    | ETypeArrow { domain ; codomain } ->
      let%bind domain = eval domain in
      let%bind codomain = eval codomain in
      return (VTypeArrow { domain ; codomain })
    | ETypeArrowD { binding ; domain ; codomain } ->
      let%bind domain = eval domain in
      using_env @@ fun env ->
      VTypeArrowD { binding ; domain ; codomain = { expr = codomain ; env = lazy env } }
    | ETypeRefinement { tau ; predicate } ->
      let%bind tau = eval tau in
      let%bind predicate = eval predicate in
      return (VTypeRefinement { tau ; predicate })
    | ETypeIntersect e_ls ->
      let%bind ls = list_map (fun (label, tau, tau') ->
        let%bind vtau = eval tau in
        let%bind vtau' = eval tau' in
        return (label, vtau, vtau')
        ) e_ls
      in
      return (VTypeIntersect ls)
    | ETypeVariant e_ls ->
      let%bind ls = list_map (fun (label, tau) ->
        let%bind vtau = eval tau in
        return (label, vtau)
        ) e_ls
      in
      return (VTypeVariant ls)
    | ERecord record_body ->
      let%bind new_record = eval_record_body record_body in
      return (VRecord new_record)
    | EModule stmts -> eval (Ast_tools.Utils.pgm_to_module stmts)
    | ETypeRecord record_type_body ->
      let%bind new_record = eval_record_body record_type_body in
      return (VTypeRecord new_record)
    | ETypeModule e_ls ->
      using_env @@ fun env ->
      VTypeRecordD (List.map e_ls ~f:(fun (label, tau) -> label, { expr = tau ; env = lazy env } ))
    | EThaw e ->
      let%bind v_frozen = eval e in
      let%orzero (VFrozen { expr = e_frozen ; env = lazy env }) = v_frozen in
      local (fun _ -> env) (eval e_frozen)
    (* bindings *)
    | EAppl { func ; arg } -> begin
      let%bind vfunc = eval func in
      let%bind arg = eval arg in
      match vfunc with
      | VFunClosure { param ; body = { expr ; env = lazy env } } ->
        local (fun _ -> Env.add param arg env) (eval expr)
      | VId -> return arg
      | VMultiArgFunClosure { params ; body = { expr ; env = lazy env }} -> begin
        match params with
        | [] -> type_mismatch ()
        | [ param ] ->
          local (fun _ -> Env.add param arg env) (eval expr)
        | param :: params ->
          local (fun _ -> Env.add param arg env) (eval (EMultiArgFunction { params ; body = expr }))
        end
      | _ -> type_mismatch ()
    end
    | ELet { var ; body ; cont } -> eval_let var ~body ~cont
    | ELetBind { var ; body ; cont } -> (* just do the desugar in place instead of a repeating behavior from EAppl *)
      eval (
        EAppl
          { func = EAppl { func = EVar (Ident "bind") ; arg = body }
          ; arg = EFunction { param = var ; body = cont }
          }
      )
    | ELetTyped { typed_var = { var ; _ } ; body ; cont ; _ } -> eval_let var ~body ~cont
    | EIgnore { ignored ; cont } ->
      let%bind _ = eval ignored in
      eval cont
    | ETypeMu { var ; body } ->
      let%bind env = read_env in
      let rec rec_env = lazy (
        Env.add var (VTypeMu { var ; body = { expr = body ; env = rec_env } }) env
      )
      in
      local (fun _ -> force rec_env) (eval (EVar var))
    (* operations *)
    | EListCons (e_hd, e_tl) -> begin
      let%bind hd = eval e_hd in
      let%bind tl = eval e_tl in
      let%orzero (VList ls) = tl in
      return (VList (hd :: ls))
    end
    | EBinop { left ; binop ; right } -> begin
      let%bind a = eval left in
      let%bind b = eval right in
      match binop, a, b with
      | BPlus, VInt n1, VInt n2                 -> return (VInt (n1 + n2))
      | BMinus, VInt n1, VInt n2                -> return (VInt (n1 - n2))
      | BTimes, VInt n1, VInt n2                -> return (VInt (n1 * n2))
      | BDivide, VInt n1, VInt n2 when n2 <> 0  -> return (VInt (n1 / n2))
      | BModulus, VInt n1, VInt n2 when n2 <> 0 -> return (VInt (n1 % n2))
      | BEqual, VInt n1, VInt n2                -> return (VBool (n1 = n2))
      | BEqual, VBool b1, VBool b2              -> return (VBool Bool.(b1 = b2))
      | BNeq, VInt n1, VInt n2                  -> return (VBool (n1 <> n2))
      | BNeq, VBool b1, VBool b2                -> return (VBool Bool.(b1 <> b2))
      | BLessThan, VInt n1, VInt n2             -> return (VBool (n1 < n2))
      | BLeq, VInt n1, VInt n2                  -> return (VBool (n1 <= n2))
      | BGreaterThan, VInt n1, VInt n2          -> return (VBool (n1 > n2))
      | BGeq, VInt n1, VInt n2                  -> return (VBool (n1 >= n2))
      | BAnd, VBool b1, VBool b2                -> return (VBool (b1 && b2))
      | BOr, VBool b1, VBool b2                 -> return (VBool (b1 || b2))
      | _ -> type_mismatch ()
    end
    | ENot e_not_body ->
      let%bind e_b = eval e_not_body in
      let%orzero (VBool b) = e_b in
      return (VBool (not b))
    | EAssert e_assert_body ->
      let%bind e_b = eval e_assert_body in
      let%orzero (VBool b) = e_b in
      if b
      then return (VRecord RecordLabel.Map.empty)
      else abort ()
    | EAssume e_assert_body ->
      let%bind e_b = eval e_assert_body in
      let%orzero (VBool b) = e_b in
      if b
      then return (VRecord RecordLabel.Map.empty)
      else diverge ()
    | EIf { cond ; true_body ; false_body } ->
      let%bind e_b = eval cond in
      let%orzero (VBool b) = e_b in
      if b
      then eval true_body
      else eval false_body
    | EProject { record ; label } ->
      let%bind r = eval record in
      let%orzero (VRecord record_body) = r in (* Note: this means we can't project from a record type *)
      let%orzero (Some v) = Map.find record_body label in
      return v
    (* casing *)
    | EMatch { subject ; patterns } -> 
      let%bind v = eval subject in
      let%orzero Some (e, f) =
        List.find_map patterns ~f:(fun (pat, body) ->
          match pat, v with
          | PAny, _ -> Some (body, Fn.id)
          | PVariable id, _ -> Some (body, Env.add id v)
          | PVariant { variant_label ; payload_id }, VVariant { label ; payload } 
              when VariantLabel.equal variant_label label -> 
            Some (body, Env.add payload_id payload)
          | PEmptyList, VList [] -> Some (body, Fn.id)
          | PDestructList { hd_id ; tl_id }, VList (v_hd :: v_tl) ->
            Some (body, fun env -> Env.add tl_id (VList v_tl) (Env.add hd_id v_hd env))
          | _ -> None
        )
      in
      local f (eval e)
    | ECase { subject ; cases ; default } -> begin
      let%bind v = eval subject in
      let%orzero VInt i = v in
      List.find_map cases ~f:(fun (case_i, body) ->
        Option.some_if (i = case_i) body
      )
      |> function
        | Some body -> eval body
        | None -> eval default
    end
    (* let funs *)
    | ELetFunRec { funcs ; cont } -> begin
      let%bind env = read_env in
      let rec rec_env = lazy (
        List.fold funcs ~init:env ~f:(fun acc fsig ->
          let comps = Ast_tools.Funsig.to_components fsig in
          match Ast_tools.Utils.abstract_over_ids comps.params comps.body with
          | EFunction { param ; body } -> 
            Env.add comps.func_id (VFunClosure { param ; body = { expr = body ; env = rec_env } }) acc
          | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
        )
      )
      in
      local (fun _ -> force rec_env) (eval cont)
    end
    | ELetFun { func ; cont } ->
      let comps = Ast_tools.Funsig.to_components func in
      Ast_tools.Utils.abstract_over_ids comps.params comps.body
      |> function
        | EFunction { param ; body } ->
          local (fun env ->
            Env.add comps.func_id (VFunClosure { param ; body = { expr = body ; env = lazy env } }) env
          ) (eval cont)
        | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"

    and eval_let (var : Ident.t) ~(body : a Expr.t) ~(cont : a Expr.t) : a V.t m =
      let%bind v = eval body in
      local (Env.add var v) (eval cont)

    and eval_record_body (record_body : a Expr.t RecordLabel.Map.t) : a V.t RecordLabel.Map.t m =
      Map.fold record_body ~init:(return RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
        let%bind acc = acc_m in
        let%bind v = eval e in
        return (Map.set acc ~key ~data:v)
      )
  in

  (run (eval e) () Env.empty)
  |> function
    | Ok (r, ()) -> Format.printf "OK:\n  %s\n" (V.to_string r); r
    | Error Type_mismatch -> Format.printf "TYPE MISMATCH\n"; VTypeMismatch
    | Error Abort -> Format.printf "FOUND ABORT\n"; VAbort
    | Error Diverge -> Format.printf "DIVERGE\n"; VDiverge
    | Error Unbound_variable Ident s -> Format.printf "UNBOUND VARIBLE %s\n" s; VUnboundVariable (Ident s)

let eval_pgm (type a) (pgm : a Program.t) : a V.t =
  eval_exp
  @@ Ast_tools.Utils.pgm_to_module pgm
