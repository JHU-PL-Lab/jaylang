(**
  Module [Interp].

  This module interprets any language defined in this system.
  It uses GADTs and type constraints to define the interpreter
  for all languages in one function.

  Note that all input clauses get the default `0` or `false`.
  There is currently not support for custom inputs.
*)

open Core
open Ast
open Expr
open Ast_tools.Exceptions

module V = Value.Make (Value.Map_store) (Value.Lazy_cell) (Utils.Identity)
open V

module CPS_Error_M (Env : Interp_monad.ENV) = struct
  (* State tracks step count *)
  module State = Int 
  let max_step = Int.(10 ** 6)

  module Err = struct
    type t =
      | Abort of string 
      | Diverge 
      | Type_mismatch
      | Unbound_variable of Ident.t
      | Reached_max_step
    (* we might consider adding an Assert_false and Assume_false construct *)

    let fail_on_nondeterminism_misuse (_ : State.t) : t =
      Abort "Nondeterminism used when not allowed."

    let fail_on_fetch (id : Ident.t) (_ : State.t) : t =
      Unbound_variable id
  end

  include Interp_monad.Make (State) (Env) (Err)

  let abort (type a) (msg : string) : a m =
    fail @@ Err.Abort msg

  (* unit is needed to surmount the value restriction *)
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

  let using_env (f : Env.t -> 'a) : 'a m =
    let%bind env = read_env in
    return (f env)

  let incr_step : unit m =
    { run =
      fun ~reject ~accept s _ ->
        let step = s + 1 in
        if step > max_step
        then reject Reached_max_step
        else accept () step
    }
end

let eval_exp (type a) (e : a Expr.t) : a V.t =
  let module E = struct
    type value = a V.t
    type t = a Env.t
    let empty : t = Env.empty
    let fetch = Env.fetch
  end in
  let open CPS_Error_M (E) in
  let zero () = type_mismatch () in
  let rec eval (e : a Expr.t) : a V.t m =
    let%bind () = incr_step in
    match e with
    (* Determinism *)
    | EDet e -> with_incr_depth @@ eval e
    | EEscapeDet e -> with_escaped_det @@ eval e
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
    | EType -> return VType
    | EAbort msg -> abort msg
    | EDiverge -> diverge ()
    | EFunction { param ; body } -> 
      using_env @@ fun env ->
      VFunClosure { param ; closure = { body ; env = lazy env } }
    | EMultiArgFunction { params ; body } -> 
      using_env @@ fun env ->
      VMultiArgFunClosure { params ; closure = { body ; env = lazy env } }
    | EFreeze body ->
      using_env @@ fun env ->
      VFrozen { body ; env = lazy env }
    | EId -> return VId
    (* inputs *) (* Consider: use an input stream to allow user to provide inputs *)
    | EPick_i -> 
      let%bind () = assert_nondeterminism in
      return (VInt 0)
    | EPick_b -> 
      let%bind () = assert_nondeterminism in
      return (VBool false)
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
    | ETypeFun { domain ; codomain ; dep ; det } -> begin
      let%bind domain = eval domain in
      match dep with
      | `Binding binding ->
        using_env @@ fun env ->
        VTypeDepFun { binding ; domain ; codomain = { body = codomain ; env = lazy env } ; det }
      | `No ->
        let%bind codomain = eval codomain in
        return (VTypeFun { domain ; codomain ; det })
    end
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
      VTypeModule (List.map e_ls ~f:(fun (label, tau) -> label, { body = tau ; env = lazy env } ))
    | EThaw e ->
      let%bind v_frozen = eval e in
      let%orzero (VFrozen { body = e_frozen ; env = lazy env }) = v_frozen in
      local_env (fun _ -> env) (eval e_frozen)
    | EGen e ->
      let%bind _ : a V.t = eval e in
      return VAbort
    (* bindings *)
    | EAppl { func ; arg } -> begin
      let%bind vfunc = eval func in
      let%bind arg = eval arg in
      match vfunc with
      | VFunClosure { param ; closure = { body ; env = lazy env } } ->
        local_env (fun _ -> Env.add param arg env) (eval body)
      | VId -> return arg
      | VMultiArgFunClosure { params ; closure = { body ; env = lazy env }} -> begin
        match params with
        | [] -> type_mismatch ()
        | [ param ] ->
          local_env (fun _ -> Env.add param arg env) (eval body)
        | param :: params ->
          local_env (fun _ -> Env.add param arg env) (eval (EMultiArgFunction { params ; body }))
        end
      | _ -> type_mismatch ()
    end
    | ELet { var ; defn ; body } -> eval_let var ~defn ~body
    | ELetTyped { typed_var = { var ; _ } ; defn ; body ; _ } -> eval_let var ~defn ~body
    | EIgnore { ignored ; body } ->
      let%bind _ = eval ignored in
      eval body
    | ETypeMu { var ; params ; body } ->
      let%bind env = read_env in
      let rec rec_env = lazy (
        Env.add var (VTypeMu { var ; params ; closure = { body ; env = rec_env } }) env
      )
      in
      local_env (fun _ -> force rec_env) (eval (Ast_tools.Utils.abstract_over_ids params (EVar var)))
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
    | EIntensionalEqual { left ; right } ->
      let%bind vleft = eval left in
      let%bind vright = eval right in
      return @@ VBool (V.equal vleft vright)
    | ENot e_not_body ->
      let%bind e_b = eval e_not_body in
      let%orzero (VBool b) = e_b in
      return (VBool (not b))
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
    (* failures *)
    | EAssert e_assert_body ->
      let%bind e_b = eval e_assert_body in
      let%orzero (VBool b) = e_b in
      if b
      then return (VRecord RecordLabel.Map.empty)
      else abort "Failed assertion"
    | EAssume e_assert_body ->
      let%bind e_b = eval e_assert_body in
      let%orzero (VBool b) = e_b in
      if b
      then return (VRecord RecordLabel.Map.empty)
      else diverge ()
    (* casing *)
    | EMatch { subject ; patterns } -> 
      let%bind v = eval subject in
      let%orzero Some (e, f) =
        List.find_map patterns ~f:(fun (pat, body) ->
          match V.matches v pat with
          | Some bindings -> Some (body, fun env ->
            List.fold bindings ~init:env ~f:(fun acc (v_bind, id_bind) -> Env.add id_bind v_bind acc)
          )
          | None -> None
        )
      in
      local_env f (eval e)
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
    | ELetFunRec { funcs ; body } -> begin
      let%bind env = read_env in
      let rec rec_env = lazy (
        List.fold funcs ~init:env ~f:(fun acc fsig ->
          let comps = Ast_tools.Funsig.to_components fsig in
          match Ast_tools.Utils.abstract_over_ids comps.params comps.defn with
          | EFunction { param ; body } -> 
            Env.add comps.func_id (VFunClosure { param ; closure = { body ; env = rec_env } }) acc
          | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
        )
      )
      in
      local_env (fun _ -> force rec_env) (eval body)
    end
    | ELetFun { func ; body = body' } -> begin
      let comps = Ast_tools.Funsig.to_components func in
      Ast_tools.Utils.abstract_over_ids comps.params comps.defn
      |> function
        | EFunction { param ; body } ->
          local_env (fun env ->
            Env.add comps.func_id (VFunClosure { param ; closure = { body ; env = lazy env } }) env
          ) (eval body')
        | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
    end
    (* tables *)
    | ETable -> return (VTable { alist = [] })
    | ETblAppl { tbl ; gen ; arg } -> begin
      match%bind eval tbl with
      | VTable mut_r -> begin
        let%bind v = eval arg in
        let%bind output_opt = 
          List.fold mut_r.alist ~init:(return None) ~f:(fun acc_m (input, output) ->
            match%bind acc_m with
            | None when V.equal input v -> return (Some output)
            | _ -> acc_m (* already found an output or doesn't match input, so go unchanged *)
            )
        in
        match output_opt with
        | Some output -> return output
        | None ->
          let%bind new_output = with_escaped_det @@ eval gen in
          mut_r.alist <- (v, new_output) :: mut_r.alist;
          return new_output
      end
      | _ -> type_mismatch ()
    end


    and eval_let (var : Ident.t) ~(defn : a Expr.t) ~(body : a Expr.t) : a V.t m =
      let%bind v = eval defn in
      local_env (Env.add var v) (eval body)

    and eval_record_body (record_body : a Expr.t RecordLabel.Map.t) : a V.t RecordLabel.Map.t m =
      Map.fold record_body ~init:(return RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
        let%bind acc = acc_m in
        let%bind v = eval e in
        return (Map.set acc ~key ~data:v)
      )
  in

  (run (eval e) 0 Read.empty)
  |> function
    | Ok (r, _) -> Format.printf "OK:\n  %s\n" (V.to_string r); r
    | Error Type_mismatch -> Format.printf "TYPE MISMATCH\n"; VTypeMismatch
    | Error Abort msg -> Format.printf "FOUND ABORT %s\n" msg; VAbort
    | Error Diverge -> Format.printf "DIVERGE\n"; VDiverge
    | Error Unbound_variable Ident s -> Format.printf "UNBOUND VARIBLE %s\n" s; VUnboundVariable (Ident s)
    | Error Reached_max_step -> Format.printf "REACHED MAX STEP\n"; VDiverge

let eval_pgm (type a) (pgm : a Program.t) : a V.t =
  eval_exp
  @@ Ast_tools.Utils.pgm_to_module pgm
