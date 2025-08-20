(**
   Module [Interp].

   This module interprets any language defined in this system.
   It uses GADTs and type constraints to define the interpreter
   for all languages in one function.
*)

open Core
open Lang.Ast
open Lang.Ast.Expr
open Lang.Ast_tools.Exceptions

module V = Lang.Value.Make (Lang.Value.Map_store) (Lang.Value.Lazy_cell) (Utils.Identity)
open V

module Feeder = Interp_common.Input_feeder

module Input_log = struct
  type t = (Interp_common.Input.t * Interp_common.Timestamp.t) list

  let empty : t = []

  let cons i t ls = (i, t) :: ls

  let to_sequence : t -> Interp_common.Input.t list = fun t ->
    List.map t ~f:Tuple2.get1
    |> List.rev

  let to_time_feeder : t -> Interp_common.Timestamp.t Feeder.t =
    fun t ->
    let mt = Map.empty (module Interp_common.Timestamp) in
    let m_ints, m_bools =
      List.fold t ~init:(mt, mt) ~f:(fun (mi, mb) (input, t) ->
          match input with
          | Interp_common.Input.I i -> (Map.set mi ~key:t ~data:i, mb)
          | Interp_common.Input.B b -> (mi, Map.set mb ~key:t ~data:b)
        )
    in
    let get : type a. a Interp_common.Key.Timekey.t -> a = fun key ->
      let a_opt : a option =
        match key with
        | I k -> Map.find m_ints k
        | B k -> Map.find m_bools k
      in
      Option.value a_opt ~default:(Interp_common.Input_feeder.zero.get key)
    in
    { get }
end

module CPS_Error_M (Env : Interp_common.Effects.ENV) = struct
  module State = struct
    type t =
      { time : Interp_common.Timestamp.t
      ; n_inputs : int
      ; timed_inputs : Input_log.t }

    let initial : t =
      { time = Interp_common.Timestamp.initial
      ; n_inputs = 0
      ; timed_inputs = Input_log.empty }
  end

  let max_step : Interp_common.Step.t = Step Int.(10 ** 6)

  module Err = struct
    (* Not putting state in the error because it's returned anyways *)
    type t = unit Interp_common.Errors.Runtime.t

    let fail_on_nondeterminism_misuse (s : State.t) : t * State.t =
      `XAbort { msg = "Nondeterminism used when not allowed." ; body = () }, s

    let fail_on_fetch (id : Ident.t) (s : State.t) : t * State.t =
      `XUnbound_variable (id, ()), s

    let fail_on_max_step (_step : int) (s : State.t) : t * State.t =
      `XReach_max_step (), s
  end

  include Interp_common.Effects.Make (State) (Interp_common.Effects.Unit_builder) (Env) (Err)

  let incr_time : unit m =
    modify (fun s -> { s with time = Interp_common.Timestamp.increment s.time })

  let push_time : unit m =
    modify (fun s -> { s with time = Interp_common.Timestamp.push s.time })

  let abort (type a) (msg : string) : a m =
    fail @@ `XAbort { msg ; body = () }

  (* unit is needed to surmount the value restriction *)
  let vanish (type a) (() : unit) : a m =
    let%bind s = get in
    Format.printf "Vanishing at time %s\n" (Interp_common.Timestamp.to_string s.time);
    fail @@ `XVanish ()

  let type_mismatch (type a) (() : unit) : a m =
    fail @@ `XType_mismatch { msg = "No type mismatch message today, sorry" ; body = () }

  let unbound_variable (type a) (id : Ident.t) : a m =
    fail @@ `XUnbound_variable (id, ())

  let list_map (f : 'a -> 'b m) (ls : 'a list) : 'b list m =
    List.fold_right ls ~init:(return []) ~f:(fun a acc_m ->
        let%bind acc = acc_m in
        let%bind b = f a in
        return (b :: acc)
      )

  let using_env (f : Env.t -> 'a) : 'a m =
    let%bind env = read_env in
    return (f env)

  let log_input (input : Interp_common.Input.t) : unit m =
    modify (fun s ->
        { s with n_inputs = s.n_inputs + 1 ; timed_inputs = Input_log.cons input s.time s.timed_inputs }
      )

  let n_inputs : int m =
    let%bind s = get in
    return s.n_inputs

  let with_time_snapback (x : 'a m) : 'a m =
    let%bind s = get in
    let%bind a = x in (* runs x with original time *)
    let%bind () = modify (fun s' -> { s' with time = s.time }) in
    return a

  let get_input (type a) (fkey : int -> a Interp_common.Key.Indexkey.t) (pack : a -> Interp_common.Input.t) (feeder : int Feeder.t) : a m =
    let%bind () = assert_nondeterminism in
    let%bind n = n_inputs in
    let a = feeder.get (fkey n) in
    let%bind () = log_input (pack a) in
    let%bind () = incr_time in
    return a
end

let eval_exp (type a) (e : a Expr.t) (feeder : int Feeder.t) : a V.t * Input_log.t =
  let module E = struct
    type value = a V.t
    type t = a Env.t
    let empty : t = Env.empty
    let fetch = Env.fetch
  end in
  let open CPS_Error_M (E) in
  let zero () = type_mismatch () in
  let rec eval (e : a Expr.t) : a V.t m =
    let%bind () = incr_step ~max_step in
    match e with
    (* Determinism *)
    | EDet e -> with_incr_depth @@ eval e
    | EEscapeDet e -> with_escaped_det @@ eval e
    (* direct values *)
    | EUnit -> return VUnit
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
    | ETypeUnit -> return VTypeUnit
    | EType -> return VType
    | EAbort msg -> abort msg
    | EVanish () -> vanish ()
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
    (* inputs *)
    | EInput | EPick_i -> 
      let%bind i = get_input Interp_common.Key.Indexkey.int_ (fun i -> Interp_common.Input.I i) feeder in
      return (VInt i)
    | EPick_b -> 
      let%bind b = get_input Interp_common.Key.Indexkey.bool_ (fun b -> Interp_common.Input.B b) feeder in
      return (VBool b)
    (* deferred expressions *)
    | EDefer e -> (* eagerly evaluate, but still track time correctly *)
      let%bind v = with_time_snapback (
          let%bind () = push_time in
          eval e
        ) in
      let%bind () = incr_time in
      return v
    (* simple propogation *)
    | EVariant { label ; payload } ->
      let%bind payload = eval payload in
      return (VVariant { label ; payload = payload })
    | EList e_list ->
      let%bind ls = list_map eval e_list in
      return (VList ls)
    | ETypeList -> return VTypeListFun
    | ETypeSingle -> return VTypeSingleFun
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
    | EModule stmts -> eval_stmt_list stmts
    | ETypeRecord record_type_body ->
      let%bind new_record = eval_record_body record_type_body in
      return (VTypeRecord new_record)
    | ETypeModule e_ls ->
      using_env @@ fun env ->
      VTypeModule (List.map e_ls ~f:(fun (label, tau) -> label, { body = tau ; env = lazy env } ))
    | EThaw e ->
      let%bind v_frozen = eval e in
      let%orzero (VFrozen { body = e_frozen ; env = lazy env }) = v_frozen in
      local (fun _ -> env) (eval e_frozen)
    | EGen e ->
      let%bind _ : a V.t = eval e in
      return VAbort
    | EUntouchable e ->
      let%bind v = eval e in
      return (VUntouchable v)
    (* bindings *)
    | EAppl { func ; arg } -> begin
        let%bind vfunc = eval func in
        let%bind arg = eval arg in
        match vfunc with
        | VFunClosure { param ; closure = { body ; env = lazy env } } ->
          local (fun _ -> Env.add param arg env) (eval body)
        | VId -> return arg
        | VMultiArgFunClosure { params ; closure = { body ; env = lazy env }} -> begin
            match params with
            | [] -> type_mismatch ()
            | [ param ] ->
              local (fun _ -> Env.add param arg env) (eval body)
            | param :: params ->
              local (fun _ -> Env.add param arg env) (eval (EMultiArgFunction { params ; body }))
          end
        | VTypeSingleFun -> return (VTypeSingle arg)
        | VTypeListFun -> return (VTypeList arg)
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
      local (fun _ -> force rec_env) (eval (Lang.Ast_tools.Utils.abstract_over_ids params (EVar var)))
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
      let%bind () = incr_time in
      if b
      then eval true_body
      else eval false_body
    | EProject { record ; label } ->
      let%bind r = eval record in
      let%orzero (VRecord body | VModule body) = r in (* Note: this means we can't project from a record type or module type *)
      let%orzero (Some v) = Map.find body label in
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
      else vanish ()
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
      local f (eval e)
    | ECase { subject ; cases ; default } -> begin
        let%bind v = eval subject in
        let%orzero VInt i = v in
        let%bind () = incr_time in
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
              let comps = Lang.Ast_tools.Funsig.to_components fsig in
              match Lang.Ast_tools.Utils.abstract_over_ids comps.params comps.defn with
              | EFunction { param ; body } -> 
                Env.add comps.func_id (VFunClosure { param ; closure = { body ; env = rec_env } }) acc
              | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
            )
        )
        in
        local (fun _ -> force rec_env) (eval body)
      end
    | ELetFun { func ; body = body' } -> begin
        let comps = Lang.Ast_tools.Funsig.to_components func in
        Lang.Ast_tools.Utils.abstract_over_ids comps.params comps.defn
        |> function
        | EFunction { param ; body } ->
          local (fun env ->
              Env.add comps.func_id (VFunClosure { param ; closure = { body ; env = lazy env } }) env
            ) (eval body')
        | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
      end
    (* tables *)
    | ETableCreate -> return (VTable { alist = [] })
    | ETableAppl { tbl ; gen ; arg } -> begin
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
    local (Env.add var v) (eval body)

  and eval_record_body (record_body : a Expr.t RecordLabel.Map.t) : a V.t RecordLabel.Map.t m =
    Map.fold record_body ~init:(return RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
        let%bind acc = acc_m in
        let%bind v = eval e in
        return (Map.set acc ~key ~data:v)
      )

  (* evaluates statement list to a module. This is a total pain for rec funs *)
  and eval_stmt_list (stmts : a Expr.statement list) : a V.t m =
    let%bind module_body =
      let rec fold_stmts acc_m : a Expr.statement list -> a V.t RecordLabel.Map.t m = function
        | [] -> acc_m
        | SUntyped { var ; defn } :: tl ->
          let%bind acc = acc_m in
          let%bind v = eval defn in
          local (Env.add var v) (
            fold_stmts (return (Map.set acc ~key:(RecordLabel.RecordLabel var) ~data:v)) tl
          )
        | STyped { typed_var = { var ; _ } ; defn ; _ } :: tl ->
          let%bind acc = acc_m in
          let%bind v = eval defn in
          local (Env.add var v) (
            fold_stmts (return (Map.set acc ~key:(RecordLabel.RecordLabel var) ~data:v)) tl
          )
        | SFun fsig :: tl -> begin
            let%bind acc = acc_m in
            let comps = Lang.Ast_tools.Funsig.to_components fsig in
            match Lang.Ast_tools.Utils.abstract_over_ids comps.params comps.defn with
            | EFunction { param ; body } ->
              let%bind env = read_env in
              let v = VFunClosure { param ; closure = { body ; env = lazy env } } in
              local (Env.add comps.func_id v) (
                fold_stmts (return (Map.set acc ~key:(RecordLabel.RecordLabel comps.func_id) ~data:v)) tl
              )
            | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
          end
        | SFunRec fsigs :: tl ->
          let%bind acc = acc_m in
          let func_comps = List.map fsigs ~f:Lang.Ast_tools.Funsig.to_components in
          let%bind env = read_env in
          let rec rec_env = lazy (
            List.fold func_comps ~init:env ~f:(fun acc comps ->
                match Lang.Ast_tools.Utils.abstract_over_ids comps.params comps.defn with
                | EFunction { param ; body } -> 
                  let v = VFunClosure { param ; closure = { body ; env = rec_env } } in
                  Env.add comps.func_id v acc
                | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
              )
          ) in
          let m =
            List.fold func_comps ~init:acc ~f:(fun acc comps ->
                Map.set acc ~key:(RecordLabel comps.func_id) ~data:(Obj.magic @@ (* FIXME *)
                                                                    Env.fetch comps.func_id (force rec_env)
                                                                    |> Option.value_exn
                                                                   )
              )
          in
          local (fun _ -> force rec_env) (fold_stmts (return m) tl )
      in
      fold_stmts (return RecordLabel.Map.empty) stmts
    in
    return (VModule module_body)
  in

  (run (eval e) State.initial Read.empty)
  |> fun (res, state, _, _) ->
  (match res with
   | Ok r -> Format.printf "OK:\n  %s\n" (V.to_string r); r
   | Error `XType_mismatch { Interp_common.Errors.msg = _ ; body = () } -> Format.printf "TYPE MISMATCH\n"; VTypeMismatch
   | Error `XAbort  { Interp_common.Errors.msg ; body = () } -> Format.printf "FOUND ABORT %s\n" msg; VAbort
   | Error `XVanish () -> Format.printf "VANISH\n"; VVanish
   | Error `XUnbound_variable (Lang.Ast.Ident.Ident s, ()) -> Format.printf "UNBOUND VARIBLE %s\n" s; VUnboundVariable (Ident s)
   | Error `XReach_max_step () -> Format.printf "REACHED MAX STEP\n"; VVanish
  ), state.State.timed_inputs

let eval_pgm 
    (type a)
    ?(feeder : int Feeder.t = Interp_common.Input_feeder.zero)
    (pgm : a Program.t) 
  : a V.t
  =
  Tuple2.get1
  @@ eval_exp (EModule pgm) feeder

let eval_pgm_to_time_feeder
    (type a)
    ?(feeder : int Feeder.t = Interp_common.Input_feeder.zero)
    (pgm : a Program.t) 
  : a V.t * Interp_common.Timestamp.t Feeder.t
  =
  let v, log = eval_exp (EModule pgm) feeder in
  v, Input_log.to_time_feeder log
