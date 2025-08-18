
(* Concolic main *)

open Core
open Lang.Ast
open Concolic_common

module V = Ceffects.V

(* this should be abstracted *)
type 'a res =  
  | V of 'a V.v
  | E of Ceffects.Err.t

let res_to_err (type a) (x : a res Ceffects.s) : a V.v Ceffects.m =
  Ceffects.bind (Ceffects.make_unsafe x) (function
      | V v -> Ceffects.return v
      | E e -> Ceffects.fail e
    )

let deferred_eval expr input_feeder ~max_step =
  let open Ceffects in

  let rec eval ?(is_stern : bool = false) (expr : Embedded.t) : V.t m =
    let open V in
    let k = eval ~is_stern in
    let%bind () = incr_step ~max_step in
    match expr with
    | EUnit -> return VUnit
    | EInt i -> return @@ VInt (i, Smt.Formula.const_int i)
    | EBool b -> return @@ VBool (b, Smt.Formula.const_bool b)
    | EVar id -> fetch id
    | EId -> return VId
    (* inputs *)
    | EPick_i -> get_input Interp_common.Key.Timekey.int_ input_feeder
    | EPick_b -> get_input Interp_common.Key.Timekey.bool_ input_feeder
    (* operations *)
    | EBinop { left ; binop ; right } -> begin
        let%bind vleft = stern_eval left in
        let%bind vright = stern_eval right in
        let k f e1 e2 op =
          return @@ f (Smt.Formula.binop op e1 e2)
        in
        let open Smt.Binop in
        let v_int n = fun e -> VInt (n, e) in
        let v_bool b = fun e -> VBool (b, e) in
        match binop, vleft, vright with
        | BPlus        , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 + n2)) e1 e2 Plus
        | BMinus       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 - n2)) e1 e2 Minus
        | BTimes       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 * n2)) e1 e2 Times
        | BDivide      , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 / n2)) e1 e2 Divide
        | BModulus     , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 mod n2)) e1 e2 Modulus
        | BEqual       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 = n2)) e1 e2 Equal
        | BEqual       , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool Bool.(b1 = b2)) e1 e2 Equal
        | BNeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <> n2)) e1 e2 Not_equal
        | BLessThan    , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 < n2)) e1 e2 Less_than
        | BLeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <= n2)) e1 e2 Less_than_eq
        | BGreaterThan , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 > n2)) e1 e2 Greater_than
        | BGeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 >= n2)) e1 e2 Greater_than_eq
        | BOr          , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool (b1 || b2)) e1 e2 Or
        | BAnd         , VBool (b1, e1) , VBool (b2, e2)             -> return @@ VBool (b1 && b2, Smt.Formula.and_ [ e1 ; e2 ])
        | _ -> type_mismatch @@ Error_msg.bad_binop vleft binop vright
      end
    | ENot expr -> begin
        match%bind stern_eval expr with
        | VBool (b, e_b) -> return @@ VBool (not b, Smt.Formula.not_ e_b)
        | v -> type_mismatch @@ Error_msg.bad_not v
      end
    | EProject { record ; label } -> begin
        match%bind stern_eval record with
        | (VRecord body | VModule body) as v -> begin
            match Map.find body label with
            | Some v -> return (V.cast_up v)
            | None -> type_mismatch @@ Error_msg.project_missing_label label v
          end
        | v -> type_mismatch @@ Error_msg.project_non_record label v
      end
    | EIntensionalEqual { left ; right } ->
      let%bind vleft = eval left in
      let%bind vright = eval right in
      return @@ VBool (V.equal vleft vright)
    (* control flow / branches *)
    | EMatch { subject ; patterns  } -> begin
        let%bind v = stern_eval subject in
        match
          List.find_map patterns ~f:(fun (pat, body) ->
              match V.matches v pat with
              | `Matches -> Some (body, fun x -> x)
              | `Matches_with (v', id) -> Some (body, Env.add id v')
              | `No_match -> None
            )
        with
        | Some (e, f) -> local f (k e)
        | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
      end
    | EIf { cond ; true_body ; false_body } -> begin
        match%bind stern_eval cond with
        | VBool (b, e_b) ->
          (* let%bind () = incr_time in *) (* time is not actually needed in practice on branches *)
          let body = if b then true_body else false_body in
          let%bind () = push_branch (Direction.Bool_direction (b, e_b)) in
          k body
        | v -> type_mismatch @@ Error_msg.cond_non_bool v
      end
    | ECase { subject ; cases ; default } -> begin
        let int_cases = List.map cases ~f:Tuple2.get1 in
        match%bind stern_eval subject with
        | VInt (i, e_i) -> begin
            (* let%bind () = incr_time in *) (* time is not actually needed in practice on branches *)
            let body_opt = List.find_map cases ~f:(fun (i', body) -> if i = i' then Some body else None) in
            match body_opt with
            | Some body -> 
              let not_in = List.filter int_cases ~f:((<>) i) in
              let%bind () = push_branch (Direction.Int_direction { dir = Case_int i ; expr = e_i ; not_in }) in
              k body
            | None -> 
              let%bind () = push_branch (Direction.Int_direction { dir = Case_default ; expr = e_i ; not_in = int_cases }) in
              k default
          end
        | v -> type_mismatch @@ Error_msg.case_non_int v
      end
    (* closures and applications *)
    | EFunction { param ; body } ->
      let%bind env = read_env in
      return (VFunClosure { param ; closure = { body ; env }})
    | EFreeze body ->
      let%bind env = read_env in
      return (VFrozen { body ; env })
    | ELet { var ; defn ; body } ->
      let%bind v = eval defn in
      local (Env.add var v) (eval body)
    | EIgnore { ignored ; body } ->
      let%bind _ : V.t = eval ignored in
      eval body
    | EAppl { func ; arg } -> begin
        match%bind stern_eval func with
        | VId -> eval arg
        | VFunClosure { param ; closure } ->
          let%bind v = eval arg in 
          local (fun _ -> Env.add param v closure.env) (k closure.body)
        | v -> type_mismatch @@ Error_msg.bad_appl v
      end
    | EThaw expr -> begin
        match%bind stern_eval expr with
        | VFrozen closure ->
          local (fun _ -> closure.env) (k closure.body)
        | v -> type_mismatch @@ Error_msg.thaw_non_frozen v
      end
    (* modules, records, and variants  *)
    | ERecord label_map ->
      let%bind value_record_body =
        Map.fold label_map ~init:(return Lang.Ast.RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
            let%bind acc = acc_m in
            let%bind v = eval e in
            return @@ Map.set acc ~key ~data:v
          )
      in
      return @@ VRecord value_record_body
    | EVariant { label ; payload } ->
      let%bind v = eval payload in
      return (VVariant { label ; payload = v })
    | EModule stmt_ls ->
      let%bind module_body =
        let rec fold_stmts acc_m : Embedded.statement list -> V.t Lang.Ast.RecordLabel.Map.t m = function
          | [] -> acc_m
          | SUntyped { var ; defn } :: tl ->
            let%bind acc = acc_m in
            let%bind v = eval defn in
            local (Env.add var v) (
              fold_stmts (return @@ Map.set acc ~key:(Lang.Ast.RecordLabel.RecordLabel var) ~data:v) tl
            )
        in
        fold_stmts (return Lang.Ast.RecordLabel.Map.empty) stmt_ls
      in
      return @@ VModule module_body 
    | EUntouchable e ->
      let%bind v = eval e in
      return (VUntouchable v)
    (* deferral *)
    | EDefer body -> if is_stern then k body else defer body
    (* termination *)
    | EVanish () -> vanish
    | EAbort msg -> abort msg
    (* determinism stuff *)
    | EDet expr -> with_incr_depth (k expr)
    | EEscapeDet expr -> with_escaped_det (k expr)
    (* tables *)
    | ETableCreate -> return (VTable { alist = [] })
    | ETableAppl { tbl ; gen ; arg } -> begin
      match%bind stern_eval tbl with
      | VTable mut_r -> begin
        let%bind v = stern_eval arg in
        let%bind output_opt =
          List.fold mut_r.alist ~init:(return None) ~f:(fun acc_m (input, output) ->
            match%bind acc_m with
            | None ->
              let (b, e) = V.equal input (cast_up v) in
              let%bind () = push_branch (Direction.Bool_direction (b, e)) in
              if b
              then return (Some output)
              else return None
            | Some _ -> acc_m (* already found an output, so go unchanged *)
          )
        in
        match output_opt with
        | Some output -> return output
        | None ->
          let%bind new_output = with_escaped_det @@ k gen in
          mut_r.alist <- (cast_up v, new_output) :: mut_r.alist;
          return new_output
      end
      | tb -> type_mismatch @@ Error_msg.appl_non_table tb
    end

  (*
    This stern eval may error monadically so that we get propagation of
    errors in relaxed eval.

    We have this because in practice, we don't want to clean up many deferred
    proofs on stern evals. This one is very direct, and it does (most of the time)
    the minimal amount of work to get to whnf.
  *)
  and stern_eval (expr : Embedded.t) : V.whnf m = 
    match expr with
    | EDefer body -> stern_eval body (* When sternly evaluating a deferred thing, we can just directly eval the thing *)
    | _ ->
      let%bind v = eval ~is_stern:true expr in
      let%bind () = incr_step ~max_step in
      let%bind () = incr_n_stern_steps in
      V.split v
        ~symb:(fun ((VSymbol t) as sym) ->
            let%bind s = get in
            match Time_map.find_opt t s.symbol_env with
            | Some v -> return v
            | None -> map_deferred_proof sym stern_eval
          )
        ~whnf:(fun v -> 
            let%bind () = optionally_map_some_deferred_proof stern_eval in
            return v
          )

  (* 
    This does not monadically error.
    Any error is packed into Res, so there is no implicit error propagation.
    This is helpful because we don't want errors propagating and messing with
    the cleanup in our stern semantics, where we must evaluate everything and
    keep the smallest error.
  *)
  and clean_up_deferred (final : V.ok res) : V.ok res s =
    let%bind s = get in
    match Time_map.choose_opt s.pending_proofs with
    | None -> return final (* done! can finish with how we're told to finish *)
    | Some (t, _) -> (* some cleanup to do, so do it, and then keep looping after that *)
      (* Do some cleanup by running this timestamp *)
      handle_error (map_deferred_proof (VSymbol t) stern_eval)
        (fun _ -> clean_up_deferred final) (* ignore value of deferred proof because we already have the final value *)
        (fun e -> clean_up_deferred (E e)) (* deferred proof errored, so it must be the smaller error, so keep it and continue *)
  in

  let begin_stern_loop (expr : Embedded.t) : V.ok res s =
    let%bind r =
      handle_error (stern_eval expr)
        (fun v -> return (V v))
        (fun e -> return (E e))
    in
    clean_up_deferred r
  in

  run (res_to_err (begin_stern_loop expr))
