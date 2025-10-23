
open Core
open Lang.Ast
open Lang.Ast.Expr
open Common

module Error_msg = Lang.Value.Error_msg (Value)

(*
  It's likely a good idea to use real OCaml state and effects to get
  performance, but for this pass, we use a monad to encode the concolic
  semantics. It just helps with correctness that way.
*)
let eager_eval 
    (expr : Embedded.t) 
    (input_feeder : Interp_common.Step.t Interp_common.Input_feeder.t)
    ~(max_step : Interp_common.Step.t)
  : Status.Eval.t * Interp_common.Step.t Path.t
  =
  let open Effects in

  let rec eval (expr : Embedded.t) : Value.t m =
    let open Value.M in (* puts the value constructors in scope *)
    let%bind () = incr_step ~max_step in
    match expr with
    (* Determinism *)
    | EDet e -> with_incr_depth @@ eval e
    | EEscapeDet e -> with_escaped_det @@ eval e
    (* Ints and bools -- constant expressions *)
    | EInt i -> return @@ VInt (i, Smt.Formula.const_int i)
    | EBool b -> return @@ VBool (b, Smt.Formula.const_bool b)
    (* Simple -- no different than interpreter *)
    | EDefer e -> eval e (* this concolic evaluator is eager *)
    | EUnit -> return VUnit
    | EVar id -> fetch id
    | EFunction { param ; body } ->
      let%bind env = read_env in
      return @@ VFunClosure { param ; closure = { body ; env } }
    | EId -> return VId
    | EFreeze e_freeze_body -> 
      let%bind env = read_env in
      return @@ VFrozen { body = e_freeze_body ; env }
    | EVariant { label ; payload = e_payload } -> 
      let%bind payload = eval e_payload in
      return @@ VVariant { label ; payload }
    | EUntouchable e ->
      let%bind v = eval e in
      return @@ VUntouchable v
    | EProject { record = e_record ; label } -> begin
        match%bind eval e_record with
        | (VRecord body | VModule body) as v -> begin
            match Map.find body label with
            | Some v -> return v
            | None -> type_mismatch @@ Error_msg.project_missing_label label v
          end
        | v -> type_mismatch @@ Error_msg.project_non_record label v
      end
    | EThaw e_frozen -> begin
        match%bind eval e_frozen with
        | VFrozen { body ; env } -> local (fun _ -> env) (eval body)
        | v -> type_mismatch @@ Error_msg.thaw_non_frozen v
      end
    | ERecord record_body ->
      let%bind value_record_body =
        Map.fold record_body ~init:(return RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
            let%bind acc = acc_m in
            let%bind v = eval e in
            return @@ Map.set acc ~key ~data:v
          )
      in
      return @@ VRecord value_record_body
    | EModule stmt_ls ->
      let%bind module_body =
        let rec fold_stmts acc_m = function
          | [] -> acc_m
          | SUntyped { var ; defn } :: tl ->
            let%bind acc = acc_m in
            let%bind v = eval defn in
            local (Env.add var v) (
              fold_stmts (return @@ Map.set acc ~key:(RecordLabel.RecordLabel var) ~data:v) tl
            )
        in
        fold_stmts (return RecordLabel.Map.empty) stmt_ls
      in
      return @@ VModule module_body 
    | EIgnore { ignored ; body } ->
      let%bind _ : Value.t = eval ignored in
      eval body
    | EMatch { subject ; patterns } -> begin (* Note: there cannot be symbolic branching on match *)
        let%bind v = eval subject in
        match
          (* find the matching pattern and add to env any values capture by the pattern *)
          List.find_map patterns ~f:(fun (pat, body) ->
              match Value.matches v pat with
              | Some bindings -> Some (body, fun env ->
                  List.fold bindings ~init:env ~f:(fun acc (v_bind, id_bind) -> Env.add id_bind v_bind acc)
                )
              | None -> None
            )
        with
        | Some (e, f) -> local f (eval e)
        | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
      end
    | ELet { var ; defn ; body } ->
      let%bind v = eval defn in
      local (Env.add var v) (eval body)
    | EAppl { func ; arg } -> begin
        let%bind vfunc = eval func in
        match vfunc with
        | VId -> eval arg
        | VFunClosure { param ; closure } ->
          let%bind varg = eval arg in
          local (fun _ -> Env.add param varg closure.env) (eval closure.body)
        | _ -> type_mismatch @@ Error_msg.bad_appl vfunc
      end
    (* Operations -- build new expressions *)
    | EBinop { left ; binop ; right } -> begin
        let%bind vleft = eval left in
        let%bind vright = eval right in
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
        | BEqual       , VUntouchable r1, VUntouchable r2 -> return (VBool (Value.equal r1 r2))
        | _ -> type_mismatch @@ Error_msg.bad_binop vleft binop vright
      end
    | ENot e_not_body -> begin
        match%bind eval e_not_body with
        | VBool (b, e_b) -> return @@ VBool (not b, Smt.Formula.not_ e_b) 
        | v -> type_mismatch @@ Error_msg.bad_not v
      end
    | EIntensionalEqual { left ; right } ->
      let%bind vleft = eval left in
      let%bind vright = eval right in
      return @@ VBool (Value.equal vleft vright)
    (* Branching *)
    | EIf { cond ; true_body ; false_body } -> begin
        match%bind eval cond with
        | VBool (b, e) ->
          let body = if b then true_body else false_body in
          let%bind () = push_branch (Direction.Bool_direction (b, e)) in
          eval body
        | v -> type_mismatch @@ Error_msg.cond_non_bool v
      end
    | ECase { subject ; cases ; default } -> begin
        let int_cases = List.map cases ~f:Tuple2.get1 in
        match%bind eval subject with
        | VInt (i, e) -> begin
            let body_opt = List.find_map cases ~f:(fun (i', body) -> if i = i' then Some body else None) in
            match body_opt with
            | Some body -> (* found a matching case *)
              let not_in = List.filter int_cases ~f:((<>) i) in
              let%bind () = push_branch (Direction.Int_direction { dir = Case_int i ; formula = e ; not_in }) in
              eval body
            | None -> (* no matching case, so take default case *)
              let%bind () = push_branch (Direction.Int_direction { dir = Case_default ; formula = e ; not_in = int_cases }) in
              eval default
          end
        | v -> type_mismatch @@ Error_msg.case_non_int v
      end
    (* Inputs *)
    | EPick_i -> get_input Interp_common.Key.Stepkey.int_ input_feeder
    | EPick_b -> get_input Interp_common.Key.Stepkey.bool_ input_feeder
    (* Tables -- includes some branching *)
    | ETableCreate -> return (VTable { alist = [] })
    | ETableAppl { tbl ; gen ; arg } -> begin
        match%bind eval tbl with
        | VTable mut_r -> begin
            let%bind v = eval arg in
            let%bind output_opt =
              List.fold mut_r.alist ~init:(return None) ~f:(fun acc_m (input, output) ->
                  match%bind acc_m with
                  | None -> 
                    let (b, e) = Value.equal input v in
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
              let%bind new_output = with_escaped_det @@ eval gen in
              mut_r.alist <- (v, new_output) :: mut_r.alist;
              return new_output
          end
        | tb -> type_mismatch @@ Error_msg.appl_non_table tb
      end
    (* Failure cases *)
    | EAbort msg -> abort msg
    | EVanish () -> vanish
  in

  run (eval expr)