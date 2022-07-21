open Batteries
open Jhupllib
open Odefa_ast
open Ast
open On_to_odefa_monad.TranslationMonad

let lazy_logger = Logger_utils.make_lazy_logger "Type_instrumentation"

let add_abort_expr _ =
  let%bind abort_var = fresh_var "ab" in
  let abort_clause = Clause (abort_var, Abort_body) in
  return @@ Expr [ abort_clause ]

(** Update any abort clause variables with a new variable. This is called when a
    conditional constraint is formed, since the inner conditional may itself
    have abort clauses encoded with the identifying variable. *)
let rec change_abort_vars (old_var : var) (new_var : var) (clause : clause) :
    clause =
  let (Clause (v, body)) = clause in
  match body with
  | Value_body value -> (
      match value with
      | Value_function f ->
          let (Function_value (arg, Expr f_body)) = f in
          let f_body' = List.map (change_abort_vars old_var new_var) f_body in
          let value' = Value_function (Function_value (arg, Expr f_body')) in
          Clause (v, Value_body value')
      | _ -> clause)
  | Conditional_body (pred, e1, e2) ->
      let (Expr cl1) = e1 in
      let (Expr cl2) = e2 in
      let cl1' = List.map (change_abort_vars old_var new_var) cl1 in
      let cl2' = List.map (change_abort_vars old_var new_var) cl2 in
      let body' = Conditional_body (pred, Expr cl1', Expr cl2') in
      Clause (v, body')
  | _ -> clause

let rec instrument_clauses (c_list : clause list) : clause list m =
  match c_list with
  | clause :: clauses' -> (
      let (Clause (v, body)) = clause in
      (* Add var-clause pair to side mapping before instrumentation *)
      let%bind () = add_var_clause_pair v clause in
      (* Instrument depending on clause body type *)
      match body with
      | Value_body value -> (
          match value with
          | Value_function f ->
              (* Instrument function body *)
              let (Function_value (arg, Expr body)) = f in
              let%bind new_body = instrument_clauses body in
              let new_fun_val = Function_value (arg, Expr new_body) in
              let new_val_body = Value_body (Value_function new_fun_val) in
              let new_clause = Clause (v, new_val_body) in
              let%bind new_clauses' = instrument_clauses clauses' in
              return @@ (new_clause :: new_clauses')
          | _ ->
              (* Nothing to constrain *)
              let%bind new_clauses' = instrument_clauses clauses' in
              return @@ (clause :: new_clauses'))
      | Var_body _ | Input_body | Assert_body _ (*TODO *) | Assume_body _
      | Abort_body | Match_body _ ->
          (* Nothing to constrain *)
          let%bind new_clauses' = instrument_clauses clauses' in
          return @@ (clause :: new_clauses')
      (* | Match_body (v', pat) ->
         begin
           match pat with
           | Untouched_pattern _ ->
             begin
               let%bind new_clauses' = instrument_clauses clauses' in
               return @@ clause :: new_clauses'
             end
           | Any_untouched_pattern -> failwith "This shouldn't happen!"
           | Any_pattern | Int_pattern | Bool_pattern | Rec_pattern _
           | Strict_rec_pattern _ | Fun_pattern ->
             (* Don't instrument an instrumentation *)
             let%bind is_already_inst = is_instrument_var v in
             if is_already_inst then begin
               let%bind new_clauses' = instrument_clauses clauses' in
               return @@ clause :: new_clauses'
             end
             else begin
               (*
                 match = x ~ p;
                 ==>
                 m = x ~ untouched;
                 match = m ? ( c_match = x ~ p ) : ( ab = abort );
               *)
               (* Variables *)
               let%bind mb = fresh_var "m_b" in
               let%bind tvar = fresh_var "t_var" in
               let%bind m = fresh_var "m" in
               let%bind () = add_instrument_var mb in
               let%bind () = add_instrument_var tvar in
               let%bind () = add_instrument_var m in
               (* Clauses *)
               let mb_cls = Clause(mb, Match_body(v', Any_untouched_pattern)) in
               let tvar_cls = Clause(tvar, Value_body(Value_bool true)) in
               let m_bod = Binary_operation_body(mb, Binary_operator_xor, tvar) in
               let m_cls = Clause(m, m_bod) in
               (* Conditional *)
               let%bind c_match = fresh_var "c_match" in
               let%bind () = add_instrument_var_pair c_match v in
               let%bind t_path = return @@ Expr([Clause(c_match, body)]) in
               let%bind f_path = add_abort_expr v in
               let cond_clause = Clause(v, Conditional_body(m, t_path, f_path)) in
               let%bind cont = instrument_clauses clauses' in
               return @@ [mb_cls; tvar_cls; m_cls; cond_clause] @ cont
             end
         end *)
      | Not_body _v -> failwith "not implemented"
      | Binary_operation_body (v1, binop, v2) ->
          (* Don't instrument if the v is already counted as a variable added
              during instrumentation (i.e. pattern match conversion *)
          let%bind is_already_inst = is_instrument_var v in
          if is_already_inst
          then
            let%bind new_clauses' = instrument_clauses clauses' in
            return @@ (clause :: new_clauses')
          else
            (*
            binop = x + y
            ==>
            m_bl = x ~ int;
            m_br = y ~ int;
            m_b = x and y;
            binop = m ? ( c_binop = x + y ) : ( ab = abort );
          *)
            let pattern =
              match binop with
              | Binary_operator_plus | Binary_operator_minus
              | Binary_operator_times | Binary_operator_divide
              | Binary_operator_modulus | Binary_operator_less_than
              | Binary_operator_less_than_or_equal_to | Binary_operator_equal_to
              | Binary_operator_not_equal_to ->
                  Int_pattern
              | Binary_operator_and | Binary_operator_or -> Bool_pattern
            in
            (* Variables *)
            let%bind m1 = fresh_var "m_bl" in
            let%bind m2 = fresh_var "m_br" in
            let%bind m = fresh_var "m_b" in
            let%bind () = add_instrument_var m1 in
            let%bind () = add_instrument_var m2 in
            let%bind () = add_instrument_var m in
            (* Clauses *)
            let m1_cls = Clause (m1, Match_body (v1, pattern)) in
            let m2_cls = Clause (m2, Match_body (v2, pattern)) in
            let m_bod = Binary_operation_body (m1, Binary_operator_and, m2) in
            let m_cls = Clause (m, m_bod) in
            (* Conditional *)
            let%bind c_binop = fresh_var "c_binop" in
            let%bind () = add_instrument_var_pair c_binop v in
            (* Inner clause (eg. divide by zero check) *)
            let%bind inner_clauses =
              match binop with
              | Binary_operator_divide | Binary_operator_modulus ->
                  (* Variables *)
                  let%bind z = fresh_var "b_zero" in
                  let%bind b = fresh_var "b_b" in
                  let%bind () = add_instrument_var z in
                  let%bind () = add_instrument_var b in
                  (* We need to have this line because we are adding a new value
                     source *)
                  let%bind () =
                    add_odefa_natodefa_mapping z
                      (On_ast.new_expr_desc @@ On_ast.Int 0)
                  in
                  (* Clauses *)
                  let z_cls = Clause (z, Value_body (Value_int 0)) in
                  let b_bod =
                    Binary_operation_body (v2, Binary_operator_not_equal_to, z)
                  in
                  let b_cls = Clause (b, b_bod) in
                  (* Conditional *)
                  let%bind c_binop_sub = fresh_var "c_binop" in
                  let%bind () = add_instrument_var_pair c_binop_sub v in
                  let%bind t_path =
                    return @@ Expr [ Clause (c_binop_sub, body) ]
                  in
                  let%bind f_path = add_abort_expr c_binop in
                  let c_cls =
                    Clause (c_binop, Conditional_body (b, t_path, f_path))
                  in
                  return @@ [ z_cls; b_cls; c_cls ]
              | _ -> return @@ [ Clause (c_binop, body) ]
            in
            let%bind t_path = return @@ Expr inner_clauses in
            let%bind f_path = add_abort_expr v in
            let c_cls = Clause (v, Conditional_body (m, t_path, f_path)) in
            let%bind cont = instrument_clauses clauses' in
            return @@ [ m1_cls; m2_cls; m_cls; c_cls ] @ cont
      | Projection_body (r, lbl) ->
          let%bind is_already_inst = is_instrument_var v in
          if is_already_inst
          then
            let%bind new_clauses' = instrument_clauses clauses' in
            return @@ (clause :: new_clauses')
          else
            (*
            proj = r.lbl;
            ==>
            m = r ~ {lbl};
            proj = m ? ( c_proj = r.lbl ) : ( ab = abort );
          *)
            (* Pattern match *)
            let%bind m = fresh_var "m" in
            let%bind () = add_instrument_var m in
            let rec_pat_set = Ident_set.add lbl Ident_set.empty in
            let rec_pat = Rec_pattern rec_pat_set in
            let m_clause = Clause (m, Match_body (r, rec_pat)) in
            (* Conditional *)
            let%bind c_proj = fresh_var "c_proj" in
            let%bind () = add_instrument_var_pair c_proj v in
            let%bind t_path = return @@ Expr [ Clause (c_proj, body) ] in
            let%bind f_path = add_abort_expr v in
            let cond_clause =
              Clause (v, Conditional_body (m, t_path, f_path))
            in
            let%bind cont = instrument_clauses clauses' in
            return @@ [ m_clause; cond_clause ] @ cont
      | Appl_body (f, _) ->
          let%bind is_already_inst = is_instrument_var v in
          if is_already_inst
          then
            let%bind new_clauses' = instrument_clauses clauses' in
            return @@ (clause :: new_clauses')
          else
            (*
            appl = f x;
            ==>
            m = f ~ fun;
            appl = m ? ( c_appl = f x ) : ( ab = abort );
          *)
            (* Pattern match *)
            let%bind m = fresh_var "m" in
            let%bind () = add_instrument_var m in
            let m_clause = Clause (m, Match_body (f, Fun_pattern)) in
            (* Conditional *)
            let%bind c_appl = fresh_var "c_appl" in
            let%bind () = add_instrument_var_pair c_appl v in
            let%bind t_path = return @@ Expr [ Clause (c_appl, body) ] in
            let%bind f_path = add_abort_expr v in
            let cond_clause =
              Clause (v, Conditional_body (m, t_path, f_path))
            in
            let%bind cont = instrument_clauses clauses' in
            return @@ [ m_clause; cond_clause ] @ cont
      | Conditional_body (pred, Expr path1, Expr path2) ->
          let%bind is_already_inst = is_instrument_var v in
          if is_already_inst
          then
            (* Still instrument the inner expressions *)
            let%bind path1' = instrument_clauses path1 in
            let%bind path2' = instrument_clauses path2 in
            let body' = Conditional_body (pred, Expr path1', Expr path2') in
            let clause' = Clause (v, body') in
            let%bind new_clauses' = instrument_clauses clauses' in
            return @@ (clause' :: new_clauses')
          else
            (*
            cond = pred ? true_path : false_path;
            ==>
            m = pred ~ bool;
            cond = m ? ( c_cond = pred ? true_path : false_path )
                     : ( ab = abort );
          *)
            (* Pattern match *)
            let%bind m = fresh_var "m" in
            let%bind () = add_instrument_var m in
            let m_clause = Clause (m, Match_body (pred, Bool_pattern)) in
            (* Underlying conditional *)
            let%bind path1' = instrument_clauses path1 in
            let%bind path2' = instrument_clauses path2 in
            let body' = Conditional_body (pred, Expr path1', Expr path2') in
            (* Constrain conditional *)
            let%bind c_cond = fresh_var "c_cond" in
            let%bind () = add_instrument_var_pair c_cond v in
            let clause' = Clause (c_cond, body') in
            let clause'' = change_abort_vars v c_cond clause' in
            let%bind t_path = return @@ Expr [ clause'' ] in
            let%bind f_path = add_abort_expr v in
            let cond_clause =
              Clause (v, Conditional_body (m, t_path, f_path))
            in
            let%bind cont = instrument_clauses clauses' in
            return @@ [ m_clause; cond_clause ] @ cont)
  | [] -> return []

(* This function is necessary in the case where the first variable defines a
   function value definition.  If a function is the first variable and we look
   it up from within said function (e.g. from an abort clause), the lookup
   will zero out because the variable is always outside the scope of the
   function. *)
let add_first_var (c_list : clause list) : clause list m =
  let (Clause (_, first_body)) = List.first c_list in
  match first_body with
  | Value_body (Value_function _) ->
      let%bind fresh_str = freshness_string in
      let unit_rec = Value_record (Record_value Ident_map.empty) in
      let first_var = Var (Ident (fresh_str ^ "first"), None) in
      let first_cls = Clause (first_var, Value_body unit_rec) in
      return @@ (first_cls :: c_list)
  | _ -> return c_list

let add_result_var (c_list : clause list) : clause list m =
  let (Clause (last_var, _)) = List.last c_list in
  let%bind fresh_string = freshness_string in
  let result_var = Var (Ident (fresh_string ^ "result"), None) in
  let result_cls = Clause (result_var, Var_body last_var) in
  return @@ c_list @ [ result_cls ]

let instrument_odefa (odefa_ast : expr) : expr * On_to_odefa_maps.t =
  let (monad_val : (expr * On_to_odefa_maps.t) m) =
    (* Transform odefa program *)
    lazy_logger `debug (fun () ->
        Printf.sprintf "Initial program:\n%s" (Ast_pp.show_expr odefa_ast)) ;
    let (Expr odefa_clist) = odefa_ast in
    let%bind transformed_clist =
      return odefa_clist >>= instrument_clauses >>= add_first_var
      >>= add_result_var
    in
    let t_expr = Expr transformed_clist in
    lazy_logger `debug (fun () ->
        Printf.sprintf "Result of instrumentation:\n%s"
          (Ast_pp.show_expr t_expr)) ;
    (* Add "~result" to the end of the program *)
    let%bind on_odefa_maps = odefa_natodefa_maps in
    return (t_expr, on_odefa_maps)
  in
  let context = On_to_odefa_monad.new_translation_context () in
  run context monad_val
