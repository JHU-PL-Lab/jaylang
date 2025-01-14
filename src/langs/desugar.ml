
open Core
open Ast

(*
  ([x1 ; ... ; xn], e) |->
    fun x1 -> ... -> fun xn -> e
*)
let abstract_over_ids (type a) (ids : Ident.t list) (body : a Expr.t) : a Expr.t =
  List.fold_right ids ~init:body ~f:(fun param body ->
    Expr.EFunction { param ; body }
  )

(* 'a is not fully abstract. It is constrained to be bluejay or desugared. I just don't feel like writing all that *)
let tau_list_to_arrow_type (taus : 'a Expr.t list) (codomain : 'a Expr.t) : 'a Expr.t =
  List.fold_right taus ~init:codomain ~f:(fun domain codomain ->
    Expr.ETypeArrow { domain ; codomain }
    )

let rec desugar_bluejay (expr : Bluejay.t) : Desugared.t =
  match expr with
  | EAssert assert_expr ->
    EIf
      { cond = desugar_bluejay assert_expr
      ; true_body = ERecord Parsing_tools.empty_record
      ; false_body = EAbort
      }
  | EAssume assume_expr ->
    EIf
      { cond = desugar_bluejay assume_expr
      ; true_body = ERecord Parsing_tools.empty_record
      ; false_body = EDiverge
      }
  | ELetFun { func ; cont } -> begin
    match func with
    | FUntyped { func_id ; params ; body } ->
      ELet
        { var = func_id
        ; body = abstract_over_ids params (desugar_bluejay body)
        ; cont = desugar_bluejay cont
        }
    | FTyped { func_id ; params ; body ; ret_type } ->
      let param_ids, param_taus = List.unzip @@ List.map params ~f:(fun { var ; tau } -> var, tau) in
      desugar_funsig
        ~func_id ~body ~cont
        ~func_tau:(tau_list_to_arrow_type param_taus ret_type)
        ~parameters:param_ids
    | FPolyTyped { func = { func_id ; params ; ret_type ; body } ; type_vars } ->
      let param_ids, param_taus = List.unzip @@ List.map params ~f:(fun { var ; tau } -> var, tau) in
      desugar_funsig
        ~func_id ~body ~cont
        ~func_tau:(ETypeForall { type_variables = type_vars ; tau = tau_list_to_arrow_type param_taus ret_type })
        ~parameters:(type_vars @ param_ids)
    | FDepTyped { func_id ; params ; ret_type ; body } ->
      desugar_funsig
        ~func_id ~body ~cont
        ~func_tau:(ETypeArrowD { binding = params.var ; domain = params.tau ; codomain = ret_type })
        ~parameters:[ params.var ]
    | FPolyDepTyped { func = { func_id ; params ; ret_type ; body } ; type_vars } ->
      desugar_funsig
        ~func_id ~body ~cont
        ~func_tau:(
          ETypeForall
            { type_variables = type_vars
            ; tau = ETypeArrowD { binding = params.var ; domain = params.tau ; codomain = ret_type } }
        )
        ~parameters:(type_vars @ [ params.var ])
  end
  | _ -> failwith "unimplemented "

(*
  Makes abstracted typed let-expression:

    let (func_id : func_tau) =
      fun < parameters > -> < body >
    in
    < cont >

  This is used when desugaring let-funs into let-expressions of anonymous functions
*)
and desugar_funsig ~(func_id : Ident.t) ~(func_tau : Bluejay.t) ~(parameters : Ident.t list) ~(body : Bluejay.t) ~(cont : Bluejay.t) : Desugared.t =
  ELetTyped
    { typed_var = 
      { var = func_id
      ; tau = desugar_bluejay func_tau }
    ; body = abstract_over_ids parameters @@ desugar_bluejay body
    ; cont = desugar_bluejay cont
    }