(**
  Module [Ast_tools].

  Defined here are general tools that are helpful to manipulate
  programs.

  There are the labels and names that are reserved to be introduced
  during translation, tools to abstract, apply, and project, and
  methods to extract the function components from a function signature.
*)

open Core
open Ast

(*
  Here are some reserved names that don't parse, so the programmer cannot
  write them, and we are safe to inject them into the AST.
*)
module Reserved = struct
  (* For conciseness, don't nest modules to separate into the following groups *)

  (* Record labels *)
  let gen : RecordLabel.t = RecordLabel (Ident "~gen")
  let check : RecordLabel.t = RecordLabel (Ident "~check")
  let wrap : RecordLabel.t = RecordLabel (Ident "~wrap")
  let hd : RecordLabel.t = RecordLabel (Ident "~hd")
  let tl : RecordLabel.t = RecordLabel (Ident "~tl")

  (* Variant constructors *)
  let cons : VariantLabel.t = VariantLabel (Ident "~Cons") 
  let nil : VariantLabel.t = VariantLabel (Ident "~Nil") 
  let untouched : VariantLabel.t = VariantLabel (Ident "~Untouched")
  let top : VariantLabel.t = VariantLabel (Ident "~Top")
  let bottom : VariantLabel.t = VariantLabel (Ident "~Bottom")
  let predicate_failed : VariantLabel.t = VariantLabel (Ident "~Predicate_failed")

  (* Variant type constructors *)
  let cons_type : VariantTypeLabel.t = VariantTypeLabel (Ident "~Cons") 
  let nil_type : VariantTypeLabel.t = VariantTypeLabel (Ident "~Nil") 

  (* Idents *)
  let catchall : Ident.t = Ident "_"
end

module Utils = struct
  let unit_value : type a. a Expr.t = ERecord RecordLabel.Map.empty
  let unit_type : 'a Constraints.bluejay_or_desugared Expr.t = ETypeRecord RecordLabel.Map.empty

  (*
    [ tau1 ; ... ; taun ], tau |->
      tau1 -> ..> taun -> tau
  *)
  let tau_list_to_arrow_type (taus : 'a Expr.t list) (codomain : 'a Expr.t) : 'a Constraints.bluejay_or_desugared Expr.t =
    List.fold_right taus ~init:codomain ~f:(fun domain codomain ->
      Expr.ETypeArrow { domain ; codomain }
    )

  (*
    [ x1 ; ... ; xn ], e |->
      fun x1 -> ... -> fun xn -> e
  *)
  let abstract_over_ids (type a) (ids : Ident.t list) (body : a Expr.t) : a Expr.t =
    List.fold_right ids ~init:body ~f:(fun param body ->
      Expr.EFunction { param ; body }
    )

  (*
    f, [ x1 ; ... ; xn ] |->
      f x1 ... xn
  *)
  let appl_list (type a) (f : a Expr.t) (args : a Expr.t list) : a Expr.t =
    List.fold args ~init:f ~f:(fun func arg ->
      EAppl { func ; arg }
    )

  (*
    Is a partially-evaluating apply, where the function is checked against the identity function.
  *)
  let apply (type a) (func : a Expr.t) (arg : a Expr.t) : a Expr.t =
    match func with
    | EId -> arg
    | _ -> EAppl { func ; arg }

  (*
    Is a partially-evaluating record projection, which is safe because we have no side effects coming
    from the evaluation of the non-projected fields.
  *)
  let proj (type a) (tau : a Expr.t) (label : RecordLabel.t) : a Expr.t =
    match tau with
    | ERecord m when Map.mem m label ->
      Map.find_exn m label
    | _ -> EProject { record = tau ; label }
end

module Function_components = struct
  type 'a t =
    { func_id : Ident.t
    ; tau_opt : 'a Constraints.bluejay_or_desugared Expr.t option
    ; params  : Ident.t list
    ; body    : 'a Constraints.bluejay_or_desugared Expr.t
    } 

  let map (x : 'a t) ~(f : 'a Expr.t -> 'b Expr.t) : 'b t =
    { func_id = x.func_id
    ; tau_opt = Option.map x.tau_opt ~f
    ; params  = x.params
    ; body    = f x.body
    }
end

module Param = struct
  type 'a t = 'a Expr.param

  let to_id : 'a t -> Ident.t = function
    | TVar { var ; _ }
    | TVarDep { var ; _ } -> var
end

module Funsig = struct
  type 'a t = 'a Expr.funsig

  (*
    Breaks a function signature into its id, type (optional), parameter names, and function body.
  *)
  let to_components (fsig : 'a t) : 'a Constraints.bluejay_or_desugared Function_components.t =
    match fsig with
    | FUntyped { func_id ; params ; body } ->
      { func_id ; tau_opt = None ; params ; body }
    | FTyped { type_vars ; func_id ; params ; ret_type ; body } ->
      { func_id ; body ; params = type_vars @ List.map params ~f:Param.to_id
      ; tau_opt = Some (
        ETypeForall
          { type_variables = type_vars
          ; tau = List.fold_right params ~init:ret_type ~f:(fun tvar codomain ->
              match tvar with
              | TVar { var = _ ; tau } -> Expr.ETypeArrow { domain = tau ; codomain }
              | TVarDep { var ; tau } -> ETypeArrowD { binding = var ; domain = tau ; codomain }
            )
          }
      ) }
end
