
open Core
open Ast

module Reserved_labels = struct
  module Records = struct
    let gen : RecordLabel.t = RecordLabel (Ident "~gen")
    let check : RecordLabel.t = RecordLabel (Ident "~check")
    let wrap : RecordLabel.t = RecordLabel (Ident "~wrap")
    let hd : RecordLabel.t = RecordLabel (Ident "~hd")
    let tl : RecordLabel.t = RecordLabel (Ident "~tl")
  end

  module Variants = struct
    let cons : VariantLabel.t = VariantLabel (Ident "~Cons") 
    let nil : VariantLabel.t = VariantLabel (Ident "~Nil") 
    let untouched : VariantLabel.t = VariantLabel (Ident "~Untouched")
  end

  module VariantTypes = struct
    let cons : VariantTypeLabel.t = VariantTypeLabel (Ident "~Cons") 
    let nil : VariantTypeLabel.t = VariantTypeLabel (Ident "~Nil") 
  end

  module Idents = struct
    let catchall : Ident.t = Ident "_"
  end
end

module Utils = struct
  let dummy_value : type a. a Expr.t = EInt 0

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

module Funsig = struct
  type 'a t = 'a Expr.funsig
(* 
  let to_id (fsig : 'a t) : Ident.t =
    match fsig with
    | FUntyped { func_id ; _ }
    | FTyped { func_id ; _ }
    | FPolyTyped { func = { func_id ; _ } ; _ }
    | FDepTyped { func_id ; _ }
    | FPolyDepTyped { func = { func_id ; _ } ; _ } -> func_id *)

  (*
    Breaks a function signature into its id, type (optional), parameter names, and function body.
  *)
  let to_components (fsig : 'a t) : 'a Constraints.bluejay_or_desugared Function_components.t =
    match fsig with
    | FUntyped { func_id ; params ; body } ->
      { func_id ; tau_opt = None ; params ; body }
    | FTyped { func_id ; params ; body ; ret_type } ->
      let param_ids, param_taus = List.unzip @@ List.map params ~f:(fun { var ; tau } -> var, tau) in
      { func_id ;  body ; params = param_ids
      ; tau_opt = Some (Utils.tau_list_to_arrow_type param_taus ret_type) }
    | FPolyTyped { func = { func_id ; params ; ret_type ; body } ; type_vars } ->
      let param_ids, param_taus = List.unzip @@ List.map params ~f:(fun { var ; tau } -> var, tau) in
      { func_id ; body ; params = type_vars @ param_ids
      ; tau_opt = Some (ETypeForall { type_variables = type_vars ; tau = Utils.tau_list_to_arrow_type param_taus ret_type }) }
    | FDepTyped { func_id ; params ; ret_type ; body } ->
      { func_id ; body ; params = [ params.var ]
      ; tau_opt = Some (ETypeArrowD { binding = params.var ; domain = params.tau ; codomain = ret_type }) }
    | FPolyDepTyped { func = { func_id ; params ; ret_type ; body } ; type_vars } ->
      { func_id ; body ; params = type_vars @ [ params.var ]
      ; tau_opt = Some (
          ETypeForall
            { type_variables = type_vars
            ; tau = ETypeArrowD { binding = params.var ; domain = params.tau ; codomain = ret_type } }
      )}
end
