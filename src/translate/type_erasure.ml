
open Lang
open Lang.Ast
open Core

(* this is a no-op but is needed for typing purposes *)
let erase_from_pattern (p : Bluejay.pattern) : Type_erased.pattern =
  match p with
  | (PAny | PVariable _ | PVariant _ | PEmptyList | PDestructList _) as p' -> p'

let erase (pgm : Bluejay.pgm) : Type_erased.pgm =
  let rec erase (e : Bluejay.t) : Type_erased.t =
    match e with
    (* base cases *)
    | (EInt _ | EBool _ | EVar _ | EInput | EUnit) as e -> e
    (* destroy types *)
    | EType
    | ETypeInt
    | ETypeBool
    | ETypeTop
    | ETypeBottom
    | ETypeRecord _
    | ETypeModule _
    | ETypeFun _
    | ETypeRefinement _
    | ETypeVariant _
    | ETypeIntersect _
    | ETypeMu { var = _ ; params = [] ; body = _ }
    | ETypeUnit -> EUnit (* send all types to unit value *)
    (* parametrized type propagation *)
    | ETypeList 
    | ETypeSingle -> EFunction { param = Ast_tools.Reserved.catchall ; body = EUnit }
    | ETypeMu { var = _ ; params ; body = _ } ->
      EMultiArgFunction { params = List.map params ~f:(fun _ -> Ast_tools.Reserved.catchall) ; body = EUnit }
    (* remove types *)
    | ELetTyped { typed_var = { var ; _ } ; defn ; body ; _ } -> 
      ELet { var ; defn = erase defn ; body = erase body }
    | ELetFun { func ; body } -> 
      ELetFun { func = erase_from_funsig func ; body = erase body }
    | ELetFunRec { funcs ; body } -> 
      ELetFunRec { funcs = List.map funcs ~f:erase_from_funsig ; body = erase body }
    (* propagate *)
    | EBinop { left ; binop ; right } -> 
      EBinop { left = erase left ; binop ; right = erase right }
    | EIf { cond ; true_body ; false_body } ->
      EIf { cond = erase cond ; true_body = erase true_body ; false_body = erase false_body }
    | ELet { var ; defn ; body } ->
      ELet { var ; defn = erase defn ; body = erase body }
    | EAppl { func ; arg } ->
      EAppl { func = erase func ; arg = erase arg }
    | EMatch { subject ; patterns } ->
      EMatch { subject = erase subject ; patterns = List.map patterns ~f:(fun (p, e) -> erase_from_pattern p, erase e) }
    | EProject { record ; label } ->
      EProject { record = erase record ; label }
    | ERecord m ->
      ERecord (Map.map m ~f:erase)
    | ENot e ->
      ENot (erase e)
    | EFunction { param ; body } ->
      EFunction { param ; body = erase body }
    | EVariant { label ; payload } ->
      EVariant { label ; payload = erase payload }
    | EList e_ls ->
      EList (List.map e_ls ~f:erase)
    | EListCons (e_hd, e_tl) ->
      EListCons (erase e_hd, erase e_tl)
    | EModule stmt_ls ->
      EModule (List.map stmt_ls ~f:erase_from_statement)
    | EAssert e ->
      EAssert (erase e)
    | EAssume e ->
      EAssume (erase e)
    | EDefer e ->
      EDefer (erase e)
    | EMultiArgFunction { params ; body } ->
      EMultiArgFunction { params ; body = erase body }

  and erase_from_funsig (fsig : Bluejay.funsig) : Type_erased.funsig =
    match fsig with
    | FUntyped { func_id ; params ; defn } ->
      FUntyped { func_id ; params ; defn = erase defn }
    | FTyped { type_vars ; func_id ; params ; defn ; _ } ->
      FUntyped
        { func_id 
        ; params = type_vars @ List.map params ~f:Ast_tools.Param.to_id
        ; defn = erase defn }

  and erase_from_statement (stmt : Bluejay.statement) : Type_erased.statement =
    match stmt with
    | SUntyped { var ; defn } ->
      SUntyped { var ; defn = erase defn }
    | STyped { typed_var = { var ; _ } ; defn ; _ } ->
      SUntyped { var ; defn = erase defn }
    | SFun fsig ->
      SFun (erase_from_funsig fsig)
    | SFunRec fsigs ->
      SFunRec (List.map fsigs ~f:erase_from_funsig)
  
  in
  List.map pgm ~f:erase_from_statement
