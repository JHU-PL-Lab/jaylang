open Batteries
open Jhupllib
open Jay
open Jayil

(* let lazy_logger = Logger_utils.make_lazy_logger "Jay_to_jayil_types";; *)

module Expr_desc = struct
  include Jay_ast.Expr_desc

  let pp = Jay_ast_pp.pp_expr_desc_without_tag
end

module Expr_desc_map = struct
  module M = Map.Make (Expr_desc)
  include M
  include Pp_utils.Map_pp (M) (Expr_desc)
end

module On_labels_map = struct
  module M = Map.Make (Jay_ast.Ident_set)
  include M
  include Pp_utils.Map_pp (M) (Jay_ast.Ident_set)
end

type t = {
  (* Mapping between an jayil variable to the jay expr that the
     jayil variable was derived from. *)
  jayil_var_to_jay_expr : Expr_desc.t Ast.Ident_map.t;
  (* Mapping between two jay expressions.  Used to create a
     mapping of jay lists and variants with their record
     equivalents as their keys, as well as mappings between let recs and
     their desugared versions. *)
  jay_expr_to_expr : Expr_desc.t Expr_desc_map.t;
  (* Mapping between two jay idents.  Used to create a mapping from
     post- to pre-alphatization variables. *)
  jay_var_to_var : Jay_ast.Ident.t Jay_ast.Ident_map.t;
  (* Mapping between sets of jay idents and jay type sigs.  Used to
     determine if a record was originally a list, variant, or record, depending
     on its labels. *)
  jay_idents_to_types : Jay_ast.type_sig On_labels_map.t;
  (* A set of odefa variables that were added during instrumentation (as
     opposed to being in the original code or added during pre-
     instrumentation translation). The instrumentation variable is the key;
     the value is the pre-instrumentation variable it aliases. Note that
     the value is an Option; it is none if the variable has no associated
     pre-instrumentation alias (namely if it was added as a pattern match
     conditional var). *)
  jay_instrument_vars_map : Ast.Ident.t option Ast.Ident_map.t;
  (* A list of odefa variables that should not be touched during the alias
     elimination pass. *)
  const_vars : Ast.var list;
  instrumented_tags : int list;
}
[@@deriving show]

let empty _is_jay =
  {
    jayil_var_to_jay_expr = Ast.Ident_map.empty;
    jay_expr_to_expr = Expr_desc_map.empty;
    jay_var_to_var = Jay_ast.Ident_map.empty;
    jay_idents_to_types = On_labels_map.empty;
    jay_instrument_vars_map = Ast.Ident_map.empty;
    const_vars = [];
    instrumented_tags = [];
  }

let add_jayil_var_jay_expr_mapping mappings jayil_ident on_expr =
  let jay_map = mappings.jayil_var_to_jay_expr in
  {
    mappings with
    jayil_var_to_jay_expr = Ast.Ident_map.add jayil_ident on_expr jay_map;
  }

let add_jay_expr_to_expr_mapping mappings expr1 expr2 =
  let jay_expr_map = mappings.jay_expr_to_expr in
  {
    mappings with
    jay_expr_to_expr = Expr_desc_map.add expr1 expr2 jay_expr_map;
  }

let add_jay_var_to_var_mapping mappings var1 var2 =
  let jay_var_map = mappings.jay_var_to_var in
  { mappings with jay_var_to_var = Jay_ast.Ident_map.add var1 var2 jay_var_map }

let add_jay_idents_to_type_mapping mappings idents type_sig =
  let jay_idents_type_map = mappings.jay_idents_to_types in
  {
    mappings with
    jay_idents_to_types = On_labels_map.add idents type_sig jay_idents_type_map;
  }

let add_jay_instrument_var mappings inst_ident ident_opt =
  let instrument_set = mappings.jay_instrument_vars_map in
  {
    mappings with
    jay_instrument_vars_map =
      Ast.Ident_map.add inst_ident ident_opt instrument_set;
  }

(** Helper function to recursively map jay expressions according to the
    expression-to-expression mapping (eg. records to lists or variants). We need
    a custom transformer function, rather than the one in utils, because we need
    to first transform the expression, then recurse (whereas transform_expr and
    transform_expr_m do the other way around). *)
let rec jay_expr_transformer
    (transformer : Jay_ast.expr_desc -> Jay_ast.expr_desc)
    (e_desc : Jay_ast.expr_desc) =
  let open Jay_ast in
  let (recurse : expr_desc -> expr_desc) = jay_expr_transformer transformer in
  let e_desc' = transformer e_desc in
  let expr' = e_desc'.body in
  let tag' = e_desc'.tag in
  (* let og_tag = e_desc.tag in *)
  let body' =
    match expr' with
    | Int _ | Bool _ | Var _ | Input -> expr'
    | Record record ->
        let record' =
          record |> Jay_ast.Ident_map.enum
          |> Enum.map (fun (lbl, e) -> (lbl, recurse e))
          |> Jay_ast.Ident_map.of_enum
        in
        Record record'
    | Match (e, pat_e_lst) ->
        let pat_e_lst' =
          List.map (fun (pat, e) -> (pat, recurse e)) pat_e_lst
        in
        Match (recurse e, pat_e_lst')
    | Function (id_lst, e) -> Function (id_lst, recurse e)
    | Appl (e1, e2) -> Appl (recurse e1, recurse e2)
    | Let (id, e1, e2) -> Let (id, recurse e1, recurse e2)
    | LetFun (fs, e) ->
        let (Funsig (fs_ident, fs_args, e_body)) = fs in
        let fs' = Funsig (fs_ident, fs_args, recurse e_body) in
        LetFun (fs', recurse e)
    | LetRecFun (fs_lst, e) ->
        let fs_lst' =
          List.map
            (fun (Funsig (id, args, e')) -> Funsig (id, args, recurse e'))
            fs_lst
        in
        LetRecFun (fs_lst', recurse e)
    | Plus (e1, e2) -> Plus (recurse e1, recurse e2)
    | Minus (e1, e2) -> Minus (recurse e1, recurse e2)
    | Times (e1, e2) -> Times (recurse e1, recurse e2)
    | Divide (e1, e2) -> Divide (recurse e1, recurse e2)
    | Modulus (e1, e2) -> Modulus (recurse e1, recurse e2)
    | Equal (e1, e2) -> Equal (recurse e1, recurse e2)
    | Neq (e1, e2) -> Neq (recurse e1, recurse e2)
    | LessThan (e1, e2) -> LessThan (recurse e1, recurse e2)
    | Leq (e1, e2) -> Leq (recurse e1, recurse e2)
    | GreaterThan (e1, e2) -> GreaterThan (recurse e1, recurse e2)
    | Geq (e1, e2) -> Geq (recurse e1, recurse e2)
    | And (e1, e2) -> And (recurse e1, recurse e2)
    | Or (e1, e2) -> Or (recurse e1, recurse e2)
    | Not e -> Not (recurse e)
    | If (e1, e2, e3) -> If (recurse e1, recurse e2, recurse e3)
    | RecordProj (e, lbl) -> RecordProj (recurse e, lbl)
    | VariantExpr (vlbl, e) -> VariantExpr (vlbl, recurse e)
    | List e_lst -> List (List.map recurse e_lst)
    | ListCons (e1, e2) -> ListCons (recurse e1, recurse e2)
    | Assert e -> Assert (recurse e)
    | Assume e -> Assume (recurse e)
    | Error x -> Error x
  in
  { tag = tag'; body = body' }

let get_jay_equivalent_expr mappings jayil_ident =
  let inst_map = mappings.jay_instrument_vars_map in
  let jayil_jay_map = mappings.jayil_var_to_jay_expr in
  let on_expr_map = mappings.jay_expr_to_expr in
  let on_ident_map = mappings.jay_var_to_var in
  (* Get pre-instrument var *)
  let jayil_ident' =
    match Ast.Ident_map.Exceptionless.find jayil_ident inst_map with
    | Some (Some pre_inst_ident) -> pre_inst_ident
    | Some None | None -> jayil_ident
  in
  (* Get jay expr from jayil var *)
  let res_opt = Ast.Ident_map.find_opt jayil_ident' jayil_jay_map in
  match res_opt with
  | None -> None
  | Some res ->
      let on_expr_transform expr =
        match Expr_desc_map.Exceptionless.find expr on_expr_map with
        | Some expr' -> expr'
        | None -> expr
      in
      let on_ident_transform e_desc =
        let open Jay_ast in
        let find_ident ident =
          Ident_map.find_default ident ident on_ident_map
        in
        let transform_funsig funsig =
          let (Funsig (fun_ident, arg_ident_list, body)) = funsig in
          let fun_ident' = find_ident fun_ident in
          let arg_ident_list' = List.map find_ident arg_ident_list in
          Funsig (fun_ident', arg_ident_list', body)
        in
        let expr = e_desc.body in
        let og_tag = e_desc.tag in
        let expr' =
          match expr with
          | Var ident -> Var (find_ident ident)
          | Function (ident_list, body) ->
              Function (List.map find_ident ident_list, body)
          | Let (ident, e1, e2) -> Let (find_ident ident, e1, e2)
          | LetFun (funsig, e) -> LetFun (transform_funsig funsig, e)
          | LetRecFun (funsig_list, e) ->
              LetRecFun (List.map transform_funsig funsig_list, e)
          | Match (e, pat_e_list) ->
              let transform_pattern pat =
                match pat with
                | RecPat record ->
                    let record' =
                      record |> Ident_map.enum
                      |> Enum.map (fun (lbl, x_opt) ->
                             match x_opt with
                             | Some x -> (lbl, Some (find_ident x))
                             | None -> (lbl, None))
                      |> Ident_map.of_enum
                    in
                    RecPat record'
                | StrictRecPat record ->
                    let record' =
                      record |> Ident_map.enum
                      |> Enum.map (fun (lbl, x_opt) ->
                             match x_opt with
                             | Some x -> (lbl, Some (find_ident x))
                             | None -> (lbl, None))
                      |> Ident_map.of_enum
                    in
                    StrictRecPat record'
                | VariantPat (vlbl, x) -> VariantPat (vlbl, find_ident x)
                | VarPat x -> VarPat (find_ident x)
                | LstDestructPat (x1, x2) ->
                    LstDestructPat (find_ident x1, find_ident x2)
                | AnyPat | IntPat | BoolPat | FunPat | EmptyLstPat -> pat
              in
              let pat_e_list' =
                List.map
                  (fun (pat, match_expr) -> (transform_pattern pat, match_expr))
                  pat_e_list
              in
              Match (e, pat_e_list')
          | _ -> expr
        in
        { tag = og_tag; body = expr' }
      in
      let final_ed =
        res
        |> jay_expr_transformer on_ident_transform
        |> jay_expr_transformer on_expr_transform
      in
      Some final_ed

let get_jay_equivalent_expr_exn mappings jayil_ident =
  let res_opt = get_jay_equivalent_expr mappings jayil_ident in
  match res_opt with
  | None ->
      raise
      @@ Invalid_argument
           (Printf.sprintf "variable %s is not associated with any jay expr."
              (Jay_ast_pp.show_ident jayil_ident))
  | Some res -> res

let get_type_from_idents mappings jayil_idents =
  let on_idents =
    jayil_idents |> Ast.Ident_set.enum
    |> Enum.map (fun (Ast.Ident lbl) -> Jay_ast.Ident lbl)
    |> Jay_ast.Ident_set.of_enum
  in
  let on_idents_type_map = mappings.jay_idents_to_types in
  match On_labels_map.Exceptionless.find on_idents on_idents_type_map with
  | Some typ -> typ
  | None -> RecType on_idents

let jayil_to_jay_aliases on_mappings aliases =
  let jayil_to_jay_expr x = get_jay_equivalent_expr_exn on_mappings x in
  aliases
  |> List.filter_map (fun alias ->
         let e_desc = jayil_to_jay_expr alias in
         match e_desc.body with
         | Jay_ast.Var _ | Error _ -> Some e_desc
         | _ -> None)
  |> List.unique

let get_jayil_vars_from_jay_expr mappings (expr : Jay_ast.expr_desc) =
  (* Getting the desugared version of core nat expression *)
  let desugared_core =
    let find_key_by_value v =
      Expr_desc_map.fold
        (fun desugared sugared acc ->
          if Jay_ast.equal_expr_desc sugared v then Some desugared else acc)
        mappings.jay_expr_to_expr None
    in
    let rec loop edesc =
      let edesc_opt' = find_key_by_value edesc in
      match edesc_opt' with None -> edesc | Some edesc' -> loop edesc'
    in
    loop expr
  in
  let () =
    Fmt.pr "\n This is the desugared expr: %a \n"
      Jay_ast_pp.pp_expr_desc_without_tag desugared_core
  in
  (* Getting the alphatized version *)
  let on_ident_transform (e_desc : Jay_ast.expr_desc) : Jay_ast.expr_desc =
    let open Jay_ast in
    let find_ident ident =
      Ident_map.fold
        (fun renamed og_name acc ->
          if equal_ident og_name ident then renamed else acc)
        mappings.jay_var_to_var ident
    in
    let transform_funsig funsig =
      let (Funsig (fun_ident, arg_ident_list, body)) = funsig in
      let fun_ident' = find_ident fun_ident in
      let arg_ident_list' = List.map find_ident arg_ident_list in
      Funsig (fun_ident', arg_ident_list', body)
    in
    let expr = e_desc.body in
    let og_tag = e_desc.tag in
    let expr' =
      match expr with
      | Var ident -> Var (find_ident ident)
      | Function (ident_list, body) ->
          Function (List.map find_ident ident_list, body)
      | Let (ident, e1, e2) -> Let (find_ident ident, e1, e2)
      | LetFun (funsig, e) -> LetFun (transform_funsig funsig, e)
      | LetRecFun (funsig_list, e) ->
          LetRecFun (List.map transform_funsig funsig_list, e)
      | Match (e, pat_e_list) ->
          let transform_pattern pat =
            match pat with
            | RecPat record ->
                let record' =
                  record |> Ident_map.enum
                  |> Enum.map (fun (lbl, x_opt) ->
                         match x_opt with
                         | Some x -> (lbl, Some (find_ident x))
                         | None -> (lbl, None))
                  |> Ident_map.of_enum
                in
                RecPat record'
            | StrictRecPat record ->
                let record' =
                  record |> Ident_map.enum
                  |> Enum.map (fun (lbl, x_opt) ->
                         match x_opt with
                         | Some x -> (lbl, Some (find_ident x))
                         | None -> (lbl, None))
                  |> Ident_map.of_enum
                in
                StrictRecPat record'
            | VariantPat (vlbl, x) -> VariantPat (vlbl, find_ident x)
            | VarPat x -> VarPat (find_ident x)
            | LstDestructPat (x1, x2) ->
                LstDestructPat (find_ident x1, find_ident x2)
            | AnyPat | IntPat | BoolPat | FunPat | EmptyLstPat -> pat
            (* | UntouchedPat _ *)
          in
          let pat_e_list' =
            List.map
              (fun (pat, match_expr) -> (transform_pattern pat, match_expr))
              pat_e_list
          in
          Match (e, pat_e_list')
      | _ -> expr
    in
    { tag = og_tag; body = expr' }
  in
  let alphatized =
    jay_expr_transformer on_ident_transform desugared_core
    (* actual_expr *)
  in
  let () =
    Fmt.pr "\n This is the alphatized expr: %a \n"
      Jay_ast_pp.pp_expr_desc_without_tag alphatized
  in
  let jayil_vars =
    Ast.Ident_map.fold
      (fun jayil_var core_expr acc ->
        if Jay_ast.equal_expr_desc core_expr alphatized
        then Ast.Var (jayil_var, None) :: acc
        else acc)
      mappings.jayil_var_to_jay_expr []
  in
  jayil_vars

let get_jay_inst_map mappings = mappings.jay_instrument_vars_map

let add_const_var mappings var =
  let consts = mappings.const_vars in
  let consts' =
    if List.mem var consts then consts else var :: mappings.const_vars
  in
  { mappings with const_vars = consts' }

let get_const_vars mappings = mappings.const_vars

let update_jayil_mappings (mappings : t)
    (replacement_map : Ast.var Ast.Var_map.t) : t =
  let jayil_var_to_jay_expr' =
    let kept =
      mappings.jayil_var_to_jay_expr
      |> Ast.Ident_map.filter (fun k _ ->
             not @@ Ast.Var_map.mem (Var (k, None)) replacement_map)
    in
    let removed =
      mappings.jayil_var_to_jay_expr
      |> Ast.Ident_map.filter (fun k _ ->
             Ast.Var_map.mem (Ast.Var (k, None)) replacement_map)
      |> Ast.Ident_map.bindings
      |> List.map (fun (k, v) ->
             let (Var (k', _)) =
               Ast.Var_map.find (Ast.Var (k, None)) replacement_map
             in
             (k', v))
    in
    let new_map =
      let folder acc (k, v) =
        let found = Ast.Ident_map.find_opt k acc in
        match found with
        | None -> Ast.Ident_map.add k v acc
        | Some _v' -> Ast.Ident_map.add k v acc
      in
      List.fold folder kept removed
    in
    new_map
  in
  let jay_instrument_vars_map' =
    let kept =
      mappings.jay_instrument_vars_map
      |> Ast.Ident_map.filter (fun k _ ->
             Ast.Var_map.mem (Var (k, None)) replacement_map)
    in
    let removed =
      mappings.jay_instrument_vars_map
      |> Ast.Ident_map.filter (fun k _ ->
             not @@ Ast.Var_map.mem (Var (k, None)) replacement_map)
      |> Ast.Ident_map.bindings
      |> List.map (fun (k, v_opt) ->
             let (Var (k', _)) =
               Ast.Var_map.find_default
                 (Ast.Var (k, None))
                 (Var (k, None))
                 replacement_map
             in
             match v_opt with
             | None -> (k', v_opt)
             | Some v ->
                 let (Var (v', _)) =
                   Ast.Var_map.find_default
                     (Ast.Var (v, None))
                     (Var (v, None))
                     replacement_map
                 in
                 (k', Some v'))
    in
    let new_map =
      let folder acc (k, v_opt) =
        let found = Ast.Ident_map.find_opt k acc in
        match found with
        | None -> Ast.Ident_map.add k v_opt acc
        | Some (Some v') -> (
            match v_opt with
            | None -> failwith "Shouldn't map to different expressions"
            | Some v ->
                if v = v'
                then acc
                else failwith "Shouldn't map to different expressions")
        | Some None -> (
            match v_opt with
            | None -> acc
            | Some _ -> failwith "Shouldn't map to different expressions")
      in
      List.fold folder kept removed
    in
    new_map
  in
  {
    mappings with
    jayil_var_to_jay_expr = jayil_var_to_jay_expr';
    jay_instrument_vars_map = jay_instrument_vars_map';
  }

let update_instrumented_tags (mappings : t) (tags : int list) : t =
  { mappings with instrumented_tags = tags }

let is_jay_instrumented (mappings : t) (tag : int) : bool =
  let instrumented_tags = mappings.instrumented_tags in
  List.mem tag instrumented_tags

let add_jay_instrumented (mappings : t) (tag : int) : t =
  let instrumented_tags = mappings.instrumented_tags in
  let instrumented_tags' = tag :: instrumented_tags in
  { mappings with instrumented_tags = instrumented_tags' }
