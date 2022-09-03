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
  (* Mapping between an odefa variable to the natodefa expr that the
      odefa variable was derived from. *)
  jayil_var_to_jay_expr : Expr_desc.t Ast.Ident_map.t;
  (* Mapping between two natodefa expressions.  Used to create a
      mapping of natodefa lists and variants with their record
      equivalents as their keys, as well as mappings between let recs and
      their desugared versions. *)
  jay_expr_to_expr : Expr_desc.t Expr_desc_map.t;
  (* Mapping between two natodefa idents.  Used to create a mapping from
      post- to pre-alphatization variables. *)
  jay_var_to_var : Jay_ast.Ident.t Jay_ast.Ident_map.t;
  (* Mapping between sets of natodefa idents and natodefa type sigs.  Used to
      determine if a record was originally a list, variant, or record, depending
      on its labels. *)
  jay_idents_to_types : Jay_ast.type_sig On_labels_map.t;
  jay_instrument_vars_map : Ast.Ident.t option Ast.Ident_map.t;
}
[@@deriving show]

let empty _is_jay =
  {
    jayil_var_to_jay_expr = Ast.Ident_map.empty;
    jay_expr_to_expr = Expr_desc_map.empty;
    jay_var_to_var = Jay_ast.Ident_map.empty;
    jay_idents_to_types = On_labels_map.empty;
    jay_instrument_vars_map = Ast.Ident_map.empty;
  }

let add_jayil_var_on_expr_mapping mappings odefa_ident on_expr =
  let natodefa_map = mappings.jayil_var_to_jay_expr in
  {
    mappings with
    jayil_var_to_jay_expr = Ast.Ident_map.add odefa_ident on_expr natodefa_map;
  }

let add_on_expr_to_expr_mapping mappings expr1 expr2 =
  let natodefa_expr_map = mappings.jay_expr_to_expr in
  {
    mappings with
    jay_expr_to_expr = Expr_desc_map.add expr1 expr2 natodefa_expr_map;
  }

let add_on_var_to_var_mapping mappings var1 var2 =
  let natodefa_var_map = mappings.jay_var_to_var in
  {
    mappings with
    jay_var_to_var = Jay_ast.Ident_map.add var1 var2 natodefa_var_map;
  }

let add_on_idents_to_type_mapping mappings idents type_sig =
  let natodefa_idents_type_map = mappings.jay_idents_to_types in
  {
    mappings with
    jay_idents_to_types =
      On_labels_map.add idents type_sig natodefa_idents_type_map;
  }

let add_jay_instrument_var mappings inst_ident ident_opt =
  let instrument_set = mappings.jay_instrument_vars_map in
  {
    mappings with
    jay_instrument_vars_map =
      Ast.Ident_map.add inst_ident ident_opt instrument_set;
  }

(** Helper function to recursively map natodefa expressions according to the
    expression-to-expression mapping (eg. records to lists or variants). We need
    a custom transformer function, rather than the one in utils, because we need
    to first transform the expression, then recurse (whereas transform_expr and
    transform_expr_m do the other way around). *)
let rec on_expr_transformer
    (transformer : Jay_ast.expr_desc -> Jay_ast.expr_desc)
    (e_desc : Jay_ast.expr_desc) =
  let open Jay_ast in
  let (recurse : expr_desc -> expr_desc) = on_expr_transformer transformer in
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

let get_natodefa_equivalent_expr mappings odefa_ident =
  let inst_map = mappings.jay_instrument_vars_map in
  let odefa_on_map = mappings.jayil_var_to_jay_expr in
  let on_expr_map = mappings.jay_expr_to_expr in
  let on_ident_map = mappings.jay_var_to_var in
  (* Get pre-instrument var *)
  let odefa_ident' =
    match Ast.Ident_map.Exceptionless.find odefa_ident inst_map with
    | Some (Some pre_inst_ident) -> pre_inst_ident
    | Some None | None -> odefa_ident
  in
  (* Get natodefa expr from odefa var *)
  let res_opt = Ast.Ident_map.find_opt odefa_ident' odefa_on_map in
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
                | VariantPat (vlbl, x) -> VariantPat (vlbl, find_ident x)
                | VarPat x -> VarPat (find_ident x)
                | LstDestructPat (x1, x2) ->
                    LstDestructPat (find_ident x1, find_ident x2)
                | AnyPat | IntPat | BoolPat | FunPat | EmptyLstPat -> pat
              in
              let pat_e_list' =
                List.map
                  (fun (pat, match_expr) ->
                    (* let show_expr = Pp_utils.pp_to_string Jay_ast_pp.pp_expr in *)
                    (* let () = print_endline @@ show_expr match_expr in *)
                    (transform_pattern pat, match_expr))
                  pat_e_list
              in
              Match (e, pat_e_list')
          | _ -> expr
        in
        { tag = og_tag; body = expr' }
      in
      let final_ed =
        res
        |> on_expr_transformer on_ident_transform
        |> on_expr_transformer on_expr_transform
      in
      Some final_ed

let get_natodefa_equivalent_expr_exn mappings odefa_ident =
  let res_opt = get_natodefa_equivalent_expr mappings odefa_ident in
  match res_opt with
  | None ->
      raise
      @@ Invalid_argument
           (Printf.sprintf
              "variable %s is not associated with any natodefa expr."
              (Ast.show_ident odefa_ident))
  | Some res -> res

let get_type_from_idents mappings odefa_idents =
  let on_idents =
    odefa_idents |> Ast.Ident_set.enum
    |> Enum.map (fun (Ast.Ident lbl) -> Jay_ast.Ident lbl)
    |> Jay_ast.Ident_set.of_enum
  in
  let on_idents_type_map = mappings.jay_idents_to_types in
  match On_labels_map.Exceptionless.find on_idents on_idents_type_map with
  | Some typ -> typ
  | None -> RecType on_idents

let odefa_to_on_aliases on_mappings aliases =
  let odefa_to_on_expr x = get_natodefa_equivalent_expr_exn on_mappings x in
  aliases
  |> List.filter_map (fun alias ->
         let e_desc = odefa_to_on_expr alias in
         match e_desc.body with
         | Jay_ast.Var _ | Error _ -> Some e_desc
         | _ -> None)
  |> List.unique

let get_odefa_var_opt_from_natodefa_expr mappings (expr : Jay_ast.expr_desc) =
  (* Getting the desugared version of core nat expression *)
  let desugared_core =
    let find_key_by_value v =
      (* let () = print_endline @@ "This is the target expr" in
         let () = print_endline @@ Jay_ast.show_expr_desc v in *)
      Expr_desc_map.fold
        (fun desugared sugared acc ->
          (* let () = print_endline "----------------------" in
             let () = print_endline @@ "This is the value in the dictionary: " in
             let () = print_endline @@ Jay_ast.show_expr_desc sugared in *)
          (* let () = print_endline @@ "This is the key in the dictionary: " in
             let () = print_endline @@ Jay_ast.show_expr_desc desugared in *)
          (* let () = print_endline "----------------------" in *)
          if Jay_ast.equal_expr_desc sugared v then Some desugared else acc)
        mappings.jay_expr_to_expr None
    in
    let rec loop edesc =
      let edesc_opt' = find_key_by_value edesc in
      match edesc_opt' with
      | None ->
          (* let () = print_endline @@ "None found!" in  *)
          edesc
      | Some edesc' ->
          (* let () = print_endline @@ "Found some!" in *)
          loop edesc'
    in
    loop expr
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
            (* | StrictRecPat record ->
               let record' =
                 record
                 |> Ident_map.enum
                 |> Enum.map
                   (fun (lbl, x_opt) ->
                     match x_opt with
                     | Some x -> (lbl, Some (find_ident x))
                     | None -> (lbl, None)
                   )
                 |> Ident_map.of_enum
               in
               StrictRecPat record' *)
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
    on_expr_transformer on_ident_transform desugared_core
    (* actual_expr *)
  in
  let odefa_var_opt =
    (* let () = print_endline @@ "This is the original expr" in
       let () = print_endline @@ Jay_ast.show_expr_desc alphatized in *)
    Ast.Ident_map.fold
      (fun odefa_var core_expr acc ->
        (* let () = print_endline @@ "This is the value in the dictionary: " in
           let () = print_endline @@ Jay_ast.show_expr_desc core_expr in *)
        (* if (core_expr.Jay_ast.tag = alphatized.tag) then  *)
        if Jay_ast.equal_expr_desc core_expr alphatized
        then Some (Ast.Var (odefa_var, None))
        else acc)
      mappings.jayil_var_to_jay_expr None
  in
  odefa_var_opt

let get_natodefa_inst_map mappings = mappings.jay_instrument_vars_map
