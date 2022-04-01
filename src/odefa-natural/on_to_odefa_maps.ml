open Batteries;;
open Jhupllib;;

open Odefa_ast;;

(* let lazy_logger = Logger_utils.make_lazy_logger "On_to_odefa_types";; *)

module Expr = struct
  include On_ast.Expr;;
  let pp = On_ast_pp.pp_expr;;
end;;

module Expr_map = struct
  module M = Map.Make(Expr);;
  include M;;
  include Pp_utils.Map_pp(M)(Expr);;
end;;

module On_labels_map = struct
  module M = Map.Make(On_ast.Ident_set);;
  include M;;
  include Pp_utils.Map_pp(M)(On_ast.Ident_set);;
end;;

module Odefa_clause = struct
  type t = Ast.clause;;
  let pp = Ast_pp.pp_clause;;
end;;

type t = {

  (** True if the mapping was created during natodefa-to-odefa
      translation, false otherwise (e.g. after instrumenting on
      odefa code). *)
  is_natodefa : bool;

  (** A set of odefa variables that were added during instrumentation
      (as opposed to being in the original code or added during pre-
      instrumentation translation).  The instrumentation variable
      is the key; the value is the pre-instrumentation variable
      it aliases.  Note that the value is an Option; it is none if
      the variable has no associated pre-instrumentation alias (namely
      if it was added as a pattern match conditional var). *)
  odefa_instrument_vars_map : (Ast.Ident.t option) Ast.Ident_map.t;

  (** Mapping between variables that were added during instrumentation
      with the variables whose clauses the instrumenting clause is
      constraining.  This is mainly used to obtain the operation that
      an instrumenting conditional is constrianing. *)
  odefa_pre_instrument_clause_mapping : Odefa_clause.t Ast.Ident_map.t;

  (** Mapping between an odefa variable to the natodefa expr that the
      odefa variable was derived from. *)
  odefa_var_to_natodefa_expr : Expr.t Ast.Ident_map.t;

  (** Mapping between two natodefa expressions.  Used to create a
      mapping of natodefa lists and variants with their record
      equivalents as their keys, as well as mappings between let recs and
      their desugared versions. *)
  natodefa_expr_to_expr : Expr.t Expr_map.t;

  (** Mapping between two natodefa idents.  Used to create a mapping from
      post- to pre-alphatization variables. *)
  natodefa_var_to_var : On_ast.Ident.t On_ast.Ident_map.t;

  (** Mapping between sets of natodefa idents and natodefa type sigs.  Used to
      determine if a record was originally a list, variant, or record, depending
      on its labels. *)
  natodefa_idents_to_types : On_ast.type_sig On_labels_map.t;
}
[@@ deriving show]
;;

let empty is_natodefa = {
  is_natodefa = is_natodefa;
  odefa_instrument_vars_map = Ast.Ident_map.empty;
  odefa_pre_instrument_clause_mapping = Ast.Ident_map.empty;
  odefa_var_to_natodefa_expr = Ast.Ident_map.empty;
  natodefa_expr_to_expr = Expr_map.empty;
  natodefa_var_to_var = On_ast.Ident_map.empty;
  natodefa_idents_to_types = On_labels_map.empty;
}
;;

let add_odefa_instrument_var mappings inst_ident ident_opt =
  let instrument_set = mappings.odefa_instrument_vars_map in
  { mappings with
    odefa_instrument_vars_map =
      Ast.Ident_map.add inst_ident ident_opt instrument_set;
  }
;;

let add_odefa_var_clause_mapping mappings var_ident clause =
  let instrument_map = mappings.odefa_pre_instrument_clause_mapping in
  { mappings with
    odefa_pre_instrument_clause_mapping =
      Ast.Ident_map.add var_ident clause instrument_map;
  }
;;

let add_odefa_var_on_expr_mapping mappings odefa_ident on_expr =
  let natodefa_map = mappings.odefa_var_to_natodefa_expr in
  { mappings with
    odefa_var_to_natodefa_expr =
      Ast.Ident_map.add odefa_ident on_expr natodefa_map;
  }
;;

let add_on_expr_to_expr_mapping mappings expr1 expr2 =
  let natodefa_expr_map = mappings.natodefa_expr_to_expr in
  { mappings with
    natodefa_expr_to_expr =
      Expr_map.add expr1 expr2 natodefa_expr_map;
  }
;;

let add_on_var_to_var_mapping mappings var1 var2 =
  let natodefa_var_map = mappings.natodefa_var_to_var in
  { mappings with
    natodefa_var_to_var =
      On_ast.Ident_map.add var1 var2 natodefa_var_map
  }
;;

let add_on_idents_to_type_mapping mappings idents type_sig =
  let natodefa_idents_type_map = mappings.natodefa_idents_to_types in
  { mappings with
    natodefa_idents_to_types =
      On_labels_map.add idents type_sig natodefa_idents_type_map
  }
;;

let get_pre_inst_equivalent_clause mappings odefa_ident =
  let inst_map = mappings.odefa_instrument_vars_map in
  let clause_map = mappings.odefa_pre_instrument_clause_mapping in
  (* Get pre-instrument var from instrument var *)
  let odefa_ident' =
    match Ast.Ident_map.Exceptionless.find odefa_ident inst_map with
    | Some (Some pre_inst_ident) -> pre_inst_ident
    | Some (None) | None -> odefa_ident
  in
  (* Get clause from var *)
  match Ast.Ident_map.Exceptionless.find odefa_ident' clause_map with
  | Some clause -> clause
  | None ->
    raise @@ Invalid_argument
      (Printf.sprintf
        "%s needs to have an associated clause."
        (Ast.show_ident odefa_ident))
;;

(** Helper function to recursively map natodefa expressions according to
    the expression-to-expression mapping (eg. records to lists or variants).
    We need a custom transformer function, rather than the one in utils, 
    because we need to first transform the expression, then recurse (whereas
    transform_expr and transform_expr_m do the other way around). *)
let rec on_expr_transformer transformer expr =
  let open On_ast in
  let recurse = on_expr_transformer transformer in
  let expr' = transformer expr in
  match expr' with
  | Int _ | Bool _ | Var _ | Input -> expr'
  | Record record ->
    let record' =
      record
      |> On_ast.Ident_map.enum
      |> Enum.map (fun (lbl, e) -> (lbl, recurse e))
      |> On_ast.Ident_map.of_enum
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
    let Funsig (fs_ident, fs_args, e_body) = fs in
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
  | List (e_lst) -> List (List.map recurse e_lst)
  | ListCons (e1, e2) -> ListCons (recurse e1, recurse e2)
  | Assert e -> Assert (recurse e)
  | Assume e -> Assume (recurse e)
;;

let get_natodefa_equivalent_expr mappings odefa_ident =
  let inst_map = mappings.odefa_instrument_vars_map in
  let odefa_on_map = mappings.odefa_var_to_natodefa_expr in
  let on_expr_map = mappings.natodefa_expr_to_expr in
  let on_ident_map = mappings.natodefa_var_to_var in
  (* Get pre-instrument var *)
  let odefa_ident' =
    match Ast.Ident_map.Exceptionless.find odefa_ident inst_map with
    | Some (Some pre_inst_ident) -> pre_inst_ident
    | Some (None) | None -> odefa_ident
  in
  (* Get natodefa expr from odefa var *)
  let natodefa_expr =
    try
      Ast.Ident_map.find odefa_ident' odefa_on_map
    with Not_found ->
      raise @@ Invalid_argument
        (Printf.sprintf
          "variable %s is not associated with any natodefa expr."
          (Ast.show_ident odefa_ident'))
  in
  (* Get any original natodefa exprs *)
  let on_expr_transform expr =
    match Expr_map.Exceptionless.find expr on_expr_map with
    | Some expr' -> expr'
    | None -> expr
  in
  let on_ident_transform expr =
    let open On_ast in
    let find_ident ident =
      Ident_map.find_default ident ident on_ident_map
    in
    let transform_funsig funsig =
      let (Funsig (fun_ident, arg_ident_list, body)) = funsig in
      let fun_ident' = find_ident fun_ident in
      let arg_ident_list' = List.map find_ident arg_ident_list in
      Funsig (fun_ident', arg_ident_list', body)
    in
    match expr with
    | Var ident -> Var (find_ident ident)
    | Function (ident_list, body) ->
      Function (List.map find_ident ident_list, body)
    | Let (ident, e1, e2) -> Let (find_ident ident, e1, e2)
    | LetFun (funsig, e) ->
      LetFun (transform_funsig funsig, e)
    | LetRecFun (funsig_list, e) ->
      LetRecFun (List.map transform_funsig funsig_list, e)
    | Match (e, pat_e_list) ->
      let transform_pattern pat =
        match pat with
        | RecPat record ->
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
          RecPat record'
        | VariantPat (vlbl, x) ->
          VariantPat (vlbl, find_ident x)
        | VarPat x ->
          VarPat (find_ident x)
        | LstDestructPat (x1, x2) ->
          LstDestructPat (find_ident x1, find_ident x2)
        | AnyPat | IntPat | BoolPat | FunPat | EmptyLstPat -> pat
      in
      let pat_e_list' =
        List.map
          (fun (pat, expr) -> (transform_pattern pat, expr))
          pat_e_list
      in
      Match (e, pat_e_list')
    | _ -> expr
  in
  natodefa_expr
  |> on_expr_transformer on_ident_transform
  |> on_expr_transformer on_expr_transform
;;

let get_type_from_idents mappings odefa_idents =
  let on_idents =
    odefa_idents
    |> Ast.Ident_set.enum
    |> Enum.map (fun (Ast.Ident lbl) -> On_ast.Ident lbl)
    |> On_ast.Ident_set.of_enum
  in
  let on_idents_type_map = mappings.natodefa_idents_to_types in
  match On_labels_map.Exceptionless.find on_idents on_idents_type_map with
  | Some typ -> typ
  | None -> RecType on_idents
;;

let is_natodefa mappings = mappings.is_natodefa;;

let is_var_instrumenting mappings odefa_ident =
  let inst_map = mappings.odefa_instrument_vars_map in
  Ast.Ident_map.mem odefa_ident inst_map
;;