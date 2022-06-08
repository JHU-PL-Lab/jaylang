open Batteries;;
open Jhupllib;;

open On_ast;;

module Original_expr_desc = struct
  include On_ast.Typed_expr_desc;;
  let pp = On_ast_pp.pp_expr_desc;;
end;;

module Intermediate_expr_desc = struct
  include On_ast.Semantic_typed_expr_desc;;
  let pp = On_ast_pp.pp_expr_desc;;
end;;

module Intermediate_expr_desc_map = struct
  module M = Map.Make(Intermediate_expr_desc);;
  include M;;
  include Pp_utils.Map_pp(M)(Intermediate_expr_desc);;
end;;

module Int_map = Map.Make(struct type t = int let compare = compare end)

type t = {
  error_to_natodefa_expr : sem_natodefa_edesc Ident_map.t;
  sem_to_syn : syn_natodefa_edesc Intermediate_expr_desc_map.t;
  error_to_expr_tag : int Ident_map.t;
  match_tag_to_error_id : ident Int_map.t;
}
;;

let empty = {
  error_to_natodefa_expr = Ident_map.empty;
  sem_to_syn = Intermediate_expr_desc_map.empty;
  error_to_expr_tag = Ident_map.empty;
  match_tag_to_error_id = Int_map.empty;
}
;;

let add_error_natodefa_expr_mapping mappings x e =
  let error_natodefa_expr_map = mappings.error_to_natodefa_expr in
  { mappings with 
    error_to_natodefa_expr = 
      Ident_map.add x e error_natodefa_expr_map;
  }
;;

let add_sem_syn_expr_mapping mappings sem syn =
  let sem_syn_expr_mapping = mappings.sem_to_syn in
  { mappings with 
    sem_to_syn = 
    Intermediate_expr_desc_map.add sem syn sem_syn_expr_mapping;
  }
;;

let add_error_expr_tag_mapping mappings err_id expr_tag =
  let error_expr_tag_mapping = mappings.error_to_expr_tag in
  { mappings with 
    error_to_expr_tag = 
      Ident_map.add err_id expr_tag error_expr_tag_mapping;
  }
;;

let add_match_tag_error_mapping mappings match_tag err_id =
  let match_tag_err_mapping = mappings.match_tag_to_error_id in
  { mappings with 
  match_tag_to_error_id = 
      Int_map.add match_tag err_id match_tag_err_mapping;
  }
;;

let transform_funsig 
  (f : 'a expr_desc -> 'b expr_desc) 
  (Funsig (fun_name, params, e) : 'a funsig) 
  : 'b funsig
  = 
  let e' = f e in
  Funsig (fun_name, params, e')
;;

let rec syn_natodefa_from_sem_natodefa 
  ton_on_maps (sem_edesc : sem_natodefa_edesc)
  : syn_natodefa_edesc =
  let og_tag = sem_edesc.tag in
  let expr = sem_edesc.body in
  match expr with
  | Int n -> {tag = og_tag; body = Int n}
  | Bool b -> {tag = og_tag; body = Bool b}
  | Var x -> {tag = og_tag; body = Var x}
  | Input ->  {tag = og_tag; body = Input}
  | Untouched s -> {tag = og_tag; body = Untouched s}
  (* TODO (Earl): Come back to here to add mappings to dictionary *)
  | TypeError x -> {tag = og_tag; body = TypeError x}
  | Function (id_lst, e) -> 
    let e' = syn_natodefa_from_sem_natodefa ton_on_maps e in
    {tag = og_tag; body = Function (id_lst, e')}
  | Appl (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Appl (e1', e2')}
  | Let (x, e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Let (x, e1', e2')}
  | LetRecFun (sig_lst, e) ->
    let sig_lst' = 
      sig_lst
      |> List.map (transform_funsig (syn_natodefa_from_sem_natodefa ton_on_maps))
    in
    let e' = syn_natodefa_from_sem_natodefa ton_on_maps e in
    {tag = og_tag; body = LetRecFun (sig_lst', e')}
  | LetFun (fun_sig, e) ->
    let fun_sig' = 
      transform_funsig (syn_natodefa_from_sem_natodefa ton_on_maps) fun_sig
    in
    let e' = syn_natodefa_from_sem_natodefa ton_on_maps e in
    {tag = og_tag; body = LetFun (fun_sig', e')}
  (* NOTE: In the ton_to_on transformation, we should have mapping for all exprs
     of the form:
     1. LetWithType ..
     2. LetFunWithType ..
     3. LetRecFunWithType ..
  *)
  | LetWithType _ | LetRecFunWithType _ | LetFunWithType _ ->
    begin
      let syn_edesc_opt = 
        Intermediate_expr_desc_map.find_opt sem_edesc ton_on_maps.sem_to_syn 
      in
      match syn_edesc_opt with
      | None -> 
        failwith "Should have a mapping!"
        (* let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
        let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
        let type_decl' = syn_natodefa_from_sem_natodefa ton_on_maps type_decl in
        {tag = og_tag; body = LetWithType (x, e1', e2', type_decl')} *)
      | Some ed -> ed
    end
  | Plus (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Plus (e1', e2')}
  | Minus (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Minus (e1', e2')}
  | Times (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Times (e1', e2')}
  | Divide (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Divide (e1', e2')}
  | Modulus (e1, e2) ->
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Modulus (e1', e2')}
  | Equal (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Equal (e1', e2')}
  | Neq (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Neq (e1', e2')}
  | LessThan (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = LessThan (e1', e2')}
  | Leq (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Leq (e1', e2')}
  | GreaterThan (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = GreaterThan (e1', e2')}
  | Geq (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Geq (e1', e2')}
  | And (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = And (e1', e2')}
  | Or (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = Or (e1', e2')}
  | Not e ->
    let e' = syn_natodefa_from_sem_natodefa ton_on_maps e in
    {tag = og_tag; body = Not e'}
  | If (e1, e2, e3) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    let e3' = syn_natodefa_from_sem_natodefa ton_on_maps e3 in
    {tag = og_tag; body = If (e1', e2', e3')}
  | Record m -> 
    let m' = 
      Ident_map.map (fun e -> (syn_natodefa_from_sem_natodefa ton_on_maps) e) m 
    in
    {tag = og_tag; body = Record m'}
  | RecordProj (e, l) -> 
    let e' = syn_natodefa_from_sem_natodefa ton_on_maps e in
    {tag = og_tag; body = RecordProj (e', l)}
  | Match (e, pattern_expr_lst) ->
    let e' = syn_natodefa_from_sem_natodefa ton_on_maps e in
    let mapper (pat, expr) =
      let expr' = syn_natodefa_from_sem_natodefa ton_on_maps expr in
      (pat, expr') 
    in
    let pattern_expr_lst' = 
      List.map mapper pattern_expr_lst
    in
    {tag = og_tag; body = Match (e', pattern_expr_lst')}
  | VariantExpr (lbl, e) -> 
    let e' = syn_natodefa_from_sem_natodefa ton_on_maps e in
    {tag = og_tag; body = VariantExpr (lbl, e')}
  | List expr_lst -> 
    let expr_lst' = 
      List.map (syn_natodefa_from_sem_natodefa ton_on_maps) expr_lst
    in
    {tag = og_tag; body = List expr_lst'}
  | ListCons (e1, e2) -> 
    let e1' = syn_natodefa_from_sem_natodefa ton_on_maps e1 in
    let e2' = syn_natodefa_from_sem_natodefa ton_on_maps e2 in
    {tag = og_tag; body = ListCons (e1', e2')}
  | Assert e -> 
    let e' = syn_natodefa_from_sem_natodefa ton_on_maps e in
    {tag = og_tag; body = Assert e'}
  | Assume e -> 
    let e' = syn_natodefa_from_sem_natodefa ton_on_maps e in
    {tag = og_tag; body = Assume e'}
;;

let rec sem_natodefa_from_on_err ton_on_maps (on_err_desc : core_natodefa_edesc) : sem_natodefa_edesc = 
  let on_err = on_err_desc.body in
  let og_tag = on_err_desc.tag in
  match on_err with
  | TypeError err_id ->
    (* let () = print_endline @@ show_ident err_id in *)
    let err_expr_op = 
      Ident_map.find_opt err_id ton_on_maps.error_to_natodefa_expr
    in
    (match err_expr_op with
    | Some err_expr -> 
      err_expr
    | None -> 
      failwith "sem_natodefa_from_on_err: unknown TypeError")
  | Int n -> {tag = og_tag; body = Int n} 
  | Bool b -> {tag = og_tag ; body = Bool b}
  | Var x -> {tag = og_tag; body = Var x}
  | Function (id_lst, f_expr) -> 
    {tag = og_tag; body = Function (id_lst, sem_natodefa_from_on_err ton_on_maps f_expr)}
  | Input -> {tag = og_tag; body = Input}
  | Appl (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Appl (e1', e2')}
  | Let (x, e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Let (x, e1', e2')}
  | LetRecFun (funsig_lst, e) -> 
    let funsig_lst' = 
      funsig_lst  
      |> List.map (transform_funsig (sem_natodefa_from_on_err ton_on_maps))
    in
    let e' = sem_natodefa_from_on_err ton_on_maps e in
    {tag = og_tag; body = LetRecFun (funsig_lst', e')}
  | LetFun (funsig, e) -> 
    let funsig' = funsig
      |> transform_funsig (sem_natodefa_from_on_err ton_on_maps)
    in
    let e' = sem_natodefa_from_on_err ton_on_maps e in
    {tag = og_tag; body = LetFun (funsig', e')}
  | Plus (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Plus (e1', e2')}
  | Minus (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Minus (e1', e2')}
  | Times (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Times (e1', e2')}
  | Divide (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Divide (e1', e2')}
  | Modulus (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Modulus (e1', e2')}
  | Equal (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Equal (e1', e2')}
  | Neq (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Neq (e1', e2')}
  | LessThan (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = LessThan (e1', e2')}
  | Leq (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Leq (e1', e2')}
  | GreaterThan (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = GreaterThan (e1', e2')}
  | Geq (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Geq (e1', e2')}
  | And (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = And (e1', e2')}
  | Or (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = Or (e1', e2')}
  | Not e -> 
    let e' = sem_natodefa_from_on_err ton_on_maps e in
    {tag = og_tag; body = Not (e')}
  | If (e1, e2, e3) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    let e3' = sem_natodefa_from_on_err ton_on_maps e3 in
    {tag = og_tag; body = If (e1', e2', e3')}
  | Record r -> 
    let r' = r
      |> Ident_map.map (sem_natodefa_from_on_err ton_on_maps)
      |> Ident_map.map (fun e -> e)
    in
    {tag = og_tag; body = Record r'}
  | RecordProj (e, l) -> 
    let e' = sem_natodefa_from_on_err ton_on_maps e in
    {tag = og_tag; body = RecordProj (e', l)}
  | Match (match_e, pat_expr_lst) -> 
    let match_e' = sem_natodefa_from_on_err ton_on_maps match_e in
    let pat_expr_lst' = 
      pat_expr_lst
      |> List.map 
        (fun (p, e) -> 
            let e' = sem_natodefa_from_on_err ton_on_maps e in 
            (p, e'))
    in 
    {tag = og_tag; body = Match (match_e', pat_expr_lst')}
  | VariantExpr (l, e)-> 
    let e' = sem_natodefa_from_on_err ton_on_maps e in
    {tag = og_tag; body = VariantExpr (l, e')}
  | List es ->
    let es' = es
    |> List.map (sem_natodefa_from_on_err ton_on_maps)
    in
    {tag = og_tag; body = List es'}
  | ListCons (e1, e2) -> 
    let e1' = sem_natodefa_from_on_err ton_on_maps e1 in
    let e2' = sem_natodefa_from_on_err ton_on_maps e2 in
    {tag = og_tag; body = ListCons (e1', e2')}
  | Assert e -> 
    let e' = sem_natodefa_from_on_err ton_on_maps e in
    {tag = og_tag; body = Assert (e')}  
  | Assume e -> 
    let e' = sem_natodefa_from_on_err ton_on_maps e in
    {tag = og_tag; body = Assume (e')}
  | Untouched s -> {tag = og_tag; body = Untouched s}
;;
