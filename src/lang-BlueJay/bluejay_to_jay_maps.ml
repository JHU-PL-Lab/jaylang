open Batteries
open Jhupllib
open Bluejay_ast_internal

module Intermediate_expr_desc = struct
  include Bluejay_ast_internal.Semantic_typed_expr_desc

  let pp = Bluejay_ast_internal_pp.pp_expr_desc
end

module Core_expr_desc = struct
  include Bluejay_ast_internal.Core_expr_desc

  let pp = Bluejay_ast_internal_pp.pp_expr_desc
end

module Intermediate_expr_desc_map = struct
  module M = Map.Make (Intermediate_expr_desc)
  include M
  include Pp_utils.Map_pp (M) (Intermediate_expr_desc)
end

module Core_expr_desc_map = struct
  module M = Map.Make (Core_expr_desc)
  include M
  include Pp_utils.Map_pp (M) (Core_expr_desc)
end

module Int_map = Map.Make (struct
  type t = int

  let compare = compare
end)

type t = {
  error_to_bluejay_expr : sem_bluejay_edesc Ident_map.t;
  sem_to_syn : syn_bluejay_edesc Intermediate_expr_desc_map.t;
  core_to_sem : sem_bluejay_edesc Core_expr_desc_map.t;
  error_to_expr_tag : int Intermediate_expr_desc_map.t;
  error_to_rec_fun_type : sem_bluejay_edesc Ident_map.t;
  error_to_value_expr : sem_bluejay_edesc Intermediate_expr_desc_map.t;
  syn_tags : int list;
}

let empty =
  {
    error_to_bluejay_expr = Ident_map.empty;
    sem_to_syn = Intermediate_expr_desc_map.empty;
    core_to_sem = Core_expr_desc_map.empty;
    error_to_expr_tag = Intermediate_expr_desc_map.empty;
    error_to_rec_fun_type = Ident_map.empty;
    error_to_value_expr = Intermediate_expr_desc_map.empty;
    syn_tags = [];
  }

let add_error_bluejay_expr_mapping mappings x e =
  let error_bluejay_expr_map = mappings.error_to_bluejay_expr in
  {
    mappings with
    error_to_bluejay_expr = Ident_map.add x e error_bluejay_expr_map;
  }

let add_sem_syn_expr_mapping mappings sem syn =
  let sem_syn_expr_mapping = mappings.sem_to_syn in
  {
    mappings with
    sem_to_syn = Intermediate_expr_desc_map.add sem syn sem_syn_expr_mapping;
  }

let add_core_sem_expr_mapping mappings core sem =
  let core_sem_expr_mapping = mappings.core_to_sem in
  {
    mappings with
    core_to_sem = Core_expr_desc_map.add core sem core_sem_expr_mapping;
  }

let add_error_expr_tag_mapping mappings err_expr expr_tag =
  let error_expr_tag_mapping = mappings.error_to_expr_tag in
  {
    mappings with
    error_to_expr_tag =
      Intermediate_expr_desc_map.add err_expr expr_tag error_expr_tag_mapping;
  }

let add_error_rec_fun_type_mapping mappings x e =
  let error_rec_fun_type_map = mappings.error_to_rec_fun_type in
  {
    mappings with
    error_to_rec_fun_type = Ident_map.add x e error_rec_fun_type_map;
  }

let add_error_value_expr_mapping mappings err_e v_e =
  let error_to_value_expr_map = mappings.error_to_value_expr in
  {
    mappings with
    error_to_value_expr =
      Intermediate_expr_desc_map.add err_e v_e error_to_value_expr_map;
  }

let transform_funsig (f : 'a expr_desc -> 'b expr_desc)
    (Funsig (fun_name, params, e) : 'a funsig) : 'b funsig =
  let e' = f e in
  Funsig (fun_name, params, e')

let find_all_syn_tags bluejay_jay_maps (edesc : syn_bluejay_edesc) =
  let rec loop acc cur =
    let cur_tag = cur.tag in
    let expr = cur.body in
    match expr with
    | Int _ | Bool _ | Var _ | Input | TypeError _ -> cur_tag :: acc
    | Function (_, e)
    | Not e
    | VariantExpr (_, e)
    | RecordProj (e, _)
    | Assert e
    | Assume e ->
        let acc' = cur_tag :: acc in
        loop acc' e
    | Appl (e1, e2)
    | Let (_, e1, e2)
    | Plus (e1, e2)
    | Minus (e1, e2)
    | Times (e1, e2)
    | Divide (e1, e2)
    | Modulus (e1, e2)
    | Equal (e1, e2)
    | LessThan (e1, e2)
    | Leq (e1, e2)
    | GreaterThan (e1, e2)
    | Geq (e1, e2)
    | And (e1, e2)
    | Or (e1, e2)
    | Neq (e1, e2)
    | ListCons (e1, e2)
    | TypeArrow (e1, e2)
    | TypeArrowD ((_, e1), e2)
    | TypeSet (e1, e2)
    | TypeUnion (e1, e2)
    | TypeIntersect (e1, e2) ->
        let acc' = cur_tag :: acc in
        let acc'' = loop acc' e1 in
        loop acc'' e2
    | LetRecFun (sig_lst, e) ->
        let acc' =
          List.fold (fun acc (Funsig (_, _, e)) -> loop acc e) acc sig_lst
        in
        loop acc' e
    | LetFun (Funsig (_, _, fed), e) ->
        let acc' = loop acc fed in
        loop acc' e
    | LetWithType (_, e1, e2, t) ->
        let acc' = loop acc e1 in
        let acc'' = loop acc' e2 in
        loop acc'' t
    | LetRecFunWithType (sig_lst, e, ts) ->
        let acc' =
          List.fold (fun acc (Funsig (_, _, e)) -> loop acc e) acc sig_lst
        in
        let acc'' = List.fold (fun acc e -> loop acc e) acc' ts in
        loop acc'' e
    | LetFunWithType (Funsig (_, _, fed), e, t) ->
        let acc' = loop acc fed in
        let acc'' = loop acc' e in
        loop acc'' t
    | If (e1, e2, e3) ->
        let acc' = loop acc e1 in
        let acc'' = loop acc' e2 in
        loop acc'' e3
    | Record m | TypeRecord m ->
        Ident_map.fold (fun _ e acc -> loop acc e) m acc
    | Match (e, pattern_expr_lst) ->
        let acc' =
          List.fold (fun acc (_, e) -> loop acc e) acc pattern_expr_lst
        in
        loop acc' e
    | List expr_lst -> List.fold (fun acc e -> loop acc e) acc expr_lst
    | TypeInt | TypeBool | TypeVar _ -> cur_tag :: acc
    | TypeList e | TypeRecurse (_, e) -> loop acc e
  in
  let all_syn_tags = loop [] edesc in
  { bluejay_jay_maps with syn_tags = all_syn_tags }

let rec syn_bluejay_from_sem_bluejay bluejay_jay_maps
    (sem_edesc : sem_bluejay_edesc) : syn_bluejay_edesc =
  let entry_opt =
    Intermediate_expr_desc_map.find_opt sem_edesc bluejay_jay_maps.sem_to_syn
  in
  match entry_opt with
  | Some expr' -> expr'
  | None -> (
      let og_tag = sem_edesc.tag in
      let expr = sem_edesc.body in
      match expr with
      | Int n -> { tag = og_tag; body = Int n }
      | Bool b -> { tag = og_tag; body = Bool b }
      | Var x -> { tag = og_tag; body = Var x }
      | Input -> { tag = og_tag; body = Input }
      (* | Untouched s -> {tag = og_tag; body = Untouched s} *)
      | TypeError x -> { tag = og_tag; body = TypeError x }
      | Function (id_lst, e) ->
          let e' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = Function (id_lst, e') }
      | Appl (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Appl (e1', e2') }
      | Let (x, e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Let (x, e1', e2') }
      | LetRecFun (sig_lst, e) ->
          let sig_lst' =
            sig_lst
            |> List.map
                 (transform_funsig
                    (syn_bluejay_from_sem_bluejay bluejay_jay_maps))
          in
          let e' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = LetRecFun (sig_lst', e') }
      | LetFun (fun_sig, e) ->
          let fun_sig' =
            transform_funsig
              (syn_bluejay_from_sem_bluejay bluejay_jay_maps)
              fun_sig
          in
          let e' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = LetFun (fun_sig', e') }
      (* NOTE: In the syn -> sem transformation, we should have mapping for all
         exprs of the form:
         1. LetWithType ..
         2. LetFunWithType ..
         3. LetRecFunWithType .. *)
      | LetWithType _ | LetRecFunWithType _ | LetFunWithType _ ->
          failwith "Should have a mapping!"
      | Plus (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Plus (e1', e2') }
      | Minus (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Minus (e1', e2') }
      | Times (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Times (e1', e2') }
      | Divide (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Divide (e1', e2') }
      | Modulus (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Modulus (e1', e2') }
      | Equal (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Equal (e1', e2') }
      | Neq (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Neq (e1', e2') }
      | LessThan (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = LessThan (e1', e2') }
      | Leq (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Leq (e1', e2') }
      | GreaterThan (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = GreaterThan (e1', e2') }
      | Geq (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Geq (e1', e2') }
      | And (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = And (e1', e2') }
      | Or (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Or (e1', e2') }
      | Not e ->
          let e' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = Not e' }
      | If (e1, e2, e3) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          let e3' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e3 in
          { tag = og_tag; body = If (e1', e2', e3') }
      | Record m ->
          let m' =
            Ident_map.map
              (fun e -> (syn_bluejay_from_sem_bluejay bluejay_jay_maps) e)
              m
          in
          { tag = og_tag; body = Record m' }
      | RecordProj (e, l) ->
          let e' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = RecordProj (e', l) }
      | Match (e, pattern_expr_lst) ->
          let e' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e in
          let mapper (pat, expr) =
            let expr' = syn_bluejay_from_sem_bluejay bluejay_jay_maps expr in
            (pat, expr')
          in
          let pattern_expr_lst' = List.map mapper pattern_expr_lst in
          { tag = og_tag; body = Match (e', pattern_expr_lst') }
      | VariantExpr (lbl, e) ->
          let e' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = VariantExpr (lbl, e') }
      | List expr_lst ->
          let expr_lst' =
            List.map (syn_bluejay_from_sem_bluejay bluejay_jay_maps) expr_lst
          in
          { tag = og_tag; body = List expr_lst' }
      | ListCons (e1, e2) ->
          let e1' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e1 in
          let e2' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = ListCons (e1', e2') }
      | Assert e ->
          let e' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = Assert e' }
      | Assume e ->
          let e' = syn_bluejay_from_sem_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = Assume e' })

let rec sem_bluejay_from_core_bluejay bluejay_jay_maps
    (on_err_desc : core_bluejay_edesc) : sem_bluejay_edesc =
  match
    Core_expr_desc_map.Exceptionless.find on_err_desc
      bluejay_jay_maps.core_to_sem
  with
  | Some expr' -> expr'
  | None -> (
      let on_err = on_err_desc.body in
      let og_tag = on_err_desc.tag in
      match on_err with
      | TypeError err_id -> (
          let err_expr_op =
            Ident_map.find_opt err_id bluejay_jay_maps.error_to_bluejay_expr
          in
          match err_expr_op with
          | Some err_expr -> err_expr
          | None -> failwith "sem_bluejay_from_core_bluejay: unknown TypeError")
      | Int n -> { tag = og_tag; body = Int n }
      | Bool b -> { tag = og_tag; body = Bool b }
      | Var x -> { tag = og_tag; body = Var x }
      | Function (id_lst, f_expr) ->
          {
            tag = og_tag;
            body =
              Function
                (id_lst, sem_bluejay_from_core_bluejay bluejay_jay_maps f_expr);
          }
      | Input -> { tag = og_tag; body = Input }
      | Appl (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Appl (e1', e2') }
      | Let (x, e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Let (x, e1', e2') }
      | LetRecFun (funsig_lst, e) ->
          let funsig_lst' =
            funsig_lst
            |> List.map
                 (transform_funsig
                    (sem_bluejay_from_core_bluejay bluejay_jay_maps))
          in
          let e' = sem_bluejay_from_core_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = LetRecFun (funsig_lst', e') }
      | LetFun (funsig, e) ->
          let funsig' =
            funsig
            |> transform_funsig (sem_bluejay_from_core_bluejay bluejay_jay_maps)
          in
          let e' = sem_bluejay_from_core_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = LetFun (funsig', e') }
      | Plus (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Plus (e1', e2') }
      | Minus (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Minus (e1', e2') }
      | Times (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Times (e1', e2') }
      | Divide (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Divide (e1', e2') }
      | Modulus (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Modulus (e1', e2') }
      | Equal (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Equal (e1', e2') }
      | Neq (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Neq (e1', e2') }
      | LessThan (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = LessThan (e1', e2') }
      | Leq (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Leq (e1', e2') }
      | GreaterThan (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = GreaterThan (e1', e2') }
      | Geq (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Geq (e1', e2') }
      | And (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = And (e1', e2') }
      | Or (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = Or (e1', e2') }
      | Not e ->
          let e' = sem_bluejay_from_core_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = Not e' }
      | If (e1, e2, e3) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          let e3' = sem_bluejay_from_core_bluejay bluejay_jay_maps e3 in
          { tag = og_tag; body = If (e1', e2', e3') }
      | Record r ->
          let r' =
            r
            |> Ident_map.map (sem_bluejay_from_core_bluejay bluejay_jay_maps)
            |> Ident_map.map (fun e -> e)
          in
          { tag = og_tag; body = Record r' }
      | RecordProj (e, l) ->
          let e' = sem_bluejay_from_core_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = RecordProj (e', l) }
      | Match (match_e, pat_expr_lst) ->
          let match_e' =
            sem_bluejay_from_core_bluejay bluejay_jay_maps match_e
          in
          let pat_expr_lst' =
            pat_expr_lst
            |> List.map (fun (p, e) ->
                   let e' = sem_bluejay_from_core_bluejay bluejay_jay_maps e in
                   (p, e'))
          in
          { tag = og_tag; body = Match (match_e', pat_expr_lst') }
      | VariantExpr (l, e) ->
          let e' = sem_bluejay_from_core_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = VariantExpr (l, e') }
      | List es ->
          let es' =
            es |> List.map (sem_bluejay_from_core_bluejay bluejay_jay_maps)
          in
          { tag = og_tag; body = List es' }
      | ListCons (e1, e2) ->
          let e1' = sem_bluejay_from_core_bluejay bluejay_jay_maps e1 in
          let e2' = sem_bluejay_from_core_bluejay bluejay_jay_maps e2 in
          { tag = og_tag; body = ListCons (e1', e2') }
      | Assert e ->
          let e' = sem_bluejay_from_core_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = Assert e' }
      | Assume e ->
          let e' = sem_bluejay_from_core_bluejay bluejay_jay_maps e in
          { tag = og_tag; body = Assume e' })
(* | Untouched s -> {tag = og_tag; body = Untouched s} *)

let get_syn_nat_equivalent_expr bluejay_jay_maps
    (expr : Bluejay_ast_internal.core_bluejay_edesc) =
  expr
  |> sem_bluejay_from_core_bluejay bluejay_jay_maps
  |> syn_bluejay_from_sem_bluejay bluejay_jay_maps

let get_core_expr_from_sem_expr bluejay_jay_maps sem_expr =
  Core_expr_desc_map.fold
    (fun core_ed sem_ed acc ->
      if sem_expr.tag = sem_ed.tag then Some core_ed else acc)
    bluejay_jay_maps.core_to_sem None

let get_value_expr_from_sem_expr bluejay_jay_maps sem_expr =
  Intermediate_expr_desc_map.find_opt sem_expr
    bluejay_jay_maps.error_to_value_expr

let rec replace_type (t_desc : syn_bluejay_edesc) (new_t : syn_bluejay_edesc)
    (tag : int) : syn_bluejay_edesc =
  let cur_tag = t_desc.tag in
  let t = t_desc.body in
  if tag = cur_tag
  then
    match t with
    (* TODO: HACK *)
    | TypeSet (td, _) -> (
        match new_t.body with
        | TypeError _ -> new_expr_desc @@ TypeSet (td, new_t)
        | _ -> new_t)
    | _ -> new_t
  else
    let transform_funsig (Funsig (fid, args, fe_desc)) =
      Funsig (fid, args, replace_type fe_desc new_t tag)
    in
    let t' =
      match t with
      | Int _ | Bool _ | Var _ | Input | TypeError _ -> t
      | Function (args, fe_desc) ->
          Function (args, replace_type fe_desc new_t tag)
      | Appl (ed1, ed2) ->
          Appl (replace_type ed1 new_t tag, replace_type ed2 new_t tag)
      | Let (x, ed1, ed2) ->
          Let (x, replace_type ed1 new_t tag, replace_type ed2 new_t tag)
      | LetRecFun (funsigs, e_desc) ->
          let funsigs' = List.map transform_funsig funsigs in
          let e_desc' = replace_type e_desc new_t tag in
          LetRecFun (funsigs', e_desc')
      | LetFun (funsig, e_desc) ->
          let funsig' = transform_funsig funsig in
          let e_desc' = replace_type e_desc new_t tag in
          LetFun (funsig', e_desc')
      | LetWithType (x, e1_desc, e2_desc, e3_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          let e3_desc' = replace_type e3_desc new_t tag in
          LetWithType (x, e1_desc', e2_desc', e3_desc')
      | LetRecFunWithType (funsigs, e_desc, ts) ->
          let funsigs' = List.map transform_funsig funsigs in
          let e_desc' = replace_type e_desc new_t tag in
          let ts' = List.map (fun ed -> replace_type ed new_t tag) ts in
          LetRecFunWithType (funsigs', e_desc', ts')
      | LetFunWithType (funsig, e_desc, t) ->
          let funsig' = transform_funsig funsig in
          let e_desc' = replace_type e_desc new_t tag in
          let t' = replace_type t new_t tag in
          LetFunWithType (funsig', e_desc', t')
      | Plus (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Plus (e1_desc', e2_desc')
      | Minus (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Minus (e1_desc', e2_desc')
      | Times (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Times (e1_desc', e2_desc')
      | Divide (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Divide (e1_desc', e2_desc')
      | Modulus (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Modulus (e1_desc', e2_desc')
      | Equal (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Equal (e1_desc', e2_desc')
      | Neq (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Neq (e1_desc', e2_desc')
      | LessThan (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          LessThan (e1_desc', e2_desc')
      | Leq (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Leq (e1_desc', e2_desc')
      | GreaterThan (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          GreaterThan (e1_desc', e2_desc')
      | Geq (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Geq (e1_desc', e2_desc')
      | And (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          And (e1_desc', e2_desc')
      | Or (e1_desc, e2_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          Or (e1_desc', e2_desc')
      | Not e_desc ->
          let e_desc' = replace_type e_desc new_t tag in
          Not e_desc'
      | If (e1_desc, e2_desc, e3_desc) ->
          let e1_desc' = replace_type e1_desc new_t tag in
          let e2_desc' = replace_type e2_desc new_t tag in
          let e3_desc' = replace_type e3_desc new_t tag in
          If (e1_desc', e2_desc', e3_desc')
      | Record ed_map ->
          let ed_map' =
            Ident_map.map (fun ed -> replace_type ed new_t tag) ed_map
          in
          Record ed_map'
      | RecordProj (e_desc, l) ->
          let e_desc' = replace_type e_desc new_t tag in
          RecordProj (e_desc', l)
      | Match (me_desc, ped_lst) ->
          let me_desc' = replace_type me_desc new_t tag in
          let ped_lst' =
            List.map (fun (p, ped) -> (p, replace_type ped new_t tag)) ped_lst
          in
          Match (me_desc', ped_lst')
      | VariantExpr (vl, e_desc) ->
          let e_desc' = replace_type e_desc new_t tag in
          VariantExpr (vl, e_desc')
      | List ts ->
          let ts' = List.map (fun t -> replace_type t new_t tag) ts in
          List ts'
      | ListCons (hd_desc, tl_desc) ->
          let hd_desc' = replace_type hd_desc new_t tag in
          let tl_desc' = replace_type tl_desc new_t tag in
          ListCons (hd_desc', tl_desc')
      | Assert e_desc ->
          let e_desc' = replace_type e_desc new_t tag in
          Assert e_desc'
      | Assume e_desc ->
          let e_desc' = replace_type e_desc new_t tag in
          Assume e_desc'
      | TypeVar _ | TypeInt | TypeBool -> t
      | TypeRecord td_map ->
          let td_map' =
            Ident_map.map (fun td -> replace_type td new_t tag) td_map
          in
          TypeRecord td_map'
      | TypeList td ->
          let td' = replace_type td new_t tag in
          TypeList td'
      | TypeArrow (td1, td2) ->
          let td1' = replace_type td1 new_t tag in
          let td2' = replace_type td2 new_t tag in
          TypeArrow (td1', td2')
      | TypeArrowD ((tid, td1), td2) ->
          let td1' = replace_type td1 new_t tag in
          let td2' = replace_type td2 new_t tag in
          TypeArrowD ((tid, td1'), td2')
      | TypeSet (td, pred) ->
          let td' = replace_type td new_t tag in
          let pred' = replace_type pred new_t tag in
          TypeSet (td', pred')
      | TypeUnion (td1, td2) ->
          let td1' = replace_type td1 new_t tag in
          let td2' = replace_type td2 new_t tag in
          TypeUnion (td1', td2')
      | TypeIntersect (td1, td2) ->
          let td1' = replace_type td1 new_t tag in
          let td2' = replace_type td2 new_t tag in
          TypeIntersect (td1', td2')
      | TypeRecurse (tv, td) ->
          let td' = replace_type td new_t tag in
          TypeRecurse (tv, td')
    in
    { tag = cur_tag; body = t' }

let sem_from_syn (bluejay_jay_maps : t) (syn_expr : syn_bluejay_edesc) :
    sem_bluejay_edesc =
  let mappings = bluejay_jay_maps.sem_to_syn in
  let res_opt =
    Intermediate_expr_desc_map.fold
      (fun k v acc ->
        if Bluejay_ast_internal.equal_expr_desc v syn_expr then Some k else acc)
      mappings None
  in
  match res_opt with
  | Some res -> res
  | None ->
      failwith
      @@ "Doesn't have a mapping from this syntatic bluejay expression!"
