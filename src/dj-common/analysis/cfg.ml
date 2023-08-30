(* This is block-indexed control-flow-graph that serves two purposes
   1. query source code information including
   - find the block of an id
   - find the outer block of an id
   - find the previous clause(s)
   2. query static analysis information including
   - find the possible values of a cond variable
   - find the possible functions to call at a callsite
   - find the possible callsites to return at in a function body

   The implementation is done by
   1. create a map from block to all clauses details in that block
   (`Cfg.block_map_of_expr`)
   2. modify the details from the analysis
   (`Cfg_of_ddpa.annotate`)

   The struct will be stored in the global state as `block_map` and used later.
*)

open Core
open Jayil.Ast

type clause_cat = Direct | Fun | Cond | App of ident list
[@@deriving show { with_path = false }]

type tl_clause = { id : ident; cat : clause_cat; clause : clause [@opaque] }
[@@deriving show { with_path = false }]

type clause_list = tl_clause list [@@deriving show { with_path = false }]

type fun_block_info = { outer_id : ident; para : ident; callsites : ident list }
[@@deriving show { with_path = false }]

type cond_case_info = {
  outer_id : ident;
  cond : ident;
  condsite : ident;
  choice : bool;
  reachable : bool;
}
[@@deriving show { with_path = false }]

type block_kind = Main | Fun of fun_block_info | Cond of cond_case_info
[@@deriving show { with_path = false }]

type block = { id : Id.t; clauses : clause_list; kind : block_kind }
[@@deriving show { with_path = false }]

module Block = struct
  module T = struct
    type t = block

    let compare b1 b2 = Id.compare b1.id b2.id
    let equal b1 b2 = Id.equal b1.id b2.id
    let hash b = Id.hash b.id
    let sexp_of_t b = Id.sexp_of_t b.id
  end

  include T
  include Comparator.Make (T)

  let pp oc b = Id.pp oc b.id
end

type cond_both_info = { then_ : block option; else_ : block option }

type def_site =
  | At_clause of tl_clause
  | At_fun_para of bool * fun_block_info
  | At_chosen of cond_case_info
  | Lookup_mismatch

type t = block [@@deriving show { with_path = false }]

(* intra-block query *)

let cast_to_cond_block_info block =
  match block.kind with
  | Cond cb -> cb
  | _ -> failwith "cast_to_cond_block_info"

let cast_to_fun_block_info block =
  match block.kind with Fun fb -> fb | _ -> failwith "cast_to_fun_block_info"

let ret_of block =
  let clauses = block.clauses in
  (List.last_exn clauses).id

let find_block_by_id x block_map =
  block_map |> Ident_map.values |> bat_list_of_enum
  |> List.find_map ~f:(fun block ->
         let is_reachable =
           match block.kind with Cond cb -> cb.reachable | _ -> true
         in
         if is_reachable
         then
           if List.exists ~f:(fun tc -> Ident.equal tc.id x) block.clauses
           then Some block
           else None
         else None)
  |> Option.value_exn

let find_cond_blocks x block_map =
  let cond_case_infos =
    block_map |> Ident_map.values |> bat_list_of_enum
    |> List.filter_map ~f:(fun block ->
           match block.kind with
           | Cond cb ->
               if Id.equal cb.condsite x
               then Some (cb.choice, block, cb)
               else None
           | _ -> None)
  in
  match cond_case_infos with
  | [ (true, block_true, cb_true); (false, block_false, cb_false) ]
  | [ (false, block_false, cb_false); (true, block_true, cb_true) ] ->
      {
        then_ = (if cb_true.reachable then Some block_true else None);
        else_ = (if cb_false.reachable then Some block_false else None);
      }
  | _ -> failwith "find_cond_blocks must find two blocks"

let clause_of_x block x =
  List.find ~f:(fun tc -> Ident.equal tc.id x) block.clauses

let clause_of_x_exn block x = clause_of_x block x |> Option.value_exn

let clause_body_of_x block x =
  let c = clause_of_x_exn block x in
  let (Clause (_, cv)) = c.clause in
  cv

let clauses_before_x block x =
  match clause_of_x block x with
  | Some _ ->
      List.fold_until ~init:[]
        ~f:(fun acc tc ->
          if Ident.equal tc.id x then Stop acc else Continue (tc :: acc))
        ~finish:List.rev block.clauses
  | None -> []

let clauses_of_expr e =
  let (Expr clauses) = e in
  List.fold_left clauses ~init:[] ~f:(fun cs (Clause (Var (cid, _), b) as c) ->
      let c' =
        match b with
        | Appl_body (_, _) -> { id = cid; cat = App []; clause = c }
        | Conditional_body (Var (_, _), _, _) ->
            { id = cid; cat = Cond (* [] *); clause = c }
        | Value_body (Value_function _) -> { id = cid; cat = Fun; clause = c }
        | _ -> { id = cid; cat = Direct; clause = c }
      in
      cs @ [ c' ])

(* above-block query *)

let outer_block map block =
  let outer_id =
    match block.kind with
    | Main -> failwith "no outer_id for main block"
    | Fun fb -> fb.outer_id
    | Cond cb -> cb.outer_id
  in
  Ident_map.find outer_id map

let fun_info_of_callsite map callsite =
  let callsite_block = find_block_by_id callsite map in
  let tc = clause_of_x_exn callsite_block callsite in
  let x', x'', x''' =
    match tc.clause with
    | Clause (Var (x', _), Appl_body (Var (x'', _), Var (x''', _))) ->
        (x', x'', x''')
    | _ -> failwith "incorrect clause for callsite"
  in
  (callsite_block, x', x'', x''')

let is_before map x1 x2 =
  let open Continue_or_stop in
  let block = find_block_by_id x1 map in
  List.fold_until block.clauses ~init:false
    ~f:(fun _ x ->
      if Id.equal x.id x1
      then Stop true
      else if Id.equal x.id x2
      then Stop false
      else Continue true)
    ~finish:Fn.id

(* for analysis use *)

let update_clauses f block = { block with clauses = f block.clauses }

let add_dsts dst0 dsts =
  if List.mem dsts dst0 ~equal:Ident.equal then dsts else dst0 :: dsts

let update_id_dst block id dst0 =
  let add_dst_in_clause (tc : tl_clause) =
    if Ident.equal tc.id id
    then
      {
        tc with
        cat =
          (match tc.cat with
          | App dsts -> App (add_dsts dst0 dsts)
          (* | Cond dsts -> Cond (add_dsts dst0 dsts) *)
          | other -> other);
      }
    else tc
  in
  update_clauses (List.map ~f:add_dst_in_clause) block

let add_id_dst tl_map site_x def_x =
  let tl = find_block_by_id site_x tl_map in
  let tl' = update_id_dst tl site_x def_x in
  Ident_map.add tl.id tl' tl_map

(* ddpa-related cfg update*)
let _add_callsite site block =
  match block.kind with
  | Fun b ->
      { block with kind = Fun { b with callsites = site :: b.callsites } }
  | _ -> failwith "wrong precondition to call add_callsite"

let add_callsite tl_map f_def site =
  let tl = Ident_map.find f_def tl_map in
  let tl' = _add_callsite site tl in
  Ident_map.add f_def tl' tl_map

let set_block_unreachable tl_map block =
  let cond_block_info = cast_to_cond_block_info block in
  let cond_block_info' = { cond_block_info with reachable = false } in
  let block' = { block with kind = Cond cond_block_info' } in
  tl_map := Ident_map.add block.id block' !tl_map
