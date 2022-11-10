open Core
open Jayil.Ast
open Ddpa
open Ddpa_abstract_ast
open Ddpa_graph
open Ddpa_helper

type clause_cat = Direct | Fun | App of ident list | Cond of ident list
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
  possible : bool;
  choice : bool;
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

let cast_to_cond_block_info block =
  match block.kind with
  | Cond cb -> cb
  | _ -> failwith "cast_to_cond_block_info"

let cast_to_fun_block_info block =
  match block.kind with Fun fb -> fb | _ -> failwith "cast_to_fun_block_info"

let outer_block block map =
  let outer_id =
    match block.kind with
    | Main -> failwith "no outer_id for main block"
    | Fun fb -> fb.outer_id
    | Cond cb -> cb.outer_id
  in
  Ident_map.find outer_id map

let ret_of block =
  let clauses = block.clauses in
  (List.last_exn clauses).id

let update_clauses f block = { block with clauses = f block.clauses }

let find_block_by_id x block_map =
  block_map |> Ident_map.values |> bat_list_of_enum
  |> List.find_map ~f:(fun block ->
         let is_possible =
           match block.kind with Cond cb -> cb.possible | _ -> true
         in
         if is_possible
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
        then_ = (if cb_true.possible then Some block_true else None);
        else_ = (if cb_false.possible then Some block_false else None);
      }
  | _ -> failwith "find_cond_blocks must find two blocks"

let clause_of_x block x =
  List.find ~f:(fun tc -> Ident.equal tc.id x) block.clauses

let clause_of_x_exn block x =
  List.find_exn ~f:(fun tc -> Ident.equal tc.id x) block.clauses

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

let update_id_dst id dst0 block =
  let add_dsts dst0 dsts =
    if List.mem dsts dst0 ~equal:Ident.equal then dsts else dst0 :: dsts
  in
  let add_dst_in_clause (tc : tl_clause) =
    if Ident.equal tc.id id
    then
      {
        tc with
        cat =
          (match tc.cat with
          | App dsts -> App (add_dsts dst0 dsts)
          | Cond dsts -> Cond (add_dsts dst0 dsts)
          | other -> other);
      }
    else tc
  in
  update_clauses (List.map ~f:add_dst_in_clause) block

let add_id_dst site_x def_x tl_map =
  let tl = find_block_by_id site_x tl_map in
  let tl' = update_id_dst site_x def_x tl in
  Ident_map.add tl.id tl' tl_map

let make_cond_block_possible tl_map acls cfg =
  let cond_site, possible =
    match acls with
    | [
     Unannotated_clause (Abs_clause (Abs_var cond_site, Abs_conditional_body _));
     choice_clause;
    ] ->
        let choice = find_cond_choice choice_clause cfg in
        (cond_site, Some choice)
    | [
     Unannotated_clause (Abs_clause (Abs_var cond_site, Abs_conditional_body _));
     _clause1;
     _clause2;
    ] ->
        (cond_site, None)
    | _ -> failwith "wrong precondition to call"
  in
  let make_block_impossible block =
    let cond_block_info = cast_to_cond_block_info block in
    let cond_block_info' = { cond_block_info with possible = false } in
    let block' = { block with kind = Cond cond_block_info' } in
    tl_map := Ident_map.add block.id block' !tl_map
  in

  Fmt.pr "cond_site: %a, possible: %a" Jayil.Ast_pp.pp_ident cond_site
    (Fmt.Dump.option Fmt.bool) possible ;

  let cond_both = find_cond_blocks cond_site !tl_map in
  match possible with
  | Some beta ->
      let beta_block =
        if beta
        then Option.value_exn cond_both.else_
        else Option.value_exn cond_both.then_
      in
      make_block_impossible beta_block
  | None -> ()

let _add_callsite site block =
  match block.kind with
  | Fun b ->
      { block with kind = Fun { b with callsites = site :: b.callsites } }
  | _ -> failwith "wrong precondition to call add_callsite"

let add_callsite f_def site tl_map =
  let tl = Ident_map.find f_def tl_map in
  let tl' = _add_callsite site tl in
  Ident_map.add f_def tl' tl_map

let clauses_of_expr e =
  let (Expr clauses) = e in
  List.fold_left clauses ~init:[] ~f:(fun cs (Clause (Var (cid, _), b) as c) ->
      let c' =
        match b with
        | Appl_body (_, _) -> { id = cid; cat = App []; clause = c }
        | Conditional_body (Var (_, _), _, _) ->
            { id = cid; cat = Cond []; clause = c }
        | Value_body (Value_function _) -> { id = cid; cat = Fun; clause = c }
        | _ -> { id = cid; cat = Direct; clause = c }
      in
      cs @ [ c' ])

let block_map_of_expr e : t Ident_map.t =
  let map = ref Ident_map.empty in

  let main_block =
    let clauses = clauses_of_expr e in
    { id = Id.main_block; clauses; kind = Main }
  in
  map := Ident_map.add Id.main_block main_block !map ;

  let rec loop outer_id e =
    let (Expr clauses) = e in
    let handle_clause = function
      | Clause
          ( Var (cid, _),
            Value_body (Value_function (Function_value (Var (para, _), fbody)))
          ) ->
          let clauses = clauses_of_expr fbody in
          let block =
            { id = cid; clauses; kind = Fun { outer_id; para; callsites = [] } }
          in
          map := Ident_map.add cid block !map ;
          loop cid fbody
      | Clause (Var (cid, _), Conditional_body (Var (cond, _), e1, e2)) ->
          let make_block e beta =
            let clauses = clauses_of_expr e in
            {
              id = Id.cond_block_id cid beta;
              clauses;
              kind =
                Cond
                  {
                    outer_id;
                    condsite = cid;
                    cond;
                    possible = true;
                    choice = beta;
                  };
            }
          in
          let block_then = make_block e1 true in
          let block_else = make_block e2 false in
          map := Ident_map.add block_then.id block_then !map ;
          map := Ident_map.add block_else.id block_else !map ;
          loop block_then.id e1 ;
          loop block_else.id e2
      | _ -> ()
    in
    List.iter clauses ~f:handle_clause
  in

  loop Id.main_block e ;
  !map

let cfg_of e =
  let open Ddpa in
  let conf : (module Ddpa_context_stack.Context_stack) =
    (module Ddpa_single_element_stack.Stack)
  in
  let module Stack = (val conf) in
  let module Analysis = Ddpa_analysis.Make (Stack) in
  e |> Analysis.create_initial_analysis |> Analysis.perform_full_closure
  |> Analysis.cfg_of_analysis

(* we cannot use block map to represent the dynamic call graph/stack.
   the point is for one block, we can have a full version and a partial version
   at the same time.
   For this, we may set the original or annotated source code as a (static) map
   and use another data structure for dynamic
*)

(* annotate block from the ddpa cfg.
   for call-site `s = e1 e2`, annotate e1 with the real function def_var
   for cond-site `s = c ? e1 : e2`, replace s with
*)

let annotate e pt : block Ident_map.t =
  let map = ref (block_map_of_expr e)
  and cfg = cfg_of e
  (* and id_first = first_var e *)
  and ret_to_fun_def_map = make_ret_to_fun_def_mapping e
  and para_to_fun_def_map = make_para_to_fun_def_mapping e in
  let pt_clause = Ident_map.find pt (clause_mapping e) in
  (* let is_abort_clause =
       match pt_clause with Clause (_, Abort_body) -> true | _ -> false
     in *)
  let acl = Unannotated_clause (lift_clause pt_clause) in

  let visited = ref Annotated_clause_set.empty in
  let rec loop acl is_main_track : unit =
    if Annotated_clause_set.mem acl !visited
    then ()
    else (
      visited := Annotated_clause_set.add acl !visited ;

      let prev_acls = preds_l acl cfg in

      Fmt.pr "cls = %a\n#prev = %d\nis_cond=%B\n\n"
        Ddpa_abstract_ast.pp_annotated_clause acl (List.length prev_acls)
        (has_condition_clause prev_acls) ;

      (* process logic *)
      (* if cfg shows only one of then-block and else-block is possible,
         we can change the block accordingly.
         e.g. [prev: [r = c ? ...; r = r1 @- r]]
      *)
      if List.length prev_acls > 1 && has_condition_clause prev_acls
      then make_cond_block_possible map prev_acls cfg
      else () ;

      (* step logic *)
      let is_skipped, is_sub =
        match acl with
        | Unannotated_clause _ | Start_clause _ | End_clause _ -> (true, false)
        (* into fbody *)
        | Binding_exit_clause
            ( Abs_var _,
              Abs_var ret_var,
              Abs_clause (Abs_var site_r, Abs_appl_body _) ) ->
            (* para can also be ignored in Fun since para is a property of a Fun block, defined in the source code *)
            let f_def = Ident_map.find ret_var ret_to_fun_def_map in
            map := add_id_dst site_r f_def !map ;
            (false, true)
        (* out of fbody *)
        | Binding_enter_clause
            (Abs_var para, _, Abs_clause (Abs_var site_r, Abs_appl_body _)) ->
            let f_def = Ident_map.find para para_to_fun_def_map in
            map := add_id_dst site_r f_def !map ;
            map := add_callsite f_def site_r !map ;
            (false, false)
        (* into cond-body *)
        | Binding_exit_clause
            (_, Abs_var _, Abs_clause (_, Abs_conditional_body _)) ->
            (false, true)
        (* out of cond-body *)
        | Nonbinding_enter_clause
            (Abs_value_bool _, Abs_clause (_, Abs_conditional_body _)) ->
            (false, false)
        | Binding_exit_clause (_, _, _) ->
            failwith "impossible binding exit for non-sites"
        | Binding_enter_clause (_, _, _) ->
            failwith "impossible binding enter for non callsites"
        | Nonbinding_enter_clause (_, _) ->
            failwith "impossible non-binding enter for non condsites"
      in
      if is_skipped || is_sub
      then
        List.iter
          ~f:(fun acl -> loop acl ((not is_sub) && is_main_track))
          (preds_l acl cfg)
      else ())
  in
  let succ_acls = succs_l acl cfg in
  List.iter ~f:(fun acl -> loop acl true) succ_acls ;
  (* loop acl true ; *)
  !map

let fun_info_of_callsite callsite map =
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
