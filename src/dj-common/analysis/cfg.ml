open Core
open Jayil.Ast

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

let find_block_by_id x block_map =
  block_map |> Ident_map.values |> Ddpa.Ddpa_helper.bat_list_of_enum
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
    block_map |> Ident_map.values |> Ddpa.Ddpa_helper.bat_list_of_enum
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
            { id = cid; cat = Cond []; clause = c }
        | Value_body (Value_function _) -> { id = cid; cat = Fun; clause = c }
        | _ -> { id = cid; cat = Direct; clause = c }
      in
      cs @ [ c' ])

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
