open Core
(* module SuduZ3 = From_dbmc.Solver.SuduZ3 *)
(* open SuduZ3 *)
open Jayil.Ast

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = Z3.mk_context []
end)
open SuduZ3

module Record_logic =
  struct
    module Table =
      struct
        (* mutable type! *)
        type t =
          { tbl : (ident, int) Hashtbl.t (* maps ident to bit, where rightmost bit is 0 *)
          ; mutable n : int } (* is the greatest unused offset *)

        let create () : t =
          { tbl = Hashtbl.create (module Ident_new) ; n = 0 }

        let clear (x : t) : unit =
          Hashtbl.clear x.tbl;
          x.n <- 0

        (* assigns id an offset and returns that offset. mutates the table *)
        let set_found ({ tbl ; n } as x : t) (id : Ident_new.t) : int =
          match Hashtbl.find tbl id with
          | None ->
            if n > 62 then failwith "too many record labels" else (* fail if about to assign to 63rd index or greater *)
            Hashtbl.set tbl ~key:id ~data:n; x.n <- n + 1; n (* give next offset and increment *)
          | Some n -> n (* id has already been found *)
      end

    let tbl = Table.create ()

    (* it is suggested (but not always necessary) to clear the table of record labels before running over a new program *)
    let clear_labels () : unit =
      Table.clear tbl

    (* assigns bitvector positions from the given labels *)
    let set_labels (labels : Ident_new.t list) : unit =
      Table.clear tbl;
      List.iter labels ~f:(fun id -> let _ = Table.set_found tbl id in ());
      if tbl.n > 63 then failwith "too many record labels" else ()

    (* Use the table to create a bitvector to indicate which labels are given in the [labels] list. *)
    let create_bv_from_labels (labels : ident list) : int =
      let set_bit i b = i lor (1 lsl b) in
      List.fold
        labels
        ~init:0
        ~f:(fun acc id -> set_bit acc (Table.set_found tbl id))
  end (* Record_logic *)

include Record_logic

let ctx = SuduZ3.ctx
let solver = Z3.Solver.mk_simple_solver ctx

let set_timeout_sec sec =
  let time_s =
    sec |> Time_float.Span.to_sec |> Float.iround_up_exn |> fun t ->
    t * 1000 |> string_of_int
  in
  Z3.Params.update_param_value ctx "timeout" time_s

let key_to_var key =
  SuduZ3.var_i
  @@ Concolic_key.x key

let solve formulas =
  Z3.Solver.check solver formulas

let get_model () =
  Z3.Solver.get_model solver

(* Note this doesn't actually clear the solver or context *)
let reset () =
  clear_labels ()


(* Solver primitives *)

let ( @=> ) = SuduZ3.( @=> )
let true_ = box_bool true
let false_ = box_bool false
let bool_ = SuduZ3.bool_
let and_ = SuduZ3.and_

(* AST primitive (no picked) *)

let not_ t t1 =
  let e = key_to_var t in
  let e1 = key_to_var t1 in
  fn_not e e1

let binop t op t1 t2 =
  let open Jayil.Ast in
  let e = key_to_var t in
  let e1 = key_to_var t1 in
  let e2 = key_to_var t2 in
  let fop =
    match op with
    | Binary_operator_plus -> fn_plus
    | Binary_operator_minus -> fn_minus
    | Binary_operator_times -> fn_times
    | Binary_operator_divide -> fn_divide
    | Binary_operator_modulus -> fn_modulus
    | Binary_operator_less_than -> fn_lt
    | Binary_operator_less_than_or_equal_to -> fn_le
    | Binary_operator_equal_to -> fn_eq
    (* TODO: This might be buggy. Check later *)
    | Binary_operator_not_equal_to -> fn_neq
    | Binary_operator_and -> fn_and
    | Binary_operator_or -> fn_or
  in
  fop e e1 e2

(* let eq_bool key b = SuduZ3.eq (key_to_var key) (SuduZ3.bool_ b) *)
let z_of_fid (Ident fid) = SuduZ3.fun_ fid
let is_bool key = ifBool (key_to_var key)

let phi_of_value (key : Concolic_key.t) = function
  | Value_function _ -> z_of_fid @@ Concolic_key.id key
  | Value_int i -> SuduZ3.int_ i
  | Value_bool i -> SuduZ3.bool_ i
  | Value_record (Record_value m) ->
    m
    |> Ident_map.key_list
    |> create_bv_from_labels
    |> SuduZ3.record_

let eqv key v = SuduZ3.eq (key_to_var key) (phi_of_value key v)
let eq key key' = SuduZ3.eq (key_to_var key) (key_to_var key')


let if_pattern term pat =
  let x = key_to_var term in
  match pat with
  | Fun_pattern -> ifFun x
  | Int_pattern -> ifInt x
  | Bool_pattern -> ifBool x
  | Rec_pattern label_set ->
    let sub_bv = create_bv_from_labels (Ident_set.to_list label_set) in (* this bitvector should be contained within the record's bv *)
    let projected = SuduZ3.project_record (SuduZ3.record_ sub_bv) in
    SuduZ3.and_
      [ ifRecord x
      ; SuduZ3.eq
          projected
          (Z3.BitVector.mk_and ctx projected (SuduZ3.project_record x))
      ]
  | Strict_rec_pattern label_set ->
    let eq_bv = create_bv_from_labels (Ident_set.to_list label_set) in (* the record's bv should be exactly this *)
    let desired_record = SuduZ3.record_ eq_bv in
    SuduZ3.and_
      [ ifRecord x
      ; SuduZ3.eq
          (SuduZ3.project_record desired_record)
          (SuduZ3.project_record x)
      ]
  | Any_pattern -> true_

let match_ key m pat =
  let k_expr = key_to_var key in
  SuduZ3.eq (SuduZ3.project_bool k_expr) (if_pattern m pat)