open Core
open Dj_common
open Jayil.Ast

module Table = struct
  (* mutable type! *)
  type t = {
    tbl :
      (ident, int) Hashtbl.t (* maps ident to bit, where rightmost bit is 0 *);
    mutable n : int; (* is the greatest unused offset *)
  }

  let create () : t = { tbl = Hashtbl.create (module Ident_new); n = 0 }

  let clear (x : t) : unit =
    Hashtbl.clear x.tbl ;
    x.n <- 0

  (* assigns id an offset and returns that offset. mutates the table *)
  let set_found ({ tbl; n } as x : t) (id : Ident_new.t) : int =
    match Hashtbl.find tbl id with
    | None ->
        if n > 62
        then failwith "too many record labels"
        else
          (* fail if about to assign to 63rd index or greater *)
          Hashtbl.set tbl ~key:id ~data:n ;
        x.n <- n + 1 ;
        n (* give next offset and increment *)
    | Some n -> n (* id has already been found *)
end

let tbl = Table.create ()

(* it is suggested (but not always necessary) to clear the table of record labels before running over a new program *)
let clear_labels () : unit = Table.clear tbl

(* assigns bitvector positions from the given labels *)
let set_labels (labels : Ident_new.t list) : unit =
  Table.clear tbl ;
  List.iter labels ~f:(fun id ->
      let _ = Table.set_found tbl id in
      ()) ;
  if tbl.n > 63 then failwith "too many record labels" else ()

(* find all labels used anywhere in the ast, then set the labels *)
(* Currently, this should not be used because the option ?are_labels_predefined below is applied as false in this file. *)
let set_labels_from_ast (expr : Jayil.Ast.expr) : unit =
  let rec find_labels_in_ast (Expr clauses : Jayil.Ast.expr) : ident list =
    List.fold clauses ~init:[] ~f:(fun acc clause ->
        find_labels_in_clause clause @ acc)
  and find_labels_in_clause (clause : Jayil.Ast.clause) : ident list =
    let (Clause (Var (x, _), cbody)) = clause in
    match cbody with
    | Conditional_body (_, e1, e2) ->
        find_labels_in_ast e1 @ find_labels_in_ast e2
    | Value_body (Value_function (Function_value (_, e))) ->
        find_labels_in_ast e
    | Value_body (Value_record (Record_value m)) -> Ident_map.key_list m
    | Match_body (_, Rec_pattern id_set)
    | Match_body (_, Strict_rec_pattern id_set) ->
        Ident_set.to_list id_set
    | _ -> []
  in
  find_labels_in_ast expr |> set_labels

(* Use the table to create a bitvector to indicate which labels are given in the [labels] list. *)
let create_bv_from_labels ?(are_labels_predefined : bool = true)
    (labels : ident list) : int =
  let set_bit i b = i lor (1 lsl b) in
  if are_labels_predefined
  then
    List.fold labels ~init:0 ~f:(fun acc id ->
        set_bit acc (Hashtbl.find_exn tbl.tbl id))
  else
    List.fold labels ~init:0 ~f:(fun acc id ->
        set_bit acc (Table.set_found tbl id))
