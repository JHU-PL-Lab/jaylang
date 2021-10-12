open Core
open Fix
open Share
open Odefa_ast.Ast
open Odefa_ddpa.Ddpa_abstract_ast

type avalue =
  | Direct of Abs_value.t
  | Closure of Id.t * abstract_function_value * aenv

and aenv = avalue Ident_map.t

let pp_aval oc = function
  | Direct av -> pp_abstract_value oc av
  | Closure (x, v, _ae) ->
      Fmt.pr "Closure(%a,%a)" Id.pp x pp_abstract_function_value v

module Env = struct
  type t = avalue Map.M(Id).t

  let empty = Map.empty (module Id)

  let get map x : avalue = Map.find_exn map x

  let add map x v : t = Map.add_exn map ~key:x ~data:v
end

module AValueSet = struct
  include Caml.Set.Make (struct
    type t = avalue

    let compare = Caml.compare
  end)

  let pp _oc vset = Fmt.(pr "%a" (Dump.list pp_aval)) (elements vset)
end

module AContext = struct
  module T = struct
    type element = Id.t

    and t = element list [@@deriving sexp, compare, equal, hash]
  end

  include T
  include Comparator.Make (T)

  let k = 2

  let empty : t = []

  let push (x : element) c : t =
    let context_new = x :: c in
    if List.length context_new <= 2 then
      context_new
    else
      List.drop_last_exn context_new
end

module Store = struct
  type t = Env.t Map.M(AContext).t

  let empty = Map.empty (module AContext)

  let equal : Env.t -> Env.t -> bool = Map.equal Poly.equal
end

module FKey : HashedType with type t = Id.t * AContext.t = struct
  type t = Id.t * AContext.t

  let equal (i1, c1) (i2, c2) = Id.equal i1 i2 && AContext.equal c1 c2

  let hash = Caml.Hashtbl.hash
end

module F = Memoize.ForHashedType (FKey)

exception Found of AValueSet.t

let rec aeval_expr_top arg0 eval expr =
  try aeval_expr arg0 eval Env.empty expr with Found v -> v

and aeval_expr arg0 eval env expr =
  let (Abs_expr clauses) = expr in
  let _, vs =
    List.fold_map clauses ~init:env ~f:(fun env clause ->
        aeval_clause arg0 eval env clause)
  in
  List.last_exn vs

and aeval_clause arg0 eval env clause =
  let (Abs_clause (Abs_var x, cbody)) = clause in
  let v =
    match cbody with
    | Abs_value_body Abs_value_int -> AValueSet.singleton (Direct Abs_value_int)
    | Abs_var_body (Abs_var y) ->
        eval (y, AContext.empty)
        (* AValueSet.singleton (Direct Abs_value_int) *)
    | Abs_binary_operation_body (Abs_var x1, op, Abs_var x2) -> (
        let v1 = Env.get env x1 in
        let v2 = Env.get env x2 in
        match op with
        | Binary_operator_plus -> AValueSet.singleton (Direct Abs_value_int)
        | _ -> AValueSet.empty)
    | _ -> AValueSet.empty
  in
  let id0, _ = arg0 in
  (* De-lazy fix *)
  if Ident.equal x id0 then (
    Format.printf "[Debug] %a = %a\n" Id.pp x AValueSet.pp v;
    raise (Found v))
  else (* (Map.add env x v, v) *)
    (env, v)

(* AValueSet.empty *)

type result_map_t = AValueSet.t Caml.Hashtbl.Make(FKey).t

module ResultMap = Caml.Hashtbl.Make (FKey)

(* make sure to call it with the last id of the program once *)
let eval_result program id =
  let compute = F.fix (fun eval arg0 -> aeval_expr_top arg0 eval program) in
  let compute, table = F.visibly_memoize compute in
  let r = compute (id, AContext.empty) in
  (r, table)

(* result_table () *)

let run filename =
  let program = Load.load filename in
  print_endline @@ Odefa_ast.Ast_pp.show_expr program;
  let _x_first = first_id program in
  let x_last = last_id program in
  let aprogram = Odefa_ddpa.Ddpa_graph.lift_expr program in
  let _r, table = eval_result aprogram x_last in
  print_endline @@ string_of_int (ResultMap.length table);
  ResultMap.iter
    (fun (id, context) vset ->
      Format.printf "%a[%a] = %a\n" Id.pp id Id.pp_list context AValueSet.pp
        vset)
    table
