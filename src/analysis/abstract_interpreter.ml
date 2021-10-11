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

type env = avalue Map.M(Id).t

module AValueSet = Caml.Set.Make (struct
  type t = avalue

  let compare = Caml.compare
end)

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

module AStore = struct
  type t = env Map.M(AContext).t

  let empty = Map.empty (module AContext)

  let equal = Map.equal Poly.equal
end

module FKey : HashedType with type t = Id.t * AContext.t * AStore.t = struct
  type t = Id.t * AContext.t * AStore.t

  let equal (i1, c1, s1) (i2, c2, s2) =
    Id.equal i1 i2 && AContext.equal c1 c2 && AStore.equal s1 s2

  let hash = Caml.Hashtbl.hash
end

module F = Memoize.ForHashedType (FKey)

exception Found of AValueSet.t

let rec aeval_expr_top expr _x _eval =
  (* let rec eval_expr  *)
  aeval_expr expr _eval

and aeval_expr _expr _eval = AValueSet.singleton (Direct Abs_value_int)
(* AValueSet.empty *)

type result_map_t = AValueSet.t Caml.Hashtbl.Make(FKey).t

module ResultMap = Caml.Hashtbl.Make (FKey)

(* make sure to call it with the last id of the program once *)
let eval_result program id =
  let compute = F.fix (aeval_expr_top program) in
  let compute, table = F.visibly_memoize compute in
  let r = compute (id, AContext.empty, AStore.empty) in
  (r, table)

(* result_table () *)

let run filename =
  let program = Load.load filename in
  print_endline @@ Odefa_ast.Ast_pp.show_expr program;
  let size = eval_result program in
  let x_last = last_id program in
  let _r, table = eval_result program x_last in
  print_endline @@ string_of_int (ResultMap.length table);
  ResultMap.iter
    (fun (id, context, _) vset ->
      Format.printf "%a[%a] = %a\n" Id.pp id Id.pp_list context
        Fmt.(Dump.list pp_aval)
        (AValueSet.elements vset))
    table
