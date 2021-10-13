open Core
open Fix
open Share
open Odefa_ast.Ast
open Odefa_ddpa.Ddpa_abstract_ast

type dvalue =
  | Direct of Abs_value.t
  | Closure of Id.t * abstract_function_value * env

and env = dvalue Map.M(Id).t

let pp_aval oc = function
  | Direct av -> pp_abstract_value oc av
  | Closure (x, v, _ae) ->
      Fmt.pr "Closure(%a,%a)" Id.pp x pp_abstract_function_value v

module Env = struct
  type t = env

  let empty = Map.empty (module Id)

  let get map x : dvalue = Map.find_exn map x

  let add map x v : t = Map.add_exn map ~key:x ~data:v
end

module AValueSet = struct
  include Caml.Set.Make (struct
    type t = dvalue

    let compare = Caml.compare
  end)

  let pp _oc vset = Fmt.(pr "%a" (Dump.list pp_aval)) (elements vset)

  let to_list set = set |> to_seq |> Caml.List.of_seq

  let bools =
    of_list [ Direct (Abs_value_bool true); Direct (Abs_value_bool false) ]

  let binop s1 s2 (f : elt -> elt -> elt option) =
    fold
      (fun e1 acc_s1 ->
        fold
          (fun e2 acc_s2 ->
            match f e1 e2 with Some v -> add v acc_s2 | None -> acc_s2)
          acc_s1 s2)
      empty s1
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

  let push c (x : element) : t =
    let context_new = x :: c in
    if List.length context_new <= 2 then
      context_new
    else
      List.drop_last_exn context_new

  let pp = Fmt.Dump.list Id.pp
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

let bool_binop v1 v2 op =
  match (v1, v2) with
  | Direct (Abs_value_bool b1), Direct (Abs_value_bool b2) ->
      AValueSet.singleton (Direct (Abs_value_bool (op b1 b2)))
  | _, _ -> AValueSet.empty

let make_fix_f program =
  let rec aeval_expr_top arg0 eval expr =
    try aeval_expr arg0 eval Env.empty AContext.empty expr with Found v -> v
  and aeval_expr arg0 eval env ctx expr : AValueSet.t =
    let (Abs_expr clauses) = expr in
    let id0, _ = arg0 in
    let _, vs =
      List.fold_map clauses ~init:[ env ] ~f:(fun envs clause ->
          let (Abs_clause (Abs_var x, _)) = clause in
          Format.printf "@.[Debug(%a%a)][Start] %a" Id.pp id0 AContext.pp ctx
            Id.pp x;
          (* It's unnecessary to traverse the program to estabilish the equations for the asked
             `x` a.k.a. `arg0`. For this, we may use equivalent representation of the program, rather
              than run one-by-one like this
          *)
          if FKey.equal arg0 (x, ctx) then (
            let v =
              List.fold envs ~init:AValueSet.empty ~f:(fun vs env ->
                  AValueSet.union vs (aeval_clause arg0 eval env ctx clause))
            in
            Format.printf "@.[Debug(%a%a)][Found] %a = %a" Id.pp id0 AContext.pp
              ctx Id.pp x AValueSet.pp v;
            (* what's the difference between `raise` at `x` at truncating the expression at `x`.
               Assume the expression has multiple value a.k.a abstract value. In each non-deterministic
               track, it collects one value and when they all merged you can the final values.
               What is doing now it immediately raise when this trace finds `x`.
            *)
            raise (Found v))
          else
            let vs : AValueSet.t = eval (x, ctx) in
            let envs' =
              List.bind (AValueSet.to_list vs) ~f:(fun v ->
                  List.map envs ~f:(fun env -> Env.add env x v))
            in
            (envs', vs))
    in
    List.last_exn vs
  and aeval_clause arg0 eval env ctx clause : AValueSet.t =
    let (Abs_clause (Abs_var y, cbody)) = clause in
    let v =
      match cbody with
      | Abs_value_body Abs_value_int ->
          AValueSet.singleton (Direct Abs_value_int)
      | Abs_value_body (Abs_value_bool beta) ->
          AValueSet.singleton (Direct (Abs_value_bool beta))
      | Abs_var_body (Abs_var y) -> AValueSet.singleton (Env.get env y)
      | Abs_input_body -> AValueSet.singleton (Direct Abs_value_int)
      | Abs_binary_operation_body (Abs_var x1, op, Abs_var x2) -> (
          let v1 = Env.get env x1 in
          let v2 = Env.get env x2 in
          match op with
          | Binary_operator_plus | Binary_operator_minus | Binary_operator_times
          | Binary_operator_divide | Binary_operator_modulus ->
              AValueSet.singleton (Direct Abs_value_int)
          | Binary_operator_less_than | Binary_operator_less_than_or_equal_to
          | Binary_operator_equal_to ->
              AValueSet.bools
          | Binary_operator_and -> bool_binop v1 v2 ( && )
          | Binary_operator_or -> bool_binop v1 v2 ( || )
          | Binary_operator_xor -> bool_binop v1 v2 Bool.( <> ))
      | Abs_conditional_body (Abs_var c, e1, e2) -> (
          match Env.get env c with
          | Direct (Abs_value_bool b) ->
              if b then
                aeval_expr arg0 eval env ctx e1
              else
                aeval_expr arg0 eval env ctx e2
          | _ -> AValueSet.empty)
      | Abs_value_body (Abs_value_function f) ->
          AValueSet.singleton (Closure (y, f, env))
      | Abs_appl_body (Abs_var xf, Abs_var xa) -> (
          let f = Env.get env xf in
          let a = Env.get env xa in
          match f with
          | Closure (_y, Abs_function_value (Abs_var p, fbody), env) ->
              let env' = Env.add env p a in
              let ctx' = AContext.push ctx y in
              aeval_expr arg0 eval env' ctx' fbody
          | _ -> AValueSet.empty)
      (* not yet *)
      | Abs_value_body (Abs_value_record _) -> AValueSet.empty
      | Abs_match_body (Abs_var _x, _p) -> AValueSet.empty
      | Abs_projection_body (Abs_var _r, _x) -> AValueSet.empty
    in
    v
  in

  let truncate_program _arg0 = program in

  let fix eval arg0 = aeval_expr_top arg0 eval (truncate_program arg0) in
  fix

type result_map_t = AValueSet.t Caml.Hashtbl.Make(FKey).t

module ResultMap = Caml.Hashtbl.Make (FKey)

(* It makes no much diffence to between these (should be set instead of list)
   eval : Id.t * Ctx.t * Env.t -> [AValue]
   eval : Id.t * Ctx.t * Env.t -> [AValue * Env.t]

   Considering when the `eval` is used, the env can be computed from the result AValue,
   or be retrieved from the result Env.t.
*)

(* make sure to call it with the last id of the program once *)
let eval_result program =
  let compute = F.defensive_fix (make_fix_f program) in
  let compute, table = F.visibly_memoize compute in
  (compute, table)

let run filename =
  let program = Load.load filename in
  print_endline @@ Odefa_ast.Ast_pp.show_expr program;
  let _x_first = first_id program in
  let x_last = last_id program in
  let aprogram = Odefa_ddpa.Ddpa_graph.lift_expr program in
  let compute, table = eval_result aprogram in
  (* Compute *)
  ignore @@ compute (x_last, AContext.empty);
  print_endline @@ string_of_int (ResultMap.length table);
  (* let (Expr clauses) = program in
     clauses |> List.rev
     |> List.iter ~f:(fun clause ->
            let (Clause (Var (x, _), _)) = clause in
            ignore @@ compute (x, AContext.empty)); *)
  print_endline @@ string_of_int (ResultMap.length table);
  ResultMap.iter
    (fun (id, context) vset ->
      Fmt.pr "@.%a%a = %a" Id.pp id Id.pp_list context AValueSet.pp vset)
    table
