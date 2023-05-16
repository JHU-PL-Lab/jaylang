open Core
open Fix
open Share
module F = Fix.Make (ClauseMap) (IdSet)
(* module FE = Fix.Make (ExprMap) (IdSet) *)

let id_of_var (Jayil.Ast.Var (id, _)) = id

let rec clause_uses clause request =
  let open Jayil.Ast in
  let (Clause (_var, cbody)) = clause in
  match cbody with
  | Value_body (Value_int _) -> Set.empty (module Id)
  | Value_body (Value_record (Record_value map)) ->
      Ident_map.key_list map |> Set.of_list (module Id)
  | Value_body (Value_function (Function_value (para, fbody))) ->
      Set.add (expr_uses fbody request) (id_of_var para)
      (*
      | Var_body (Var (x, _)) -> SSet.singleton x
      | Input_body -> SSet.empty
      | Appl_body (Var (x1, _), Var (x2, _)) -> SSet.of_list [ x1; x2 ] *)
  | _ -> Set.singleton (module Id) (Ident "x")
(* SSet.(add empty (Ident "_")) *)

and expr_uses expr request =
  let open Jayil.Ast in
  let (Expr clauses) = expr in
  List.fold clauses
    ~init:(Set.empty (module Id))
    ~f:(fun acc clause ->
      let this_answer = clause_uses clause request in
      Set.union acc this_answer)

let compute_closed : F.variable -> F.valuation -> IdSet.property =
  let open Jayil.Ast in
  fun clause request -> clause_uses clause request

(* let close_ids program =
   F.lfp compute_closed program |> Set.to_list |> Fmt.(pr "%a" Id.pp_list) *)

let close_ids clause =
  F.lfp compute_closed clause |> Set.to_list |> Fmt.(pr "%a" Id.pp_list)

let run filename =
  let program = load filename in
  print_endline @@ Jayil.Ast_pp.show_expr program ;
  let (Expr clauses) = program in
  List.iter clauses ~f:close_ids
