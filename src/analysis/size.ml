open Core
open Fix
open Share
open Jayil.Ast

(* module SizeF = Fix.ForHashedType() *)

module IdWithHash : HashedType with type t = Id.t = struct
  type t = Id.t

  let hash = Id.hash
  let equal = Id.equal
end

module IntProp = Prop.Set (struct
  type t = int

  let empty = 0
  let equal i1 i2 = i1 = i2
end)

module F = Fix.ForHashedType (IdWithHash) (IntProp)

(* let compute_size : F.variable -> F.valuation -> IntProp.property =
   let open Jayil.Ast in
   let rec clause_size _clause _request = 1
   and expr_size expr request =
     let (Expr clauses) = expr in
     List.fold clauses ~init:0 ~f:(fun acc clause ->
         acc + clause_size clause request)
   in
   fun clause request -> clause_size clause request *)

(*
   sig
     type variable = IdHashtbl.t
     type property = int
     type valuation = variable -> property
     type rhs = valuation -> property
     type equations = variable -> rhs
     val lfp : equations -> valuation
   end
*)

exception Found of int

let once = ref false
let answer = ref 0

let rec expr_size f x0 expr =
  let (Expr clauses) = expr in
  List.fold clauses ~init:0 ~f:(fun acc clause -> acc + clause_size f x0 clause)

and clause_size f x0 clause =
  let (Clause (Var (x, _), cbody)) = clause in
  let v =
    match cbody with
    | Value_body (Value_int i) -> i
    | Var_body (Var (_y, _)) -> 1
    | Binary_operation_body (Var (x1, _), _, Var (x2, _)) -> f x1 + f x2
    | _ -> 0
  in
  if Id.equal x x0
  then (
    if Id.equal x (Ident "one")
    then if !once then failwith "Boom" else once := true ;
    raise (Found v))
  else v
(* if Id.equal x x0 then answer := v;
   v *)

let compute_size program : Id.t -> int =
  F.lfp (fun x f : int -> try expr_size f x program with Found i -> i)
(* F.lfp (fun x f : int ->
    ignore @@ expr_size f x program;
    !answer) *)

let run filename =
  let program = Load.load filename in
  print_endline @@ Jayil.Ast_pp.show_expr program ;
  let size = compute_size program in
  let (Jayil.Ast.Expr clauses) = program in
  clauses |> List.rev
  |> List.iter ~f:(fun clause ->
         let (Jayil.Ast.Clause (Var (x, _), _)) = clause in
         Format.printf "%a: %d\n" Id.pp x (size x)) ;
  ()
(* print_endline @@ string_of_int (compute_size program) *)
(* let (Expr clauses) = program in
   List.iter clauses ~f:close_ids *)
