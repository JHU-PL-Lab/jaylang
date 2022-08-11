open Core

open Dbmc
open Odefa_ast.Ast

(* The alias should follow the rule that each node has a single successor *)
let rec find_alias graph acc x_with_stk = 
  (* let () = print_endline "Current target: " in
  let () = print_endline @@ Interpreter.show_ident_with_stack x_with_stk in *)
  if Interpreter.G.mem_vertex graph x_with_stk then
    let (succ : Interpreter.Ident_with_stack.t list) = 
      Interpreter.G.succ graph x_with_stk 
    in
    match succ with
    | [] -> x_with_stk :: acc
    | [succ] -> find_alias graph (x_with_stk :: acc) succ
    | _ -> failwith "Should not have more than one successor!" 
  else
    x_with_stk :: acc

let get_expected_type_from_operator op = 
  match op with
  | Binary_operator_plus | Binary_operator_minus
  | Binary_operator_times | Binary_operator_divide
  | Binary_operator_modulus | Binary_operator_less_than
  | Binary_operator_less_than_or_equal_to | Binary_operator_equal_to
  | Binary_operator_not_equal_to -> Int_type
  | Binary_operator_and | Binary_operator_or -> Bool_type

let get_value_type v = 
  match v with
  | Value_int _ -> Int_type
  | Value_bool _ -> Bool_type
  | Value_function _ -> Fun_type
  | Value_record (Record_value r) -> 
    let lbls = Ident_set.of_enum @@ Ident_map.keys r in
    Rec_type lbls

let get_expected_type_from_cls cls_body = 
  match cls_body with
  | Binary_operation_body (_, op, _) ->
    get_expected_type_from_operator op
  | Not_body _ -> Bool_type
  | Appl_body _ -> Fun_type
  | Projection_body (_, lbl) -> Rec_type (Ident_set.singleton lbl)
  | Conditional_body _ -> Bool_type
  | _ -> failwith @@
    "Errors should only arise from unary/binary operations, function application, record projection, or a conditional."

let get_expected_type_from_pattern nat_pat = 
  let open Odefa_natural.On_ast in
  match nat_pat with
  | AnyPat -> TopType
  | IntPat -> IntType
  | BoolPat -> BoolType
  | FunPat -> FunType
  | RecPat m -> 
    let s = Ident_set.of_enum @@ Ident_map.keys m in
    RecType s
  | VariantPat (vl, _) -> VariantType vl
  | VarPat _ -> failwith "Type unknown!"
  | EmptyLstPat | LstDestructPat _ -> ListType
