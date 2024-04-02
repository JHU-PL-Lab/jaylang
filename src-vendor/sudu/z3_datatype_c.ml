open Core
open Z3
open Z3_helper

type t_v2 =
  | Int of int
  | Bool of bool
  | Fun of string
  | Record of int (* record can have 63 diff labels *)

module Make_z3_datatype_V2 (C : Context) : Jil_z3_datatye with type t = t_v2 =
struct
  type t = t_v2
  type case = Z3_datatype.case = Int_case | Bool_case | Fun_case | Record_case

  let cases = Z3_datatype.cases

  open C
  include Make_basic_to_z3_basic (C)

  (* making constructors, checkers, and selectors *)
  let intC =
    Datatype.mk_constructor_s ctx "Int"
      (Symbol.mk_string ctx "is-Int")
      [ Symbol.mk_string ctx "i" ]
      [ Some int_sort ] [ 1 ]

  let boolC =
    Datatype.mk_constructor_s ctx "Bool"
      (Symbol.mk_string ctx "is-Bool")
      [ Symbol.mk_string ctx "b" ]
      [ Some bool_sort ] [ 1 ]

  let funC =
    Datatype.mk_constructor_s ctx "Fun"
      (Symbol.mk_string ctx "is-Fun")
      [ Symbol.mk_string ctx "fid" ]
      [ Some string_sort ] [ 1 ]

  let recordC =
    Datatype.mk_constructor_s ctx "Record"
      (Symbol.mk_string ctx "is-Record")
      [ Symbol.mk_string ctx "r" ]
      [ Some (bitvector_sort 63) ]
      [ 1 ]

  let the_sort =
    Datatype.mk_sort_s ctx "Jil_type" [ intC; boolC; funC; recordC ]

  (* making recognizers *)
  let mk_int, mk_bool, mk_fun, mk_record =
    match Datatype.get_recognizers the_sort with
    | [ r1; r2; r3; r4 ] -> (r1, r2, r3, r4)
    | _ -> failwith "recogniziers mismatch"

  (* building Z3 bool expressions with the recognizers  *)
  let is_int e = FuncDecl.apply mk_int [ e ]
  let is_bool e = FuncDecl.apply mk_bool [ e ]
  let is_fun e = FuncDecl.apply mk_fun [ e ]
  let is_record e = FuncDecl.apply mk_record [ e ]

  (* making field getters *)
  let getInt, getBool, getFun, getRecord =
    match Datatype.get_accessors the_sort with
    | [ [ a1 ]; [ a2 ]; [ a3 ]; [ a4 ] ] -> (a1, a2, a3, a4)
    | _ -> failwith "accessors mismatch"

  (* making declarations from constructors *)
  let intD = Datatype.Constructor.get_constructor_decl intC
  let boolD = Datatype.Constructor.get_constructor_decl boolC
  let funD = Datatype.Constructor.get_constructor_decl funC
  let recordD = Datatype.Constructor.get_constructor_decl recordC

  (* basic builders *)
  let inject_int e = FuncDecl.apply intD [ e ]
  let inject_bool e = FuncDecl.apply boolD [ e ]
  let inject_string e = FuncDecl.apply funD [ e ]
  let inject_record e = FuncDecl.apply recordD [ e ]
  let project_int e = FuncDecl.apply getInt [ e ]
  let project_bool e = FuncDecl.apply getBool [ e ]
  let project_string e = FuncDecl.apply getFun [ e ]
  let project_record e = FuncDecl.apply getRecord [ e ]

  let case_to_recognizer = function
    | Int_case -> is_int
    | Bool_case -> is_bool
    | Fun_case -> is_fun
    | Record_case -> is_record

  let case_to_injector = function
    | Int_case -> inject_int
    | Bool_case -> inject_bool
    | Fun_case -> inject_string
    | Record_case -> inject_record

  let case_to_projecter = function
    | Int_case -> project_int
    | Bool_case -> project_bool
    | Fun_case -> project_string
    | Record_case -> project_record

  let case_of_value_exn v =
    List.find_exn cases ~f:(fun case ->
        v |> case_to_recognizer case |> simplify |> unbox_bool)

  (* let project_int v = v |> project_int |> simplify *)

  let project_value case v =
    match case with
    | Int_case -> v |> project_int |> simplify
    | Bool_case -> v |> project_bool |> simplify
    | Fun_case -> v |> project_string |> simplify
    | Record_case -> v |> project_record |> simplify

  let unbox_value v =
    let case = case_of_value_exn v in
    let pv = project_value case v in
    match case with
    | Int_case -> Int (unbox_int pv)
    | Bool_case -> Bool (unbox_bool pv)
    | Fun_case -> Fun (unbox_string pv)
    | Record_case -> Record (unbox_bitvector pv)

  let box_value = function
    | Int i -> i |> box_int |> inject_int
    | Bool b -> b |> box_bool |> inject_bool
    | Fun fs -> fs |> box_string |> inject_string
    | Record rs -> rs |> box_bitvector |> inject_record

  let eval_value model e =
    let v = eval_exn_ model e in
    Some (unbox_value v)

  (* use variable expression to query model for int input *)
  let get_int_expr model e =
    match eval_value model e with
    | Some (Int i) -> Some i
    | Some _ ->
        Logs.warn (fun m -> m "Get non-int for input%s" (Z3.Expr.to_string e)) ;
        Some 0
    | None -> None
end
