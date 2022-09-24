open! Core
open! Z3

let ctx = Z3.mk_context []

module Z3API = Z3_api.Make (struct
  let ctx = ctx
end)

let int_sort = Arithmetic.Integer.mk_sort ctx

let int_list_sort =
  Z3List.mk_sort ctx (Z3.Symbol.mk_string ctx "a_list") int_sort

let cons_decl = Z3List.get_cons_decl int_list_sort
let is_cons_decl = Z3List.get_is_cons_decl int_list_sort
let head_decl = Z3List.get_head_decl int_list_sort
let tail_decl = Z3List.get_tail_decl int_list_sort

(* API *)
let nil = Z3List.nil int_list_sort

let cons i s =
  FuncDecl.apply cons_decl [ Z3.Arithmetic.Integer.mk_numeral_i ctx i; s ]

let is_cons s = FuncDecl.apply is_cons_decl [ s ]
let is_nil s = Z3.Boolean.mk_eq ctx s nil
let hd s = FuncDecl.apply head_decl [ s ]
let tl s = FuncDecl.apply tail_decl [ s ]
let mk_list name = Expr.mk_const_s ctx name int_list_sort
let mk_int name = Expr.mk_const_s ctx name int_sort

(* Example *)

let eq_hd s i =
  Z3.Boolean.mk_eq ctx (hd s) (Z3.Arithmetic.Integer.mk_numeral_i ctx i)

let s0 = mk_list "s0"
let s1 = mk_list "s1"
let s2 = mk_list "s2"
let x0 = mk_int "x0"
let x1 = mk_int "x1"
let x2 = mk_int "x2"
let to_list_s i = "s" ^ string_of_int i |> mk_list
let to_int_s i = "x" ^ string_of_int i |> mk_int

let plus_k i0 i1 k =
  Z3.Boolean.mk_eq ctx (to_int_s i0)
    Z3.Arithmetic.(mk_add ctx [ to_int_s i1; Integer.mk_numeral_i ctx k ])

let ge_k i k =
  Z3.Arithmetic.(mk_ge ctx (to_int_s i) (Integer.mk_numeral_i ctx k))

let is_k i k =
  Z3.Boolean.mk_eq ctx (to_int_s i) (Z3.Arithmetic.Integer.mk_numeral_i ctx k)

let nil_or_tl i0 i1 =
  let is_s0_nil = Z3.Boolean.mk_and ctx [ is_nil (to_list_s i0); is_k i0 0 ] in
  let is_s0_from_s1 =
    Z3.Boolean.mk_and ctx
      [
        is_cons (to_list_s i0);
        Z3.Boolean.mk_eq ctx (to_list_s i1) (tl (to_list_s i0));
        plus_k i0 i1 2;
      ]
  in
  Z3.Boolean.mk_or ctx [ is_s0_nil; is_s0_from_s1 ]

let this_solver = Z3.Solver.mk_solver ctx None
let () = Z3.Solver.add this_solver [ nil_or_tl 0 1; nil_or_tl 1 2; ge_k 0 3 ]

(* s0_choices *)
let status = Z3.Solver.check this_solver []
let result = Z3API.check_with_assumption this_solver []

let model =
  match result with Result.Ok model -> model | Result.Error _ -> failwith "a"

let () = Fmt.pr "%s" @@ Z3.Model.to_string model
let s0 = Option.value_exn @@ Model.eval model s0 false
let () = Fmt.pr "%s" @@ Z3.Solver.to_string this_solver
let () = Fmt.pr "%s" @@ Expr.to_string s0