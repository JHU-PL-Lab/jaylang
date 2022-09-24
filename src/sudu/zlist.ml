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
let nil = Z3List.nil int_list_sort

let cons i s =
  FuncDecl.apply cons_decl [ Z3.Arithmetic.Integer.mk_numeral_i ctx i; s ]

let is_cons s = FuncDecl.apply is_cons_decl [ s ]
let hd s = FuncDecl.apply head_decl [ s ]
let tl s = FuncDecl.apply tail_decl [ s ]
let s0 = nil
let s1 = cons 3 s0
let eq_s1_s1 = Z3.Boolean.mk_eq ctx s1 s1
let eq_s1_s0 = Z3.Boolean.mk_eq ctx s1 s0
let x1 = Expr.mk_const_s ctx "x1" int_list_sort
let eq_x1_s1 = Z3.Boolean.mk_eq ctx x1 s1

let eq_hd s i =
  Z3.Boolean.mk_eq ctx (hd s) (Z3.Arithmetic.Integer.mk_numeral_i ctx i)

let this_solver = Z3.Solver.mk_solver ctx None

let () =
  Z3.Solver.add this_solver
    [ eq_s1_s1; eq_hd x1 4; is_cons x1; eq_hd (tl x1) 5; is_cons (tl x1) ]

let status = Z3.Solver.check this_solver []
let result = Z3API.check_with_assumption this_solver []

let model =
  match result with Result.Ok model -> model | Result.Error _ -> failwith "a"

let v1 = Option.value_exn @@ Model.eval model x1 false
let () = Fmt.pr "%s" @@ Z3.Solver.to_string this_solver
let () = Fmt.pr "%s" @@ Expr.to_string v1