open Core
open Jay_ast

exception Evaluation_failure of string

type evaluation_environment = (ident, expr_desc) Hashtbl.t

let stdin_input_source () =
  let input =
    Out_channel.(flush stdout) ;
    Int.of_string In_channel.(input_line_exn stdin)
  in
  Int input

let rec substitute (x : ident) (subs : expr) (og_expr : expr) : expr =
  match og_expr with
  | Var x' -> if Ident.equal x x' then subs else og_expr
  | Int _ | Bool _ | Input -> og_expr
  | _ -> failwith "TBI!"

let rec eval ?(input_source = stdin_input_source) (e_desc : expr_desc) :
    expr_desc =
  let e = e_desc.body in
  match e with
  | Int _ | Bool _ | Function _ -> e_desc
  | Input -> new_expr_desc @@ input_source ()
  (* | _ -> failwith "TBI!" *)
  | Var x ->
      raise
      @@ Evaluation_failure
           ("Cannot find the variable " ^ Jay_ast_pp.show_ident x
          ^ " in the environment!")
  | Appl (e_desc1, e_desc2) -> (
      let e1_eval = (eval ~input_source e_desc1).body in
      let e2_eval = (eval ~input_source e_desc2).body in
      match e1_eval with
      | Function (x :: _, f_edesc) ->
          let f_expr = f_edesc.body in
          let res = substitute x e2_eval f_expr in
          new_expr_desc res
      | _ -> raise @@ Evaluation_failure "Evaluation of non-function!")
  | _ -> failwith "TBI!"
(* | Let of ident * expr_desc * expr_desc
   | LetRecFun of funsig list * expr_desc
   | LetFun of funsig * expr_desc
   | Plus of expr_desc * expr_desc
   | Minus of expr_desc * expr_desc
   | Times of expr_desc * expr_desc
   | Divide of expr_desc * expr_desc
   | Modulus of expr_desc * expr_desc
   | Equal of expr_desc * expr_desc
   | Neq of expr_desc * expr_desc
   | LessThan of expr_desc * expr_desc
   | Leq of expr_desc * expr_desc
   | GreaterThan of expr_desc * expr_desc
   | Geq of expr_desc * expr_desc
   | And of expr_desc * expr_desc
   | Or of expr_desc * expr_desc
   | Not of expr_desc
   | If of expr_desc * expr_desc * expr_desc
   | Record of (expr_desc Ident_map.t)
   | RecordProj of expr_desc * label
   | Match of expr_desc * (pattern * expr_desc) list
   | VariantExpr of variant_label * expr_desc
   | List of expr_desc list
   | ListCons of expr_desc * expr_desc
   | Assert of expr_desc
   | Assume of expr_desc *)
