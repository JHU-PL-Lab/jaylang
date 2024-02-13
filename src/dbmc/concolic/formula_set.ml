open Core

module Z3_expr =
  struct
    include Z3.Expr
    type t = Z3.Expr.expr

    (* Set.Make expects sexp conversions, but we don't ever use them. *)
    let t_of_sexp _ = failwith "fail t_of_sexp z3 expr"
    let sexp_of_t _ = failwith "fail sexp_of_t x3 expr" 
  end

module S = Set.Make (Z3_expr)

type t = S.t [@@deriving compare]

(* This is just a little experiment to use lists because we expect formula sets to be small *)
(* type t = Z3_expr.t list [@@deriving compare]
let empty = []
let singleton = List.return
let add = Fn.flip List.cons
let add_multi = List.append
let union = Fn.const (* FIXME: this is only okay for strict concolic *)
let to_list = Fn.id
let equal = fun a b -> compare a b = 0
let and_ = Riddler.and_ *)


let empty = S.empty
let singleton = S.singleton
let add = Set.add
let add_multi (s : t) = List.fold ~init:s ~f:add
let union = Set.union
let to_list = Set.to_list
let equal = Set.equal

let and_ (fset : t) : Z3_expr.t =
  match Set.to_list fset with
  | [] -> Riddler.true_
  | exp :: [] -> exp
  | exps -> Riddler.and_ exps

(* let or_ (fset : t) : Z3_expr.t =
  match Set.to_list fset with
  | [] -> Riddler.true_
  | exp :: [] -> exp
  | exps -> Solver.SuduZ3.or_ exps *)