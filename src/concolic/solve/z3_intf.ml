
open Core

module Solve_status = struct
  type t =
    | Sat of Z3.Model.model (* should this be generated in the functor because of model? *)
    | Unknown
    | Unsat
end

module type S = sig
  type 'a t (* expressions *)

  val set_timeout : Time_float.Span.t -> unit

  (* val ctx : Z3.context *)

  (*
    -------------
    MAKE FORMULAS
    -------------
  *)
  val box_int : int -> int t
  val box_bool : bool -> bool t
  
  val var_of_key : 'a Stepkey.t -> 'a t

  (*
    ------------------
    VALUES OF FORMULAS 
    ------------------
  *)
  val value_of_key : Z3.Model.model -> 'a Stepkey.t -> 'a option

  (*
    ----------------
    COMBINE FORMULAS
    ----------------
  *)
  val not_ : bool t -> bool t
  val plus : int t -> int t -> int t
  val minus : int t -> int t -> int t
  val times : int t -> int t -> int t
  val divide : int t -> int t -> int t
  val modulus : int t -> int t -> int t
  val less_than : int t -> int t -> bool t
  val less_than_eq : int t -> int t -> bool t
  val eq_ints : int t -> int t -> bool t
  val eq_bools : bool t -> bool t -> bool t
  val neq : int t -> int t -> bool t
  val and_ : bool t -> bool t -> bool t
  val or_ : bool t -> bool t -> bool t

  (*
    -----
    SOLVE
    -----
  *)
  val solve : bool t list -> Solve_status.t
end

module Make () : S = struct
  include Utils.Z3_api.Make (struct let ctx = Z3.mk_context [] end) 
  type 'a t = 'a E.t

  let solver = Z3.Solver.mk_solver ctx None

  let var_of_key (type a) (key : a Stepkey.t) : a E.t =
    match key with
    | I id -> int_var id
    | B id -> bool_var id

  let value_of_key model key =
    key
    |> var_of_key
    |> value_of_expr model

  let set_timeout time =
    time
    |> Time_float.Span.to_ms
    |> Float.iround_up_exn
    |> Int.to_string
    |> Z3.Params.update_param_value ctx "timeout"

  let solve bool_formulas =
    Z3.Solver.add solver (E.extract_list bool_formulas);
    (* Format.printf "Model is %s\n" (Z3.Solver.to_string solver); *)
    let res = Z3.Solver.check solver [] in
    match res with
    | Z3.Solver.SATISFIABLE ->
      let model = Z3.Solver.get_model solver in
      Z3.Solver.reset solver;
      Solve_status.Sat (Option.value_exn model)
    | _ -> Z3.Solver.reset solver; Unsat
end