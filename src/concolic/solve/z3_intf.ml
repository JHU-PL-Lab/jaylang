
open Core

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
  module Solve_status : sig
    type t =
      | Sat of Z3.Model.model
      | Unknown
      | Unsat
  end

  val solve : bool t list -> Solve_status.t
end

module Make () : S = struct
  include Utils.Z3_api.Make (struct let ctx = Z3.mk_context [] end) 
  type 'a t = 'a E.t

  let var_of_key (type a) (key : a Stepkey.t) : a t =
    match key with
    | I id -> int_var id
    | B id -> bool_var id

  let value_of_key model key =
    key
    |> var_of_key
    |> value_of_expr model
end