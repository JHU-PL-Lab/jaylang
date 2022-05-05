open Core

(*
   module type X_int = sig val x : int end;;

   utop # let rec mm = (module struct let x = y end : X_int) and y = 4;;
   val mm : (module X_int) = <module>
*)

module Unroll_S_dbmc :
  Unroll.M_sig
    with type message = Lookup_result.t
     and type result = Lookup_result.t
     and type key = Lookup_key.t = struct
  type message = Lookup_result.t
  type result = Lookup_result.t
  type key = Lookup_key.t

  let equal_message (m1 : Lookup_result.t) (m2 : Lookup_result.t) =
    Lookup_key.equal m1.from m2.from
end

module U_dbmc = Unroll.Make (Lookup_key) (Unroll_S_dbmc)

(* module Unroll_S_ddse :
     Unroll.M_sig
       with type message = Lookup_key.t * Phi_set.t
        and type result = Lookup_key.t * Phi_set.t
        and type key = Lookup_key.t = struct
     type message = Lookup_key.t * Phi_set.t
     type result = Lookup_key.t * Phi_set.t
     type key = Lookup_key.t

     let equal_message (k1, phis1) (k2, phis2) =
       Lookup_key.compare k1 k2 = 0
       (* && (Ast.equal_value v1 v2) *)
       && Phi_set.compare phis1 phis2 = 0
   end *)

module Unroll_S_ddse :
  Unroll.M_sig
    with type message = Ddse_result.t
     and type result = Ddse_result.t
     and type key = Lookup_key.t = struct
  type message = Ddse_result.t
  type result = Ddse_result.t
  type key = Lookup_key.t

  let equal_message = Ddse_result.equal
end

module U_ddse = Unroll.Make (Lookup_key) (Unroll_S_ddse)
