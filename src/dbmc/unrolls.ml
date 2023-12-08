open Core

(*
   module type X_int = sig val x : int end;;

   utop # let rec mm = (module struct let x = y end : X_int) and y = 4;;
   val mm : (module X_int) = <module>
*)

module Unroll_S_dbmc = struct
  type payload = Lookup_result.t

  let equal_payload (m1 : Lookup_result.t) (m2 : Lookup_result.t) =
    Lookup_key.equal m1.from m2.from
end

module U_dbmc = Unroll.Make_just_payload_no_wait (Lookup_key) (Unroll_S_dbmc)

module Unroll_S_ddse = struct
  type payload = Ddse_result.t

  let equal_payload = Ddse_result.equal
end

module U_ddse = Unroll.Make_just_payload_no_wait (Lookup_key) (Unroll_S_ddse)
