open Core
open Unroll
open Core
open Unroll.Stream_helper

module Int_payload = struct
  type payload = int [@@deriving equal]
end

module U = Unroll.Make_use_key (Int) (Int_payload)

let () =
  let nums = List.init 10 ~f:Fn.id in
  let u = U.create () in
  List.iter nums ~f:(fun msg -> U.push u 2 (Some msg)) ;
  let ans = U.get_available_payloads u 2 in
  Fmt.(pr "%a" (Dump.list int) ans)
