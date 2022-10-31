open! Core
module R = Dbmc.Rstack

let x = Dbmc.Id.Ident "x"
let f = Dbmc.Id.Ident "f"
let r0 = R.empty
let xf : R.frame = (x, f)
let r1 = R.push r0 xf

let%expect_test _ =
  Fmt.pr "%a" R.pp r1 ;
  [%expect {| +(x,f); |}] ;
  Fmt.pr "%s" (R.to_string r1) ;
  [%expect {| 5775445702 |}]
(* String.equal printed showed *)
