
(* 
  Use polymorphic variants so they are extensible but safer
  than actual extensible variants.

  Performance shouldn't often matter for errors because they
  are typically handled only at the end of a run.
*)

type 'a with_message = { body : 'a ; msg : string }

type 'a t =
  [ `XAbort of 'a with_message
  | `XType_mismatch of 'a with_message
  | `XUnbound_variable of Lang.Ast.Ident.t * 'a
  ]

module Runtime = struct
  type nonrec 'a t =
    [ 'a t (* all the above errors *)
    | `XVanish of 'a 
    | `XReach_max_step of 'a ]
end
