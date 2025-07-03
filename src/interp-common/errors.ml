
(* 
  Use polymorphic variants so they are extensible but safer
  than actual extensible variants.

  Performance shouldn't often matter for errors because they
  are typically handled only at the end of a run.
*)

type 'a t =
  [ `XAbort of string * 'a
  | `XType_mismatch of string * 'a
  | `XUnbound_variable of Lang.Ast.Ident.t * 'a
  ]

module Runtime = struct
  type nonrec 'a t =
    [ 'a t (* all the above errors *)
    | `XDiverge of 'a 
    | `XReach_max_step of 'a ]
end
