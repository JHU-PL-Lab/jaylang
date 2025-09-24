
(*
  Answers from concolic evaluation. See the formal spec.
  Well_typed encompasses Ok.

  This type makes sense in the context of using concolic evaluation
  for type checking type-embedded programs.
*)

type t = 
  | Well_typed   (* The program has been proven well-typed *)
  | Unknown      (* There is no sure answer. The program may be ill-typed or well-typed. *)
  | Ill_typed    (* A type error has be found. *)
