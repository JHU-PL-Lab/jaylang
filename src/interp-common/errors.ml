
(* 
  Use polymorphic variants so they are extensible but safer
  than actual extensible variants.

  Performance shouldn't often matter for errors because they
  are typically handled only at the end of a run.
*)