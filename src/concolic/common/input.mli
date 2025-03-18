(**
  File: input.mli
  Purpose: represent inputs for concrete interpretation

  Detailed description:
    The language in this project supports integer and boolean
    inputs, which we often want to store together. Thus, this
    modules packs integers and booleans into a common type to 
    represent concrete program inputs.
*)

include Utils.Pack.S with type 'a x := 'a

val to_string : t -> string
