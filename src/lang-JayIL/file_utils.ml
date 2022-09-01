open Core

let check_ext s =
  Filename.check_suffix s "odefa" || Filename.check_suffix s "jil"