open Core

let check_ext s =
  Filename.check_suffix s "natodefa" || Filename.check_suffix s "bjy"
