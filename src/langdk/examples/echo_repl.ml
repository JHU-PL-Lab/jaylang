open Langdk

module Echo_E : Repl.E_sig = struct
  type e = string

  let eval e = e
  let of_string e = e
  let to_string e = e
end

module Echo_repl = Repl.Make (Echo_E)

let () = Echo_repl.run ()
