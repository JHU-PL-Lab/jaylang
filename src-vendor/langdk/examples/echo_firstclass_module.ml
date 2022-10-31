open Langdk

let () =
  let module Echo_E = (val (module struct
                             type e = string

                             let eval e = e
                             let of_string e = e
                             let to_string e = e
                           end) : Repl.E_sig)
  in
  let module Echo_repl = Repl.Make (Echo_E) in
  Echo_repl.run ()
