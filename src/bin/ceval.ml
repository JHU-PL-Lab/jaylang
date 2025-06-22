(**
  Bin [ceval].

  This executable runs the concolic evaluator on the Bluejay
  program at the given file path.
*)

let () = 
  match Cmdliner.Cmd.eval_value' Concolic.Driver.ceval with
  | `Ok _ -> ()
  | `Exit i -> exit i
