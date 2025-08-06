
let () = 
  match Cmdliner.Cmd.eval_value' Deferred.Cmain.eval with
  | `Ok _ -> ()
  | `Exit i -> exit i