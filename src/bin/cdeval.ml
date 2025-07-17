
let () = 
  match Cmdliner.Cmd.eval_value' Deferred.Cmain.cdeval with
  | `Ok _ -> ()
  | `Exit i -> exit i