
let () = 
  match Cmdliner.Cmd.eval_value' Concolic.Driver.Deferred.eval with
  | `Ok _ -> ()
  | `Exit i -> exit i