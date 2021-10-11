let () =
  let filename = Sys.argv.(1) in
  (* Analysis.Size_dataflow.run filename *)
  (* Analysis.Size.run filename *)
  Analysis.Abstract_interpreter.run filename
(* Analysis.Size_tabulate.run filename *)
(* Analysis.Main.run filename *)
