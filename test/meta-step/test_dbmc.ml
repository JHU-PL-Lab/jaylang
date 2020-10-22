open Batteries
open Program_samples

open Odefa_symbolic_interpreter

let test program target = 
  let phis = Dbmc.lookup_main program (Ident target) in
  print_endline @@ Printf.sprintf "%s"
    (Jhupllib.Pp_utils.pp_to_string
       (Jhupllib.Pp_utils.pp_list Dbmc.Phi.pp_phi) phis)

let () =
  (* test ep1 "y";
     test ep2 "y";
     test ep3 "y";
     test ep4 "y";
     test ep5 "y";
     test ep6 "y"; 
  *)
  test ep7 "r";
  ();
  test ep8 "target";
  ()