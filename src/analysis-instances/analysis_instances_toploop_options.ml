open Batteries;;
open BatOptParse.Opt;;
open Analysis_instances;;

module type A = Analysis.Analysis_sig;;

(** This logging option selects a particular CBA analysis to perform. *)
let select_analysis_option =
  (* This ref contains a module option.  If the option is None, no analysis is
     to be performed. *)
  let analysis_module_ref = ref (Some (module Cba_1stack : A)) in
  {
    option_set = (fun option_name args ->
      match args with
      | [analysis_name] ->
        let analysis_module =
          begin
            match analysis_name with
            | "cba0" -> Some (module Cba_0stack : A)
            | "cba1" -> Some (module Cba_1stack : A)
            | "cba2" -> Some (module Cba_2stack : A)
            | "cbanr" -> Some (module Cba_nonrepeating_stack : A)
            | "none" -> None
            | _ -> raise @@ Option_error (option_name,
                      Printf.sprintf "Invalid analysis name: %s" analysis_name)
          end
        in
        analysis_module_ref := analysis_module
      | _ ->
        raise @@ Option_error (option_name,
          Printf.sprintf "Invalid argument count: %d" (List.length args))
      )
    ;
    option_set_value = (fun analysis_module_option ->
      analysis_module_ref := analysis_module_option
      )
    ;
    option_get = (fun () -> Some (!analysis_module_ref))
    ;
    option_metavars = ["ANALYSIS"]
    ;
    option_defhelp = Some("Selects an analysis (cba0,cba1,cba1,cbanr,none).")
    ;
  };;