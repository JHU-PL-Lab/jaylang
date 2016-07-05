open Batteries;;

module type Stack = Ddpa_context_stack.Context_stack;;
let name_parsing_functions =
  [
    (* A function for the literally-named modules. *)
    (fun name ->
       match name with
       | "0ddpa" ->
         Some (module Ddpa_unit_stack.Stack : Stack)
       | "1ddpa" ->
         Some (module Ddpa_single_element_stack.Stack : Stack)
       | "2ddpa" ->
         Some (module Ddpa_two_element_stack.Stack : Stack)
       | "ddpaNR" ->
         Some (module Ddpa_nonrepeating_stack.Stack : Stack)
       | "none" -> None
       | _ -> raise Not_found
    )
    ;
    (* A function for parsing kddpa *)
    (fun name ->
       if not @@ String.ends_with name "ddpa" then raise Not_found;
       let num_str = String.sub name 0 @@ String.length name - 4 in
       try
         let num = int_of_string num_str in
         let module Spec : Ddpa_n_element_stack.Spec =
         struct
           let size = num
         end
         in
         let module NStack = Ddpa_n_element_stack.Make(Spec) in
         Some (module NStack : Stack)
       with
       | Failure _ -> raise Not_found
    )
  ];;
let stack_from_name name =
  let rec loop fns =
    match fns with
    | [] -> raise Not_found
    | fn::fns' ->
      begin
        try
          fn name
        with
        | Not_found -> loop fns'
      end
  in
  loop name_parsing_functions
;;
