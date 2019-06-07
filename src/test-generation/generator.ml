open Batteries;;
open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;

open Generator_configuration;;
open Generator_types;;

module Symbolic_interpreter = Odefa_symbolic_interpreter;;
module Formulae = Symbolic_interpreter.Formulae;;

let input_sequence_from_formulae
    (formulae : Formulae.t)
    (e : expr)
    (x : Ident.t)
  : int list =
  ignore formulae; ignore e; ignore x;
  raise @@ Jhupllib.Utils.Not_yet_implemented "input_sequence_from_formulae"
;;

let rec take_steps
    (e : expr)
    (x : Ident.t)
    (max_steps : int)
    (evaluation : Symbolic_interpreter.Interpreter.evaluation)
  : test_generation_result =
  let rec loop
      (step_count : int)
      (ev : Symbolic_interpreter.Interpreter.evaluation)
    : test_generation_result =
    if step_count = max_steps then
      { tgr_inputs = None;
        tgr_steps = step_count;
        tgr_total_steps = raise @@ Jhupllib.Utils.Not_yet_implemented "take_steps";
        tgr_generator =
          { tg_program = e;
            tg_target = x;
            tg_generator_fn =
              Some(fun n -> take_steps e x n ev)
          };
      }
    else
      let results, ev'_opt = Odefa_symbolic_interpreter.Interpreter.step ev in
      if List.is_empty results then
        match ev'_opt with
        | Some ev' ->
          (* No result and no termination.  Keep running. *)
          loop (step_count + 1) ev'
        | None ->
          (* No result and no remaining computation; we terminated!  Give back a
             result indicating as much. *)
          { tgr_inputs = None;
            tgr_steps = step_count;
            tgr_total_steps = raise @@ Jhupllib.Utils.Not_yet_implemented "take_steps";
            tgr_generator =
              { tg_program = e;
                tg_target = x;
                tg_generator_fn = None;
              };
          }
      else
        (* We have results!  We want to report them, but the interface only
           allows one to be reported at a time... so it's time we hack this up.
           We'll just build a routine that, for each step, generates one
           additional result from our list. *)
        let rec delayed_results
            (steps_for_this_result : int)
            (results :
               Symbolic_interpreter.Interpreter.evaluation_result list)
            (evaluation_opt :
               Symbolic_interpreter.Interpreter.evaluation option)
          : test_generation_result =
          match results with
          | [] -> raise @@
            Jhupllib.Utils.Invariant_failure "delayed_results with empty list"
          | [result] ->
            let formulae = result.er_formulae in
            { tgr_inputs = Some(input_sequence_from_formulae formulae e x);
              tgr_steps = steps_for_this_result;
              tgr_total_steps = raise @@ Jhupllib.Utils.Not_yet_implemented "take_steps";
              tgr_generator =
                { tg_program = e;
                  tg_target = x;
                  tg_generator_fn = None;
                };
            }
          | result::results' ->
            let formulae = result.er_formulae in
            { tgr_inputs = Some(input_sequence_from_formulae formulae e x);
              tgr_steps = steps_for_this_result;
              tgr_total_steps = raise @@ Jhupllib.Utils.Not_yet_implemented "take_steps";
              tgr_generator =
                { tg_program = e;
                  tg_target = x;
                  tg_generator_fn =
                    (Some(fun _ -> delayed_results 1 results' evaluation_opt))
                };
            }
        in
        delayed_results step_count results ev'_opt
  in
  loop 0 evaluation
;;

let create (conf : configuration) (e : expr) (x : Ident.t) : test_generator =
  let module Stack = (val conf.conf_context_model) in
  let module Analysis = Ddpa_analysis.Make(Stack) in
  let cfg =
    e
    |> Analysis.create_initial_analysis
    |> Analysis.perform_full_closure
    |> Analysis.cfg_of_analysis
  in
  let evaluation = Odefa_symbolic_interpreter.Interpreter.start cfg e x in
  { tg_program = e;
    tg_target = x;
    tg_generator_fn = Some(fun n -> take_steps e x n evaluation)
  }
;;

let generate_inputs
    ?generation_callback:(generation_callback=fun _ _ -> ())
    (max_steps_opt : int option)
    (original_generator : test_generator)
  : (int list * int) list * test_generator option =
  let max_steps_per_loop = 100 in
  let rec loop
      (generator : test_generator)
      (steps_left_opt : int option)
      (steps_taken : int)
      (results : (int list * int) list)
    : (int list * int) list * test_generator option =
    let steps_to_take =
      match steps_left_opt with
      | None -> max_steps_per_loop
      | Some n -> min n max_steps_per_loop
    in
    if steps_to_take = 0 then
      (* We're quitting now! *)
      (results, Some generator)
    else
      match generator.tg_generator_fn with
      | None ->
        (* No further generation is possible. *)
        (results, None)
      | Some fn ->
        let result = fn max_steps_per_loop in
        let steps_taken' = steps_taken + result.tgr_steps in
        let results' =
          match result.tgr_inputs with
          | Some input_sequence ->
            generation_callback input_sequence steps_taken';
            (input_sequence, steps_taken') :: results
          | None ->
            results
        in
        let steps_left_opt' =
          Option.map (fun n -> max 0 @@ n - result.tgr_steps) steps_left_opt
        in
        loop result.tgr_generator steps_left_opt' steps_taken' results'
  in
  loop original_generator max_steps_opt 0 []
;;
