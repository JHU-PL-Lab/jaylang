open Lwt
open LTerm_text
open Dj_common

(* +-----------------------------------------------------------------+
   | Interpreter                                                     |
   +-----------------------------------------------------------------+ *)

type state = { n : int; paused : bool }

(* A simple model of an interpreter. It maintains some state, and exposes a function
 *   eval : state -> input -> (new_state, output) *)
module Interpreter = struct
  let eval state s =
    let out = "evaluated " ^ s in
    let new_state = { state with n = state.n + 1 } in
    (new_state, out)
end

(* +-----------------------------------------------------------------+
   | Prompt and output wrapping                                      |
   +-----------------------------------------------------------------+ *)

(* Create a prompt based on the current interpreter state *)
let make_prompt state =
  let prompt = Printf.sprintf "[%2d] ⋊> " state.n in
  eval [ S prompt ]

(* Format the interpreter output for REPL display *)
let make_output state _out =
  let output = Printf.sprintf "Out [%d]: " state.n (* out *) in
  eval [ S output ]

(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

class read_line ~term ~history ~state =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term
    method! show_box = false

    (* method completion_words : (Zed_string.t * Zed_string.t) list signal *)
    method! completion_words =
      React.S.const
        [
          (Zed_string.of_utf8 "step (default)", Zed_string.of_utf8 "");
          (Zed_string.of_utf8 "pause", Zed_string.of_utf8 "");
          (Zed_string.of_utf8 "play", Zed_string.of_utf8 "");
        ]

    initializer
      self#set_prompt
        (let _ = state in
         React.S.const (make_prompt state))
  end

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)
(* loop term history state () *)

let readline_loop term history prompt_state (config : Global_config.t) program
    state =
  let rec loop prompt_state () =
    let rl =
      new read_line
        ~term
        ~history:(LTerm_history.contents history)
        ~state:prompt_state
    in
    (rl#run >|= fun command -> Some command) >>= function
    | Some command ->
        LTerm_history.add history command ;
        let command_utf8 = Zed_string.to_utf8 command in
        let prompt_state, _out = Interpreter.eval prompt_state command_utf8 in
        (* LTerm.printls (LTerm_text.of_utf8 "some command") >>= fun () -> *)
        (* LTerm.fprintls term (make_output state out) >>= fun () -> *)
        ignore config ;
        ignore program ;
        ignore state ;

        (if command = Zed_string.of_utf8 "pause"
         then Lwt.return_unit
         else if command = Zed_string.of_utf8 "graph"
         then (
           Dbmc.Graphviz.output_graph ~model:None ~testname:config.filename
             state ;
           Lwt.return_unit)
         else (
           (* if command = Zed_string.of_utf8 "step" *)
           Lwt_mutex.unlock Control_center.mutex ;
           Lwt.pause () >>= fun () -> Lwt_mutex.lock Control_center.mutex))
        (* >>= fun () -> Lwt.return_unit *)
        >>= fun () -> loop prompt_state ()
    | None -> fail_with "why none"
  in
  Lwt.catch (loop prompt_state) (function
    | Sys.Break -> return None
    | exn -> Lwt.fail exn)

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let make_lookup () =
  let open Core in
  let open Dbmc in
  let config : Global_config.t = Argparse.parse_commandline () in
  let program = File_utils.read_source config.filename in
  let state : Global_state.t = Global_state.create config program in
  let lookup_task () =
    Lwt_mutex.lock Control_center.mutex >>= fun () ->
    Main.main_lookup ~config ~state >>= fun inputss ->
    (match List.hd inputss with
    | Some inputs ->
        Format.printf "[%s]\n"
          (String.concat ~sep:","
          @@ List.map
               ~f:(function Some i -> string_of_int i | None -> "-")
               inputs)
    | None -> Format.printf "Unreachable") ;
    Lwt.return_unit
  in
  (config, program, state, lookup_task)

let main () =
  (* Core.Command.run Dj.command ; *)
  LTerm_inputrc.load () >>= fun () ->
  Lwt.catch
    (fun () ->
      Lazy.force LTerm.stdout >>= fun term ->
      let config, program, state, lookup_task = make_lookup () in
      Lwt.async lookup_task ;
      let prompt_state = { n = 1; paused = true } in
      let history = LTerm_history.create [] in
      readline_loop term history prompt_state config program state >>= fun _ ->
      Lwt.return_unit)
    (function
      | LTerm_read_line.Interrupt -> Lwt.return () | exn -> Lwt.fail exn)

let () = Lwt_main.run (main ())
