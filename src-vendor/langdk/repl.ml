(* -E-- *)
module type E_sig = sig
  type e

  val eval : e -> e
  val of_string : string -> e
  val to_string : e -> string
end

open Lwt
open LTerm_text

type state = { n : int }

let make_prompt state =
  let prompt = Printf.sprintf "In  [%d]: " state.n in
  eval [ LTerm_text.S prompt ]

let make_output state out =
  let output = Printf.sprintf "Out [%d]: %s" state.n out in
  eval [ LTerm_text.S output ]

open Core

class read_line ~term ~history ~state =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term
    method! show_box = true

    method! completion =
      let prefix = Zed_rope.to_string self#input_prev in
      let candidates =
        List.map ~f:Zed_string.unsafe_of_utf8 [ "first"; "second" ]
        |> List.append history
        |> List.filter ~f:(fun file -> Zed_string.starts_with ~prefix file)
        |> List.map ~f:(fun file -> (file, Zed_string.unsafe_of_utf8 " "))
      in
      self#set_completion 0 candidates

    initializer self#set_prompt (React.S.const (make_prompt state))
  end

(* R-PL *)
module Make (E : E_sig) = struct
  let readline_loop term history prompt_state =
    let rec loop prompt_state () =
      let rl =
        new read_line
          ~term
          ~history:(LTerm_history.contents history)
          ~state:prompt_state
      in
      (rl#run >|= fun command -> Some command) >>= function
      | Some command ->
          LTerm.flush term >>= fun () ->
          LTerm_history.add history command ;
          let e = Zed_string.to_utf8 command in
          let prompt_state' = { n = prompt_state.n + 1 } in
          let e' = e |> E.of_string |> E.eval |> E.to_string in
          LTerm.fprintls term (make_output prompt_state e') >>= fun () ->
          loop prompt_state' ()
      | None -> fail_with "readline_loop: command is None"
    in
    Lwt.catch (loop prompt_state) (function
      | Sys_unix.Break -> return None
      | exn -> Lwt.fail exn)

  let run_lwt () =
    LTerm_inputrc.load () >>= fun () ->
    Lwt.catch
      (fun () ->
        Lazy.force LTerm.stdout >>= fun term ->
        LTerm.show_cursor term >>= fun () ->
        let prompt_state = { n = 1 } in
        let history = LTerm_history.create [] in
        readline_loop term history prompt_state >>= fun _ -> Lwt.return_unit)
      (function
        | LTerm_read_line.Interrupt -> Lwt.return () | exn -> Lwt.fail exn)

  let run () = Lwt_main.run (run_lwt ())
end
