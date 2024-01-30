(* How many states a stream can be?
   From a lively perspective
   not created, created-and-running, paused, complete

   From a good-and-evil perspective
   good until complete
   becoming evil now (ok with the sent messages)
   unveil evil now (previous message should be revoked)
   evil turning good (resent previous messages)

   the good-and-evil can be extended into the priority-based ranking, while the better message should be
   handled earlier while the worse message should be handler later.

   the motivation to do this is at least a simple wrapped for lwt_stream which should be
   1. the difference bewteen `becoming evil now` and `complete` is a new stream can request a `complete`
   stream for message sent but request a `fail` stream can only get empty
   2. `fail` can propagate via _stream creation_ user functions

   who is in charge of an fail-message handling? Each stream (depending on how it's created) know how it
   should handle the failure (state-changing)
*)

open Core

type t =
  (* Pre-work : before a key-associated detail is created *)
  | Not_created
  (* Having receiver no working push *)
  | Initial
  (* Having receiver and a push *)
  | Running
  (* Having receiver and a push, but not work *)
  | Paused
  (* All work done *)
  | Done
  (* Work is failed. *)
  | Fail
[@@deriving sexp, equal]

let pp_status =
  Fmt.(using (fun status -> Sexp.to_string_hum (sexp_of_t status)) string)

let is_status_initial = function Initial -> true | _ -> false

let safe_transform from_ to_ =
  let is_valid =
    match (from_, to_) with
    | Initial, Running
    | Initial, Paused
    | Initial, Fail
    | Running, Paused
    | Paused, Running
    | Running, Done
    | Running, Fail
    | Paused, Done
    | Paused, Fail ->
        true
    | _, _ -> false
  in
  if is_valid
  then to_
  else failwith (Fmt.str "%a -> %a" pp_status from_ pp_status to_)

let to_running s = safe_transform s Running
let to_done s = safe_transform s Done
let to_fail s = safe_transform s Fail
