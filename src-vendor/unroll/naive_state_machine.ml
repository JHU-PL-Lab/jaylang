open Core

type t =
  (* Pre-work *)
  | Not_created
  | Initial
  (* Working *)
  | Running
  (* Closed *)
  | Done
  | Fail
[@@deriving sexp]

let pp_status =
  Fmt.(using (fun status -> Sexp.to_string_hum (sexp_of_t status)) string)

let is_status_initial = function Initial -> true | _ -> false

let safe_transform from_ to_ =
  let is_valid =
    match (from_, to_) with
    | Initial, Running -> true
    | Running, Done -> true
    | Running, Fail -> true
    | _, _ -> false
  in
  if is_valid
  then to_
  else failwith (Fmt.str "%a -> %a" pp_status from_ pp_status to_)

let to_running s = safe_transform s Running
let to_done s = safe_transform s Done
let to_fail s = safe_transform s Fail
