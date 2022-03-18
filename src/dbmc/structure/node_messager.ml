open Core

type rule_based_details =
  | NA
  | FunEnter of { mutable funs : Id.t list; mutable args : Id.t list }
  | FunExit of { mutable funs : Id.t list; mutable rets : Id.t list }

type detail_kind = NA_kind | FunEnter_kind | FunExit_kind

type t = {
  stream : Lookup_result.t Lwt_stream.t;
  push : Lookup_result.t option -> unit;
  mutable outputs : Id.t list;
  details : rule_based_details;
}

let get_stream_for_read m = Lwt_stream.clone m.stream

let create () : t =
  let stream, push = Lwt_stream.create () in
  { stream; push; outputs = []; details = NA }

let create_fun_enter () : t =
  { (create ()) with details = FunEnter { funs = []; args = [] } }

let create_fun_exit () : t =
  { (create ()) with details = FunExit { funs = []; rets = [] } }

let create_with_kind kind : t =
  match kind with
  | NA_kind -> create ()
  | FunEnter_kind -> create_fun_enter ()
  | FunExit_kind -> create_fun_exit ()

let push_if_fresh m (x : Lookup_result.t) =
  if List.mem m.outputs x.from ~equal:Id.equal
  then ()
  else (
    m.outputs <- m.outputs @ [ x.from ];
    m.push (Some x))
