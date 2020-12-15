open Core

module T = struct
  type callsite = Id.t
  [@@deriving sexp, compare, equal]
  (* type caller = Id.t *)
  type fid = Id.t
  [@@deriving sexp, compare, equal]

  type frame = 
    | Call of callsite * fid
    | Fun of fid
    | Caller of callsite
  [@@deriving sexp, compare, equal]

  type t = frame list * frame list
  [@@deriving sexp, compare, equal]
end

include T
include Comparator.Make(T)

let pp format v =
  v |> sexp_of_t |> Sexp.pp_hum format

let to_string v = 
  v |> sexp_of_t |> Sexp.to_string_mach

let of_string s =
  s |> Sexp.of_string |> t_of_sexp

let empty = ([], [])

let push (co_stk,stk) callsite fid =
  co_stk, Call(callsite,fid) :: stk

let pop (co_stk,stk) ~callsite = 
  match stk with
  | Call(cs, _) ::stk' ->
    if Id.equal cs callsite then
      co_stk, stk'
    else
      failwith "dismatch"
  | _ :: _ ->
    failwith "frame mismatch"
  | [] ->
    Caller(callsite) ::co_stk, stk

let stackize (co_stk,stk) =
  if not @@ List.is_empty stk then
    failwith "non-empty stack in main block"
  else
    ()
  ;
  [], List.rev co_stk

let%test _ =
  let x = Id.Ident "x" in
  let y = Id.Ident "y" in
  let s = empty |> fun s -> push s x x |> fun s -> push s x x in
  equal s (s |> to_string |> of_string)