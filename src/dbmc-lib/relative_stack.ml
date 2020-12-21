open Core

module T = struct
  type callsite = Id.t
  [@@deriving sexp, compare, equal]
  (* type caller = Id.t *)
  type fid = Id.t
  [@@deriving sexp, compare, equal]

  type frame = 
    (* cancelable call-return stack op *)
    | Call of callsite * fid

    (* it means how to reach the target, though there is no fid `f` here
       c = f x 
       the `f` can be constructed in interpreting
    *)
    | Caller of callsite * fid
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

let pop (co_stk,stk) callsite fid = 
  match stk with
  | Call(cs, _) ::stk' ->
    if Id.equal cs callsite then
      co_stk, stk'
    else
      failwith "dismatch"
  | _ :: _ ->
    failwith "frame mismatch"
  | [] ->
    Caller(callsite,fid) ::co_stk, stk

let concretize (co_stk,stk) =
  if not @@ List.is_empty stk then
    failwith "non-empty stack in main block"
  else
    ()
  ;
  [], List.rev co_stk

let relativize target_stk call_stk : t =
  let call_stk = List.map ~f:(fun (cs,fid) -> Id.of_ast_id cs, Id.of_ast_id fid) call_stk in
  let rec discard_common ts cs =
    match ts,cs with
    | Caller(cs1,fid1)::target_stk', (cs2,fid2)::call_stk' ->
      if Id.equal cs1 cs2 && Id.equal fid1 fid2 then
        discard_common target_stk' call_stk'
      else
        ts, cs
    | _, []
    | [], _ ->
      ts,cs
    | _, _ ->
      failwith "impossible discard_common"
  in
  let target_rev = List.rev target_stk in
  let call_rev = List.rev call_stk in
  let target_stk', call_stk' = discard_common target_rev call_rev in
  let co_stk = List.rev target_stk' in
  let stk = List.map ~f:(fun (cs,fid) -> Call(cs,fid)) call_stk' in
  co_stk, stk

(* 
when call_stk == target_stk
  return []

when call_sck == []
  return - target_stk
   *)
(* let relativize target_stk call_stk = *)

let%test _ =
  let x = Id.Ident "x" in
  let y = Id.Ident "y" in
  let s = empty |> fun s -> push s x x |> fun s -> push s x x in
  equal s (s |> to_string |> of_string)