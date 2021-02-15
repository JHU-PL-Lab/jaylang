open Core

module T = struct
  type callsite = Id.t
  [@@deriving sexp, compare, equal, show {with_path = false}]

  type fid = Id.t
  [@@deriving sexp, compare, equal, show {with_path = false}]

  type frame = 
    | Pairable of callsite * fid
    | Dangling of callsite * fid
  [@@deriving sexp, compare, equal]

  let pp_frame oc frame = 
    let cf_pair = Fmt.Dump.pair pp_callsite pp_fid in
    match frame with
    | Pairable (cs, fid) -> Fmt.(pf oc "-%a" cf_pair (cs,fid))
    | Dangling (cs, fid) -> Fmt.(pf oc "+%a" cf_pair (cs,fid))
  let show_frame = Fmt.to_to_string pp_frame

  type t = frame list * frame list
  [@@deriving sexp, compare, equal]

  let pp oc (s1,s2) = 
    Fmt.(pf oc "%a" (Dump.list pp_frame) (s1 @ s2))

  let show = Fmt.to_to_string pp
end

include T
include Comparator.Make(T)

let empty = ([], [])

let push (co_stk,stk) callsite fid =
  co_stk, Pairable(callsite,fid) :: stk

let paired_callsite (_co_stk,stk) this_f = 
  match stk with
  | Pairable(cs,fid) ::_ ->
    if Id.equal fid this_f then
      Some cs
    else
      failwith "paired_callsite 1"  
  | [] ->
    None
  | _ :: _ ->
    failwith "paired_callsite 2"  

let pop (co_stk,stk) callsite fid = 
  match stk with
  | Pairable(cs, _) ::stk' ->
    if Id.equal cs callsite then
      Some (co_stk, stk')
    else
      (* failwith "dismatch" *)
      None
  | _ :: _ ->
    failwith "frame mismatch"
  | [] ->
    Some (Dangling(callsite,fid)::co_stk, stk)

let concretize (co_stk,stk) : Concrete_stack.t =
  if List.is_empty stk then
    ()
  else
    failwith "non-empty stack in main block"
  ;
  List.map co_stk ~f:(function 
      | Pairable(_,_) -> failwith "wrong stack frame"
      | Dangling(callsite,fid) -> (callsite,fid)
    )

let relativize (target_stk : Concrete_stack.t) (call_stk : Concrete_stack.t) : t =
  let rec discard_common ts cs =
    match ts,cs with
    | (cs1,fid1)::target_stk', (cs2,fid2)::call_stk' ->
      if Id.equal cs1 cs2 && Id.equal fid1 fid2 then
        discard_common target_stk' call_stk'
      else
        ts, cs
    | _, []
    | [], _ ->
      ts,cs
  in
  (* target_stk is already _rev_ when `concretize` *)
  let call_rev = List.rev call_stk in
  let target_stk', call_stk' = discard_common target_stk call_rev in
  let co_stk = target_stk'
               |> List.rev
               |> List.map ~f:(fun (cs,fid) -> Dangling(cs,fid)) in
  let stk = List.map ~f:(fun (cs,fid) -> Pairable(cs,fid)) call_stk' in
  co_stk, stk

(* 
let rec same_stack rstk (cstk : Concrete_stack.t) =
  match rstk, cstk with
  | Caller(cs1,fid1)::rs, (cs2,fid2)::cs ->
    Id.equal cs1 cs2 && Id.equal fid1 fid2 &&
    same_stack rs cs
  | [], [] -> true
  | _, _ -> false *)