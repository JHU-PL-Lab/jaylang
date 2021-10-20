open Core

module T = struct
  type frame = Id.t * Id.t [@@deriving sexp, compare, equal, hash]

  let pp_frame oc (cs, fid) = Fmt.(pf oc "(%a,%a)" Id.pp cs Id.pp fid)

  type t = frame list * frame list [@@deriving sexp, compare, equal, hash]

  let pp oc (s1, s2) =
    Fmt.(pf oc "[%a]" (list ~sep:(any ";") pp_frame) (s1 @ s2))

  let pp_chucked oc (s1, s2) =
    let sc = List.chunks_of (s1 @ s2) ~length:3 in
    Fmt.(pf oc "[%a]" (Std.pp_llist pp_frame) sc)

  let show = Fmt.to_to_string pp
end

include T
include Comparator.Make (T)

let empty = ([], [])

(* stack grows on headers: the toper (of the callstack/source), the header (of the list) *)
let push (co_stk, stk) callsite fid = (co_stk, (callsite, fid) :: stk)

let paired_callsite (_co_stk, stk) this_f =
  match stk with
  | (cs, fid) :: _ ->
      if Id.equal fid this_f then
        Some cs
      else
        failwith "paired_callsite 1"
  | [] -> None

(* costack grows on headers: stack top ~ list head ~ source first *)
let pop_check_paired (co_stk, stk) callsite fid =
  match stk with
  | (cs, _) :: stk' ->
      if Id.equal cs callsite then
        Some ((co_stk, stk'), true)
      else (* failwith "dismatch" *)
        None
  | [] -> Some (((callsite, fid) :: co_stk, stk), false)

let pop rel_stack callsite fid =
  match pop_check_paired rel_stack callsite fid with
  | Some (stk, _) -> Some stk
  | None -> None

(* concretize the costack: stack top ~ list head ~ source first *)
let concretize (co_stk, stk) : Concrete_stack.t =
  if not @@ List.is_empty stk then
    failwith "concretize: stack is not empty"
  else
    co_stk

(* the target stack is stack top ~ list head ~ source first ,
   the call stack is stack top ~ list head ~ source last,
   the constraint is in the former style, the result of `relativize`
   also need to be in the former style.
*)
let relativize (target_stk : Concrete_stack.t) (call_stk : Concrete_stack.t) : t
    =
  let rec discard_common ts cs =
    match (ts, cs) with
    | (cs1, fid1) :: target_stk', (cs2, fid2) :: call_stk' ->
        if Id.equal cs1 cs2 && Id.equal fid1 fid2 then
          discard_common target_stk' call_stk'
        else
          (ts, cs)
    | _, _ -> (ts, cs)
  in
  (* Reverse the call stack to make it stack top ~ list head ~ source first *)
  let call_rev = List.rev call_stk in
  let target_stk', call_stk' = discard_common target_stk call_rev in
  (target_stk', List.rev call_stk')
