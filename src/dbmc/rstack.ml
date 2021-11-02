open Core

module T = struct
  type frame = Id.t * Id.t [@@deriving sexp, compare, equal, hash]

  type op = Push | Push_cond | Co_pop [@@deriving sexp, compare, equal, hash]

  (* `co_stk` and `stk` are not necessary here if we choose to record frame change in `op` *)
  type t = Cons of { prev : t; op : op; frame : frame } | Empty
  [@@deriving sexp, compare, equal, hash]
end
(* When symbolic executing, we create counter on the fly.
   However, when relativize, we lose the info on the counter.

   Two solution to solve this:
   1. Keep the stack-tree, which is the collection of all the possible stacks.
   Thus, any concrete stacks can be treated as _paths_ on the tree.
   We can retrieve the nodes as needed.
   The pro is the counter is simple. The con is to maintain the tree.
   2. Use hashcons. We use hash-cons instead of the counter.
   The pro is the hashcons is a standard way. The con is slightly heavier than the counter.
*)

include T
include Comparator.Make (T)

let empty : t = Empty

let push rstk frame : t = Cons { prev = rstk; op = Push; frame }

let push_cond rstk frame : t = Cons { prev = rstk; op = Push_cond; frame }

let rec pop rstk frame : t option =
  match rstk with
  | Empty ->
      (* TODO: what if starting from a then-block *)
      Some (Cons { prev = rstk; op = Co_pop; frame })
  | Cons { op = Push; prev; frame = frame_prev } ->
      let f1, _ = frame in
      let f2, _ = frame_prev in
      if Id.equal f1 f2 then
        Some prev
      else (* failwith "unmathch pop from stk" *)
        None
  | Cons { op = Co_pop; _ } -> Some (Cons { prev = rstk; op = Co_pop; frame })
  | Cons { op = Push_cond; prev; _ } -> pop prev frame

let rec paired_callsite rstk this_f =
  match rstk with
  | Empty -> None
  | Cons { op = Push; frame; _ } ->
      let cs, fid = frame in
      if Id.equal fid this_f then
        Some cs
      else
        failwith "inequal f when stack is not empty"
  | Cons { op = Co_pop; _ } -> None
  | Cons { op = Push_cond; prev; _ } -> paired_callsite prev this_f

let rec concretize_top rstk =
  match rstk with
  | Empty -> []
  | Cons { op = Co_pop; prev; frame } -> frame :: concretize_top prev
  | _ -> failwith "non-empty stack when concretize"

let relativize (target_stk : Concrete_stack.t) (call_stk : Concrete_stack.t) : t
    =
  let rec discard_common ts cs =
    match (ts, cs) with
    | fm1 :: ts', fm2 :: cs' ->
        if equal_frame fm1 fm2 then
          discard_common ts' cs'
        else
          (ts, cs)
    | _, _ -> (ts, cs)
  in
  (* Reverse the call stack to make it stack top ~ list head ~ source first *)
  let call_rev = List.rev call_stk in
  let target_stk', call_stk' = discard_common target_stk call_rev in
  (* (target_stk', List.rev call_stk') *)
  let rstk' =
    List.fold (List.rev target_stk') ~init:empty ~f:(fun acc fm ->
        Option.value_exn (pop acc fm))
  in
  let rstk = List.fold (List.rev call_stk') ~init:rstk' ~f:push in
  rstk

let str_of_frame (Id.Ident x1, Id.Ident x2) = "(" ^ x1 ^ "," ^ x2 ^ ")"

let str_of_op = function Push -> "<-" | Co_pop -> "!" | Push_cond -> "<."

let rec str_of_id rstk =
  match rstk with
  | Empty -> ""
  | Cons { op = Co_pop; prev; frame } ->
      str_of_id prev ^ "<@" ^ str_of_frame frame ^ ";"
  | Cons { op = Push; prev; frame } ->
      str_of_id prev ^ "<-" ^ str_of_frame frame ^ ";"
  | Cons { op = Push_cond; prev; frame } ->
      str_of_id prev ^ "<." ^ str_of_frame frame ^ ";"

let pp_id = Fmt.of_to_string str_of_id

let str_of_t = str_of_id

let pp = pp_id

let construct_stks r_stk =
  let rec loop r_stk co_stk stk =
    match r_stk with
    | Empty -> (co_stk, stk)
    | Cons { op = Co_pop; prev; frame } -> loop prev (frame :: co_stk) stk
    | Cons { op = Push; prev; frame } -> loop prev co_stk (frame :: stk)
    | Cons { op = Push_cond; prev; frame } -> loop prev (frame :: co_stk) stk
  in
  loop r_stk [] []
