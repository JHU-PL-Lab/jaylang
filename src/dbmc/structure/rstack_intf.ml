open Core
open Dj_common
open Hashcons

module TT = struct
  module T = struct
    type frame = Id.t * Id.t [@@deriving hash, equal]
    type op = Push | Co_pop [@@deriving hash, equal]

    type stack = Cons of { prev : t; op : op; frame : frame } | Empty
    and t = stack Hashcons.hash_consed
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

  module X = struct
    open T

    type t = T.stack

    let equal t1 t2 =
      match (t1, t2) with
      | Empty, Empty -> true
      | Cons cell1, Cons cell2 ->
          T.equal_op cell1.op cell2.op
          && T.equal_frame cell1.frame cell2.frame
          && phys_equal cell1.prev cell2.prev
      | _, _ -> false

    let hash = function
      | Empty -> Hashtbl.hash Empty
      | Cons { op; frame; prev } ->
          (prev.tag * 19) + (T.hash_frame frame * 7) + T.hash_op op
  end

  module H = Hashcons.Make (X)
  include T

  type t = T.t

  let ht = H.create 100
  let empty = H.hashcons ht Empty
  let cons op prev frame = H.hashcons ht (Cons { op; prev; frame })
  let push rstk frame = cons Push rstk frame

  let pop rstk frame =
    match rstk.node with
    | Cons { op = Push; prev; frame = frame_prev } ->
        let f1, _ = frame in
        let f2, _ = frame_prev in
        if Id.equal f1 f2
        then Some prev
        else (* failwith "unmathch pop from stk" *)
          None
    | Empty | Cons { op = Co_pop; _ } -> Some (cons Co_pop rstk frame)

  let pop_at_condtop rstk frame =
    match rstk.node with
    | Cons { op = Push; prev; frame = frame_prev } ->
        let f1, _ = frame in
        let f2, _ = frame_prev in
        if Id.equal f1 f2
        then (true, prev)
        else failwith "impossible in CondTop"
    | Empty | Cons { op = Co_pop; _ } -> (false, cons Co_pop rstk frame)

  let concretize_top rstk : Concrete_stack.t =
    let rec loop rstk =
      match rstk.node with
      | Empty -> []
      | Cons { op = Co_pop; prev; frame } -> frame :: loop prev
      | _ -> failwith "non-empty stack when concretize"
    in
    Concrete_stack.of_list (loop rstk)

  let relativize (target_stk : Concrete_stack.t) (call_stk : Concrete_stack.t) :
      t =
    let rec discard_common ts cs =
      match (ts, cs) with
      | fm1 :: ts', fm2 :: cs' ->
          if equal_frame fm1 fm2 then discard_common ts' cs' else (ts, cs)
      | _, _ -> (ts, cs)
    in
    (* Reverse the call stack to make it stack top ~ list head ~ source first *)
    let call_stk = Concrete_stack.to_list call_stk in
    let target_stk = Concrete_stack.to_list target_stk in
    let call_rev = List.rev call_stk in
    let target_stk', call_stk' = discard_common target_stk call_rev in
    let rstk' =
      List.fold (List.rev target_stk') ~init:empty ~f:(fun acc fm ->
          Option.value_exn (pop acc fm))
    in
    let rstk = List.fold call_stk' ~init:rstk' ~f:push in
    rstk

  let str_of_frame (Id.Ident x1, Id.Ident x2) = "(" ^ x1 ^ "," ^ x2 ^ ")"
  let str_of_op = function Push -> "<-" | Co_pop -> "!"
  let to_string h = string_of_int h.hkey

  let rec length r_stk =
    match r_stk.node with Empty -> 0 | Cons { prev; _ } -> 1 + length prev

  let construct_stks r_stk =
    let rec loop r_stk co_stk stk =
      match r_stk.node with
      | Empty -> (co_stk, stk)
      | Cons { op = Co_pop; prev; frame } -> loop prev (frame :: co_stk) stk
      | Cons { op = Push; prev; frame } -> loop prev co_stk (frame :: stk)
    in
    loop r_stk [] []

  let rec pp oc rstk =
    match rstk.node with
    | Empty -> ()
    | Cons { op = Co_pop; prev; frame } ->
        Fmt.pf oc "-%s;%a" (str_of_frame frame) pp prev
    | Cons { op = Push; prev; frame } ->
        Fmt.pf oc "+%s;%a" (str_of_frame frame) pp prev

  let paired_callsite rstk this_f =
    match rstk.node with
    | Cons { op = Push; frame; _ } ->
        let cs, fid = frame in
        if Id.equal fid this_f
        then Some cs
        else
          failwith
            (Format.sprintf "paired_callsite: {rsk=%s} fid=%s, this_f=%s"
               (Fmt.to_to_string pp rstk) (Id.show fid) (Id.show this_f))
    | Empty | Cons { op = Co_pop; _ } -> None

  (* Used in Lookup_key *)
  let sexp_of_t (r : t) = [%sexp_of: int] r.hkey
  let compare (r1 : t) (r2 : t) = Int.compare r1.hkey r2.hkey
  let equal (r1 : t) (r2 : t) = X.equal r1.node r2.node
  let hash_fold_t state (r : t) = Hash.fold_int state r.hkey
  let hash r = r.hkey
end

module CC = struct
  include Comparator.Make (TT)
end

include TT
include CC
