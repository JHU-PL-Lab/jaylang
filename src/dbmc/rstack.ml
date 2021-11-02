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

type stack =
  | H_empty
  | H_cons of { h_prev : h_stack; h_op : op; h_frame : frame }

and h_stack = stack Hashcons.hash_consed

module X = struct
  type t = stack

  let equal t1 t2 =
    match (t1, t2) with
    | H_empty, H_empty -> true
    | H_cons cell1, H_cons cell2 ->
        T.equal_op cell1.h_op cell2.h_op
        && T.equal_frame cell1.h_frame cell2.h_frame
        && phys_equal cell1.h_prev cell2.h_prev
    | _, _ -> false

  let hash = function
    | H_empty -> Hashtbl.hash H_empty
    | H_cons { h_op; h_frame; h_prev } ->
        (h_prev.tag * 19) + T.hash_frame h_frame + T.hash_op h_op
end

module H = Hashcons.Make (X)

let ht = H.create 17

let he () = H.hashcons ht H_empty

let cons h_op h_prev h_frame = H.hashcons ht (H_cons { h_op; h_prev; h_frame })

let x = he ()

let r1 () = cons Push x (Id.Ident "x", Id.Ident "y")

let () = assert (phys_equal x x)

let () = assert (phys_equal (r1 ()) (r1 ()))

let empty : h_stack = he ()

let push rstk frame : h_stack = cons Push rstk frame

let push_cond rstk frame : h_stack = cons Push_cond rstk frame

let rec lift_to_hstack = function
  | Empty -> empty
  | Cons { prev; op; frame } -> cons op (lift_to_hstack prev) frame

let rec lift_to_stack (h : h_stack) =
  match h.node with
  | H_empty -> Empty
  | H_cons { h_prev; h_op; h_frame } ->
      Cons { op = h_op; frame = h_frame; prev = lift_to_stack h_prev }

let h_stack_of_sexp s =
  let node = t_of_sexp s in
  let hnode = lift_to_hstack node in
  hnode

let sexp_of_h_stack (h : h_stack) =
  let node = lift_to_stack h in
  sexp_of_t node

let rec compare_h_stack (h1 : h_stack) (h2 : h_stack) =
  match (h1.node, h2.node) with
  | H_empty, H_empty -> 0
  | H_cons cell1, H_cons cell2 ->
      Std.chain_compare
        (fun () ->
          Std.chain_compare
            (fun () -> T.compare_op cell1.h_op cell2.h_op)
            (fun () -> T.compare_frame cell1.h_frame cell2.h_frame))
        (fun () -> compare_h_stack cell1.h_prev cell2.h_prev)
  | H_empty, _ -> -1
  | _, H_empty -> 1

let equal_h_stack (h1 : h_stack) (h2 : h_stack) = X.equal h1.node h2.node

let hash_fold_h_stack state (h_stack : h_stack) =
  match h_stack.node with
  | H_empty -> Hash.fold_int state 0
  | H_cons { h_prev; h_op; h_frame } ->
      Hash.fold_int
        (T.hash_fold_op (T.hash_fold_frame state h_frame) h_op)
        h_prev.tag

let rec pop (rstk : h_stack) frame : h_stack option =
  match rstk.node with
  | H_empty ->
      (* TODO: what if starting from a then-block *)
      Some (cons Co_pop rstk frame)
  | H_cons { h_op = Push; h_prev; h_frame = frame_prev } ->
      let f1, _ = frame in
      let f2, _ = frame_prev in
      if Id.equal f1 f2 then
        Some h_prev
      else (* failwith "unmathch pop from stk" *)
        None
  | H_cons { h_op = Co_pop; _ } -> Some (cons Co_pop rstk frame)
  | H_cons { h_op = Push_cond; h_prev; _ } -> pop h_prev frame

let rec paired_callsite (rstk : h_stack) this_f =
  match rstk.node with
  | H_empty -> None
  | H_cons { h_op = Push; h_frame; _ } ->
      let cs, fid = h_frame in
      if Id.equal fid this_f then
        Some cs
      else
        failwith "inequal f when stack is not empty"
  | H_cons { h_op = Co_pop; _ } -> None
  | H_cons { h_op = Push_cond; h_prev; _ } -> paired_callsite h_prev this_f

let rec concretize_top (rstk : h_stack) =
  match rstk.node with
  | H_empty -> []
  | H_cons { h_op = Co_pop; h_prev; h_frame } ->
      h_frame :: concretize_top h_prev
  | _ -> failwith "non-empty stack when concretize"

let relativize (target_stk : Concrete_stack.t) (call_stk : Concrete_stack.t) :
    h_stack =
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

let str_of_id h = str_of_id (lift_to_stack h)

let pp_id = Fmt.of_to_string str_of_id

let str_of_t = str_of_id

let pp = pp_id

let construct_stks (r_stk : h_stack) =
  let rec loop (r_stk : h_stack) co_stk stk =
    match r_stk.node with
    | H_empty -> (co_stk, stk)
    | H_cons { h_op = Co_pop; h_prev; h_frame } ->
        loop h_prev (h_frame :: co_stk) stk
    | H_cons { h_op = Push; h_prev; h_frame } ->
        loop h_prev co_stk (h_frame :: stk)
    | H_cons { h_op = Push_cond; h_prev; h_frame } ->
        loop h_prev (h_frame :: co_stk) stk
  in
  loop r_stk [] []

type t = h_stack
