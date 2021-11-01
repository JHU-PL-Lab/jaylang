open Core

module T = struct
  type frame = Id.t * Id.t [@@deriving sexp, compare, equal, hash]

  (* No `Pop`. `Pop` won't generate a new rstk, t.prev will be returned. *)
  type op = Push | Pop | Co_pop

  (* `co_stk` and `stk` are not necessary here if we choose to record frame change in `op` *)
  and structure = Cons of { prev : structure; op : op } | Empty

  and view = { co_stk : frame list; stk : frame list }

  and t = (structure[@hash.ignore]) * view
  [@@deriving sexp, compare, equal, hash]
end

(* a stack can have id and view. the id is unique while the view is not.
   Stacks with different ids can have the same view.
   The view is used as hash key.
*)

include T
include Comparator.Make (T)

let empty : t = (Empty, { co_stk = []; stk = [] })

let push rstk frame : t =
  let s, v = rstk in
  let s' = Cons { prev = s; op = Push } in
  let v' = { co_stk = v.co_stk; stk = frame :: v.stk } in
  (s', v')

let pop rstk frame : t option =
  let s, v = rstk in
  match s with
  | Empty ->
      let s' = Cons { prev = s; op = Co_pop } in
      let v' = { co_stk = [ frame ]; stk = [] } in
      Some (s', v')
  | Cons { op = Push; prev } ->
      let frame2 = List.hd_exn v.stk in
      let f1, _ = frame in
      let f2, _ = frame2 in
      if Id.equal f1 f2 then (
        let v' = { co_stk = v.co_stk; stk = List.tl_exn v.stk } in
        let choice1 = (prev, v') in
        let choice2 = (Cons { prev = s; op = Pop }, v') in

        assert (T.hash choice1 = T.hash choice2);
        Some choice1)
      else (* failwith "unmathch pop from stk" *)
        None
  | Cons { op = Co_pop; _ } ->
      if List.is_empty v.stk then
        let s' = Cons { prev = s; op = Co_pop } in
        let v' = { co_stk = frame :: v.co_stk; stk = [] } in
        Some (s', v')
      else
        failwith "non-empty stack when co_pop"
  | Cons { op = Pop; prev } ->
      if List.is_empty v.stk then (* Co_pop case *)
        let s' = Cons { prev = s; op = Co_pop } in
        let v' = { co_stk = frame :: v.co_stk; stk = [] } in
        Some (s', v')
      else (* Push case *)
        let frame2 = List.hd_exn v.stk in
        let f1, _ = frame in
        let f2, _ = frame2 in
        if Id.equal f1 f2 then (
          (* if equal_frame frame frame2 then *)
          let v' = { co_stk = v.co_stk; stk = List.tl_exn v.stk } in
          let choice1 = (prev, v') in
          let choice2 = (Cons { prev = s; op = Pop }, v') in
          assert (T.hash choice1 = T.hash choice2);
          Some choice2)
        else (* failwith "unmathch pop from stk" *)
          None

let paired_callsite rstk this_f =
  let s, v = rstk in
  match s with
  | Empty -> None
  | Cons _ -> (
      match List.hd v.stk with
      | Some (cs, fid) ->
          if Id.equal fid this_f then
            Some cs
          else
            failwith "inequal f when stack is not empty"
      | None -> None)

let concretize rstk =
  let s, v = rstk in
  match s with
  | Empty -> []
  | Cons _ ->
      if not @@ List.is_empty v.stk then
        failwith "non-empty stack when concretize"
      else
        ();
      v.co_stk

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

let str_of_t (rstk : t) =
  let _s, v = rstk in
  let str_costk =
    List.fold v.co_stk ~init:"" ~f:(fun acc fm -> acc ^ str_of_frame fm)
  in
  let str_stk =
    List.fold v.stk ~init:"" ~f:(fun acc fm -> acc ^ str_of_frame fm)
  in
  Printf.sprintf "[-%s;+%s]" str_costk str_stk

let pp = Fmt.of_to_string str_of_t

let str_of_op = function Push -> "<-" | Co_pop -> "!" | Pop -> "->"

let rec str_of_id rstk =
  let s, v = rstk in
  match s with
  | Empty -> ""
  | Cons { op = Co_pop; prev } ->
      let v' = { stk = v.stk; co_stk = List.tl_exn v.co_stk } in
      str_of_id (prev, v') ^ "<@" ^ str_of_frame (List.hd_exn v.co_stk) ^ ";"
  | Cons { op = Push; prev } ->
      let v' = { stk = List.tl_exn v.stk; co_stk = v.co_stk } in
      str_of_id (prev, v') ^ "<-" ^ str_of_frame (List.hd_exn v.stk) ^ ";"
  | Cons { op = Pop; prev } -> str_of_id (prev, v) ^ "->;"

let pp_id = Fmt.of_to_string str_of_id

let chrono_compare r1 r2 =
  let rec step_back s1 s2 =
    match (s1, s2) with
    | Empty, _ -> -1
    | _, Empty -> 1
    | Cons { prev = p1; _ }, Cons { prev = p2; _ } -> step_back p1 p2
  in
  let s1, _ = r1 in
  let s2, _ = r2 in
  if T.equal_structure s1 s2 then
    0
  else
    step_back s1 s2
