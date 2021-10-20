open Core

module T = struct
  type frame = Id.t * Id.t
  [@@deriving sexp, compare, equal, hash, show { with_path = false }]

  (* No `Pop`. `Pop` won't generate a new rstk, t.prev will be returned. *)
  type op = Push | Co_pop

  (* `co_stk` and `stk` are not necessary here if we choose to record frame change in `op` *)
  and prev_stk = {
    prev : t;
    op : op;
    co_stk : (frame list[@ignore]);
    stk : (frame list[@ignore]);
  }

  (* we cannot use `prev_stk option` here since co_pop can return a empty stack or nothing. If using option, both will be `None`. *)
  and t = Cons of prev_stk | Empty
  [@@deriving sexp, compare, equal, hash, show { with_path = false }]
end

include T
include Comparator.Make (T)

let empty : t = Empty

let push prev frame =
  match prev with
  | Empty -> Cons { prev; op = Push; co_stk = []; stk = [ frame ] }
  | Cons rstk ->
      Cons { prev; op = Push; co_stk = rstk.co_stk; stk = frame :: rstk.stk }

let pop prev frame =
  match prev with
  | Empty -> Some (Cons { prev; op = Co_pop; co_stk = [ frame ]; stk = [] })
  | Cons rstk -> (
      match rstk.stk with
      | (cs, _) :: stk' ->
          let cs', _ = frame in
          if Id.equal cs cs' then (* Some rstk.prev *)
            Some
              (Cons
                 {
                   prev = rstk.prev;
                   op = rstk.op;
                   co_stk = rstk.co_stk;
                   stk = stk';
                 })
          else
            None
      | [] ->
          Some
            (Cons { prev; op = Co_pop; co_stk = frame :: rstk.co_stk; stk = [] })
          (* match rstk.op with
             | Push ->
                 let c1, _ = frame in
                 let c2, _ = List.hd_exn rstk.stk in
                 if Id.equal c1 c2 then
                   Some rstk.prev
                 else (* failwith "unmathch pop from stk" *)
                   None
             | Co_pop ->
                 if not @@ List.is_empty rstk.stk then
                   failwith "non-empty stack when co_pop"
                 else
                   ();
                 Some
                   (Cons { prev; op = Co_pop; co_stk = frame :: rstk.co_stk; stk = [] }) *)
      )

let paired_callsite rstk this_f =
  match rstk with
  | Empty -> None
  | Cons rstk -> (
      match List.hd rstk.stk with
      | Some (cs, fid) ->
          if Id.equal fid this_f then
            Some cs
          else
            failwith "inequal f when stack is not empty"
      | None -> None)

let concretize rstk =
  match rstk with
  | Empty -> []
  | Cons rstk ->
      if not @@ List.is_empty rstk.stk then
        failwith "non-empty stack when concretize"
      else
        ();
      rstk.co_stk

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
    List.fold target_stk' ~init:Empty ~f:(fun acc fm ->
        Option.value_exn (pop acc fm))
  in
  let rstk = List.fold (List.rev call_stk') ~init:rstk' ~f:push in
  rstk

let str_of_frame (Id.Ident x1, Id.Ident x2) = "(" ^ x1 ^ "," ^ x2 ^ ")"

let str_of_t rstk =
  match rstk with
  | Empty -> "[]"
  | Cons rstk ->
      let str_costk =
        List.fold rstk.co_stk ~init:"" ~f:(fun acc fm -> acc ^ str_of_frame fm)
      in
      let str_stk =
        List.fold rstk.co_stk ~init:"" ~f:(fun acc fm -> acc ^ str_of_frame fm)
      in
      Printf.sprintf "-%s;+%s" str_costk str_stk
