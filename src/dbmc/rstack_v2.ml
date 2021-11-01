open Core

module T = struct
  type frame = Id.t * Id.t [@@deriving sexp, compare, equal, hash]

  (* No `Pop`. `Pop` won't generate a new rstk, t.prev will be returned. *)
  type op = Push | Pop | Co_pop

  (* `co_stk` and `stk` are not necessary here if we choose to record frame change in `op` *)
  and prev_stk = {
    prev : (t[@hash.ignore]);
    op : (op[@hash.ignore]);
    co_stk : frame list;
    stk : frame list;
  }

  (* we cannot use `prev_stk option` here since co_pop can return a empty stack or nothing. If using option, both will be `None`. *)
  and t = Cons of prev_stk * (frame[@hash.ignore]) | Empty
  [@@deriving sexp, compare, equal, hash]
end

(* a stack can have id and view. the id is unique while the view is not.
   Stacks with different ids can have the same view.
   The view is used as hash key.
*)

include T
include Comparator.Make (T)

let empty : t = Empty

let push rstk frame =
  match rstk with
  | Empty ->
      Cons ({ prev = Empty; op = Push; co_stk = []; stk = [ frame ] }, frame)
  | Cons (rstk, f0) ->
      Cons
        ( {
            prev = Cons (rstk, f0);
            op = Push;
            co_stk = rstk.co_stk;
            stk = frame :: rstk.stk;
          },
          frame )

let pop rstk frame =
  match rstk with
  | Empty ->
      Some
        (Cons
           ({ prev = Empty; op = Co_pop; co_stk = [ frame ]; stk = [] }, frame))
  | Cons ({ op = Push; stk; prev; _ }, _) as this_prev ->
      let f2 = List.hd_exn stk in
      if equal_frame frame f2 then
        Some prev (* Some (Cons { prev = this_prev; op = Pop; frame }) *)
      else (* failwith "unmathch pop from stk" *)
        None
  | Cons ({ op = Pop; _ }, _) as this_prev ->
      failwith "not yet"
      (* if List.is_empty stk then (* Co_pop case *)
           Some
             (Cons
                ( {
                    prev = this_prev;
                    op = Co_pop;
                    co_stk = frame :: co_stk;
                    stk = [];
                  },
                  frame ))
         else (* Push case *)
           let f2 = List.hd_exn stk in
           if equal_frame frame f2 then
             Some
               (Cons
                  ( { prev = this_prev; op = Pop; co_stk; stk = List.tl_exn stk },
                    frame ))
           else (* failwith "unmathch pop from stk" *)
             None *)
  | Cons ({ op = Co_pop; co_stk; stk; _ }, _) as this_prev ->
      if List.is_empty stk then
        Some
          (Cons
             ( {
                 prev = this_prev;
                 op = Co_pop;
                 co_stk = frame :: co_stk;
                 stk = [];
               },
               frame ))
      else
        failwith "non-empty stack when co_pop"

let paired_callsite rstk this_f =
  match rstk with
  | Empty -> None
  | Cons (rstk, _) -> (
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
  | Cons (rstk, _) ->
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
    List.fold (List.rev target_stk') ~init:Empty ~f:(fun acc fm ->
        Option.value_exn (pop acc fm))
  in
  let rstk = List.fold (List.rev call_stk') ~init:rstk' ~f:push in
  rstk

let str_of_frame (Id.Ident x1, Id.Ident x2) = "(" ^ x1 ^ "," ^ x2 ^ ")"

let str_of_t rstk =
  match rstk with
  | Empty -> "[]"
  | Cons (rstk, _) ->
      let str_costk =
        List.fold rstk.co_stk ~init:"" ~f:(fun acc fm -> acc ^ str_of_frame fm)
      in
      let str_stk =
        List.fold rstk.stk ~init:"" ~f:(fun acc fm -> acc ^ str_of_frame fm)
      in
      Printf.sprintf "[-%s;+%s]" str_costk str_stk

let pp = Fmt.of_to_string str_of_t

let str_of_op = function Push -> "<-" | Co_pop -> "!" | Pop -> "->"

let rec str_of_id rstk =
  match rstk with
  | Empty -> ""
  | Cons ({ op = Co_pop; prev; _ }, frame) ->
      str_of_id prev ^ "<@" ^ str_of_frame frame ^ ";"
  | Cons ({ op = Push; prev; _ }, frame) ->
      str_of_id prev ^ "<-" ^ str_of_frame frame ^ ";"
  | Cons ({ op = Pop; prev; _ }, _frame) -> str_of_id prev ^ "->;"

let pp_id = Fmt.of_to_string str_of_id
