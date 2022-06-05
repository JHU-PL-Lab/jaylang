open Core

module T = struct
  type t = { x : Id.t; r_stk : Rstack.t }
  [@@deriving sexp_of, compare, equal, hash]
end

include T
include Comparator.Make (T)

let start (x : Id.t) : t = { x; r_stk = Rstack.empty }
let of2 x r_stk = { x; r_stk }
let to2 key = (key.x, key.r_stk)
let with_x key x = { key with x }
let with_stk key r_stk = { key with r_stk }
let to_first = with_x

let to_string key =
  Printf.sprintf "%s_%s" (Id.show key.x) (Rstack.to_string key.r_stk)

let pp oc key = Fmt.pf oc "%a[%a]" Id.pp key.x Rstack.pp key.r_stk
let to_str2 xs r_stk = to_string (of2 xs r_stk)

let chrono_compare map k1 k2 =
  let x1, r_stk1 = to2 k1 in
  let x2, r_stk2 = to2 k2 in
  (* assert (List.is_empty xs1);
     assert (List.is_empty xs2); *)
  let rec compare_stack s1 s2 =
    match (s1, s2) with
    | [], [] ->
        if Id.equal x1 x2 then 0 else if Cfg.is_before map x1 x2 then 1 else -1
    | (cs1, f1) :: ss1, (cs2, f2) :: ss2 ->
        if Rstack.equal_frame (cs1, f1) (cs2, f2)
        then compare_stack ss1 ss2
        else if Id.equal cs1 cs2
        then 0 (* failwith "the same promgram points" *)
        else if Cfg.is_before map cs1 cs2
        then 1
        else -1
    | _, _ -> 1
  in
  let stk1, co_stk1 = Rstack.construct_stks r_stk1 in
  let stk2, co_stk2 = Rstack.construct_stks r_stk2 in
  let result_co_stk = compare_stack co_stk1 co_stk2 in
  if result_co_stk = 0 then compare_stack stk1 stk2 else result_co_stk

let length key = 1 + Rstack.length key.r_stk

let get_f_return map fid r_stk x =
  let fblock = Odefa_ast.Ast.Ident_map.find fid map in
  let x' = Cfg.ret_of fblock in
  let r_stk' = Rstack.push r_stk (x, fid) in
  let key_ret = of2 x' r_stk' in
  key_ret

let get_cond_block_and_return cond_block beta r_stk x =
  let open Cfg in
  let case_block = Cond { cond_block with choice = Some beta } in
  let x_ret = Cfg.ret_of case_block in
  let cbody_stack = Rstack.push r_stk (x, Id.cond_fid beta) in
  let key_ret = of2 x_ret cbody_stack in
  (case_block, key_ret)

let return_of_cond_block cond_block beta r_stk x =
  get_cond_block_and_return cond_block beta r_stk x |> snd

let get_callsites r_stk (fb : Cfg.fun_block) =
  let fid = fb.point in
  let callsites =
    match Rstack.paired_callsite r_stk fid with
    | Some callsite -> [ callsite ]
    | None -> fb.callsites
  in
  callsites