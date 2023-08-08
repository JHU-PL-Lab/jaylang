(* Version: 0.1 *)
(* Caution: DO NOT EDIT! The file is copied from outside. *)

open! Core

[@@@warning "-32"]

module For_core = struct
  module Printing = struct
    let pp_b = Fmt.(using (function true -> "t" | false -> "f") string)

    let pp_set pp_val oc =
      let set_iter f set = Set.iter set ~f in
      Fmt.pf oc "{%a}" Fmt.(iter ~sep:(any ",") set_iter pp_val)

    let dump_list s pp = Fmt.pr "@.@[<v>%a@]@;@." (Fmt.list pp) s

    let dump_list domain pp =
      Fmt.(pr "@.%d@.@[<v>%a@]@;@." (List.length domain) (list pp) domain)

    let list_split es =
      let rec loop p1 p2 =
        match p2 with
        | [] -> []
        | e :: es ->
            let p1' = p1 @ [ e ] in
            let p2' = es in
            (p1', p2') :: loop p1' p2'
      in
      ([], es) :: loop [] es

    (* let pp_set ?(name = "set") pp_elem oc s =
       let pp_name oc _ = Fmt.string oc name in
       let set_iter f set = Set.iter set ~f in
       (Fmt.Dump.iter set_iter pp_name pp_elem) oc s *)
    (*
     let mk_pp_set name pp_val =
        let pp_name oc _ = Fmt.string oc name in
        let set_iter f set = Set.iter set ~f in
        Fmt.Dump.iter set_iter pp_name pp_val *)

    (* let mk_pp_set pp_val =
       let set_iter f set = Set.iter set ~f in
       Fmt.(iter ~sep:(any ",") set_iter pp_val) *)

    let pp_tuple3 pp_a pp_b pp_c oc (a, b, c) =
      Fmt.pf oc "(%a, %a, %a)" pp_a a pp_b b pp_c c

    let string_of_opt_int_list ?(none = "-") inputs =
      String.concat ~sep:","
      @@ List.map
           ~f:(function Some i -> string_of_int i | None -> none)
           inputs
  end

  include Printing

  module File_util = struct
    open Stdlib

    let write_marshal file v =
      let oc = open_out file in
      Marshal.to_channel oc v [] ;
      close_out oc

    let read_marshal file =
      let ic = open_in file in
      let v = Marshal.from_channel ic in
      close_in ic ;
      v
  end

  include File_util

  module More_bool = struct
    (*
  type ternary = True | False | Unknown
  [@@deriving equal, show { with_path = false }]
  
  
     let bool_of_ternary_exn = function
       | True -> true
       | False -> false
       | Unknown -> failwith "ternary unknown"
  
     let bool_of_ternary = function
       | True -> Some true
       | False -> Some false
       | Unknown -> None
  
     let t_and tb1 tb2 =
       match (tb1, tb2) with
       | False, _ | _, False -> False
       | True, True -> True
       | _, _ -> Unknown
  
     let t_or tb1 tb2 =
       match (tb1, tb2) with
       | True, _ | _, True -> True
       | False, False -> False
       | _, _ -> Unknown
  *)

    let set_of_bool b = Set.singleton (module Bool) b
    let just_true = set_of_bool true
    let just_false = set_of_bool false
    let true_or_false = Set.of_list (module Bool) [ true; false ]
  end

  include More_bool

  module More_list = struct
    let list_split lst =
      let rec loop part1 part2 =
        match part2 with
        | [] -> []
        | e :: es ->
            let part1' = part1 @ [ e ] in
            let part2' = es in
            (part1', part2') :: loop part1' part2'
      in
      ([], lst) :: loop [] lst
  end

  include More_list

  module More_fn = struct
    let rec naive_fix step e = step (naive_fix step) e

    (* let chain_compare f1 f2 =
         let r1 = f1 () in
         if r1 = 0 then f2 () else r1

       let just_side_effect = ignore

       let ignore2 _ _ = () *)
  end

  include More_fn
end

include For_core

module For_vanilla = struct
  let pp_std_table table_iter pp_elem oc s =
    Fmt.(vbox @@ iter_bindings ~sep:nop table_iter (pair string pp_elem)) oc s

  (* let pp_dump_std_table ?(name = "set") iter pp_elem oc s =
     let pp_name oc _ = Fmt.string oc name in
     (Fmt.Dump.iter_bindings iter pp_name Fmt.(string ++ cut) pp_elem) oc s *)
end

include For_vanilla
