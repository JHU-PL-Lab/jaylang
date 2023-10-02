(* Version: 0.1.6 *)
(* Caution: DO NOT EDIT! The file is copied from outside. *)

[@@@warning "-32"]

module For_core = struct
  open! Core

  module Printing = struct
    let pp_b = Fmt.(using (function true -> "t" | false -> "f") string)
    let pp_bo = Fmt.(using (function true -> "#t" | false -> "#f") string)

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

    let iter_core_set f set = Set.iter set ~f

    let iteri_core_map f map =
      let core_f ~key ~data = f key data in
      Core.Map.iteri map ~f:core_f

    let iteri_core_hashtbl f map =
      let core_f ~key ~data = f key data in
      Core.Hashtbl.iteri map ~f:core_f
  end

  include Printing

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

  module Cool_strict = struct
    type t = Gt | Eq | Lt
    type cool = t
  end

  module Cool_eq = struct
    type t = Gt | Eq | Ls | Ge | Le
    type coole = t
  end

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

  module File_utils = struct
    let group_dir ~filter dir =
      let rec loop dir =
        let acc_f, acc_p =
          Sys_unix.fold_dir ~init:([], [])
            ~f:(fun (acc_f, acc_p) path ->
              match String.get path 0 with
              | '.' (* including "." ".." *) | '_' -> (acc_f, acc_p)
              | _ -> (
                  let fullpath = Filename.concat dir path in
                  match Sys_unix.is_directory fullpath with
                  | `Yes -> (acc_f, loop fullpath @ acc_p)
                  | `No when filter fullpath -> (fullpath :: acc_f, acc_p)
                  | `No -> (acc_f, acc_p)
                  | `Unknown -> (acc_f, acc_p)))
            dir
        in
        (dir, List.sort acc_f ~compare:String.compare) :: acc_p
      in
      loop dir
  end

  module More_Command = struct
    let param_of_command (all_params : 't Command.Param.t) summary : 't =
      let store = ref None in
      let save_param : (unit -> unit) Command.Param.t =
        Command.Param.(all_params >>| fun params () -> store := Some params)
      in
      let command = Command.basic ~summary save_param in
      Command_unix.run command ;
      Option.value_exn !store
  end

  include More_Command
end

include For_core

module For_vanilla = struct
  let pp_std_table table_iter pp_key pp_elem oc s =
    Fmt.(vbox @@ iter_bindings ~sep:nop table_iter (pair pp_key pp_elem)) oc s
  (* let pp_dump_std_table ?(name = "set") iter pp_elem oc s =
     let pp_name oc _ = Fmt.string oc name in
     (Fmt.Dump.iter_bindings iter pp_name Fmt.(string ++ cut) pp_elem) oc s *)

  module File_util = struct
    open Stdlib

    module File_infix = struct
      (* The precedence in OCaml is (See https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity for full):
         (functio application) > `/...` > `@...` > `^...` > `$/`.
         Therefore, if we have
         "1" ^ "a" // "a" ^ "2";;
         "1" ^ "b" @/ "b" ^ "2";;
         "1" ^ "b" $/ "b" ^ "2";;

         It should be equivalent to
         "1" ^ ("a" // "a") ^ "2";;
         "1" ^ ("b" @/ "b") ^ "2";;
         ("1" ^ "b") $/ ("b" ^ "2");;
      *)
      let ( $/ ) a b = Filename.concat a b
    end

    open File_infix

    let read_file_all path = In_channel.with_open_text path In_channel.input_all

    let write_file_all path content =
      Out_channel.with_open_text path (fun c ->
          Out_channel.output_string c content)

    let write_marshal file v =
      let oc = open_out file in
      Marshal.to_channel oc v [] ;
      close_out oc

    let read_marshal file =
      let ic = open_in file in
      let v = Marshal.from_channel ic in
      close_in ic ;
      v

    let remove_dir path =
      let rec loop path =
        Sys.readdir path
        |> Array.iter (fun sub ->
               let subpath = path $/ sub in
               if Sys.is_directory subpath
               then loop subpath
               else Sys.remove subpath) ;
        Sys.rmdir path
      in
      if Sys.file_exists path && Sys.is_directory path then loop path
  end

  include File_util
end

include For_vanilla
