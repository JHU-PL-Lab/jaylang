
open Core

(*
  This is mostly just copied and only slightly edited from the old
  utils. I cannot promise it is clean or thought-through.
*)

module Dirs = struct
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

  let chop_parent_dir dir file =
    let dir_parts = Filename.parts dir in
    let file_parts = Filename.parts file in
    let rest_parts = List.drop file_parts (List.length dir_parts - 1) in
    Filename.of_parts rest_parts

  let group_and_simplify dir =
    let grouped_testfiles =
      group_dir ~filter:(Fn.flip Filename.check_suffix ".bjy") dir
    in
    List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
        ( chop_parent_dir dir group_name,
          List.map test_names ~f:(fun test_name ->
              (chop_parent_dir dir test_name, test_name)) ))

  let map ~f files_in_group =
    let f_per_file = f in
    List.map files_in_group ~f:(fun (group_name, test_names) ->
        ( group_name,
          List.map test_names ~f:(fun (test_name, test_path) ->
              f_per_file group_name test_name test_path) ))

  let map_in_groups ~f path = map ~f (group_and_simplify path)
end