open! Core

let chop_parent_dir dir file =
  let dir_parts = Filename.parts dir in
  let file_parts = Filename.parts file in
  let rest_parts = List.drop file_parts (List.length dir_parts - 1) in
  Filename.of_parts rest_parts

let group_and_simplify dir =
  let grouped_testfiles =
    Std.File_utils.group_dir ~filter:File_utils.check_upto_bluejay dir
  in
  List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
      ( chop_parent_dir dir group_name,
        List.map test_names ~f:(fun test_name ->
            (chop_parent_dir dir test_name, test_name)) ))

let iter ~f files_in_group =
  let f_per_file = f in
  List.iter files_in_group ~f:(fun (group_name, test_names) ->
      List.iter test_names ~f:(fun (test_name, test_path) ->
          f_per_file group_name test_name test_path))

let iter_in_groups ~f path = iter ~f (group_and_simplify path)

let map ~f files_in_group =
  let f_per_file = f in
  List.map files_in_group ~f:(fun (group_name, test_names) ->
      ( group_name,
        List.map test_names ~f:(fun (test_name, test_path) ->
            f_per_file group_name test_name test_path) ))

let map_in_groups ~f path = map ~f (group_and_simplify path)
