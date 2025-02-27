open Core

let read_file file =
  In_channel.read_all file |> String.strip

(* We call a 'non-char' anything that is not alphanumeric or an underscore. *)
let is_non_char c =
  (Char.is_alphanum c || Char.equal '_' c) |> not

let flatten_content (s : string) : string =
  s
  |> String.map ~f:(fun c -> if is_non_char c then ' ' else c) (* Turn non-chars into whitespace *)
  |> String.split ~on:' ' (* May cause empty strings in result *)
  |> List.filter ~f:(Fn.non String.is_empty) (* Keep only non-empty strings *)
  |> String.concat ~sep:" "

let insert_metadata basename =
  let features_file = basename ^ ".features.s" in
  let reasons_file = basename ^ ".reasons.s" in
  let bjy_file = basename ^ ".bjy" in

  let features_exists = Sys_unix.file_exists_exn features_file in
  let reasons_exists = Sys_unix.file_exists_exn reasons_file in

  if not features_exists && not reasons_exists then
    ()
  else
    let features = if features_exists then Some (read_file features_file |> flatten_content) else None in
    let reasons = if reasons_exists then Some (read_file reasons_file |> flatten_content) else None in

    let metadata_comment =
      match features, reasons with
      | Some features, Some reasons ->
        Printf.sprintf "(***\n  (features (%s))\n  (reasons (%s))\n*)" features reasons
      | Some features, None ->
        Printf.sprintf "(***\n  (features (%s))\n*)" features
      | None, _ -> ""
    in

    let bjy_content = In_channel.read_all bjy_file in
    let new_content = metadata_comment ^ "\n\n" ^ bjy_content in
    Out_channel.write_all bjy_file ~data:new_content

let process_directory dir =
  let bjy_files = Sys_unix.ls_dir dir |> List.filter ~f:(fun f -> Filename.check_suffix f ".bjy") in
  List.iter bjy_files ~f:(fun bjy_file ->
    let basename = Filename.chop_suffix bjy_file ".bjy" in
    insert_metadata (Filename.concat dir basename)
  )

let () =
  let dirs = Sys.get_argv () |> Array.to_list |> List.tl_exn in
  List.iter dirs ~f:process_directory
