
open Core

(* Gets all files into a flat list from all given dirs *)
let get_all_files ?(filter : Filename.t -> bool = fun _ -> true) (dirs : Filename.t list) : Filename.t list =
  let rec loop outlist = function
    | [] -> outlist
    | f :: fs -> begin
      match Sys_unix.is_directory f with
      | `Yes ->
          f
          |> Sys_unix.ls_dir
          |> List.map ~f:(( ^ ) (f ^ "/"))
          |> List.append fs
          |> loop outlist
      | _ when filter f -> loop (f :: outlist) fs
      | _ -> loop outlist fs
    end
  in
  loop [] dirs

let get_all_bjy_files (dirs : Filename.t list) : Filename.t list =
  get_all_files ~filter:(fun f -> Filename.check_suffix f ".bjy") dirs
