open Lexing;;
open Source_origin;;
open Uid;;

let uid_table = ref @@ [];;

let reset () = uid_table := [];;

let put (uid : uid) file_region =
  uid_table := (uid, file_region)::!uid_table
;;

let get_all () =
  List.fold_right
    (fun (key,value) acc -> Uid_map.add key value acc) !uid_table Uid_map.empty
;;

let next_uid startpos endpos =
  let uid : uid = Uid.next_uid () in
  let start = { file_pos_lineno = startpos.pos_lnum
              ; file_pos_colno = startpos.pos_bol
              } in
  let stop = { file_pos_lineno = endpos.pos_lnum
             ; file_pos_colno = endpos.pos_bol
             } in
  let region = { file_region_filename = startpos.pos_fname
               ; file_region_start = start
               ; file_region_end = stop
               } in
  put uid region;
  uid
;;
