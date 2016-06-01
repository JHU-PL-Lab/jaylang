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
