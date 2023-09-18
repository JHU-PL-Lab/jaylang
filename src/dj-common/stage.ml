type t = Argparse | Load_file | State_init | Lookup | Post_check | All_done
[@@deriving variants]

let of_str str =
  match str with
  | "argparse" | "ap" -> Argparse
  | "load_file" | "lf" -> Load_file
  | "state_init" | "si" -> State_init
  | "lookup" | "lu" -> Lookup
  | "post_check" | "pc" -> Post_check
  | "all_done" | "ad" | "all" | _ -> All_done
