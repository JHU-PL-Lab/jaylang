type t = Argparse | Load_file | State_init | Lookup | All_done
[@@deriving variants, equal]

let of_str str =
  match str with
  | "argparse" | "ap" -> Argparse
  | "load_file" | "lf" -> Load_file
  | "state_init" | "si" -> State_init
  | "lookup" | "lu" -> Lookup
  | "all_done" | "ad" | "all" | _ -> All_done
