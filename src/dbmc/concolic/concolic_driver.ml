
let test_program_concolic : (string -> Branch_info.t) Concolic_options.Fun.t =
  Concolic_options.Fun.compose
    Concolic.eval
    Dj_common.File_utils.read_source

let test : (string -> [ `Found_abort | `Exhausted | `Exhausted_pruned_tree | `Timeout ]) Concolic_options.Fun.t =
  Concolic_options.Fun.compose
    Concolic.test
    Dj_common.File_utils.read_source


  (* Toy example of propogating a change in optional args *)
  (* Concolic_options.Fun.make (fun r -> fun source ->
    Concolic_options.Fun.appl Concolic.eval { r with global_timeout_sec = 1.0 } (Dj_common.File_utils.read_source source)
    ) *)