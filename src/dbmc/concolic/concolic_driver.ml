

let test : (string -> Concolic.Test_result.t) Concolic_options.Fun.t =
  Concolic_options.Fun.compose
    Concolic.test
    Dj_common.File_utils.read_source
