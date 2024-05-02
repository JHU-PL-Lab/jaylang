      

let test_jil : (string -> Concolic.Test_result.t) Concolic_options.Fun.t =
  let open Concolic_options.Fun in
  Concolic.test
  @. Dj_common.File_utils.read_source

let test_bjy : (string -> Concolic.Test_result.t) Concolic_options.Fun.t =
  let open Concolic_options.Fun in
  Concolic.test
  @. (fun filename ->
    filename
    |> Dj_common.File_utils.read_source_full ~do_instrument:false ~do_wrap:true
    |> Dj_common.Convert.jil_ast_of_convert)

let test : (string -> Concolic.Test_result.t) Concolic_options.Fun.t =
  Concolic_options.Fun.make
  @@ fun r ->
      fun filename ->
      match Core.Filename.split_extension filename with 
      | _, Some "jil" -> Concolic_options.Fun.appl test_jil r filename
      | _, Some "bjy" -> Concolic_options.Fun.appl test_bjy r filename
      | _ -> failwith "expected jil or bjy file"