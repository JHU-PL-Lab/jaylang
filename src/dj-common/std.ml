open Core

type ternary = True | False | Unknown
[@@deriving equal, show { with_path = false }]

let bool_of_ternary_exn = function
  | True -> true
  | False -> false
  | Unknown -> failwith "ternary unknown"

let bool_of_ternary = function
  | True -> Some true
  | False -> Some false
  | Unknown -> None

let t_and tb1 tb2 =
  match (tb1, tb2) with
  | False, _ | _, False -> False
  | True, True -> True
  | _, _ -> Unknown

let t_or tb1 tb2 =
  match (tb1, tb2) with
  | True, _ | _, True -> True
  | False, False -> False
  | _, _ -> Unknown

let just_side_effect = ignore
let ignore2 _ _ = ()

let generate_inputs test_generator : (int list * int) list * 'a option =
  (* let generation_callback inputs steps = prints results *)
  let inputs = [ 1; 2 ] in
  let step = 3 in
  let answers = [ (inputs, step) ] in
  let generator_opt = Some test_generator in
  (answers, generator_opt)

(* Noting one model may generate more solutions *)
let build_input_sequence _solution _program _target : int list = []

let chain_compare f1 f2 =
  let r1 = f1 () in
  if r1 = 0 then f2 () else r1

let pp_tuple3 pp_a pp_b pp_c oc (a, b, c) =
  Fmt.pf oc "(%a, %a, %a)" pp_a a pp_b b pp_c c

let string_of_inputs inputs =
  String.concat ~sep:","
  @@ List.map ~f:(function Some i -> string_of_int i | None -> "-") inputs
