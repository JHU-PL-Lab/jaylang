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

let default_target = Odefa_ast.Ast.Ident "target"

let with_seq ?(start = 0) xs = List.mapi xs ~f:(fun i x -> (i + start, x))

let pp_with_seq ?(pp_int = Fmt.int) pp_x oc xps =
  Fmt.(pf oc "%a" (list ~sep:(any "@,") (pair ~sep:sp pp_int pp_x)) xps)

let pp_llist ?(sepc = Fmt.any "\\n") pp_one oc xss =
  Fmt.(pf oc "%a" (list ~sep:sepc (list ~sep:(any "; ") pp_one)) xss)

let to_indexed_list xps =
  let max_i = List.map ~f:fst xps |> List.max_elt ~compare:Int.compare in
  match max_i with
  | Some len ->
      let arr = Array.create ~len:(len + 1) (snd (List.hd_exn xps)) in
      List.iter xps ~f:(fun (i, x) -> arr.(i) <- x);
      Array.to_list arr
  | None -> []

let wait_once f x _ = f x

let generate_inputs test_generator : (int list * int) list * 'a option =
  (* let generation_callback inputs steps = prints results *)
  let inputs = [ 1; 2 ] in
  let step = 3 in
  let answers = [ (inputs, step) ] in
  let generator_opt = Some test_generator in
  (answers, generator_opt)

(*
   type solution = (symbol -> Ast.value option) * Relative_stack.concrete_stack option
*)
(* Noting one model may generate more solutions *)
let build_input_sequence _solution _program _target : int list = []
