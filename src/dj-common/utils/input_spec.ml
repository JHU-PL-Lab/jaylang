open Core

type t = token list
and token = Int of int | One | Star [@@deriving show { with_path = false }]

let sexp_of_t xs =
  List.sexp_of_t
    (function
      | Int x -> Sexp.Atom (string_of_int x)
      | One -> Sexp.Atom "_"
      | Star -> Sexp.Atom "*")
    xs

let token_of_string = function
  | "_" -> One
  | "*" -> Star
  | s -> Int (int_of_string s)

let t_of_sexp s =
  List.t_of_sexp
    (function
      | Sexp.List _ -> raise (Failure "wrong int_list spec")
      | Sexp.Atom x -> token_of_string x)
    s

let no_spec = [ Star ]
let is_no_spec = function [ Star ] -> true | _ -> false
let any_n n = List.init n ~f:(fun _ -> One)

let next = function
  | Int x :: s -> (Some x, s)
  | One :: s -> (None, s)
  | Star :: [] -> (None, [])
  | _ -> failwith "wrong input_spec"
