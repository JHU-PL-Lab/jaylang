open Core

let mk_unit : unit -> Sexp.t = fun () -> Sexp.List []

let mk_int : int -> Sexp.t = fun n -> Sexp.Atom (string_of_int n)

let mk_bool : bool -> Sexp.t = fun b -> Sexp.Atom (string_of_bool b)

let mk_string : string -> Sexp.t = fun s -> Sexp.Atom s

let mk_pair : ('a -> Sexp.t) -> ('b -> Sexp.t) -> 'a * 'b -> Sexp.t =
  fun f g (a,b) -> Sexp.List [f a; g b]

let mk_triple :
  ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> 'a * 'b * 'c -> Sexp.t =
  fun f g h (a,b,c) -> Sexp.List [f a; g b; h c]

let mk_list : ('a -> Sexp.t) -> 'a list -> Sexp.t = fun f xs ->
  Sexp.List(List.map xs ~f)

let mk_map :
  ('k -> Sexp.t) -> ('v -> Sexp.t) -> ('k, 'v, 'cmp) Map.t -> Sexp.t =
  fun f g m -> Sexp.List (Map.to_alist m |> List.map ~f:(mk_pair f g))

let mk_constr : string -> Sexp.t list -> Sexp.t = fun name children ->
  Sexp.List (Sexp.Atom name :: children)

