(**
  Module [Separate].

  Commonly in this repo, we have a type [t] with an underlying
  [int] or [bool] "meaning" to it (e.g. a Z3 expression representing
  an int or bool is all under the umbrella of [Z3.Expr.expr]).

  It's helpful to have the type checker do some work for us so
  that we don't have to remember that underlying "meaning". We
  let the types remember it.

  This module generalizes the pattern of separating a type [t] into
  and [int] case and a [bool] case. It is abstracted into a functor
  over a type [t], and another where [t] is comparable.
*)

type (_, 'x) t =
  | I : 'x -> (int, 'x) t
  | B : 'x -> (bool, 'x) t

module type S = sig
  type x
  type nonrec 'a t = ('a, x) t
  val extract : 'a t -> x
  val extract_list : 'a t list -> x list
  val int_ : x -> int t
  val bool_ : x -> bool t
end

let int_ : 'x -> (int, 'x) t = fun x -> I x
let bool_ : 'x -> (bool, 'x) t = fun x -> B x

let extract : type a. (a, 'x) t -> 'x = function
  | I x -> x
  | B x -> x

let rec extract_list : type a. (a, 'x) t list -> 'x list = function
  | [] -> []
  | I hd :: tl
  | B hd :: tl -> hd :: extract_list tl

module Make (X : Core.T) : S with type x = X.t = struct
  type x = X.t
  type nonrec 'a t = ('a, X.t) t
  (** Separate [X.t] into an int [I] case and a bool [B] case. *)

  let int_ = int_
  let bool_ = bool_
  let extract = extract
  let extract_list = extract_list
end

module Make_with_compare (X : Comparable.S) = struct
  include Make (X)

  let compare (type a) (x : a t) (y : a t) : int =
    match x, y with
    | I xi, I yi
    | B xi, B yi -> X.compare xi yi

  let equal (type a) (x : a t) (y : a t) : bool =
    match x, y with
    | I xi, I yi
    | B xi, B yi -> X.equal xi yi
end
