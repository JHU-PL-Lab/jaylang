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

module Make (X : sig type t end) = struct
  type _ t =
    | I : X.t -> int t
    | B : X.t -> bool t
  (** Separate [X.t] into an int [I] case and a bool [B] case. *)

  let int_ : X.t -> int t = fun x -> I x
  let bool_ : X.t -> bool t = fun x -> B x

  let extract : type a. a t -> X.t = function
    | I x -> x
    | B x -> x

  let rec extract_list : type a. a t list -> X.t list = function
    | [] -> []
    | I hd :: tl
    | B hd :: tl -> hd :: extract_list tl
end

module Make_with_compare (X : sig type t [@@deriving compare, equal] end) = struct
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
