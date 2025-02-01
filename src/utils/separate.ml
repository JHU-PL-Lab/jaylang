
module Make (X : sig type t end) = struct
  type _ t =
    | I : X.t -> int t
    | B : X.t -> bool t

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
