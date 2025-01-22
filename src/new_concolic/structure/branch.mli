
module Direction : sig
  type _ t =
    | True_direction : bool t
    | False_direction : bool t
    | Case_int : int -> int t
    | Case_default : int t
end

module Contents : sig
  type 'a t =
    { condition : 'a Expression.t[@compare.ignore]
    ; direction : 'a Direction.t  }
end

type t =
  | Bool_branch of bool Contents.t
  | Int_case of int Contents.t
  [@@deriving compare]