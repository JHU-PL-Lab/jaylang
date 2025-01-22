
open Core

module Direction = struct
  type _ t =
    | True_direction : bool t
    | False_direction : bool t
    | Case_int : int -> int t
    | Case_default : int t

  let compare (type p) (cmp : p -> p -> int) (a : p t) (b : p t) : int =
    match a, b with
    (* bool *)
    | True_direction, False_direction -> -1
    | True_direction, True_direction -> 0
    | False_direction, False_direction -> 0
    | False_direction, True_direction -> 1
    (* int *)
    | Case_int _, Case_default -> -1
    | Case_int i1, Case_int i2 -> cmp i1 i2
    | Case_default, Case_default -> 0
    | Case_default, Case_int _ -> 1
end

module Contents = struct
  type 'a t =
    { condition : 'a Expression.t[@compare.ignore]
    ; direction : 'a Direction.t }
    [@@deriving compare]

  (* let compare ({ direction = d1 ; _ } : 'a t) ({ direction = d2 ; _ } : 'a t) : int =
    Direction.compare d1 d2 *)
end

type t =
  | Bool_branch of bool Contents.t
  | Int_case of int Contents.t
  [@@deriving compare]
