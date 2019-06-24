open Batteries;;
open Jhupllib;;

type label = Label of string

type ident = Ident of string [@@deriving eq, ord, show, to_yojson];;

module Ident =
struct
  type t = ident;;
  let equal = equal_ident;;
  let compare = compare_ident;;
  let pp = pp_ident;;
  let show = show_ident;;
  let to_yojson = ident_to_yojson;;
  let hash = Hashtbl.hash
end;;

module Ident_map = struct
  module M = Map.Make(Ident);;
  include M;;
  include Pp_utils.Map_pp(M)(Ident);;
  include Yojson_utils.Map_to_yojson(M)(Ident);;
end;;

type funsig = Funsig of ident * ident list * expr

and expr =
  | Var of ident | Function of ident list * expr | Appl of expr * expr
  | Let of ident * expr * expr | LetRecFun of funsig list * expr
  | LetFun of funsig * expr
  | Plus of expr * expr | Minus of expr * expr | Equal of expr * expr
  | LessThan of expr * expr | Leq of expr * expr
  | And of expr * expr| Or of expr * expr | Not of expr
  | If of expr * expr * expr | Int of int | Bool of bool
  | String of string
  | Record of expr Ident_map.t | RecordProj of expr * label
