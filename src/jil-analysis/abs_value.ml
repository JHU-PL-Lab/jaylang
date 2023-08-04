open Core
open Jayil

module AVal = struct
  module T = struct
    type t = AInt | ABool of bool | Any
    [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
end

type aval_set = Set.M(AVal).t

open AVal

let pp fmter = function
  | AInt -> Fmt.string fmter "int"
  | ABool b -> Fmt.pf fmter "B:%B" b
  | Any -> Fmt.string fmter "any"

let show = Fmt.to_to_string
