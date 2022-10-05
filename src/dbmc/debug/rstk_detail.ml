open Core
open Dj_common

type t = {
  block_id : Id.t;
  mutable visits : int;
  mutable smt_checks : int;
  mutable smt_size : int;
  mutable smt_time : float;
}
[@@deriving show { with_path = false }]
