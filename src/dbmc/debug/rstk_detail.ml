open Core
open Dj_common

type t = {
  c_stk : Concrete_stack.t;
  mutable visited : int;
  mutable smt_check_count : int;
}
