open Core

type fc_out = {
  xs_out : Id.t list;
  site : Id.t;
  stk_out : Relative_stack.t;
  f_out : Id.t;
}
[@@deriving sexp, compare, equal, show { with_path = false }]

type fc = {
  xs_in : Id.t list;
  stk_in : Relative_stack.t;
  fun_in : Id.t;
  outs : fc_out list;
}
[@@deriving sexp, compare, equal, show { with_path = false }]

type cf_in = { xs_in : Id.t list; stk_in : Relative_stack.t; fun_in : Id.t }
[@@deriving sexp, compare, equal, show { with_path = false }]

type cf = {
  xs_out : Id.t list;
  stk_out : Relative_stack.t;
  site : Id.t;
  f_out : Id.t;
  ins : cf_in list;
}
[@@deriving sexp, compare, equal, show { with_path = false }]
