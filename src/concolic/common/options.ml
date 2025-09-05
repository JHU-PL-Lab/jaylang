open Core

type t =
  { global_timeout_sec : float [@default 90.0]
  ; global_max_step    : int   [@default Int.(10**5)]
  ; max_tree_depth     : int   [@default 30]
  ; random             : bool  [@default false]
  ; n_depth_increments : int   [@default 6]
  ; in_parallel        : bool  [@default false] }
[@@deriving sexp]

let default : t = t_of_sexp @@ Sexp.of_string "()"

let cmd_arg_term =
  let open Cmdliner.Term.Syntax in
  let open Cmdliner.Arg in
  let+ global_timeout_sec = value & opt float default.global_timeout_sec & info ["t"] ~doc:"Global timeout seconds"
  and+ global_max_step = value & opt int default.global_max_step & info ["m"] ~doc:"Global max step"
  and+ max_tree_depth = value & opt int default.max_tree_depth & info ["d"] ~doc:"Max tree depth"
  and+ random = value & flag & info ["r"] ~doc:"Random"
  and+ n_depth_increments = value & opt int default.n_depth_increments & info ["n"] ~doc:"Num depth increments"
  and+ in_parallel = value & flag & info ["p"] ~doc:"Run checks in parallel" in
  { global_timeout_sec
  ; global_max_step
  ; max_tree_depth
  ; random
  ; n_depth_increments
  ; in_parallel
  }
