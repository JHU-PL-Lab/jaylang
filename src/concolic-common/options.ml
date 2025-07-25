open Core

module T = struct
  type t =
    { global_timeout_sec : float [@default 90.0]
    ; global_max_step    : int   [@default Int.(10**5)]
    ; max_tree_depth     : int   [@default 30]
    ; random             : bool  [@default false]
    ; n_depth_increments : int   [@default 6]
    ; in_parallel        : bool  [@default false] }
    [@@deriving sexp]
end

include T

let default : t = T.t_of_sexp @@ Sexp.of_string "()"

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
  ; in_parallel }

module type V = sig
  val r : t 
end

(* `Arrow` for optional arguments on functions *)
module Arrow = struct
  module A = struct
    type ('a, 'b) t =
      ?global_timeout_sec    : float
      -> ?global_max_step    : int
      -> ?max_tree_depth     : int
      -> ?random             : bool
      -> ?n_depth_increments : int
      -> ?in_parallel        : bool
      -> 'a
      -> 'b

    let appl (x : ('a, 'b) t) (r : T.t) : 'a -> 'b =
      x
        ~global_timeout_sec:r.global_timeout_sec
        ~global_max_step:r.global_max_step
        ~max_tree_depth:r.max_tree_depth
        ~random:r.random
        ~n_depth_increments:r.n_depth_increments
        ~in_parallel:r.in_parallel

    let make : 'a 'b. (T.t -> 'a -> 'b) -> ('a, 'b) t =
      fun f ->
        fun
        ?(global_timeout_sec : float = default.global_timeout_sec)
        ?(global_max_step    : int   = default.global_max_step)
        ?(max_tree_depth     : int   = default.max_tree_depth)
        ?(random             : bool  = default.random)
        ?(n_depth_increments : int   = default.n_depth_increments)
        ?(in_parallel        : bool  = default.in_parallel)
        ->
        { global_timeout_sec
        ; global_max_step
        ; max_tree_depth
        ; random
        ; n_depth_increments
        ; in_parallel }
        |> f 

    let[@ocaml.warning "-27"] id : 'a. ('a, 'a) t =
      fun
      ?(global_timeout_sec : float = default.global_timeout_sec)
      ?(global_max_step    : int   = default.global_max_step)
      ?(max_tree_depth     : int   = default.max_tree_depth)
      ?(random             : bool  = default.random)
      ?(n_depth_increments : int   = default.n_depth_increments)
      ?(in_parallel        : bool  = default.in_parallel)
      a -> a

    let compose
      : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
      = fun bc ab ->
        make @@ fun r -> fun a ->
          appl bc r @@ appl ab r a
    end
  
  include A
  include Preface.Make.Arrow.Over_category_and_via_arrow_and_fst
    (Preface.Make.Category.Via_id_and_compose (A))
    (struct
      type ('a, 'b) t = ('a, 'b) A.t
      
      let arrow
        : 'a 'b. ('a -> 'b) -> ('a, 'b) t
        = fun ab ->
          make @@ fun _ -> ab

      let fst
        : 'a 'b 'c. ('a, 'b) t -> ('a * 'c, 'b * 'c) t
        = fun ab ->
          make @@ fun r -> fun (a, c) ->
            (appl ab r a), c
    end)
end