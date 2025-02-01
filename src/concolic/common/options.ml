open Core

module T = struct
  type t =
    { global_timeout_sec : float [@default 90.0]
    ; solver_timeout_sec : float [@default 1.0]
    ; global_max_step    : int   [@default Int.(10**5)]
    ; max_tree_depth     : int   [@default 30]
    ; random             : bool  [@default false]
    ; n_depth_increments : int   [@default 6] }
    [@@deriving sexp]
end

include T

let default : t = T.t_of_sexp @@ Sexp.of_string "()"

module Refs = struct
  type t =
    { global_timeout_sec : float ref
    ; solver_timeout_sec : float ref
    ; global_max_step    : int ref
    ; max_tree_depth     : int ref
    ; random             : bool ref
    ; n_depth_increments : int ref }

  let create_default () : t =
    { global_timeout_sec = ref default.global_timeout_sec
    ; solver_timeout_sec = ref default.solver_timeout_sec
    ; global_max_step    = ref default.global_max_step
    ; max_tree_depth     = ref default.max_tree_depth
    ; random             = ref default.random
    ; n_depth_increments = ref default.n_depth_increments }

  let without_refs (x : t) : T.t =
    { global_timeout_sec = !(x.global_timeout_sec)
    ; solver_timeout_sec = !(x.solver_timeout_sec)
    ; global_max_step    = !(x.global_max_step)
    ; max_tree_depth     = !(x.max_tree_depth)
    ; random             = !(x.random)
    ; n_depth_increments = !(x.n_depth_increments) }
end

(* `Arrow` for optional arguments on functions *)
module Arrow = struct
  module A = struct
    type ('a, 'b) t =
      ?global_timeout_sec    : float
      -> ?solver_timeout_sec : float
      -> ?global_max_step    : int
      -> ?max_tree_depth     : int
      -> ?random             : bool
      -> ?n_depth_increments : int
      -> 'a
      -> 'b

    let appl (x : ('a, 'b) t) (r : T.t) : 'a -> 'b =
      x
        ~global_timeout_sec:r.global_timeout_sec
        ~solver_timeout_sec:r.solver_timeout_sec
        ~global_max_step:r.global_max_step
        ~max_tree_depth:r.max_tree_depth
        ~random:r.random
        ~n_depth_increments:r.n_depth_increments

    let make : 'a 'b. (T.t -> 'a -> 'b) -> ('a, 'b) t =
      fun f ->
        fun
        ?(global_timeout_sec : float = default.global_timeout_sec)
        ?(solver_timeout_sec : float = default.solver_timeout_sec)
        ?(global_max_step    : int   = default.global_max_step)
        ?(max_tree_depth     : int   = default.max_tree_depth)
        ?(random             : bool  = default.random)
        ?(n_depth_increments : int   = default.n_depth_increments)
        ->
        { global_timeout_sec
        ; solver_timeout_sec
        ; global_max_step
        ; max_tree_depth
        ; random
        ; n_depth_increments }
        |> f 

    let[@ocaml.warning "-27"] id : 'a. ('a, 'a) t =
      fun
      ?(global_timeout_sec : float = default.global_timeout_sec)
      ?(solver_timeout_sec : float = default.solver_timeout_sec)
      ?(global_max_step    : int   = default.global_max_step)
      ?(max_tree_depth     : int   = default.max_tree_depth)
      ?(random             : bool  = default.random)
      ?(n_depth_increments : int   = default.n_depth_increments)
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

  let uncurry
    : 'a 'b 'c. ('a, 'b -> 'c) t -> ('a * 'b, 'c) t
    = fun abc ->
      fst abc >>^ (fun (bc, b) -> bc b)

  let thaw
    : 'a 'b. (unit, 'a -> 'b) t -> ('a, 'b) t
    = fun x ->
      uncurry x <<^ (fun y -> (), y)
end