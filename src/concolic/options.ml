open Core

module T =
  struct
    type t =
      { global_timeout_sec : float [@default 90.0]
      ; solver_timeout_sec : float [@default 1.0]
      ; global_max_step    : int   [@default Int.(5 * 10**4)]
      ; max_tree_depth     : int   [@default 60]
      ; random             : bool  [@default false]
      ; n_depth_increments : int   [@default 3] }
      [@@deriving sexp]
  end

include T

let default : t = T.t_of_sexp @@ Sexp.of_string "()"

module Refs =
  struct
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

(* `Fun` for optional arguments on functions *)
module Fun =
  struct
    (* p is a profunctor *)
    type ('a, 'b) p =
      ?global_timeout_sec    : float
      -> ?solver_timeout_sec : float
      -> ?global_max_step    : int
      -> ?max_tree_depth     : int
      -> ?random             : bool
      -> ?n_depth_increments : int
      -> 'a
      -> 'b

    let appl (x : ('a, 'b) p) (r : T.t) : 'a -> 'b =
      x
        ~global_timeout_sec:r.global_timeout_sec
        ~solver_timeout_sec:r.solver_timeout_sec
        ~global_max_step:r.global_max_step
        ~max_tree_depth:r.max_tree_depth
        ~random:r.random
        ~n_depth_increments:r.n_depth_increments

    let make (f : T.t -> 'a -> 'b) : ('a, 'b) p =
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

    let unit : (unit, T.t) p =
      make @@ fun r -> fun () -> r

    let step : type a b c. (a, b -> c) p -> a -> (b, c) p =
      fun a_bc_p a ->
        make
        @@ fun r -> fun b ->
            appl a_bc_p r a b

    let prod_snd : type a b c. (a, b) p -> (a, c) p -> (a, b * c) p =
      fun ab_p ac_p ->
        make
        @@ fun r -> fun a ->
          (appl ab_p r a, appl ac_p r a)

    let dimap : type a b c d. (b -> a) -> (c -> d) -> (a, c) p -> (b, d) p =
      fun ba cd ac_p ->
        make
        @@ fun r -> fun b ->
            cd @@ appl ac_p r @@ ba b

    let contramap_fst : type a b c. (a -> b) -> (b, c) p -> (a, c) p =
      fun ab bc_p ->
        dimap ab Fn.id bc_p

    let map_snd : type a b c. (b -> c) -> (a, b) p -> (a, c) p =
      fun bc ab_p ->
        dimap Fn.id bc ab_p

    let map_snd_given_fst : type a b c. (a -> b -> c) -> (a, b) p -> (a, c) p =
      fun abc ab_p ->
        make
        @@ fun r -> fun a ->
            abc a @@ appl ab_p r a

    module Infix =
      struct
        let (<<<^) = contramap_fst
        let (^>>>) = map_snd
      end

    include Infix
  end