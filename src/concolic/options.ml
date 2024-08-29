open Core

module T =
  struct
    type t =
      { global_timeout_sec : float [@default 90.0]
      ; solver_timeout_sec : float [@default 1.0]
      ; global_max_step    : int   [@default Int.(5 * 10**4)]
      ; max_tree_depth     : int   [@default 60]
      ; random             : bool  [@default false]
      ; n_depth_increments : int   [@default 6] }
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
    type ('a, 'b) t =
      ?global_timeout_sec    : float
      -> ?solver_timeout_sec : float
      -> ?global_max_step    : int
      -> ?max_tree_depth     : int
      -> ?random             : bool
      -> ?n_depth_increments : int
      -> 'a
      -> 'b

    let run (x : ('a, 'b) t) (r : T.t) : 'a -> 'b =
      x
        ~global_timeout_sec:r.global_timeout_sec
        ~solver_timeout_sec:r.solver_timeout_sec
        ~global_max_step:r.global_max_step
        ~max_tree_depth:r.max_tree_depth
        ~random:r.random
        ~n_depth_increments:r.n_depth_increments

    let make (f : T.t -> 'a -> 'b) : ('a, 'b) t =
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

    let return (f : 'a -> 'b) : ('a, 'b) t =
      make (fun _ -> f)

    (*
      Note we can't do the normal bind of type `('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t`.
      To see why, forget the optional argument part. Just assume `('a, 'b) t = 'a -> 'b`.
      Then such a bind would be
        `val bind : ('a -> 'r) -> ('a -> 'b -> 'r) -> 'b -> 'r`
      Then try to start by saying
        let bind x f =
          fun b ->
            ...
      And we need to make an 'r. However, this requires we have an 'a! We don't!

      So let's just call the following code `bind`, even if that's a bad name.
    *)
    let bind (x : ('a, 'b) t) (f : ('b, 'r) t) : ('a, 'r) t =
      make
      @@ fun r ->
          fun a ->
            let b = run x r a in
            run f r b

    let map (x : ('a, 'b) t) (f : 'b -> 'r) : ('a, 'r) t =
      bind x (return f)

    let compose (f : 'a -> 'b) (x : ('b, 'r) t) : ('a, 'r) t =
      bind (return f) x

    let (>>=) x f = bind x f
    let (>>|) x f = map x f
    let (>=>) f g = compose f g

  end