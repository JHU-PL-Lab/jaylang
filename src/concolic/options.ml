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

    module type ARROW =
      sig
        type ('b, 'c) a
        (* val id : ('b, 'b) a *)
        val arr : ('b -> 'c) -> ('b, 'c) a
        val first : ('b, 'c) a -> ('b * 'd, 'c * 'd) a
        val (>>>) : ('b, 'c) a -> ('c, 'd) a -> ('b, 'd) a
      end

    module A = 
      struct
        (* a is an arrow. *)
        type ('b, 'c) a =
          ?global_timeout_sec    : float
          -> ?solver_timeout_sec : float
          -> ?global_max_step    : int
          -> ?max_tree_depth     : int
          -> ?random             : bool
          -> ?n_depth_increments : int
          -> 'b
          -> 'c

        let appl (x : ('b, 'c) a) (r : T.t) : 'b -> 'a =
          x
            ~global_timeout_sec:r.global_timeout_sec
            ~solver_timeout_sec:r.solver_timeout_sec
            ~global_max_step:r.global_max_step
            ~max_tree_depth:r.max_tree_depth
            ~random:r.random
            ~n_depth_increments:r.n_depth_increments

        let make : 'b 'c. (T.t -> 'b -> 'c) -> ('b, 'c) a =
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

        let arr : 'b 'c. ('b -> 'c) -> ('b, 'c) a =
          fun b_c ->
            make @@ fun _ -> b_c

        let first : 'b 'c 'd. ('b, 'c) a -> ('b * 'd, 'c * 'd) a =
          fun b_c_a ->
            make @@ fun r -> fun (b, d) ->
              (appl b_c_a r b), d

        let (>>>) : 'b 'c 'd. ('b, 'c) a -> ('c, 'd) a -> ('b, 'd) a =
          fun b_c_a c_d_a ->
            make @@ fun r -> fun b ->
              appl c_d_a r @@ appl b_c_a r b
      end

    include A

    module Make_arrow (A : ARROW) =
      struct
        include A

        let swap (x, y) = (y, x)

        module Infix =
          struct
            let (>>>) = (>>>)

            (* split *)
            let ( *** ) : 'b 'c 'd 'e. ('b, 'c) a -> ('d, 'e) a -> ('b * 'd, 'c * 'e) a = 
              fun b_c d_e ->
                first b_c
                >>> arr swap
                >>> first d_e
                >>> arr swap

            (* fanout *)
            let (&&&) : 'b 'c 'd. ('b, 'c) a -> ('b, 'd) a -> ('b, 'c * 'd) a =
              fun f g ->
                arr (fun b -> (b, b)) >>> f *** g

            (* map second *)
            let (^>>) : 'b 'c 'd. ('b, 'c) a -> ('c -> 'd) -> ('b, 'd) a =
              fun bc_a cd ->
                bc_a >>> arr cd

            (* contramap first *)
            let (<<^) : 'b 'c 'd. ('c, 'd) a -> ('b -> 'c) -> ('b, 'd) a =
              fun cd_a bc ->
                arr bc >>> cd_a
          end

        include Infix

        let second : 'b 'c 'd. ('b, 'c) a -> ('d * 'b, 'd * 'c) a =
          fun a ->
            arr Fn.id *** a

        let dimap : 'b 'c 'd 'e. ('b -> 'c) -> ('d -> 'e) -> ('c, 'd) a -> ('b, 'e) a =
          fun b_c d_e c_d_a ->
            (c_d_a <<^ b_c) ^>> d_e

        let uncurry : 'b 'c 'd. ('b, 'c -> 'd) a -> ('b * 'c, 'd) a =
          fun b_c_d_a ->
            first b_c_d_a ^>> (fun (c_d, c) -> c_d c)

        let strong : 'b 'c 'd. ('b -> 'c -> 'd) -> ('b, 'c) a -> ('b, 'd) a =
          fun b_c_d b_c_a ->
            first b_c_a
            ^>> (fun (c, b) -> b_c_d b c)
            <<^ (fun b -> (b, b))

        let thaw : 'b 'c. (unit, 'b -> 'c) a -> ('b, 'c) a =
          fun x -> uncurry x <<^ (fun y -> (), y)
      end

    include Make_arrow (A)
  end