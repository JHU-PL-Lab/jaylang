
module T :
  sig
    type t =
      { global_timeout_sec : float
      ; solver_timeout_sec : float
      ; global_max_step    : int
      ; max_tree_depth     : int
      ; random             : bool
      ; n_depth_increments : int }
      [@@deriving sexp]
  end

type t = T.t

val default : t

module Refs :
  sig
    type t =
      { global_timeout_sec : float ref
      ; solver_timeout_sec : float ref
      ; global_max_step    : int ref
      ; max_tree_depth     : int ref
      ; random             : bool ref
      ; n_depth_increments : int ref }

    val create_default : unit -> t
    (** [create_default ()] has the default values from [default]. *)

    val without_refs : t -> T.t
    (** [without_refs t] is all the values in the cells in [t]. *)
  end

(* `Fun` for optional arguments on functions *)
module Fun :
  sig
    type (-'a, +'b) a =
      ?global_timeout_sec    : float
      -> ?solver_timeout_sec : float
      -> ?global_max_step    : int
      -> ?max_tree_depth     : int
      -> ?random             : bool
      -> ?n_depth_increments : int
      -> 'a
      -> 'b
    (** [a] is an arrow *)

    val appl : ('b, 'c) a -> T.t -> 'b -> 'c
    (** [run x r] applies the values from [r] to the arguments of [x] *)

    val make : (T.t -> 'b -> 'c) -> ('b, 'c) a
    (** [make f] accepts optional arguments and applies them in the default record to [f]. *)
    
    val arr : ('b -> 'c) -> ('b, 'c) a

    val first : ('b, 'c) a -> ('b * 'd, 'c * 'd) a

    module Infix :
      sig
        val (>>>) : ('b, 'c) a -> ('c, 'd) a -> ('b, 'd) a

        val ( *** ) : ('b, 'c) a -> ('d, 'e) a -> ('b * 'd, 'c * 'e) a
        (** [( *** )] is infix [split]. *)

        val (&&&) : ('b, 'c) a -> ('b, 'd) a -> ('b, 'c * 'd) a
        (** [(&&&)] is infix [fanout]. *)

        val (^>>) : ('b, 'c) a -> ('c -> 'd) -> ('b, 'd) a
        (** [(^>>)] is infix [map_snd] because an a is also a profunctor. *)

        (* contramap first *)
        val (<<^) : ('c, 'd) a -> ('b -> 'c) -> ('b, 'd) a
        (** [(<<^)] is infix [contramap_fst] because an a is also a profunctor. *)
      end

    val second : ('b, 'c) a -> ('d * 'b, 'd * 'c) a

    val dimap : ('b -> 'c) -> ('d -> 'e) -> ('c, 'd) a -> ('b, 'e) a

    val uncurry : ('b, 'c -> 'd) a -> ('b * 'c, 'd) a 

    val strong : ('b -> 'c -> 'd) -> ('b, 'c) a -> ('b, 'd) a

    val thaw : (unit, 'b -> 'c) a -> ('b, 'c) a
    (** [thaw x] is [uncurry x <<^ (fun y -> (), y)] *)
  end