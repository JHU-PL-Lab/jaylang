
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
    type ('a, 'b) p =
      ?global_timeout_sec    : float
      -> ?solver_timeout_sec : float
      -> ?global_max_step    : int
      -> ?max_tree_depth     : int
      -> ?random             : bool
      -> ?n_depth_increments : int
      -> 'a
      -> 'b
    (** [p] is a profunctor *)

    val appl : ('a, 'b) p -> T.t -> 'a -> 'b
    (** [run x r] applies the values from [r] to the arguments of [x] *)

    val make : (T.t -> 'a -> 'b) -> ('a, 'b) p
    (** [make f] accepts optional arguments and applies them in the default record to [f]. *)

    val thaw : (unit, 'a -> 'b) p -> ('a, 'b) p
    
    val uncurry : ('a, 'b -> 'c) p -> ('a * 'b, 'c) p

    val curry : ('a * 'b, 'c) p -> ('a, 'b -> 'c) p

    val split : ('a, 'b) p -> ('c, 'd) p -> ('a * 'c, 'b * 'd) p

    val fanout : ('a, 'b) p -> ('a, 'c) p -> ('a, 'b * 'c) p
    (** Note that when ['a] is [unit], then [fanout x y = thaw @@ curry @@ split x y]. *)

    val unit : (unit, T.t) p

    val dimap : ('b -> 'a) -> ('c -> 'd) -> ('a, 'c) p -> ('b, 'd) p

    val contramap_fst : ('a -> 'b) -> ('b, 'c) p -> ('a, 'c) p

    val (<<<^) : ('a -> 'b) -> ('b, 'c) p -> ('a, 'c) p
    (** [<<<^] is infix [contramap_fst] *)

    val map_snd : ('b -> 'c) -> ('a, 'b) p -> ('a, 'c) p

    val (^>>>) : ('b -> 'c) -> ('a, 'b) p -> ('a, 'c) p
    (** [^>>>] is infix [map_snd] *)

    val map_sndt : ('b -> T.t -> 'c) -> ('a, 'b) p -> ('a, 'c) p

    val (^^>>>) : ('b -> T.t -> 'c) -> ('a, 'b) p -> ('a, 'c) p
    (** [^^>>>] is infix [map_sndt] *)

    val map_snd_given_fst : ('a -> 'b -> 'c) -> ('a, 'b) p -> ('a, 'c) p

    module Infix :
      sig
        val (<<<^) : ('a -> 'b) -> ('b, 'c) p -> ('a, 'c) p
        val (^>>>) : ('b -> 'c) -> ('a, 'b) p -> ('a, 'c) p
        val (^^>>>) : ('b -> T.t -> 'c) -> ('a, 'b) p -> ('a, 'c) p
      end
  end