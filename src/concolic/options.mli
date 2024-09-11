
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
    type ('a, 'b) t =
      ?global_timeout_sec    : float
      -> ?solver_timeout_sec : float
      -> ?global_max_step    : int
      -> ?max_tree_depth     : int
      -> ?random             : bool
      -> ?n_depth_increments : int
      -> 'a
      -> 'b

    val run : ('a, 'b) t -> T.t -> 'a -> 'b
    (** [run x r] applies the values from [r] to the arguments of [x] *)

    val make : (T.t -> 'a -> 'b) -> ('a, 'b) t
    (** [make f] accepts optional arguments and applies them in the default record to [f]. *)

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

      So let's just call the following behavior `bind`, even if that's a bad name.
    *)
    val bind : ('a, 'b) t -> ('b, 'r) t -> ('a, 'r) t
    (** [bind x f] is a function that runs [x] and then runs [f] on that result. *)

    val map : ('a, 'b) t -> ('b -> 'r) -> ('a, 'r) t
    (** [map x f] is a function that runs [x] and then maps the result with [f]. *)

    val compose : ('a -> 'b) -> ('b, 'r) t -> ('a, 'r) t
    (** [compose f x] first applies [f] without any optional arguments and then runs [x] on that result. *)

    val (>>=) : ('a, 'b) t -> ('b, 'r) t -> ('a, 'r) t
    (** [(>>=)] is infix [bind]. *)

    val (>>|) : ('a, 'b) t -> ('b -> 'r) -> ('a, 'r) t
    (** [(>>|)] is infix [map]. *)

    val (>=>) : ('a -> 'b) -> ('b, 'r) t -> ('a, 'r) t
    (** [(>=>)] is infix [compose]. *)
  end