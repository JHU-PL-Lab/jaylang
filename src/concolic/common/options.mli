
module T : sig
  type t =
    { global_timeout_sec : float
    ; global_max_step    : int
    ; max_tree_depth     : int
    ; random             : bool
    ; n_depth_increments : int
    ; in_parallel        : bool }
    [@@deriving sexp]
end

type t = T.t

val default : t

module type V = sig
  val r : t
end

module Refs : sig
  type t =
    { global_timeout_sec : float ref
    ; global_max_step    : int ref
    ; max_tree_depth     : int ref
    ; random             : bool ref
    ; n_depth_increments : int ref
    ; in_parallel        : bool ref }

  val create_default : unit -> t
  (** [create_default ()] has the default values from [default]. *)

  val without_refs : t -> T.t
  (** [without_refs t] is all the values in the cells in [t]. *)
end

(* `Arrow` for optional arguments on functions *)
module Arrow : sig
  type ('a, 'b) t =
    ?global_timeout_sec    : float
    -> ?global_max_step    : int
    -> ?max_tree_depth     : int
    -> ?random             : bool
    -> ?n_depth_increments : int
    -> ?in_parallel        : bool
    -> 'a
    -> 'b
  (** [t] is an arrow *)

  include Preface.Specs.ARROW with type ('a, 'b) t := ('a, 'b) t

  val appl : ('b, 'c) t -> T.t -> 'b -> 'c
  (** [run x r] applies the values from [r] to the arguments of [x] *)

  val make : (T.t -> 'b -> 'c) -> ('b, 'c) t
  (** [make f] accepts optional arguments and applies them in the default record to [f]. *)

  val thaw : (unit, 'a -> 'b) t -> ('a, 'b) t
  (** [thaw x] is [uncurry x <<^ (fun y -> (), y)] *)
end