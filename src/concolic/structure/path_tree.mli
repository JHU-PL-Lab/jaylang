(**
  File: path_tree.mli
  Purpose: track and solve for paths

  Detailed description:
    This module is really important. It takes the results of concolic
    interpretation and collects them into a tree that holds all program
    paths.

    Then, satisfiable targets can be popped from the tree.

    While the interface here is quite small, there is a lot going on
    (and a big place for a bug to leak in) behind in the implementation.

    Note that this is implicitly stateful because it uses the [Solve] module.
    No two parallel computations should share the same path tree. However,
    the state is very contained, and this module can appear persistent.

  Dependencies:
    Solve -- only satisfiable targets are returned, which requires solving
    Pause -- solving can take a while, so timeout needs to be able to interrupt
    Options -- the options constrain path depth and how targets are popped
    Target_queue -- targets found are pushed to queues to allow for easy popping
    Stem -- stems get attached to the tree
    Input_feeder -- input feeders are returned as a well to tell how to hit a target
    Target -- these are leaves of the path tree and are the goal of future runs
*)

module Make : functor (_ : Solve.S) (_ : Target_queue.S) (P : Pause.S) (_ : Options.V) -> sig
  type t

  val empty : t
  (** [empty] knows no path or constraints *)

  val add_stem : t -> Stem.t -> t
  (** [add_stem tree stem] is a new path tree where the [stem] has been placed onto the [tree]. *)

  val pop_sat_target : t -> (t * Target.t * Input_feeder.t) option P.t
  (** [pop_sat_target tree] is a new tree, target, and input feeder to hit that target, or
      is none if there are not satisfiable targets left in the [tree]. *)
end
