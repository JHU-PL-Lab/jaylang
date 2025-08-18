
open Core
open Concolic_common

module type CONFIG = sig
  module Key : Smt.Symbol.KEY
  module TQ : Target_queue.S with type k = Key.t
  val ceval : Key.t Evaluator.eval
end

module type S = sig
  type ceval_witness

  val test_some_program :
    options:Options.t ->
    do_wrap:bool ->
    do_type_splay:bool ->
    Lang.Ast.some_program ->
    Concolic_common.Status.Terminal.t

  val test_some_file :
    options:Options.t ->
    do_wrap:bool ->
    do_type_splay:bool ->
    Core.Filename.t ->
    Concolic_common.Status.Terminal.t

  val eval : Concolic_common.Status.Terminal.t Cmdliner.Cmd.t
end

(* This is generative because it uses a generative functor to make the default evaluator *)
module Make (Cfg : CONFIG) () : S = struct
  type ceval_witness

  module Eval (S : Smt.Formula.SOLVABLE) = Evaluator.Make (Cfg.Key) (Cfg.TQ) (S) (Pause.Id)
  module Default_eval = Evaluator.Make (Cfg.Key) (Cfg.TQ) (Overlays.Typed_z3.Make ()) (Pause.Lwt)

  (*
    ----------------------
    TESTING BY EXPRESSIONS   
    ----------------------
  *)

  (* runs [lwt_eval] and catches lwt timeout *)
  let test_with_timeout :
    options:Options.t -> Lang.Ast.Embedded.t -> Status.Terminal.t =
    fun ~options prog ->
    let status = Default_eval.c_loop ~options Cfg.ceval prog in
    try Lwt_main.run status with
    | Lwt_unix.Timeout -> Timeout

  module Compute (O : sig val options : Options.t end) = struct
    module Compute_result = struct
      include Preface.Make.Monoid.Via_combine_and_neutral (struct
          type t = Status.Terminal.t
          let neutral : t = Exhausted_full_tree
          let combine : t -> t -> t =
            fun a b ->
            let open Status in
            match a, b with
            (* keep the message that says to quit *)
            | Found_abort _, _ | Type_mismatch _, _ | Unbound_variable _, _ -> a
            | _, Found_abort _ | _, Type_mismatch _ | _, Unbound_variable _ -> b
            (* none say to quit, so keep the message that says we know the LEAST *)
            | Timeout, _ | _, Timeout -> Timeout
            | Unknown, _ | _, Unknown -> Unknown
            | Exhausted_pruned_tree, _ | _, Exhausted_pruned_tree -> Exhausted_pruned_tree
            | Exhausted_full_tree, Exhausted_full_tree -> Exhausted_full_tree
        end)

      let is_signal_to_quit : t -> bool = Status.is_error_found
      let timeout_res : t = Timeout
    end

    module Work = struct
      type t = Lang.Ast.Embedded.t

      let run : t -> Compute_result.t =
        fun expr ->
        (* makes a new solver for this thread *)
        let module E = Eval (Overlays.Typed_z3.Make ()) in
        Pause.Id.run (E.c_loop ~options:O.options Cfg.ceval expr)

      let run_with_internal_timeout : t -> Compute_result.t =
        fun expr ->
        test_with_timeout ~options:O.options expr
    end

    let timeout_sec = O.options.global_timeout_sec
  end

  (*
    ----------------
    TESTING PROGRAMS
    ----------------
  *)

  let test_some_program :
    options:Options.t ->
    do_wrap:bool ->
    do_type_splay:bool ->
    Lang.Ast.some_program ->
    Status.Terminal.t =
    fun ~options ~do_wrap ~do_type_splay program ->
    let programs =
      if options.in_parallel
      then Translate.Convert.some_program_to_many_emb program ~do_wrap ~do_type_splay
      else Preface.Nonempty_list.Last (Translate.Convert.some_program_to_emb program ~do_wrap ~do_type_splay)
    in
    let module C = Compute (struct let options = options end) in
    let module P = Overlays.Computation_pool.Process (C) in
    let res = P.process_all @@ Preface.Nonempty_list.map Lang.Ast_tools.Utils.pgm_to_module programs in
    Format.printf "%s\n" (Status.to_loud_string res);
    res

  (*
    -------------------
    TESTING BY FILENAME
    -------------------
  *)

  let test_some_file :
    options:Options.t -> do_wrap:bool -> do_type_splay:bool -> Filename.t ->
    Status.Terminal.t =
    fun ~options ~do_wrap ~do_type_splay filename ->
    test_some_program
      ~options
      ~do_wrap
      ~do_type_splay
      (Lang.Parser.parse_program_from_file filename)

  (*
    ------------------------------
    TESTING FROM COMMAND LINE ARGS
    ------------------------------
  *)

  let eval : Status.Terminal.t Cmdliner.Cmd.t =
    let open Cmdliner in
    let open Cmdliner.Term.Syntax in
    Cmd.v (Cmd.info "ceval") @@
    let+ options = Options.cmd_arg_term
    and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term
    and+ pgm = Lang.Parser.parse_program_from_argv in
    test_some_program ~options ~do_wrap ~do_type_splay pgm
end

module Eager = Make (struct
  module Key = Interp_common.Step
  module TQ_made = Target_queue.Make (Key)
  module TQ = TQ_made.BFS
  let ceval = Eager_concolic.Main.eager_eval
end) ()

module Deferred = Make (struct
  module Key = Interp_common.Timestamp
  module TQ_made = Target_queue.Make (Key)
  module TQ = TQ_made.BFS
  let ceval = Deferred.Cmain.deferred_eval
end) ()

module Default = Eager

include Default
