
open Core
open Concolic_common

module type S = sig
  type tape
  module type DRIVER = sig
    val test_some_program :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:bool ->
      Lang.Ast.some_program ->
      Concolic_common.Status.Terminal.t * tape

    val test_some_file :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:bool ->
      Core.Filename.t ->
      Concolic_common.Status.Terminal.t * tape

    val eval : Concolic_common.Status.Terminal.t Cmdliner.Cmd.t
  end

  module Make (Key : Smt.Symbol.KEY) (_ : Target_queue.MAKE) (_ : Evaluator.EVAL with type k := Key.t) () : DRIVER

  module Eager : DRIVER

  module Deferred : DRIVER

  module Default = Eager

  include DRIVER (* Is Default *)
end

module Of_logger (T : Utils.Logger.TRANSFORMER with type B.a = Stat.t) : S with type tape = T.tape = struct
  type tape = T.tape

  module type DRIVER = sig
    val test_some_program :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:bool ->
      Lang.Ast.some_program ->
      Concolic_common.Status.Terminal.t * tape

    val test_some_file :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:bool ->
      Core.Filename.t ->
      Concolic_common.Status.Terminal.t * tape

    val eval : Concolic_common.Status.Terminal.t Cmdliner.Cmd.t
  end

  module Make (Key : Smt.Symbol.KEY) (Make_tq : Target_queue.MAKE) 
    (C : Evaluator.EVAL with type k := Key.t) () : DRIVER = struct

    module Lwt_log = T.Transform (Pause.Lwt)
    module Log = T.Transform (Pause.Id)

    module Lwt_eval = Evaluator.Make (Key) (Make_tq) (Pause.Lwt) (Lwt_log)
    module Eval = Evaluator.Make (Key) (Make_tq) (Pause.Id) (Log)

    module Default_Z3 = Overlays.Typed_z3.Make ()
    module Default_solver = Smt.Formula.Make_solver (Default_Z3)


    (*
      ----------------------
      TESTING BY EXPRESSIONS   
      ----------------------
    *)

    (* runs [lwt_eval] and catches lwt timeout *)
    let test_with_timeout 
      : options:Options.t -> Lang.Ast.Embedded.t -> Status.Terminal.t * tape
      = fun ~options prog ->
      let main = Lwt_eval.c_loop ~options C.ceval Default_solver.solve prog in
      try Lwt_main.run @@ Lwt_log.run main with
      | Lwt_unix.Timeout -> Status.Timeout, T.B.empty (* FIXME: timeout doesn't provide stats *)

    module Compute (O : sig val options : Options.t end) = struct
      module Compute_result = struct
        open Log

        include Preface.Make.Monoid.Via_combine_and_neutral (struct
          type t = Status.Terminal.t Log.m
          let neutral : t = Log.return Status.Exhausted_full_tree
          let combine : t -> t -> t = fun am bm ->
            let%bind a = am in
            let%bind b = bm in
            return @@
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

        let is_signal_to_quit : t -> bool = 
          (* TOOD: this is hideous, but I don't see a way around it right now *)
          fun sm -> let s, _tape = run sm in Status.is_error_found s

        let timeout_res : t = return Status.Timeout
      end

      module Work = struct
        type t = Lang.Ast.Embedded.t

        let run (expr : t) : Compute_result.t =
          (* makes a new solver for this thread *)
          let module Z = Overlays.Typed_z3.Make () in
          let module S = Smt.Formula.Make_solver (Z) in
          Eval.c_loop ~options:O.options C.ceval S.solve expr
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
      Status.Terminal.t * tape =
      fun ~options ~do_wrap ~do_type_splay program ->
      let status, tape =
        if options.in_parallel
        then
          let pgms = Translate.Convert.some_program_to_many_emb program ~do_wrap ~do_type_splay in
          match pgms with
          | Last pgm -> 
            (* Nothing to do in parallel if only one program *)
            test_with_timeout ~options @@ Lang.Ast_tools.Utils.pgm_to_module pgm
          | _ ->
            let module C = Compute (struct let options = options end) in
            let module P = Overlays.Computation_pool.Process (C) in
            let status_m = P.process_all @@ Preface.Nonempty_list.map Lang.Ast_tools.Utils.pgm_to_module pgms in
            Log.run status_m
        else
          let pgm = Translate.Convert.some_program_to_emb program ~do_wrap ~do_type_splay in
          test_with_timeout ~options @@ Lang.Ast_tools.Utils.pgm_to_module pgm
      in
      Format.printf "%s\n" (Status.to_loud_string status);
      status, tape

    (*
      -------------------
      TESTING BY FILENAME
      -------------------
    *)

    let test_some_file :
      options:Options.t -> do_wrap:bool -> do_type_splay:bool -> Filename.t ->
      Status.Terminal.t * tape =
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
      let status, _ = test_some_program ~options ~do_wrap ~do_type_splay pgm in
      status
  end

  module Eager = Make (Interp_common.Step) (Target_queue.Make_BFS) (struct
    let ceval = Eager_concolic.Main.eager_eval
  end) ()

  module Deferred = Make (Interp_common.Timestamp) (Target_queue.Make_BFS) (struct
    let ceval = Deferred.Cmain.deferred_eval
  end) ()

  module Default = Eager

  include Default
end

include Of_logger (Utils.Logger.Transformer_of_builder (Stat.Unit_builder))
