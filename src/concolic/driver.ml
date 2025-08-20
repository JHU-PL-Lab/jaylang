
open Core
open Concolic_common

module Make_with_logger (L : sig
  type tape
  module Tape : Preface_specs.MONOID with type t = tape
  module Transform (M : Utils.Types.MONAD) : sig
    include Utils.Types.TRANSFORMED with type 'a lower := 'a M.m
    include Stat.LOG_M with type 'a m := 'a m and type tape = tape
    val run : 'a m -> ('a * tape) M.m
  end
end) = struct
  module type S = sig
    val test_some_program :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:bool ->
      Lang.Ast.some_program ->
      Concolic_common.Status.Terminal.t * L.tape

    val test_some_file :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:bool ->
      Core.Filename.t ->
      Concolic_common.Status.Terminal.t * L.tape

    val eval : Concolic_common.Status.Terminal.t Cmdliner.Cmd.t
  end

  module Make (Key : Smt.Symbol.KEY) (Make_tq : Target_queue.MAKE) 
    (C : Evaluator.EVAL with type k := Key.t) () : S = struct
    module Eval (S : Smt.Formula.SOLVABLE) = Evaluator.Make (Key) (Make_tq) (S) (Pause.Id) (L.Transform)
    module Default_eval = Evaluator.Make (Key) (Make_tq) (Overlays.Typed_z3.Make ()) (Pause.Lwt) (L.Transform)

    module Lwt_log = L.Transform (Pause.Lwt)
    module Log = L.Transform (Pause.Id)

    (*
      ----------------------
      TESTING BY EXPRESSIONS   
      ----------------------
    *)

    (* runs [lwt_eval] and catches lwt timeout *)
    let test_with_timeout 
      : options:Options.t -> Lang.Ast.Embedded.t -> Status.Terminal.t * L.tape
      = fun ~options prog ->
      let main = Default_eval.c_loop ~options C.ceval prog in
      try Lwt_main.run @@ Lwt_log.run main with
      | Lwt_unix.Timeout -> Status.Timeout, failwith "unhandled log for timeout" (* FIXME: timeout doesn't provide stats *)

    module Compute (O : sig val options : Options.t end) = struct
      module Compute_result = struct
        include Preface.Make.Monoid.Via_combine_and_neutral (struct
          type t = Status.Terminal.t * L.tape
          let neutral : t = Exhausted_full_tree, L.Tape.neutral
          let combine : t -> t -> t = fun (a, x) (b, y) ->
            let open Status in
            let left =
              match a, b with
              (* keep the message that says to quit *)
              | Found_abort _, _ | Type_mismatch _, _ | Unbound_variable _, _ -> a
              | _, Found_abort _ | _, Type_mismatch _ | _, Unbound_variable _ -> b
              (* none say to quit, so keep the message that says we know the LEAST *)
              | Timeout, _ | _, Timeout -> Timeout
              | Unknown, _ | _, Unknown -> Unknown
              | Exhausted_pruned_tree, _ | _, Exhausted_pruned_tree -> Exhausted_pruned_tree
              | Exhausted_full_tree, Exhausted_full_tree -> Exhausted_full_tree
            in
            left, L.Tape.combine x y
        end)

        let is_signal_to_quit : t -> bool = fun (s, _) -> Status.is_error_found s
        let timeout_res : t = Timeout, L.Tape.neutral
      end

      module Work = struct
        type t = Lang.Ast.Embedded.t

        let run (expr : t) : Compute_result.t =
          (* makes a new solver for this thread *)
          let module E = Eval (Overlays.Typed_z3.Make ()) in
          Pause.Id.run @@ Log.run (E.c_loop ~options:O.options C.ceval expr)

        let run_with_internal_timeout (expr : t) : Compute_result.t =
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
      Status.Terminal.t * L.tape =
      fun ~options ~do_wrap ~do_type_splay program ->
      let programs =
        if options.in_parallel
        then Translate.Convert.some_program_to_many_emb program ~do_wrap ~do_type_splay
        else Preface.Nonempty_list.Last (Translate.Convert.some_program_to_emb program ~do_wrap ~do_type_splay)
      in
      let module C = Compute (struct let options = options end) in
      let module P = Overlays.Computation_pool.Process (C) in
      let status, tape = P.process_all @@ Preface.Nonempty_list.map Lang.Ast_tools.Utils.pgm_to_module programs in
      Format.printf "%s\n" (Status.to_loud_string status);
      status, tape

    (*
      -------------------
      TESTING BY FILENAME
      -------------------
    *)

    let test_some_file :
      options:Options.t -> do_wrap:bool -> do_type_splay:bool -> Filename.t ->
      Status.Terminal.t * L.tape =
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

include Make_with_logger (Stat.No_logging)
