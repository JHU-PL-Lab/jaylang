
open Core
open Concolic_common

(*
  ----------------------
  TESTING BY EXPRESSIONS   
  ----------------------
*)

(* runs [lwt_eval] and catches lwt timeout *)
let test_with_timeout :
  options:Options.t -> Lang.Ast.Embedded.t -> Status.Terminal.t =
  fun ~options prog ->
  let status = Evaluator.eager_c_loop ~options prog in
  try Lwt_main.run status with
  | Lwt_unix.Timeout -> Timeout

(*
  --------------------
  PARALLEL COMPUTATION
  --------------------
*)

module type ComputeConfig = sig
  val options : Options.t
end

module Compute (Cfg : ComputeConfig) = struct
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

    module TQ_Made = Target_queue.Make (Interp_common.Step)
    module Eval (S : Smt.Formula.SOLVABLE) = Evaluator.Make (Interp_common.Step) (TQ_Made.All) (S) (Pause.Id)

    let run : t -> Compute_result.t =
      fun expr ->
      (* makes a new solver for this thread *)
      let module E = Eval (Overlays.Typed_z3.Make ()) in
      Pause.Id.run (E.c_loop ~options:Cfg.options Evaluator.eager_eval expr)

    let run_with_internal_timeout : t -> Compute_result.t =
      fun expr ->
        test_with_timeout ~options:Cfg.options expr
  end

  let timeout_sec = Cfg.options.global_timeout_sec
end

(*
  ----------------
  TESTING PROGRAMS
  ----------------
*)

(*
  TODO: have this (or the work inside Compute functor) take an evaluation function.
*)
let test_bjy :
  options:Options.t -> do_wrap:bool -> do_type_splay:bool -> Lang.Ast.Bluejay.pgm ->
  Status.Terminal.t =
  fun ~options ~do_wrap ~do_type_splay program ->
  let programs =
    if options.in_parallel
    then Translate.Convert.bjy_to_many_emb program ~do_wrap ~do_type_splay
    else Preface.Nonempty_list.Last (Translate.Convert.bjy_to_emb program ~do_wrap ~do_type_splay)
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

let test :
  options:Options.t -> do_wrap:bool -> do_type_splay:bool -> Filename.t ->
  Status.Terminal.t =
  fun ~options ~do_wrap ~do_type_splay filename ->
  test_bjy
    ~options ~do_wrap ~do_type_splay (Lang.Parser.Bluejay.parse_file filename)

(*
  ------------------------------
  TESTING FROM COMMAND LINE ARGS
  ------------------------------
*)

let ceval =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  Cmd.v (Cmd.info "ceval") @@
  let+ options = Options.cmd_arg_term
  and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term
  and+ pgm = Lang.Parser.parse_program_from_argv in
  match pgm with
  | Lang.Ast.SomeProgram (BluejayLanguage, bjy_pgm) ->
    test_bjy ~options ~do_wrap ~do_type_splay bjy_pgm
  | _ -> failwith "TODO"