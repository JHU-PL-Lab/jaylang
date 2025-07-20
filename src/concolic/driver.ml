
open Core
open Options.Arrow.Infix (* expose infix operators *)

type 'a test = ('a, do_wrap:bool -> do_type_splay:bool -> Status.Terminal.t) Options.Arrow.t

(*
  ----------------------
  TESTING BY EXPRESSIONS   
  ----------------------
*)

(* runs [lwt_eval] and catches lwt timeout *)
let test_with_timeout : (Lang.Ast.Embedded.t, Status.Terminal.t) Options.Arrow.t =
  Evaluator.lwt_eval
  >>^ fun res_status ->
    try Lwt_main.run res_status with
    | Lwt_unix.Timeout -> Timeout

(*
  --------------------
  PARALLEL COMPUTATION
  --------------------
*)

module Compute (O : Options.V) = struct
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
        let module E = Evaluator.Make (Solver.Make ()) (Pause.Id) (O) in
        Pause.Id.run
        @@ E.eval expr

    let run_with_internal_timeout : t -> Compute_result.t =
      fun expr ->
        Options.Arrow.appl test_with_timeout O.r expr
  end

  let timeout_sec = O.r.global_timeout_sec
end

(*
  ----------------
  TESTING PROGRAMS
  ----------------
*)

let test_bjy : Lang.Ast.Bluejay.pgm test =
  Options.Arrow.make
  @@ fun r -> fun bjy -> fun ~do_wrap ~do_type_splay ->
    let programs =
      if r.in_parallel
      then Translate.Convert.bjy_to_many_emb bjy ~do_wrap ~do_type_splay
      else Preface.Nonempty_list.Last (Translate.Convert.bjy_to_emb bjy ~do_wrap ~do_type_splay)
    in
    let module C = Compute (struct let r = r end) in
    let module P = Overlays.Computation_pool.Process (C) in
    let res = P.process_all @@ Preface.Nonempty_list.map Lang.Ast_tools.Utils.pgm_to_module programs in
    Format.printf "%s\n" (Status.to_loud_string res);
    res

(*
  -------------------
  TESTING BY FILENAME
  -------------------
*)

let test : Filename.t test =
  (fun s -> Lang.Parse.parse_single_pgm_string @@ In_channel.read_all s)
  ^>> test_bjy

(*
  ------------------------------
  TESTING FROM COMMAND LINE ARGS
  ------------------------------
*)

let ceval =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  Cmd.v (Cmd.info "ceval") @@
  let+ concolic_args = Options.cmd_arg_term
  and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term
  and+ bjy_pgm = Lang.Parse.parse_bjy_file_from_argv in
  Options.Arrow.appl
    test_bjy
    concolic_args
    bjy_pgm
    ~do_wrap
    ~do_type_splay