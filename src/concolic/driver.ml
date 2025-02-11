
open Core
open Options.Arrow.Infix (* expose infix operators *)
open Moonpool

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

module type Computation = sig
  type item
  type t
  val default : t
  val timeout_res : t
  val should_quit : t -> bool
  val merge : t -> t -> t
  val run : item -> t (* will not use Lwt *)
  val run_with_timeout : item -> t (* may use Lwt *)
  val timeout_sec : float
end

module Process (C : Computation) = struct
  let process_all ls =
    match ls with
    | [] -> C.default
    | [ item ] -> C.run_with_timeout item
    | _ ->
      (* let ls = [ List.hd_exn ls ] in FIXME: delete this to run more threads *)
      let t0 = Caml_unix.gettimeofday () in
      let pool = Ws_pool.create ~num_threads:(List.length ls) () in
      let futures = 
        List.map ls ~f:(fun item ->
          Fut.spawn ~on:pool (fun () -> C.run item)
        )
      in
      let rec go acc unfinished futures =
        if Float.(Caml_unix.gettimeofday () - t0 > C.timeout_sec)
        then C.timeout_res
        else
          match futures with
          | [] ->
            if List.is_empty unfinished
            then acc (* totally done with work *)
            else go acc [] unfinished (* done with this pass, so begin new pass *)
          | promise :: tl ->
            if Fut.is_done promise
            then begin
              let res = Fut.await promise in
              if C.should_quit res
              then res
              else go (C.merge res acc) unfinished tl
            end
            else go acc (promise :: unfinished) tl
      in
      let res = go C.default [] futures in
      Ws_pool.shutdown_without_waiting pool;
      res
end

module Compute (O : Options.V) = struct
  type item = Lang.Ast.Embedded.t
  type t = Status.Terminal.t

  let default : t = Exhausted_full_tree
  let timeout_res : t = Timeout

  let should_quit : t -> bool = Status.is_error_found

  let merge : t -> t -> t =
    fun a b ->
      let open Status in
      match a, b with
      (* keep the message that says to quit *)
      | Found_abort _, _ | Type_mismatch _, _ -> a
      | _, Found_abort _ | _, Type_mismatch _ -> b
      (* none say to quit, so keep the message that says we know the LEAST *)
      | Timeout, _ | _, Timeout -> Timeout
      | Exhausted_pruned_tree, _ | _, Exhausted_pruned_tree -> Exhausted_pruned_tree
      | Exhausted_full_tree, Exhausted_full_tree -> Exhausted_full_tree

  let run_with_timeout : item -> t =
    fun expr ->
      Options.Arrow.appl test_with_timeout O.r expr

  let run : item -> t =
    fun expr ->
      let module E = Evaluator.Make (Pause.Id) (O) () in
      Pause.Id.run
      @@ E.eval expr

  let timeout_sec = O.r.global_timeout_sec
end

(*
  ----------------
  TESTING PROGRAMS
  ----------------
*)

(* let[@landmark] test_pgm : (Lang.Ast.Embedded.pgm, Status.Terminal.t) Options.Arrow.t =
  test_with_timeout
  <<^ (fun pgm -> Lang.Ast.Program.to_expr pgm)
  >>^ (fun res -> Format.printf "\n%s\n" (Status.to_loud_string res); res) *)

let test_bjy : (Lang.Ast.Bluejay.pgm, do_wrap:bool -> Status.Terminal.t) Options.Arrow.t =
  Options.Arrow.make
  @@ fun r -> fun bjy -> fun ~do_wrap ->
    let programs =
      Translate.Convert.bjy_to_emb bjy ~do_wrap
      |> List.map ~f:Lang.Ast.Program.to_expr
    in
    let module C = Compute (struct let r = r end) in
    let module P = Process (C) in
    let res = P.process_all programs in
    Format.printf "\n%s\n" (Status.to_loud_string res);
    res

(*
  -------------------
  TESTING BY FILENAME
  -------------------
*)

let test : (string, do_wrap:bool -> Status.Terminal.t) Options.Arrow.t =
  Options.Arrow.make
  @@ fun r -> fun s -> fun ~do_wrap ->
    In_channel.read_all s
    |> Lang.Parse.parse_single_pgm_string
    |> Options.Arrow.appl test_bjy r ~do_wrap