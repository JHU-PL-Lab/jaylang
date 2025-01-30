open Core
open Options.Fun.Infix

module Symbolic = Symbolic_session

module Status = struct
  type t =
    | In_progress of { pruned : bool }
    | Found_abort of (Input.t list [@compare.ignore])
    | Type_mismatch of (Input.t list [@compare.ignore])
    | Exhausted of { pruned : bool }
    [@@deriving compare, sexp]

  let prune (x : t) : t =
    match x with
    | In_progress _ -> In_progress { pruned = true }
    | Exhausted _ -> Exhausted { pruned = true }
    | _ -> x

  let quit (x : t) : bool =
    match x with
    | Found_abort _
    | Type_mismatch _
    | Exhausted _ -> true
    | In_progress _ -> false

  let finish (x : t) : t =
    match x with
    | In_progress { pruned } -> Exhausted { pruned }
    | _ -> x

  let to_string (x : t) : string =
    match x with
    | Found_abort _                 -> "Found abort in interpretation"
    | Type_mismatch _               -> "Found type mismatch in interpretation"
    | In_progress { pruned = true } -> "In progress after interpretation (has pruned so far)"
    | In_progress _                 -> "In progress after interpretation"
    | Exhausted { pruned = true }   -> "Exhausted pruned true"
    | Exhausted _                   -> "Exhausted full tree"
end

(* ignore warning about last_sym not used because it depends on current code for pop kind *)
type[@ocaml.warning "-69"] t =
  { tree         : Path_tree.t (* pointer to the root of the entire tree of paths *)
  ; run_num      : int
  ; options      : Options.t
  ; status       : Status.t
  ; prev_res     : Symbolic.Status.t option }

let empty : t =
  { tree         = Options.Fun.appl Path_tree.of_options Options.default ()
  ; run_num      = 1
  ; options      = Options.default
  ; status       = Status.In_progress { pruned = false }
  ; prev_res     = None }

let of_options : (unit, t * Symbolic.t) Options.Fun.a =
  (Options.Fun.make (fun r () -> r) &&& Path_tree.of_options) 
  ^>> (fun (r, tree) -> { empty with options = r ; tree })
  &&& (Symbolic.with_options <<^ fun () -> Symbolic.empty)

let accum_symbolic (x : t) (sym : Symbolic.t) : t =
  let sym_status = Symbolic.finish sym in
  sym_status
  |> begin function
    | Symbolic.Status.Found_abort inputs -> x.tree, Status.Found_abort inputs
    | Type_mismatch inputs -> x.tree, Type_mismatch inputs
    | Finished_interpretation { pruned ; reached_max_step ; stem } ->
      let tree = Path_tree.add_stem x.tree stem in
      if pruned || reached_max_step
      then tree, Status.prune x.status
      else tree, x.status
  end
  |> fun (tree, new_status) ->
    { x with tree
    ; status = new_status
    ; prev_res = Some sym_status }

let[@landmark] next (x : t) : [ `Done of Status.t | `Next of (t * Symbolic.t) ] Lwt.t =
  if Status.quit x.status then Lwt.return @@ `Done x.status else
  let pop_kind =
    match x.prev_res with
    | Some (Finished_interpretation { reached_max_step ; _ }) when reached_max_step -> Target_queue.Pop_kind.BFS (* only does BFS when last symbolic run reached max step *)
    | _ -> Random
  in
  let%lwt res = Path_tree.pop_sat_target ~kind:pop_kind x.tree in
  match res with
  | Some (tree, target, input_feeder) ->
    Lwt.return
    @@ `Next ( 
      { x with tree ; run_num = x.run_num + 1 }
      , Symbolic.make target input_feeder
        |> Options.Fun.appl Symbolic.with_options x.options
    )
  | None -> 
    Lwt.return 
    @@ `Done (Status.finish x.status)

let run_num ({ run_num ; _ } : t) : int =
  run_num