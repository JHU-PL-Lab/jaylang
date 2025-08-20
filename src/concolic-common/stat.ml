
open Core

type time_kind = 
  | Interp_time  (* How long was spent interpreting the program *)
  | Solve_time   (* How long was spent solving constraints *)
  | Total_time   (* How long the entire concolic evaluation (loop) took *)
  [@@deriving equal]

type count_kind =
  | N_interps (* Number of interpretations taken during concolic loop *)
  | N_solves  (* Number of times the constraints for a target were attempted to be solved *)
  [@@deriving equal]

type depth_kind =
  | Target_depth (* How many branches were in the target that led to the error *)
  | Error_depth  (* How many branches were in the path that led to the error *)
  [@@deriving equal]

module T = struct
  type t =
    | Time of time_kind * Mtime.Span.t
    | Count of count_kind * int
    | Depth of depth_kind * int
end

include T

module List_builder = Utils.Builder.Make_list_builder (T)

module type LOG_M = sig
  type log

  include Utils.Types.MONAD
  val log : t -> unit m
  val observe : log m
end

module type LOG_T = functor (M : Utils.Types.MONAD) -> sig
  include Utils.Types.TRANSFORMED with type 'a lower := 'a M.m
  include LOG_M with type 'a m := 'a m
  val run : 'a m -> ('a * log) M.m
end

module Transform (M : Utils.Types.MONAD) = struct
  type log = t list

  include Utils.Builder.Transformer (M) (List_builder)

  let fold (init : 'acc) (f : 'acc -> t -> 'acc) : 'acc m =
    let%bind l = observe in
    return @@ List.fold l ~init ~f
  
  let sum_time (kind : time_kind) : Mtime.Span.t m =
    fold Mtime.Span.zero (fun acc -> function
      | Time (k, s) when equal_time_kind kind k -> Mtime.Span.add acc s
      | _ -> acc
    )

  let sum_count (kind : count_kind) : int m =
    fold 0 (fun acc -> function
      | Count (k, c) when equal_count_kind kind k -> acc + c
      | _ -> acc
    )

  let run x =
    Lwt_main.run (x [])
end
