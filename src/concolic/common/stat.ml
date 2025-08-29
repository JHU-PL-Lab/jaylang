
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

module Unit_builder : Utils.Builder.S with type a = t and type t = unit = struct
  type a = t
  type t = unit
  let empty = ()
  let cons _ () = ()
  let combine () () = ()
end

let sum_time (kind : time_kind) (ls : t list) : Mtime.Span.t =
  List.fold ls ~init:Mtime.Span.zero ~f:(fun acc -> function
    | Time (k, s) when equal_time_kind kind k -> Mtime.Span.add acc s
    | _ -> acc
  )

let sum_count (kind : count_kind) (ls : t list) : int =
  List.fold ls ~init:0 ~f:(fun acc -> function
    | Count (k, c) when equal_count_kind kind k -> acc + c
    | _ -> acc
  )
