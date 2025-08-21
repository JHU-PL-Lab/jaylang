
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
