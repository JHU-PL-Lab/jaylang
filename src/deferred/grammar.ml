
(* open Lang.Ast *)

(*
  In the future, I think many of these should have their own
  file and be abstract behind an mli.
*)

module Input_feeder = struct
  (* TODO: work with pick_b too *)
  type t = Timestamp.t -> int
end

module Symbol_map = struct
  type t = Value.whnf Timestamp.Map.t
end

(* TODO: the grammar needs to note that v in M is whnf *)
module Pending = struct
  type t = Value.closure Timestamp.Map.t
end
