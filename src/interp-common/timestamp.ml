open Core

module type S = sig
  type t
  val initial : t
  val push : t -> t
  val increment : t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string

  type comparator_witness
  val comparator : (t, comparator_witness) Comparator.t
end

module Simple : S = struct
  module T = struct
    type t =
      | Timestamp of int list [@@unboxed]
    [@@deriving equal, sexp]
  end

  include T
  let initial = Timestamp [1]
  let push (Timestamp xs) = Timestamp (1::xs)
  let increment = function
    | Timestamp (x::xs) -> Timestamp ((x+1)::xs)
    | Timestamp [] -> failwith "Invariant broken: empty timestamp"
  let compare (Timestamp xs1) (Timestamp xs2) =
    List.compare compare (List.rev xs1) (List.rev xs2)
  let to_string (Timestamp xs) =
    List.fold_left (List.rev xs) ~init:"" ~f:(fun acc t ->
        acc ^ "." ^ string_of_int t
      )

  include Comparator.Make (struct
      include T
      let compare = compare
    end)
end

module PerfectHash : S = struct
  type t = int [@@deriving equal, compare, sexp]

  type entry = {
    mutable pushed : int;
    mutable incremented : int;
    time : int list;
  }

  let invalid = -1 (* sentinel for uninitialized indices *)

  let table : entry Vector.t =
    (* Create vector *)
    let v =
      Vector.create
        ~dummy:({pushed=invalid; incremented=invalid; time=[]})
    in
    (* Initialize first timestamp *)
    Vector.push v {pushed=invalid; incremented=invalid; time=[1]};
    v
  ;;

  let initial = 0 (* first timestamp is already initialized *)

  let push (time : t) : t =
    let entry = Vector.get table time in
    if entry.pushed <> invalid then entry.pushed else begin
      let entry' = {pushed=invalid; incremented=invalid; time=1::entry.time} in
      let entry'_idx = Vector.length table in
      Vector.push table entry';
      entry.pushed <- entry'_idx;
      entry'_idx
    end

  let increment (time : t) : t =
    let entry = Vector.get table time in
    if entry.incremented <> invalid then entry.incremented else begin
      let time' = match entry.time with
        | x::xs -> (x+1)::xs
        | [] -> failwith "Invariant broken: empty time list"
      in
      let entry' = {pushed=invalid; incremented=invalid; time=time'} in
      let entry'_idx = Vector.length table in
      Vector.push table entry';
      entry.incremented <- entry'_idx;
      entry'_idx
    end

  let to_string (time : t) : string =
    let entry = Vector.get table time in
    String.concat ~sep:"." (List.map (List.rev entry.time) ~f:string_of_int)

  include Comparator.Make(Int)
end

(* Select a default implementation *)
include PerfectHash