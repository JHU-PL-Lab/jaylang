
open Core

type 'a terminal = 'a constraint 'a = [> `Terminal ]

type 'a eval = 'a constraint 'a = [ `Eval ]

type 'a in_progress = 'a constraint 'a = [> `In_progress ]

type _ t =
  | Found_abort : Input.t list -> 'a t
  | Type_mismatch : Input.t list -> 'a t

  (* result from entire concolic evaluation *)
  | Exhausted_full_tree : 'a terminal t
  | Exhausted_pruned_tree : 'a terminal t
  | Timeout : 'a terminal t

  (* result from a single run *)
  | Finished : { pruned : bool ; reached_max_step : bool ; stem : Stem.t } -> 'a eval t

  (* status while evaluation is ongoing *)
  | Diverge : 'a in_progress t
  | In_progress : 'a in_progress t

let is_terminal (type a) (x : a t) : bool =
  match x with
  | Found_abort _ | Type_mismatch _ | Timeout
  | Exhausted_full_tree | Exhausted_pruned_tree -> true
  | Finished _ | Diverge | In_progress -> false

let is_error_found (type a) (x : a t) : bool =
  match x with
  | Found_abort _ | Type_mismatch _ -> true
  | Timeout | Exhausted_full_tree | Exhausted_pruned_tree
  | Finished _ | Diverge | In_progress -> false

let to_string (type a) (x : a t) : string =
  match x with
  | Found_abort _ -> "Found abort"
  | Type_mismatch _ -> "Type mismatch"
  | Exhausted_full_tree -> "Exhausted full tree"
  | Exhausted_pruned_tree -> "Exhausted pruned true"
  | Timeout -> "Timeout"
  | Finished _ -> "Finished interpretation"
  | Diverge -> "Diverge"
  | In_progress -> "In progress"

let to_loud_string (type a) (x : a t) : string =
  String.map (to_string x) ~f:(fun c ->
    if Char.is_alpha c
    then Char.uppercase c
    else
      if Char.is_whitespace c
      then '_'
      else c
  )

module In_progress = struct
  type nonrec t = [ `In_progress ] t
end

module Eval = struct
  type nonrec t = [ `Eval ] t
end

module Terminal = struct
  type nonrec t = [ `Terminal ] t
end

module Intra = struct
  type nonrec t = [ `Terminal | `In_progress ] t
end

