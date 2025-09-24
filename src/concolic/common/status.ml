
open Core

type 'a terminal = 'a constraint 'a = [ `Terminal ]

type 'a eval = 'a constraint 'a = [ `Eval ]

type _ t =
  | Found_abort : Interp_common.Input.t list * string -> 'a t
  | Type_mismatch : Interp_common.Input.t list * string -> 'a t
  | Unbound_variable : Interp_common.Input.t list * Lang.Ast.Ident.t -> 'a t

  (* result from entire concolic evaluation *)
  | Timeout : 'a terminal t
  | Unknown : 'a terminal t (* due to solver timeout, but continued otherwise with no error found *)
  | Exhausted_pruned_tree : 'a terminal t
  | Exhausted_full_tree : 'a terminal t

  (* result from a single run *)
  | Reached_max_step : 'a eval t
  | Finished : 'a eval t
  [@@deriving variants]

let min (type a) (x : a t) (y : a t) : a t =
  if Variants.to_rank x < Variants.to_rank y
  then x
  else y

let is_terminal (type a) (x : a t) : bool =
  match x with
  | Found_abort _ | Type_mismatch _ | Timeout | Unbound_variable _ 
  | Exhausted_full_tree | Exhausted_pruned_tree | Unknown -> true
  | Reached_max_step | Finished -> false

let is_error_found (type a) (x : a t) : bool =
  match x with
  | Found_abort _ | Type_mismatch _ | Unbound_variable _ -> true
  | Timeout | Exhausted_full_tree | Exhausted_pruned_tree | Unknown
  | Reached_max_step | Finished -> false

let to_string (type a) (x : a t) : string =
  match x with
  | Found_abort (_, s)       -> Format.sprintf "Found abort:\n  %s" s
  | Type_mismatch (_, s)     -> Format.sprintf "Type mismatch:\n  %s" s
  | Unbound_variable (_, id) -> Format.sprintf "Unbound variable:\n  %s" (Lang.Ast.Ident.to_string id)
  | Exhausted_full_tree      -> "Exhausted"
  | Exhausted_pruned_tree    -> "Exhausted pruned true"
  | Unknown                  -> "Unknown due to solver timeout"
  | Timeout                  -> "Timeout"
  | Reached_max_step         -> "Reached max step"
  | Finished                 -> "Finished interpretation"

let to_loud_string (type a) (x : a t) : string =
  let make_loud s =
    String.map s ~f:(fun c ->
      if Char.is_alpha c
      then Char.uppercase c
      else
        if Char.is_whitespace c
        then '_'
        else c
    )
  in
  let s = to_string x in
  match String.substr_index s ~pattern:":" with
  | None -> make_loud s
  | Some i ->
    let before_colon = String.prefix s i in
    let after_colon = String.drop_prefix s i in
    make_loud before_colon ^ after_colon

module Eval = struct
  type nonrec t = [ `Eval ] t
end

module Terminal = struct
  type nonrec t = [ `Terminal ] t

  let to_answer = function
    | Found_abort _
    | Type_mismatch _
    | Unbound_variable _ -> Answer.Ill_typed
    | Timeout
    | Unknown
    | Exhausted_pruned_tree -> Unknown
    | Exhausted_full_tree -> Well_typed
end

