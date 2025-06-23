
open Core

type 'a terminal = 'a constraint 'a = [ `Terminal ]

type 'a eval = 'a constraint 'a = [ `Eval ]

type _ t =
  | Found_abort : Input.t list * string -> 'a t
  | Type_mismatch : Input.t list * string -> 'a t
  | Unbound_variable : Input.t list * Lang.Ast.Ident.t -> 'a t

  (* result from entire concolic evaluation *)
  | Exhausted_full_tree : 'a terminal t
  | Exhausted_pruned_tree : 'a terminal t
  | Unknown : 'a terminal t (* due to solver timeout, but continued otherwise with no error found *)
  | Timeout : 'a terminal t

  (* result from a single run *)
  | Finished : { pruned : bool } -> 'a eval t

let is_terminal (type a) (x : a t) : bool =
  match x with
  | Found_abort _ | Type_mismatch _ | Timeout | Unbound_variable _ 
  | Exhausted_full_tree | Exhausted_pruned_tree | Unknown -> true
  | Finished _ -> false

let is_error_found (type a) (x : a t) : bool =
  match x with
  | Found_abort _ | Type_mismatch _ | Unbound_variable _ -> true
  | Timeout | Exhausted_full_tree | Exhausted_pruned_tree | Unknown
  | Finished _ -> false

let to_string (type a) (x : a t) : string =
  match x with
  | Found_abort (_, s)       -> Format.sprintf "Found abort:\n  %s" s
  | Type_mismatch (_, s)     -> Format.sprintf "Type mismatch:\n  %s" s
  | Unbound_variable (_, id) -> Format.sprintf "Unbound variable:\n  %s" (Lang.Ast.Ident.to_string id)
  | Exhausted_full_tree      -> "Exhausted"
  | Exhausted_pruned_tree    -> "Exhausted pruned true"
  | Unknown                  -> "Unknown due to solver timeout"
  | Timeout                  -> "Timeout"
  | Finished _               -> "Finished interpretation"

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
  match String.split (to_string x) ~on:':' with
  | [ s ] -> make_loud s
  | before_colon :: after_colon :: [] -> make_loud before_colon ^ ":" ^ after_colon
  | _ -> failwith "this doesn't make sense"

module Eval = struct
  type nonrec t = [ `Eval ] t
end

module Terminal = struct
  type nonrec t = [ `Terminal ] t
end

