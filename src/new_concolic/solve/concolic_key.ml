
open Core

type _ t =
  | Int_key : int -> int t
  | Bool_key : int -> bool t

let compare (type a) (x : a t) (y : a t) : int =
  match x, y with
  | Int_key xi, Int_key yi
  | Bool_key xi, Bool_key yi -> Int.compare xi yi

let equal (type a) (x : a t) (y : a t) : bool =
  match x, y with
  | Int_key xi, Int_key yi
  | Bool_key xi, Bool_key yi -> Int.equal xi yi

let to_string (type a) (x : a t) : string =
  match x with
  | Int_key i -> Format.sprintf "i_stepkey_$%d" i
  | Bool_key i -> Format.sprintf "b_stepkey_$%d" i

let uniq_id (type a) (x : a t) : int =
  match x with
  | Int_key i
  | Bool_key i -> i
