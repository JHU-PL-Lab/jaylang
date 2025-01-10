
open Core

module Ident = struct
  module T = struct
    type t = Ident of string
    [@@unboxed][@@deriving eq, ord, show, sexp, compare, hash, to_yojson]
  end

  include T
  module Map = Map.Make (T)
  module Set = Set.Make (T)
end

module Var = struct
  type t = Var of Ident.t
  [@@unboxed][@@deriving eq, ord, show, sexp, compare, hash, to_yojson]
end

type binop =
  | Bplus
  | Bminus
  | Btimes
  | Bdivide
  | Bmodulus
  | Bless_than
  | Bless_than_or_equal_to
  | Bequal_to
  | Bnot_equal_to
  | Band
  | Bor

type pattern =
  | Pfun
  | Pint
  | Pbool
  | Precord of Ident.Set.t
  | Pstrict_record of Ident.Set.t

type value =
  | Vint of int
  | Vbool of bool
  | Vrecord of { label_to_value : expr Ident.Map.t } (* note this is currently not eager *)
  | Vfunction of { param : Ident.t ; body : expr }
  | Vfreeze of expr

and expr =
  | Evalue of value
  | Ethaw of expr
  | Evar of Var.t
  | Einput_int
  | Einput_bool
  | Eapply of      { f       : expr ; arg       : expr }
  | Econdition of  { cond    : expr ; true_body : expr  ; false_body : expr }
  | Ebinop of      { left    : expr ; binop     : binop ; right      : expr }
  | Eproject of    { record  : expr ; label_id  : Ident.t }
  | Ecase_on of    { subject : expr ; cases     : expr Int.Map.t } (* cases on the integer value in the subject, or otherwise ill-typed *)
  | Ematch_with of { subject : expr ; patterns  : (pattern * expr) list }
  | Enot of expr
  | Eabort
  | Ediverge
(* I will consider whether I want to flatten the values and give them each their own expression constructor, then I can have eager records *)
(* Also we will desugar all let expressions into function applications *)
(* I want to have freeze and thaw for gens and that way we are adding less to the environment (and maybe we can find a way to not take a closure) *)
(* But we do need to note that thawing does not modify the environment (like lazy eval would where we only eval once) but instead just evaluates on the fly the body of the freeze *)

