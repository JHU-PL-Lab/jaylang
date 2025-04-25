(**
  Module [Ast].

  Here we define the languages used in this repo. They are
  defined using type constraints and GADTs to allow reuse
  of constructors.

  It's suggested to read the comments from top to bottom if
  the reader is unfamiliar with this use of GADTs.
*)

open Core

(*
  This module defines some types that constrain a polymorphic variable 'a
  to be some polymorphic variant or to be included in a set of polymorphic variants.

  We use these constraints in a GADT to allow sharing of constructors across the languages.
  This way, we can define three languages in one GADT without repetition.

  The and/or terminology can be a little confusing here. Here is a clarification:
  * Any constructor that is constrained with an "or" name is a language features that exists
    in *both* languages. i.e. it exists in "this language" and "that language"
  * We use "or" here because any instance of the constructor in code is *either* to instantiate
    the node in "this language" or "that language". It is like negating the "and" to be an "or"
    once we go from existence to actual instantiation.
    I use the "or" because it feels more close to saying the node can be used in either language,
    and it is more in line with the "<" symbol used in the typing which means the type 'a is constrained
    to be a subset of the declared polymorphic constructors, so either of them is valid.
    "either" <-> "or", so I use this wording as it aligns well with the code.
*)
module Constraints = struct
  type bluejay = [ `Bluejay ]
  type desugared = [ `Desugared ]
  type embedded = [ `Embedded ]

  (*
    Constrains 'a to be exactly `Bluejay.
    i.e. constructors in the GADT below with this constraint are only in the Bluejay language.

    We could also use "<" here instead of leaving it out, but it is pointless with one tag.
  *)
  type 'a bluejay_only = 'a constraint 'a = [ `Bluejay ]

  (*
    Constrains 'a to be included in "this tag" or "that tag" (or both, technically).
    i.e. constructors with this constraint can be used in either langauge.
  *)
  type 'a bluejay_or_desugared = 'a constraint 'a = [< `Bluejay | `Desugared ]
  type 'a desugared_or_embedded = 'a constraint 'a = [< `Desugared | `Embedded ]

  (*
    Constrains 'a to be exactly `Desugared .
    i.e. constructors with this constraint are only in the Desugared language.
  *)
  type 'a desugared_only = 'a constraint 'a = [ `Desugared ]

  (* you get the drill ... *)
  type 'a embedded_only = 'a constraint 'a = [ `Embedded ] 

  (* all languages *)
  (* type universe = [ `Bluejay | `Desugared | `Embedded ] *)
end

open Constraints

module Ident = struct
  module T = struct
    type t = Ident of string
      [@@unboxed] [@@deriving equal, compare, sexp, hash]
  end

  include T
  
  let to_string (Ident s) = s

  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

module RecordLabel = struct
  module T = struct
    type t = RecordLabel of Ident.t
      [@@unboxed] [@@deriving equal, compare, sexp, hash]
  end

  include T
  module Map = Map.Make (T)

  let to_string (RecordLabel Ident s) = s

  let record_body_to_string ?(sep : string = "=") m f =
    Core.Map.to_alist m
    |> List.map ~f:(fun (l, x) -> Format.sprintf "%s %s %s" (to_string l) sep (f x))
    |> fun ls -> "{ " ^ String.concat ~sep:" ; " ls ^ " }"
end

module VariantLabel = struct
  type t = VariantLabel of Ident.t
    [@@unboxed] [@@deriving equal, compare, sexp, hash]

  let to_string (VariantLabel Ident s) = s
end

module VariantTypeLabel = struct
  type t = VariantTypeLabel of Ident.t
    [@@unboxed] [@@deriving equal, compare, sexp, hash]

  let to_variant_label (VariantTypeLabel l) =
    VariantLabel.VariantLabel l
end

module Binop = struct
  type t =
    | BPlus
    | BMinus
    | BTimes
    | BDivide
    | BModulus
    | BEqual
    | BNeq
    | BLessThan
    | BLeq
    | BGreaterThan
    | BGeq
    | BAnd
    | BOr

  let to_string = function
    | BPlus -> "+"
    | BMinus -> "-"
    | BTimes -> "*"
    | BDivide -> "/"
    | BModulus -> "mod"
    | BEqual -> "="
    | BNeq -> "<>"
    | BLessThan -> "<"
    | BLeq -> "<="
    | BGreaterThan -> ">"
    | BGeq -> ">="
    | BAnd -> "and"
    | BOr -> "or"
end

module Pattern = struct
  type _ t =
    (* all languages *)
    | PAny : 'a t
    | PVariable : Ident.t -> 'a t
    | PVariant : { variant_label : VariantLabel.t ; payload_id : Ident.t } -> 'a t
    (* only Bluejay *)
    | PEmptyList : 'a bluejay_only t
    | PDestructList : { hd_id : Ident.t ; tl_id : Ident.t } -> 'a bluejay_only t

  let to_string : type a. a t -> string = function
    | PAny -> "any"
    | PVariable Ident s -> Format.sprintf "var %s" s
    | PVariant { variant_label  = VariantLabel Ident s ; _ } -> Format.sprintf "variant `%s" s
    | PEmptyList -> "[]"
    | PDestructList { hd_id = Ident hd ; tl_id = Ident tl } -> Format.sprintf "%s :: %s" hd tl
end

module Expr = struct
  type _ t =
    (* all languages. 'a is unconstrained *)
    | EInt : int -> 'a t
    | EBool : bool -> 'a t
    | EVar : Ident.t -> 'a t
    | EBinop : { left : 'a t ; binop : Binop.t ; right : 'a t } -> 'a t
    | EIf : { cond : 'a t ; true_body : 'a t ; false_body : 'a t } -> 'a t
    | ELet : { var : Ident.t ; body : 'a t ; cont : 'a t } -> 'a t
    | EAppl : { func : 'a t ; arg : 'a t } -> 'a t
    | EMatch : { subject : 'a t ; patterns : ('a Pattern.t * 'a t) list } -> 'a t
    | EProject : { record : 'a t ; label : RecordLabel.t } -> 'a t
    | ERecord : 'a t RecordLabel.Map.t -> 'a t
    | ENot : 'a t -> 'a t 
    | EPick_i : 'a t (* is parsed as "input", but we can immediately make it pick_i *)
    | EFunction : { param : Ident.t ; body : 'a t } -> 'a t (* note bluejay also has multi-arg function, which generalizes this *)
    | EVariant : { label : VariantLabel.t ; payload : 'a t } -> 'a t
    (* embedded only, so constrain 'a to only be `Embedded *)
    | EPick_b : 'a embedded_only t
    | ECase : { subject : 'a t ; cases : (int * 'a t) list ; default : 'a t } -> 'a embedded_only t (* simply sugar for nested conditionals *)
    | EFreeze : 'a t -> 'a embedded_only t
    | EThaw : 'a t -> 'a embedded_only t 
    | EId : 'a embedded_only t
    | EIgnore : { ignored : 'a t ; cont : 'a t } -> 'a embedded_only t (* simply sugar for `let _ = ignored in cont` but is more efficient *)
    (* these exist in the desugared and embedded languages *)
    | EAbort : string -> 'a desugared_or_embedded t (* string is error message *)
    | EDiverge : 'a desugared_or_embedded t
    (* these exist in the bluejay and desugared languages *)
    | EType : 'a bluejay_or_desugared t
    | ETypeInt : 'a bluejay_or_desugared t
    | ETypeBool : 'a bluejay_or_desugared t
    | ETypeTop : 'a bluejay_or_desugared t
    | ETypeBottom : 'a bluejay_or_desugared t
    | ETypeRecord : 'a t RecordLabel.Map.t -> 'a bluejay_or_desugared t
    | ETypeModule : (RecordLabel.t * 'a t) list -> 'a bluejay_or_desugared t (* is a list because order matters *)
    | ETypeArrow : { domain : 'a t ; codomain : 'a t } -> 'a bluejay_or_desugared t
    | ETypeArrowD : { binding : Ident.t ; domain : 'a t ; codomain : 'a t } -> 'a bluejay_or_desugared t
    | ETypeRefinement : { tau : 'a t ; predicate : 'a t } -> 'a bluejay_or_desugared t
    | ETypeMu : { var : Ident.t ; body : 'a t } -> 'a bluejay_or_desugared t
    | ETypeVariant : (VariantTypeLabel.t * 'a t) list -> 'a bluejay_or_desugared t
    | ELetTyped : { typed_var : 'a typed_var ; body : 'a t ; cont : 'a t ; do_wrap : bool ; do_check : bool } -> 'a bluejay_or_desugared t
    | ETypeSingle : 'a t -> 'a bluejay_or_desugared t
    (* bluejay only *)
    | ETypeList : 'a t -> 'a bluejay_only t
    | ETypeIntersect : (VariantTypeLabel.t * 'a t * 'a t) list -> 'a bluejay_only t
    | EList : 'a t list -> 'a bluejay_only t
    | EListCons : 'a t * 'a t -> 'a bluejay_only t
    | EModule : 'a statement list -> 'a bluejay_only t
    | EAssert : 'a t -> 'a bluejay_only t
    | EAssume : 'a t -> 'a bluejay_only t
    | EMultiArgFunction : { params : Ident.t list ; body : 'a t } -> 'a bluejay_only t
    | ELetFun : { func : 'a funsig ; cont : 'a t } -> 'a bluejay_only t
    | ELetFunRec : { funcs : 'a funsig list ; cont : 'a t } -> 'a bluejay_only t

  (* the let-function signatures *)
  and _ funsig =
    | FUntyped : { func_id : Ident.t ; params : Ident.t list ; body : 'a t } -> 'a funsig
    | FTyped : ('a, 'a param list) typed_fun -> 'a funsig

  (* the common parts of typed let-function signature. Note type_vars is empty for non polymorphic functions *)
  and ('a, 'p) typed_fun = { type_vars : Ident.t list ; func_id : Ident.t ; params : 'p ; ret_type : 'a t ; body : 'a t }

  (* a variable with its type, where the type is an expression *)
  and 'a typed_var = { var : Ident.t ; tau : 'a t }

  (* function parameters for use in funsigs *)
  and _ param =
    | TVar : 'a typed_var -> 'a bluejay_only param
    | TVarDep : 'a typed_var -> 'a bluejay_only param

  (* Bluejay and desugared have typed let-expressions *)
  and 'a let_typed = { typed_var : 'a typed_var ; body : 'a t ; cont : 'a t } constraint 'a = 'a bluejay_or_desugared

  and _ statement =
    (* all *)
    | SUntyped : { var : Ident.t ; body : 'a t } -> 'a statement
    (* bluejay or desugared *)
    | STyped : { typed_var : 'a typed_var ; body : 'a t ; do_wrap : bool ; do_check : bool } -> 'a bluejay_or_desugared statement
    (* bluejay only *)
    | SFun : 'a funsig -> 'a bluejay_only statement
    | SFunRec : 'a funsig list -> 'a bluejay_only statement
end

module Program = struct
  open Expr

  type 'a t = 'a statement list
end

module Embedded = struct
  type t = embedded Expr.t
  type pgm = embedded Program.t
  type pattern = embedded Pattern.t
  type statement = embedded Expr.statement
end

module Desugared = struct
  type t = desugared Expr.t
  type pgm = desugared Program.t
  type pattern = desugared Pattern.t
  type statement = desugared Expr.statement
end

module Bluejay = struct
  type t = bluejay Expr.t
  type pgm = bluejay Program.t
  type pattern = bluejay Pattern.t
  type funsig = bluejay Expr.funsig
  type typed_var = bluejay Expr.typed_var
  type param = bluejay Expr.param
  type statement = bluejay Expr.statement
end

module Parsing_tools = struct
  let empty_record = RecordLabel.Map.empty

  let new_record = RecordLabel.Map.singleton

  let add_record_entry k value old_record =
    match Map.add old_record ~key:k ~data:value with
    | `Duplicate -> failwith "Parse error: duplicate record label"
    | `Ok m -> m

  let record_of_list ls =
    List.fold ls ~init:empty_record ~f:(fun acc (k, v) -> add_record_entry k v acc)
end
