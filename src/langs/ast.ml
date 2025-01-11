
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

  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

module RecordLabel = struct
  type t = RecordLabel of Ident.t
    [@@unboxed] [@@deriving equal, compare, sexp, hash]
end

module VariantLabel = struct
  type t = VariantLabel of Ident.t
    [@@unboxed] [@@deriving equal, compare, sexp, hash]
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
end

module Pattern = struct
  type _ t =
    (* all languages *)
    | PFun : 'a t
    | PInt : 'a t
    | PBool : 'a t
    | PRecord : Ident.Set.t -> 'a t
    | PStrictReord : Ident.Set.t -> 'a t
    | PAny : 'a t
    (* only Bluejay *)
    | PVariant : VariantLabel.t -> 'a bluejay_only t
    | PVariable : Ident.t -> 'a bluejay_only t
    | PEmptyList : 'a bluejay_only t
    | PDestructList : 'a bluejay_only t
end

(* Length-encoded lists, originally used for type safety of function arguments *)
(* module L = struct
  module Peano = struct
    type zero = private Zero
    type 'a succ = private Succ

    type _ nat =
      | Zero : zero nat
      | Succ : 'a nat -> 'a succ nat
  end

  open Peano

  type _ t =
    | [] : < elt : 'a ; len : zero > t
    | ( :: ) : 'a * < elt : 'a ; len : 'b > t -> < elt : 'a ; len : 'b succ > t

  type ('a, 'b) nonempty = < elt : 'a ; len : 'b succ > t
  type 'a singleton = < elt : 'a ; len : zero succ > t
end *)

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
    | ERecord : 'a t Ident.Map.t -> 'a t
    | ENot : 'a t -> 'a t 
    | EPick_i : 'a t (* is parsed as "input", but we can immediately make it pick_i *)
    | EFunction : { param : Ident.t ; body : 'a t } -> 'a t (* note bluejay also has multi-arg function, which generalizes this *)
    (* embedded only, so constrain 'a to only be `Embedded *)
    | EPick_b : 'a embedded_only t
    | ECase : { subject : 'a t ; cases : (int * 'a t) list } -> 'a embedded_only t
    | EFreeze : 'a t -> 'a embedded_only t
    | EThaw : 'a t -> 'a embedded_only t 
    (* these exist in the desugared and embedded languages (these are the only spots that "desugaring" introduces new language) *)
    | EAbort : 'a desugared_or_embedded t
    | EDiverge : 'a desugared_or_embedded t
    (* desugared only *)
    | EKind : 'a desugared_only t
    (* these exist in the bluejay and desugared languages *)
    | ETypeInt : 'a bluejay_or_desugared t
    | ETypeBool : 'a bluejay_or_desugared t
    | ETypeRecord : 'a t Ident.Map.t -> 'a bluejay_or_desugared t
    | ETypeList : 'a t -> 'a bluejay_or_desugared t
    | ETypeArrow : { domain : 'a t ; codomain : 'a t } -> 'a bluejay_or_desugared t
    | ETypeArrowD : { binding : Ident.t ; domain : 'a t ; codomain : 'a t } -> 'a bluejay_or_desugared t
    | ETypeRefinment : { tau : 'a t ; predicate : 'a t } -> 'a bluejay_or_desugared t
    | ETypeIntersect : 'a t * 'a t -> 'a bluejay_or_desugared t (* TODO: maybe make this more like variant type *)
    | ETypeMu : { var : Ident.t ; body : 'a t } -> 'a bluejay_or_desugared t
    | ETypeVariant : (VariantLabel.t * 'a t) list -> 'a bluejay_or_desugared t
    | ELetTyped : { typed_var : 'a typed_var ; body : 'a t ; cont : 'a t } -> 'a bluejay_or_desugared t
    (* bluejay only *)
    | EVariant : { label : VariantLabel.t ; payload : 'a t } -> 'a bluejay_only t
    | EList : 'a t list -> 'a bluejay_only t
    | EListCons : 'a t * 'a t -> 'a bluejay_only t
    | EAssert : 'a t -> 'a bluejay_only t
    | EAssume : 'a t -> 'a bluejay_only t
    | EMultiArgFunction : { params : Ident.t list ; body : 'a t } -> 'a bluejay_only t
    | ETypeForall : { type_variables : Ident.t list ; tau : 'a t } -> 'a bluejay_only t
    | ELetFun : { func : 'a funsig ; cont : 'a t } -> 'a bluejay_only t
    | ELetFunRec : { funcs : 'a funsig list ; cont : 'a t } -> 'a bluejay_only t

  (* the let-function signatures *)
  and _ funsig =
    | FUntyped : { func_id : Ident.t ; params : Ident.t list ; body : 'a t } -> 'a funsig
    | FTyped : ('a, 'a typed_var list) typed_fun -> 'a funsig
    | FPolyTyped : { func : ('a, 'a typed_var list) typed_fun ; type_vars : Ident.t list } -> 'a funsig
    | FDepTyped : ('a, 'a typed_var) typed_fun -> 'a funsig
    | FPolyDepTyped : { func : ('a, 'a typed_var) typed_fun ; type_vars : Ident.t list } -> 'a funsig

  (* a variable with its type, where the type is an expression *)
  and 'a typed_var = { var : Ident.t ; tau : 'a t }

  (* the common parts of typed let-function signature *)
  and ('a, 'p) typed_fun = { func_id : Ident.t ; params : 'p ; ret_type : 'a t ; body : 'a t }

  (* archive the old length-encoded stuff *)
  (* and ('a, 'l) typed_fun = { func_id : Ident.t ; params : 'l ; ret_type : 'a t ; body : 'a t }
    constraint 'l = < elt : 'a typed_var ; len : _ > L.t *)
    (* | FTyped : ('a, ('a typed_var, _) L.nonempty) typed_fun -> 'a funsig
    | FPolyTyped : { func : ('a, ('a typed_var, _) L.nonempty) typed_fun ; type_vars : Ident.t list } -> 'a funsig
    | FDepTyped : ('a, ('a typed_var) L.singleton) typed_fun -> 'a funsig
    | FPolyDepTyped : { func : ('a, ('a typed_var) L.singleton) typed_fun ; type_vars : Ident.t list } -> 'a funsig *)
end

(*
  -------------
  SANITY CHECKS
  -------------

  We'll create a type for each language and match on an expression in that language
  to make sure that the OCaml typechecker agrees we have done it properly.
*)

module Embedded = struct
  type t = [ `Embedded ] Expr.t

  let f (e : t) : t =
    match e with
    | EInt _ 
    | EBool _ 
    | EVar _ 
    | EBinop _
    | EIf _
    | ELet _
    | EFunction _
    | EAppl _
    | EMatch _
    | EProject _
    | ERecord _
    | ENot _
    | EPick_i
    | EPick_b
    | ECase _
    | EFreeze _
    | EThaw _
    | EAbort
    | EDiverge -> e
end

module Desugared = struct
  type t = [ `Desugared ] Expr.t

  let f (e : t) : t =
    match e with
    | EInt _ 
    | EBool _ 
    | EVar _ 
    | EBinop _
    | EIf _
    | ELet _
    | EFunction _
    | EAppl _
    | EMatch _
    | EProject _
    | ERecord _
    | ENot _
    | EAbort
    | EDiverge
    | ELetTyped _
    | ETypeInt
    | ETypeBool 
    | ETypeRecord _
    | ETypeList _
    | ETypeArrow _
    | ETypeArrowD _
    | ETypeRefinment _
    | ETypeIntersect _
    | ETypeMu _
    | EPick_i
    | ETypeVariant _
    | EKind -> e
end

module Bluejay = struct
  type t = [ `Bluejay ] Expr.t

  let f (e : t) : t =
    match e with
    | EInt _ 
    | EBool _ 
    | EVar _ 
    | EBinop _
    | EIf _
    | ELet _
    | EAppl _
    | EMatch _
    | EProject _
    | ERecord _
    | ENot _
    | EFunction _
    | ETypeInt
    | ETypeBool 
    | ETypeRecord _
    | ETypeList _
    | ETypeArrow _
    | ETypeArrowD _
    | ETypeRefinment _
    | ETypeIntersect _
    | ETypeMu _
    | ETypeVariant _ 
    | EMultiArgFunction _
    | ETypeForall _
    | ELetTyped _
    | ELetFun _
    | ELetFunRec _
    | EList _
    | EListCons _
    | EVariant _
    | EAssert _
    | EPick_i
    | EAssume _ -> e

  let (e : t) =
    let open Expr in
    let fsig = 
      FPolyDepTyped
        { func =
          { func_id = (Ident "f")
          ; params = { var = Ident "x" ; tau = ETypeInt }
          ; ret_type = ETypeInt
          ; body = EVar (Ident "x") }
        ; type_vars = [ Ident "a"] }
    in
    ELetFun { func = fsig ; cont = EVar (Ident "f") }

  (* let rec of_old_bluejay (e : Bluejay.Bluejay_ast.expr) : t =
    let open Expr in
    match e with
    | Int i -> EInt i
    | LetFunWithType (fsig, cont) -> begin
      match fsig with
      | Typed_funsig (Ident f_id, arg_type_list, (body, ret_type)) ->
        ELetFun
          { func = FTyped
            { func_id = Ident f_id
            ; body = of_old_bluejay body.body
            ; ret_type = of_old_bluejay ret_type.body
            ; params =
              let rec loop (ls : (Bluejay.Bluejay_ast.ident * Bluejay.Bluejay_ast.expr_desc) list) =
                (* let open List in *)
                match ls with
                | [] -> []
                | (Bluejay.Bluejay_ast.Ident id, type_) :: tl ->
                  let open L in
                  L.({var = Ident id ; tau = of_old_bluejay type_.body} :: loop tl)
              in
              loop arg_type_list
            }
          ; cont = of_old_bluejay cont.body }

      | _ -> failwith "unimplemented"

    end
    | _ -> failwith "unimplemented" *)
end

module Parsing_tools = struct
  let empty_record = Ident.Map.empty

  let new_record (RecordLabel.RecordLabel k) value =
    Ident.Map.singleton k value

  let add_record_entry (RecordLabel.RecordLabel k) value old_record =
    match Map.add old_record ~key:k ~data:value with
    | `Duplicate -> failwith "Parse error: duplicate record label"
    | `Ok m -> m
end
