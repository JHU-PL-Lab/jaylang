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
  [@@deriving equal, compare]

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

exception InvalidComparison of string

module Pattern = struct
  type _ t =
    (* all languages *)
    | PAny : 'a t
    | PVariable : Ident.t -> 'a t
    | PVariant : { variant_label : VariantLabel.t ; payload_id : Ident.t } -> 'a t
    (* only Bluejay *)
    | PEmptyList : 'a bluejay_only t
    | PDestructList : { hd_id : Ident.t ; tl_id : Ident.t } -> 'a bluejay_only t

  let to_rank : type a. a t -> int = function
    | PAny -> 0
    | PVariable _ -> 1
    | PVariant _ -> 2
    | PEmptyList -> 3
    | PDestructList _ -> 4

  let compare : type a. a t -> a t -> int =
    fun a b ->
      match Int.compare (to_rank a) (to_rank b) with
      | 0 -> begin
        match a, b with
        | PAny, PAny -> 0
        | PVariable id1, PVariable id2 -> Ident.compare id1 id2
        | PVariant r1, PVariant r2 ->
          Tuple2.compare ~cmp1:VariantLabel.compare ~cmp2:Ident.compare
            (r1.variant_label, r1.payload_id) (r2.variant_label, r2.payload_id)
        | PEmptyList, PEmptyList -> 0
        | PDestructList r1, PDestructList r2 ->
          Tuple2.compare ~cmp1:Ident.compare ~cmp2:Ident.compare
            (r1.hd_id, r1.tl_id) (r2.hd_id, r2.tl_id)
        | _ -> raise @@ InvalidComparison "Impossible comparison of patterns"
      end
      | x -> x

  let to_string : type a. a t -> string = function
    | PAny -> "any"
    | PVariable Ident s -> Format.sprintf "var %s" s
    | PVariant { variant_label  = VariantLabel Ident s ; _ } -> Format.sprintf "variant `%s" s
    | PEmptyList -> "[]"
    | PDestructList { hd_id = Ident hd ; tl_id = Ident tl } -> Format.sprintf "%s :: %s" hd tl
end

module Callsite = struct
  type t = Callsite of int [@@unboxed] [@@deriving compare, equal, sexp]

  let next =
    let counter = Utils.Counter.create () in
    fun () ->
      Callsite (Utils.Counter.next counter)
end

module Expr = struct
  module Make (ApplCell : Utils.Comparable.S1) = struct
    type _ t =
      (* all languages. 'a is unconstrained *)
      | EInt : int -> 'a t
      | EBool : bool -> 'a t
      | EVar : Ident.t -> 'a t
      | EBinop : { left : 'a t ; binop : Binop.t ; right : 'a t } -> 'a t
      | EIf : { cond : 'a t ; true_body : 'a t ; false_body : 'a t } -> 'a t
      | ELet : { var : Ident.t ; body : 'a t ; cont : 'a t } -> 'a t
      | EAppl : 'a application ApplCell.t -> 'a t
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
      | EThaw : 'a t ApplCell.t -> 'a embedded_only t 
      | EId : 'a embedded_only t
      | EIgnore : { ignored : 'a t ; cont : 'a t } -> 'a embedded_only t (* simply sugar for `let _ = ignored in cont` but is more efficient *)
      | ETable : 'a embedded_only t
      | ETblAppl : { tbl : 'a t ; gen : 'a t ; arg : 'a t } -> 'a embedded_only t
      | EDet : 'a t -> 'a embedded_only t
      | EEscapeDet : 'a t -> 'a embedded_only t
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
      | ETypeFun : { domain : 'a t ; codomain : 'a t ; dep : [ `No | `Binding of Ident.t ] ; det : bool } -> 'a bluejay_or_desugared t
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

    and 'a application = { func : 'a t ; arg : 'a t }

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

    (* Completely arbitrary rank. PPX libs can't do this automatically on a GADT. *)
    let to_rank : type a. a t -> int = function
      | EInt _ -> 0            | EBool _ -> 1      | EVar _ -> 2               | EBinop _ -> 3
      | EIf _ -> 4             | ELet _ -> 5       | EAppl _ -> 6              | EMatch _ -> 7
      | EProject _ -> 8        | ERecord _ -> 9    | ENot _ -> 10              | EPick_i -> 11
      | EFunction _ -> 12      | EVariant _ -> 13  | EPick_b -> 14             | ECase _ -> 15
      | EFreeze _ -> 16        | EThaw _ -> 17     | EId -> 18                 | EIgnore _ -> 19
      | ETable -> 20           | ETblAppl _ -> 21  | EDet _ -> 22              | EEscapeDet _ -> 23
      | EAbort _ -> 24         | EDiverge -> 25    | EType -> 26               | ETypeInt -> 27
      | ETypeBool -> 28        | ETypeTop -> 29    | ETypeBottom -> 30         | ETypeRecord _ -> 31
      | ETypeModule _ -> 32    | ETypeFun _ -> 33  | ETypeRefinement _ -> 34   | ETypeMu _ -> 35
      | ETypeVariant _ -> 36   | ELetTyped _ -> 37 | ETypeSingle _ -> 38       | ETypeList _ -> 39
      | ETypeIntersect _ -> 40 | EList _ -> 41     | EListCons _ -> 42         | EModule _ -> 43 
      | EAssert _ -> 44        | EAssume _ -> 45   | EMultiArgFunction _ -> 46 | ELetFun _ -> 47
      | ELetFunRec _ -> 48

    let statement_to_rank : type a. a statement -> int = function
      | SUntyped _ -> 0
      | STyped _ -> 1
      | SFun _ -> 2
      | SFunRec _ -> 3

    (* This is super tedious but needs to be done because no ppx can do it for GADTs *)
    let rec compare : type a. a t -> a t -> int =
      fun a b ->
        match Int.compare (to_rank a) (to_rank b) with
        | 0 -> begin
          match a, b with
          | EPick_i, EPick_i | EPick_b, EPick_b | EId, EId | ETable, ETable
          | EDiverge, EDiverge | EType, EType | ETypeInt, ETypeInt | ETypeBool, ETypeBool
          | ETypeTop, ETypeTop | ETypeBottom, ETypeBottom -> 0
          | EInt i, EInt j -> Int.compare i j
          | EBool b, EBool c -> Bool.compare b c
          | EVar x, EVar y -> Ident.compare x y
          | EBinop r1, EBinop r2 ->
            Tuple3.compare ~cmp1:compare ~cmp2:Binop.compare ~cmp3:compare
              (r1.left, r1.binop, r1.right)
              (r2.left, r2.binop, r2.right)
          | EIf r1, EIf r2 ->
            compare3 (r1.cond, r1.true_body, r2.false_body) (r2.cond, r2.true_body, r2.false_body)
          | ELet r1, ELet r2 ->
            Tuple3.compare ~cmp1:Ident.compare ~cmp2:compare ~cmp3:compare
              (r1.var, r1.body, r1.cont)
              (r2.var, r2.body, r2.cont)
          | EAppl c1, EAppl c2 -> ApplCell.compare compare_application c1 c2
          | EMatch r1, EMatch r2 ->
            Tuple2.compare ~cmp1:compare ~cmp2:(List.compare (Tuple2.compare ~cmp1:Pattern.compare ~cmp2:compare))
              (r1.subject, r1.patterns) (r2.subject, r2.patterns)
          | EProject r1, EProject r2 ->
            Tuple2.compare ~cmp1:compare ~cmp2:RecordLabel.compare
              (r1.record, r1.label) (r2.record, r2.label)
          | ERecord m1, ERecord m2 -> RecordLabel.Map.compare compare m1 m2
          | ENot e1, ENot e2 -> compare e1 e2
          | EFunction r1, EFunction r2 ->
            Tuple2.compare ~cmp1:Ident.compare ~cmp2:compare
              (r1.param, r1.body) (r2.param, r2.body)
          | EVariant r1, EVariant r2 ->
            Tuple2.compare ~cmp1:VariantLabel.compare ~cmp2:compare
              (r1.label, r1.payload) (r2.label, r2.payload)
          | ECase r1, ECase r2 ->
            Tuple3.compare ~cmp1:compare ~cmp2:(List.compare (Tuple2.compare ~cmp1:Int.compare ~cmp2:compare)) ~cmp3:compare
              (r1.subject, r1.cases, r1.default) (r2.subject, r2.cases, r2.default)
          | EFreeze e1, EFreeze e2 -> compare e1 e2
          | EThaw a1, EThaw a2 -> ApplCell.compare compare a1 a2
          | EIgnore r1, EIgnore r2 -> compare2 (r1.ignored, r1.cont) (r2.ignored, r2.cont)
          | ETblAppl r1, ETblAppl r2 -> compare3 (r1.tbl, r1.gen, r1.arg) (r2.tbl, r2.gen, r2.arg)
          | EDet e1, EDet e2 -> compare e1 e2
          | EEscapeDet e1, EEscapeDet e2 -> compare e1 e2
          | EAbort s1, EAbort s2 -> String.compare s1 s2
          | ETypeRecord m1, ETypeRecord m2 -> RecordLabel.Map.compare compare m1 m2
          | ETypeFun r1, ETypeFun r2 -> begin
            match compare2 (r1.domain, r1.codomain) (r2.domain, r2.codomain) with
            | 0 -> Poly.compare (r1.dep, r1.det) (r2.dep, r2.det)
            | x -> x
          end
          | ETypeRefinement r1, ETypeRefinement r2 -> compare2 (r1.tau, r1.predicate) (r2.tau, r2.predicate)
          | ETypeMu r1, ETypeMu r2 ->
            Tuple2.compare ~cmp1:Ident.compare ~cmp2:compare
              (r1.var, r1.body) (r2.var, r2.body)
          | ETypeVariant l1, ETypeVariant l2 ->
            List.compare (Tuple2.compare ~cmp1:VariantTypeLabel.compare ~cmp2:compare) l1 l2
          | ELetTyped r1, ELetTyped r2 -> begin
            let res = 
              Tuple3.compare ~cmp1:Ident.compare ~cmp2:Bool.compare ~cmp3:Bool.compare
                (r1.typed_var.var, r1.do_wrap, r2.do_check) (r2.typed_var.var, r2.do_wrap, r2.do_check)
            in
            match res with
            | 0 -> compare3 (r1.typed_var.tau, r1.body, r1.cont) (r2.typed_var.tau, r2.body, r2.cont)
            | x -> x
          end
          | ETypeSingle e1, ETypeSingle e2 -> compare e1 e2
          | ETypeList e1, ETypeList e2 -> compare e1 e2
          | ETypeIntersect l1, ETypeIntersect l2 ->
            List.compare (Tuple3.compare ~cmp1:VariantTypeLabel.compare ~cmp2:compare ~cmp3:compare) l1 l2
          | EList l1, EList l2 -> List.compare compare l1 l2
          | EListCons (hd1, tl1), EListCons (hd2, tl2) -> compare2 (hd1, tl1) (hd2, tl2)
          | EModule l1, EModule l2 -> List.compare compare_statement l1 l2
          | EAssert e1, EAssert e2 -> compare e1 e2
          | EAssume e1, EAssume e2 -> compare e1 e2
          | EMultiArgFunction r1, EMultiArgFunction r2 ->
            Tuple2.compare ~cmp1:(List.compare Ident.compare) ~cmp2:compare
              (r1.params, r1.body) (r2.params, r2.body)
          | ELetFun r1, ELetFun r2 ->
            Tuple2.compare ~cmp1:compare_funsig ~cmp2:compare
              (r1.func, r1.cont) (r2.func, r2.cont)
          | ELetFunRec r1, ELetFunRec r2 ->
            Tuple2.compare ~cmp1:(List.compare compare_funsig) ~cmp2:compare
              (r1.funcs, r1.cont) (r2.funcs, r2.cont)
          | _ -> raise @@ InvalidComparison "Impossible comparison of expressions"
        end
        | x -> x

    and compare3 : type a. (a t * a t * a t) -> (a t * a t * a t) -> int = 
      fun t1 t2 ->
        Tuple3.compare ~cmp1:compare ~cmp2:compare ~cmp3:compare t1 t2

    and compare2 : type a. (a t * a t) -> (a t * a t) -> int = 
      fun t1 t2 ->
        Tuple2.compare ~cmp1:compare ~cmp2:compare t1 t2

    and compare_application : type a. a application -> a application -> int =
      fun a1 a2 ->
        compare2 (a1.func, a1.arg) (a2.func, a2.arg)

    and compare_statement : type a. a statement -> a statement -> int =
      fun s1 s2 ->
        match Int.compare (statement_to_rank s1) (statement_to_rank s2) with
        | 0 -> begin
          match s1, s2 with
          | SUntyped r1, SUntyped r2 ->
            Tuple2.compare ~cmp1:Ident.compare ~cmp2:compare
              (r1.var, r1.body) (r2.var, r2.body)
          | STyped r1, STyped r2 -> begin
            let res = 
              Tuple3.compare ~cmp1:Ident.compare ~cmp2:Bool.compare ~cmp3:Bool.compare
                (r1.typed_var.var, r1.do_wrap, r2.do_check) (r2.typed_var.var, r2.do_wrap, r2.do_check)
            in
            match res with
            | 0 -> compare2 (r1.typed_var.tau, r1.body) (r2.typed_var.tau, r2.body)
            | x -> x
          end
          | SFun fs1, SFun fs2 -> compare_funsig fs1 fs2
          | SFunRec l1, SFunRec l2 -> List.compare compare_funsig l1 l2
          | _ -> raise @@ InvalidComparison "Impossible comparison of statements"
        end
        | x -> x

    and compare_funsig : type a. a funsig -> a funsig -> int =
      fun fs1 fs2 ->
        match fs1, fs2 with
        | FUntyped r1, FUntyped r2 ->
          Tuple3.compare ~cmp1:Ident.compare ~cmp2:(List.compare Ident.compare) ~cmp3:compare
            (r1.func_id, r1.params, r1.body) (r2.func_id, r2.params, r2.body)
        | FTyped r1, FTyped r2 -> 
          Tuple3.compare ~cmp1:(List.compare Ident.compare) ~cmp2:(List.compare compare_param) ~cmp3:compare2
            (r1.func_id :: r1.type_vars, r1.params, (r1.ret_type, r1.body))
            (r2.func_id :: r2.type_vars, r2.params, (r2.ret_type, r2.body))
        | FUntyped _, FTyped _ -> -1
        | FTyped _, FUntyped _ -> 1

    and compare_param : type a. a param -> a param -> int =
      fun p1 p2 ->
        match p1, p2 with
        | TVar tv1, TVar tv2
        | TVarDep tv1, TVarDep tv2 ->
          Tuple2.compare ~cmp1:Ident.compare ~cmp2:compare
            (tv1.var, tv1.tau) (tv2.var, tv2.tau)
        | TVar _, TVarDep _ -> -1
        | TVarDep _, TVar _ -> 1
  end

  module Made = Make (Utils.Identity)
  include Made
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

  module With_callsites = struct
    module E = struct
      module ApplData = struct type 'a t = { appl : 'a ; callsite : Callsite.t }[@@deriving compare] end
      module T = Expr.Make (ApplData)
      type t = embedded T.t
    end

  (*
    The idea is that we rename each variable to something brand new and then
    substitute that into each subexpression.
    
    Do this with a map that maps each name to the new name. When we come across
    a usage, we replace. When we come across a declaration, we give a new name
    in the map (and replace that instance, obviously).

    Ignores unbound variables. They get a new name, too.

    I had a little bit of fun with monad transformers and state/reader to write
    this, but it was too impractical given that OCaml has effects.

    The following assumption is not sound, we think.
    Assumption: can partially evaluate `thaw (freeze e)` to `e` because the program
      analysis is effectively deterministic. In fact, we can just remove thaw and freeze.
      However, we need to be careful with the fixed point combinator because full removal
      in all spots would cause that to diverge. But does that matter in program analysis?
      Probably not.
      For now, just peval, but see about removing fully.
  *)
  let alphatized_of_expr (e : t) : E.t =
    (* order does not matter when alphatizing because we don't care about names, so mindless mutation is okay *)
    let new_name = 
      let i = ref 0 in
      fun () ->
        incr i;
        Ident.Ident (Format.sprintf "$%d" !i)
    in
    let replace key env =
      if Ident.equal key (Ident "_")
      then key, env (* no need to replace ignored values, I think. TODO: check this logic *)
      else
        let id' = new_name () in
        id', Map.set env ~key ~data:id'
    in
    let rec visit (e : t) (env : Ident.t Ident.Map.t) : E.t =
      match e with
      (* leaves -- these need to be recreated because we don't share constructors *)
      | EInt i -> EInt i
      | EBool b -> EBool b
      | EPick_i -> EPick_i
      | EPick_b -> EPick_b
      | EId -> EId
      | ETable -> ETable
      | EAbort msg -> EAbort msg
      | EDiverge -> EDiverge
      (* replacement *)
      | EVar id -> begin
        match Map.find env id with
        | Some id' -> EVar id'
        | None -> EVar (new_name ()) (* unbound variable. We promise to not fail but give a new name instead *)
      end
      (* binding a new name *)
      | ELet { var ; body ; cont } ->
        let id', env' = replace var env in
        ELet { var = id' ; body = visit body env ; cont = visit cont env' }
      | EFunction { param ; body } ->
        let id', env' = replace param env in
        EFunction { param = id' ; body = visit body env' }
      (* naming the call site *)
      | EAppl { func ; arg } -> EAppl { appl = { func = visit func env ; arg = visit arg env } ; callsite = Callsite.next () }
      (* propagation *)
      | EBinop { left ; binop ; right } -> EBinop { left = visit left env ; binop ; right = visit right env }
      | EIf { cond ; true_body ; false_body } -> EIf { cond = visit cond env ; true_body = visit true_body env ; false_body = visit false_body env }
      | EMatch { subject ; patterns } ->
        EMatch { subject = visit subject env ; patterns =
          List.map patterns ~f:(fun (pat, expr) ->
            match pat with
            | Pattern.PAny -> pat, visit expr env
            | PVariable x ->
              let id', env' = replace x env in
              PVariable id', visit expr env'
            | PVariant { variant_label ; payload_id } ->
              let id', env' = replace payload_id env in
              PVariant { variant_label ; payload_id = id' }, visit expr env'
          )
        }
      | EProject { record ; label } -> EProject { record = visit record env ; label }
      | ERecord r -> ERecord (Map.map r ~f:(fun body -> visit body env))
      | ENot expr -> ENot (visit expr env)
      | EVariant { label ; payload } -> EVariant { label ; payload = visit payload env }
      | ECase { subject ; cases ; default } ->
        ECase { subject = visit subject env ; default = visit default env ; cases =
          List.map cases ~f:(fun (i, expr) -> i, visit expr env)
        }
      | EFreeze expr -> EFreeze (visit expr env)
      | EThaw expr -> EThaw { appl = (visit expr env) ; callsite = Callsite.next () }
      | EIgnore { ignored ; cont } -> EIgnore { ignored = visit ignored env ; cont = visit cont env }
      | ETblAppl { tbl ; gen ; arg } -> ETblAppl { tbl = visit tbl env ; gen = visit gen env ; arg = visit arg env }
      | EDet expr -> EDet (visit expr env)
      | EEscapeDet expr -> EEscapeDet (visit expr env)
    in
    visit e Ident.Map.empty

    include E
  end
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
