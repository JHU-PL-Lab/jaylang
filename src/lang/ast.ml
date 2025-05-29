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

  let cmp : type a. a t -> a t -> [ `LT | `GT | `Eq of (Ident.t * Ident.t) list ] =
    fun a b ->
      match Int.compare (to_rank a) (to_rank b) with
      | 0 -> begin
        match a, b with
        | PAny, PAny -> `Eq []
        | PVariable id1, PVariable id2 -> `Eq [ id1, id2 ]
        | PVariant r1, PVariant r2 -> begin
          match VariantLabel.compare r1.variant_label r2.variant_label with
          | 0 -> `Eq [ r1.payload_id, r2.payload_id ]
          | x when x < 0 -> `LT
          | _ -> `GT
        end
        | PEmptyList, PEmptyList -> `Eq []
        | PDestructList r1, PDestructList r2 -> `Eq [ (r1.hd_id, r2.hd_id) ; (r1.tl_id, r2.tl_id) ]
        | _ -> raise @@ InvalidComparison "Impossible comparison of patterns"
      end
      | x when x < 0 -> `LT
      | _ -> `GT

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
      | ELet : { var : Ident.t ; defn : 'a t ; body : 'a t } -> 'a t
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
      | EIgnore : { ignored : 'a t ; body : 'a t } -> 'a embedded_only t (* simply sugar for `let _ = ignored in body` but is more efficient *)
      | ETable : 'a embedded_only t
      | ETblAppl : { tbl : 'a t ; gen : 'a t ; arg : 'a t } -> 'a embedded_only t
      | EDet : 'a t -> 'a embedded_only t
      | EEscapeDet : 'a t -> 'a embedded_only t
      (* these exist in the desugared and embedded languages *)
      | EAbort : string -> 'a desugared_or_embedded t (* string is error message *)
      | EDiverge : 'a desugared_or_embedded t
      (* only the desugared language *)
      | EGen : 'a t -> 'a desugared_only t (* Cannot be interpreted. Is only an intermediate step in translation *)
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
      | ELetTyped : { typed_var : 'a typed_var ; defn : 'a t ; body : 'a t ; do_wrap : bool ; do_check : bool } -> 'a bluejay_or_desugared t
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
      | ELetFun : { func : 'a funsig ; body : 'a t } -> 'a bluejay_only t
      | ELetFunRec : { funcs : 'a funsig list ; body : 'a t } -> 'a bluejay_only t

    (* the let-function signatures *)
    and _ funsig =
      | FUntyped : { func_id : Ident.t ; params : Ident.t list ; defn : 'a t } -> 'a funsig
      | FTyped : ('a, 'a param list) typed_fun -> 'a funsig

    (* the common parts of typed let-function signature. Note type_vars is empty for non polymorphic functions *)
    and ('a, 'p) typed_fun = { type_vars : Ident.t list ; func_id : Ident.t ; params : 'p ; ret_type : 'a t ; defn : 'a t }

    (* a variable with its type, where the type is an expression *)
    and 'a typed_var = { var : Ident.t ; tau : 'a t }

    (* function parameters for use in funsigs *)
    and _ param =
      | TVar : 'a typed_var -> 'a bluejay_only param
      | TVarDep : 'a typed_var -> 'a bluejay_only param

    and 'a application = { func : 'a t ; arg : 'a t }

    (* Bluejay and desugared have typed let-expressions *)
    and 'a let_typed = { typed_var : 'a typed_var ; defn : 'a t ; body : 'a t } constraint 'a = 'a bluejay_or_desugared

    and _ statement =
      (* all *)
      | SUntyped : { var : Ident.t ; defn : 'a t } -> 'a statement
      (* bluejay or desugared *)
      | STyped : { typed_var : 'a typed_var ; defn : 'a t ; do_wrap : bool ; do_check : bool } -> 'a bluejay_or_desugared statement
      (* bluejay only *)
      | SFun : 'a funsig -> 'a bluejay_only statement
      | SFunRec : 'a funsig list -> 'a bluejay_only statement

    let func_id_of_funsig = function
      | FUntyped { func_id ; _ }
      | FTyped { func_id ; _ } -> func_id

    let ids_of_statement : type a. a statement -> Ident.t list = function
      | SUntyped { var ; _ } -> [ var ]
      | STyped { typed_var = { var ; _ } ; _ } -> [ var ]
      | SFun fs -> [ func_id_of_funsig fs ]
      | SFunRec fss -> List.map fss ~f:func_id_of_funsig

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
      | ELetFunRec _ -> 48     | EGen _  -> 49

    let statement_to_rank : type a. a statement -> int = function
      | SUntyped _ -> 0
      | STyped _ -> 1
      | SFun _ -> 2
      | SFunRec _ -> 3

    module Alist = struct
      (* association list of identifiers *)
      type t = (Ident.t * Ident.t) list

      let empty : t = []
      
      let cons_assoc id1 id2 t = (id1, id2) :: t

      let concat t1 t2 = t1 @ t2

      let rec compare_in_t id1 id2 t =
        match t with
        | [] -> `Not_found
        | (d1, d2) :: tl ->
          if Ident.equal id1 d1 then
            `Found (Ident.compare id2 d2)
          else if Ident.equal id2 d2 then
            `Found (Ident.compare id1 d1)
          else
            compare_in_t id1 id2 tl

      let cons_assocs ids1 ids2 t =
        match Int.compare (List.length ids1) (List.length ids2) with
        | 0 -> `Bindings (concat (List.zip_exn ids1 ids2) t)
        | x -> `Unequal_lengths x
    end

    (**
      Param [compare_vars] compares free variables.

      Two expressions are intensionally compared.
      It is expected that expressions are typically compared in a closure, so [compare_vars] likely
      refers to some closure to check free variable equality.

      Bound variables are compared by de Bruijn index.

      Lots of boilerplate here, but I don't see a way around it.
    *)
    let compare (type a) (compare_vars : Ident.t -> Ident.t -> int) (x : a t) (y : a t) : int =
      (* [let-] is simple sugar to help us only continue when x is 0 and short-circuit f otherwise *)
      let (let-) x f =
        if x = 0
        then f ()
        else x
      in
      let rec compare : type a. Alist.t -> a t -> a t -> int =
        fun bindings a b ->
          if phys_equal a b then 0 else
          let- () = Int.compare (to_rank a) (to_rank b) in
          let cmp : type a. a t -> a t -> int = fun x y -> compare bindings x y in 
          match a, b with
          | EPick_i, EPick_i | EPick_b, EPick_b | EId, EId | ETable, ETable
          | EDiverge, EDiverge | EType, EType | ETypeInt, ETypeInt | ETypeBool, ETypeBool
          | ETypeTop, ETypeTop | ETypeBottom, ETypeBottom -> 0
          | EInt i, EInt j -> Int.compare i j
          | EBool b, EBool c -> Bool.compare b c
          | EVar x, EVar y -> begin
            match Alist.compare_in_t x y bindings with
            | `Found x -> x (* vars are bound, so was able to compare de Bruijn indices *)
            | `Not_found -> compare_vars x y (* variables are free. Use provided comparison *)
          end
          | EBinop r1, EBinop r2 ->
            let- () = cmp r1.left r2.left in
            let- () = Binop.compare r1.binop r2.binop in
            cmp r1.right r2.right
          | EIf r1, EIf r2 ->
            let- () = cmp r1.cond r2.cond in
            let- () = cmp r1.true_body r2.true_body in
            cmp r1.false_body r2.false_body
          | ELet r1, ELet r2 ->
            let- () = cmp r1.defn r2.defn in
            compare (Alist.cons_assoc r1.var r2.var bindings) r1.body r2.body
          | EAppl c1, EAppl c2 -> ApplCell.compare (compare_application bindings) c1 c2
          | EMatch r1, EMatch r2 -> begin
            let- () = cmp r1.subject r2.subject in
            Tuple2.get1 @@
            compare_lists r1.patterns r2.patterns bindings ~f:(fun (p1, e1) (p2, e2) bindings ->
              match Pattern.cmp p1 p2 with
              | `LT -> `Done (-1)
              | `GT -> `Done 1
              | `Eq bindings' ->
                let r = compare (Alist.concat bindings' bindings) e1 e2 in
                if r = 0 then `Continue_and_overwrite_bindings bindings else `Done r
            )
          end
          | EProject r1, EProject r2 ->
            let- () = RecordLabel.compare r1.label r2.label in
            cmp r1.record r2.record
          | ERecord m1, ERecord m2 -> RecordLabel.Map.compare cmp m1 m2
          | ENot e1, ENot e2 -> cmp e1 e2
          | EFunction r1, EFunction r2 -> compare (Alist.cons_assoc r1.param r2.param bindings) r1.body r2.body
          | EVariant r1, EVariant r2 ->
            let- () = VariantLabel.compare r1.label r2.label in
            cmp r1.payload r2.payload
          | ECase r1, ECase r2 ->
            let- () = cmp r1.subject r2.subject in
            let- () = List.compare (Tuple2.compare ~cmp1:Int.compare ~cmp2:cmp) r1.cases r2.cases in
            cmp r1.default r2.default
          | EFreeze e1, EFreeze e2 -> cmp e1 e2
          | EThaw a1, EThaw a2 -> ApplCell.compare cmp a1 a2
          | EIgnore r1, EIgnore r2 -> 
            let- () = cmp r1.ignored r2.ignored in
            cmp r1.body r2.body
          | ETblAppl r1, ETblAppl r2 -> 
            let- () = cmp r1.tbl r2.tbl in
            let- () = cmp r1.gen r2.gen in
            cmp r1.arg r2.arg
          | EDet e1, EDet e2 -> cmp e1 e2
          | EEscapeDet e1, EEscapeDet e2 -> cmp e1 e2
          | EAbort s1, EAbort s2 -> String.compare s1 s2
          | EGen e1, EGen e2 -> cmp e1 e2
          | ETypeRecord m1, ETypeRecord m2 -> RecordLabel.Map.compare cmp m1 m2
          | ETypeFun r1, ETypeFun r2 -> begin
            let- () = cmp r1.domain r2.domain in
            let- () = Bool.compare r1.det r2.det in
            match r1.dep, r2.dep with
            | `Binding id1, `Binding id2 ->
              compare (Alist.cons_assoc id1 id2 bindings) r1.codomain r2.codomain
            | `No, `No -> cmp r1.codomain r2.codomain
            | `No, `Binding _ -> -1
            | `Binding _, `No -> 1
          end
          | ETypeRefinement r1, ETypeRefinement r2 -> 
            let- () = cmp r1.tau r2.tau in
            cmp r1.predicate r2.predicate
          | ETypeMu r1, ETypeMu r2 -> compare (Alist.cons_assoc r1.var r2.var bindings) r1.body r2.body
          | ETypeVariant l1, ETypeVariant l2 ->
            List.compare (Tuple2.compare ~cmp1:VariantTypeLabel.compare ~cmp2:cmp) l1 l2
          | ELetTyped r1, ELetTyped r2 -> begin
            let- () = Bool.compare r1.do_wrap r2.do_wrap in
            let- () = Bool.compare r1.do_check r2.do_check in
            let- () = cmp r1.typed_var.tau r2.typed_var.tau in
            let- () = cmp r1.defn r2.defn in
            compare (Alist.cons_assoc r1.typed_var.var r2.typed_var.var bindings) r1.body r2.body
          end
          | ETypeSingle e1, ETypeSingle e2 -> cmp e1 e2
          | ETypeList e1, ETypeList e2 -> cmp e1 e2
          | ETypeIntersect l1, ETypeIntersect l2 ->
            List.compare (Tuple3.compare ~cmp1:VariantTypeLabel.compare ~cmp2:cmp ~cmp3:cmp) l1 l2
          | EList l1, EList l2 -> List.compare cmp l1 l2
          | EListCons (hd1, tl1), EListCons (hd2, tl2) -> 
            let- () = cmp hd1 hd2 in cmp tl1 tl2
          | EModule l1, EModule l2 -> 
            Tuple2.get1 @@
            compare_lists l1 l2 bindings ~f:(fun s1 s2 bindings ->
              match compare_statement bindings s1 s2 with
              | 0 -> begin
                match Alist.cons_assocs (ids_of_statement s1) (ids_of_statement s2) bindings with
                | `Bindings bindings -> `Continue_and_overwrite_bindings bindings
                | `Unequal_lengths x -> `Done x
              end
              | x -> `Done x
            )
          | EAssert e1, EAssert e2 -> cmp e1 e2
          | EAssume e1, EAssume e2 -> cmp e1 e2
          | EMultiArgFunction r1, EMultiArgFunction r2 -> begin
            match Alist.cons_assocs r1.params r2.params bindings with
            | `Bindings bindings -> compare bindings r1.body r2.body
            | `Unequal_lengths x -> x
          end
          | ELetFun r1, ELetFun r2 ->
            let- () = compare_funsig bindings r1.func r2.func in
            compare (Alist.cons_assoc (func_id_of_funsig r1.func) (func_id_of_funsig r2.func) bindings) 
              r1.body r2.body
          | ELetFunRec r1, ELetFunRec r2 -> begin
            match Alist.cons_assocs (List.map r1.funcs ~f:func_id_of_funsig) (List.map r2.funcs ~f:func_id_of_funsig) bindings with
            | `Bindings bindings ->
              let- () = List.compare (compare_funsig bindings) r1.funcs r2.funcs in
              compare bindings r1.body r2.body
            | `Unequal_lengths x -> x
          end
          | _ -> raise @@ InvalidComparison "Impossible comparison of expressions"

      and compare_application : type a. Alist.t -> a application -> a application -> int =
        fun bindings a1 a2 ->
          if phys_equal a1 a2 then 0 else
          let- () = compare bindings a1.func a2.func in
          compare bindings a1.arg a2.arg

      and compare_statement : type a. Alist.t -> a statement -> a statement -> int =
        fun bindings s1 s2 ->
          if phys_equal s1 s2 then 0 else
          let- () = Int.compare (statement_to_rank s1) (statement_to_rank s2) in
          match s1, s2 with
          | SUntyped r1, SUntyped r2 -> compare (Alist.cons_assoc r1.var r2.var bindings) r1.defn r2.defn
          | STyped r1, STyped r2 ->
            let- () = Bool.compare r1.do_wrap r2.do_wrap in
            let- () = Bool.compare r1.do_check r2.do_check in
            let- () = compare bindings r1.typed_var.tau r2.typed_var.tau in
            compare bindings r1.defn r2.defn
          | SFun fs1, SFun fs2 -> compare_funsig bindings fs1 fs2
          | SFunRec l1, SFunRec l2 -> begin
            match Alist.cons_assocs (List.map l1 ~f:func_id_of_funsig) (List.map l2 ~f:func_id_of_funsig) bindings with
            | `Bindings bindings -> List.compare (compare_funsig bindings) l1 l2
            | `Unequal_lengths x -> x
          end
          | _ -> raise @@ InvalidComparison "Impossible comparison of statements"

      and compare_funsig : type a. Alist.t -> a funsig -> a funsig -> int =
        fun bindings fs1 fs2 ->
          if phys_equal fs1 fs2 then 0 else
          match fs1, fs2 with
          | FUntyped r1, FUntyped r2 -> begin
            (* assumes the function ids have already been associated if these are recursive *)
            match Alist.cons_assocs r1.params r2.params bindings with
            | `Bindings bindings -> compare bindings r1.defn r2.defn
            | `Unequal_lengths x -> x
          end
          | FTyped r1, FTyped r2 -> begin
            match Alist.cons_assocs r1.type_vars r2.type_vars bindings with
            | `Bindings bindings ->
              (* assumes the function ids have already been associated if these are recursive *)
              (* here just compare parameters. Later will add params to bindings and compare bodies *)
              let param_cmp, bindings_after_param_cmp = 
                compare_lists r1.params r2.params bindings ~f:(fun p1 p2 bindings ->
                  match p1, p2 with
                  | TVar tv1, TVar tv2 -> begin
                    match compare bindings tv1.tau tv2.tau with
                    | 0 -> `Continue_and_overwrite_bindings bindings
                    | x -> `Done x
                  end
                  | TVarDep tv1, TVarDep tv2 -> begin
                    match compare bindings tv1.tau tv2.tau with
                    | 0 -> `Continue_and_overwrite_bindings (Alist.cons_assoc tv1.var tv2.var bindings)
                    | x -> `Done x
                  end
                  | TVar _, TVarDep _ -> `Done (-1)
                  | TVarDep _, TVar _ -> `Done 1
                )
              in
              let- () = param_cmp in
              let- () = compare bindings_after_param_cmp r1.ret_type r2.ret_type in
              compare bindings_after_param_cmp r1.defn r2.defn
            | `Unequal_lengths x -> x
          end
          | FUntyped _, FTyped _ -> -1
          | FTyped _, FUntyped _ -> 1

      and compare_lists
        : type a. a list -> a list -> Alist.t ->
            f:(a -> a -> Alist.t -> [ `Done of int | `Continue_and_overwrite_bindings of Alist.t ]) -> int * Alist.t
        = fun ls1 ls2 bindings ~f ->
          match ls1, ls2 with
          | [], [] -> 0, bindings
          | _, [] -> 1, bindings
          | [], _ -> -1, bindings
          | a1 :: tl1, a2 :: tl2 ->
            match f a1 a2 bindings with
            | `Done x -> x, bindings
            | `Continue_and_overwrite_bindings bindings -> compare_lists tl1 tl2 bindings ~f
      in
      compare Alist.empty x y
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
      | ELet { var ; defn ; body } ->
        let id', env' = replace var env in
        ELet { var = id' ; defn = visit defn env ; body = visit body env' }
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
      | EIgnore { ignored ; body } -> EIgnore { ignored = visit ignored env ; body = visit body env }
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
