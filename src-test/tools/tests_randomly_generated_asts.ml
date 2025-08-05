open Core

open Lang.Ast
open Lang.Ast.Expr
open Lang.Ast.Constraints

open Tests_utils

let make_list (n : int) (f : int -> 'a) : 'a list =
  let rec loop i =
    if i = 0 then [] else (f (n-i)) :: (loop (i-1))
  in
  loop n

(******************************************************************************)
(* This section defines a framework for randomly generating ASTs using open
   recursion.  We start with the definition of a computational context and
   primitives to generate useful data. *)

type 'a context = {
  rand_state : Random.State.t;
  ctx_gen_pattern : 'a pattern_gen;
  ctx_gen_expr : 'a expr_gen;
  ctx_gen_statement : 'a statement_gen;
  depth : int;
}

and 'a pattern_gen = ctx : 'a context -> 'a Pattern.t

and 'a expr_gen = ctx : 'a context -> 'a Expr.t

and 'a typed_binding_opts_gen = ctx : 'a context -> 'a typed_binding_opts

and 'a statement_gen = ctx : 'a context -> 'a statement

let ident_chars = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']

let pick_int ~(ctx : 'a context) ~(min:int) ~(max:int) : int =
  Random.State.int ctx.rand_state (max-min) + min

let pick_bool ~(ctx : 'a context) : bool =
  Random.State.bool ctx.rand_state

let pick_from ~(ctx : 'a context) (options : 'b list) : 'b =
  let idx = pick_int ~ctx ~min:0 ~max:(List.length options) in
  match List.nth options idx with
  | Some x -> x
  | None -> failwith "Invariant broken: pick_int returned invalid index"

let pick_char ~(ctx : 'a context) (options : char list) : char =
  pick_from ~ctx options

let pick_list
    (type a)
    ?(min_len:int=1) ?(max_len:int=5) ~(ctx : _ context) (f : unit -> a)
  : a list =
  let rec loop n : a list =
    if n = 0 then [] else
      f () :: loop (n-1)
  in
  loop (pick_int ~ctx ~min:min_len ~max:max_len)

let pick_string
    ?(min_len:int=1) ?(max_len:int=5) ~(ctx : 'a context) () : string =
  let chars = pick_list
      ~min_len ~max_len ~ctx
      (fun () -> pick_char ~ctx ident_chars)
  in
  String.of_char_list chars

let pick_ident
    ?(min_len:int=1) ?(max_len:int=5) ~(ctx : 'a context) () : Ident.t =
  Ident(pick_string ~min_len ~max_len ~ctx ())

let pick_record_label
    ?(min_len:int=1) ?(max_len:int=5) ~(ctx : 'a context) () : RecordLabel.t =
  RecordLabel.RecordLabel(pick_ident ~min_len ~max_len ~ctx ())

let pick_variant_label
    ?(min_len:int=1) ?(max_len:int=5) ~(ctx : 'a context) () : VariantLabel.t =
  VariantLabel.VariantLabel(pick_ident ~min_len ~max_len ~ctx ())

let pick_variant_type_label
    ?(min_len:int=1) ?(max_len:int=5) ~(ctx : 'a context) ()
  : VariantTypeLabel.t =
  VariantTypeLabel.VariantTypeLabel(pick_ident ~min_len ~max_len ~ctx ())

let pick_pattern ~(ctx : 'a context) : 'a Pattern.t =
  ctx.ctx_gen_pattern ~ctx

let pick_expr ~(ctx : 'a context) : 'a Expr.t =
  ctx.ctx_gen_expr ~ctx

let pick_param ~(ctx : 'a context) : 'a param =
  let tv = { var = pick_ident ~ctx (); tau = pick_expr ~ctx } in
  if pick_bool ~ctx then TVar tv else TVarDep tv

let pick_funsig ~(ctx : 'a context) : 'a funsig =
  if pick_bool ~ctx then
    FUntyped { func_id = pick_ident ~ctx ();
               params = pick_list ~ctx (pick_ident ~ctx);
               defn = pick_expr ~ctx
             }
  else
    FTyped { type_vars = pick_list ~ctx (pick_ident ~ctx);
             func_id = pick_ident ~ctx ();
             params = pick_list ~ctx (fun () -> pick_param ~ctx);
             ret_type = pick_expr ~ctx;
             defn = pick_expr ~ctx;
           }

let pick_typed_var ~(ctx : 'a context) : 'a typed_var =
  { var = pick_ident ~ctx (); tau = pick_expr ~ctx }

let pick_statement ~(ctx : 'a context) : 'a statement =
  ctx.ctx_gen_statement ~ctx

(******************************************************************************)
(* This portion of the file defines random generators for patterns. *)

let rand_PAny : 'a pattern_gen = fun ~ctx ->
  ignore ctx; PAny

let rand_PVariable : 'a pattern_gen = fun ~ctx ->
  PVariable(pick_ident ~ctx ())

let rand_PVariant : 'a pattern_gen = fun ~ctx ->
  PVariant { variant_label = pick_variant_label ~ctx ();
             payload_id = pick_ident ~ctx () }

let rand_PInt : 'a pattern_gen = fun ~ctx ->
  ignore ctx; PInt

let rand_PBool : 'a pattern_gen = fun ~ctx ->
  ignore ctx; PBool

let rand_PType : 'a pattern_gen = fun ~ctx ->
  ignore ctx; PType

let rand_PRecord : 'a pattern_gen = fun ~ctx ->
  ignore ctx; PRecord

let rand_PModule : 'a pattern_gen = fun ~ctx ->
  ignore ctx; PModule

let rand_PFun : 'a pattern_gen = fun ~ctx ->
  ignore ctx; PFun

let rand_PUnit : 'a pattern_gen = fun ~ctx ->
  ignore ctx; PUnit

let rand_PUntouchable : 'a pattern_gen = fun ~ctx ->
  PUntouchable (pick_ident ~ctx ())

let rand_PEmptyList : 'a pattern_gen = fun ~ctx ->
  ignore ctx; PEmptyList

let rand_PDestructList : 'a pattern_gen = fun ~ctx ->
  PDestructList { hd_id = pick_ident ~ctx (); tl_id = pick_ident ~ctx () }

(******************************************************************************)
(* This portion of the file defines random generators for expressions. *)

let rand_EUnit : 'a expr_gen = fun ~ctx ->
  ignore ctx; EUnit

let rand_EInt : 'a expr_gen = fun ~ctx ->
  EInt(pick_int ~ctx ~min:(-20) ~max:200)

let rand_EBool : 'a expr_gen = fun ~ctx ->
  EBool(pick_bool ~ctx)

let rand_EVar : 'a expr_gen = fun ~ctx ->
  EVar(pick_ident ~ctx ())

let rand_EBinop : 'a expr_gen = fun ~ctx ->
  let binop = pick_from ~ctx
      [Binop.BPlus;
       Binop.BMinus;
       Binop.BTimes;
       Binop.BDivide;
       Binop.BModulus;
       Binop.BEqual;
       Binop.BNeq;
       Binop.BLessThan;
       Binop.BLeq;
       Binop.BGreaterThan;
       Binop.BGeq;
       Binop.BAnd;
       Binop.BOr;
      ]
  in
  EBinop { left = pick_expr ~ctx ; binop ; right = pick_expr ~ctx }

let rand_EIf : 'a expr_gen = fun ~ctx ->
  EIf { cond = pick_expr ~ctx;
        true_body = pick_expr ~ctx;
        false_body = pick_expr ~ctx }

let rand_ELet : 'a expr_gen = fun ~ctx ->
  ELet { var = pick_ident ~ctx ();
         defn = pick_expr ~ctx; 
         body = pick_expr ~ctx }

let rand_EAppl : 'a expr_gen = fun ~ctx ->
  EAppl { func = pick_expr ~ctx; arg = pick_expr ~ctx }

let rand_EMatch : 'a expr_gen = fun ~ctx ->
  EMatch { subject = pick_expr ~ctx;
           patterns =
             pick_list ~ctx
               (fun () -> (pick_pattern ~ctx, pick_expr ~ctx))
         }

let rand_EProject : 'a expr_gen = fun ~ctx ->
  EProject { record = pick_expr ~ctx;
             label = pick_record_label ~ctx ();
           }

let rand_ERecord : 'a expr_gen = fun ~ctx ->
  ERecord(
    let rec loop () =
      match RecordLabel.Map.of_alist @@ pick_list ~ctx
          (fun () -> (pick_record_label ~ctx (), pick_expr ~ctx)) with
      | `Ok m -> m
      | `Duplicate_key _ -> loop ()
    in loop ()
  )

let rand_EModule : 'a expr_gen = fun ~ctx ->
  EModule(pick_list ~ctx (fun () -> pick_statement ~ctx))

let rand_ENot : 'a expr_gen = fun ~ctx ->
  ENot(pick_expr ~ctx)

let rand_EInput : 'a expr_gen = fun ~ctx ->
  ignore ctx; EInput

let rand_EFunction : 'a expr_gen = fun ~ctx ->
  EFunction { param = pick_ident ~ctx (); body = pick_expr ~ctx }

let rand_EVariant : 'a expr_gen = fun ~ctx ->
  EVariant { label = pick_variant_label ~ctx (); payload = pick_expr ~ctx }

let rand_EDefer : 'a expr_gen = fun ~ctx ->
  EDefer(pick_expr ~ctx)

let rand_EPick_i : 'a expr_gen = fun ~ctx ->
  ignore ctx; EPick_i

let rand_EPick_b : 'a expr_gen = fun ~ctx ->
  ignore ctx; EPick_b

let rand_ECase : 'a expr_gen = fun ~ctx ->
  ECase { subject = pick_expr ~ctx;
          cases =
            pick_list ~min_len:0 ~ctx
              (fun () -> (pick_int ~min:0 ~max:20 ~ctx, pick_expr ~ctx));
          default = pick_expr ~ctx;
        }

let rand_EFreeze : 'a expr_gen = fun ~ctx ->
  EFreeze(pick_expr ~ctx)

let rand_EThaw : 'a expr_gen = fun ~ctx ->
  EThaw(pick_expr ~ctx)

let rand_EId : 'a expr_gen = fun ~ctx ->
  ignore ctx; EId

let rand_EIgnore : 'a expr_gen = fun ~ctx ->
  EIgnore { ignored = pick_expr ~ctx; body = pick_expr ~ctx }

let rand_ETable : 'a expr_gen = fun ~ctx ->
  ignore ctx; ETableCreate

let rand_ETblAppl : 'a expr_gen = fun ~ctx ->
  ETableAppl { tbl = pick_expr ~ctx; gen = pick_expr ~ctx; arg = pick_expr ~ctx }

let rand_EDet : 'a expr_gen = fun ~ctx ->
  EDet(pick_expr ~ctx)

let rand_EEscapeDet : 'a expr_gen = fun ~ctx ->
  EEscapeDet(pick_expr ~ctx)

let rand_EIntensionalEqual : 'a expr_gen = fun ~ctx ->
  EIntensionalEqual { left = pick_expr ~ctx; right = pick_expr ~ctx }

let rand_EUntouchable : 'a expr_gen = fun ~ctx ->
  EUntouchable(pick_expr ~ctx)

let rand_EAbort : 'a expr_gen = fun ~ctx ->
  EAbort(pick_string ~ctx ())

let rand_EVanish : 'a expr_gen = fun ~ctx ->
  ignore ctx; EVanish ()

let rand_EGen : 'a expr_gen = fun ~ctx ->
  EGen(pick_expr ~ctx)

let rand_EType : 'a expr_gen = fun ~ctx ->
  ignore ctx; EType

let rand_ETypeInt : 'a expr_gen = fun ~ctx ->
  ignore ctx; ETypeInt

let rand_ETypeBool : 'a expr_gen = fun ~ctx ->
  ignore ctx; ETypeBool

let rand_ETypeTop : 'a expr_gen = fun ~ctx ->
  ignore ctx; ETypeTop

let rand_ETypeBottom : 'a expr_gen = fun ~ctx ->
  ignore ctx; ETypeBottom

let rand_ETypeUnit : 'a expr_gen = fun ~ctx ->
  ignore ctx; ETypeUnit

let rand_ETypeRecord : 'a expr_gen = fun ~ctx ->
  ETypeRecord(
    let rec loop () =
      match RecordLabel.Map.of_alist @@ pick_list ~ctx
          (fun () -> (pick_record_label ~ctx (), pick_expr ~ctx)) with
      | `Ok m -> m
      | `Duplicate_key _ -> loop ()
    in loop ()
  )

let rand_ETypeModule : 'a expr_gen = fun ~ctx ->
  ETypeModule(
    pick_list ~ctx (fun () -> (pick_record_label ~ctx (), pick_expr ~ctx)))

let rand_ETypeFun : 'a expr_gen = fun ~ctx ->
  ETypeFun {
    domain = pick_expr ~ctx;
    codomain = pick_expr ~ctx;
    dep = if pick_bool ~ctx then `No else `Binding (pick_ident ~ctx ());
    det = pick_bool ~ctx;
  }

let rand_ETypeRefinement : 'a expr_gen = fun ~ctx ->
  ETypeRefinement { tau = pick_expr ~ctx; predicate = pick_expr ~ctx; }

let rand_ETypeMu : 'a expr_gen = fun ~ctx ->
  ETypeMu { var = pick_ident ~ctx ();
            params = pick_list ~ctx (fun () -> pick_ident ~ctx ());
            body = pick_expr ~ctx;
          }

let rand_ETypeVariant : 'a expr_gen = fun ~ctx ->
  ETypeVariant(pick_list ~ctx
                 (fun () -> (pick_variant_type_label ~ctx (), pick_expr ~ctx)))

let rand_ELetTyped : 'a typed_binding_opts_gen -> 'a expr_gen =
  fun typed_binding_opts_gen ~ctx ->
  ELetTyped { typed_var = pick_typed_var ~ctx;
              defn = pick_expr ~ctx;
              body = pick_expr ~ctx;
              typed_binding_opts = typed_binding_opts_gen ~ctx;
            }

let rand_ETypeSingle : 'a expr_gen = fun ~ctx ->
  ignore ctx; ETypeSingle

let rand_EList : 'a expr_gen = fun ~ctx ->
  EList(pick_list ~ctx (fun () -> pick_expr ~ctx))

let rand_EListCons : 'a expr_gen = fun ~ctx ->
  EListCons(pick_expr ~ctx, pick_expr ~ctx)

let rand_EAssert : 'a expr_gen = fun ~ctx ->
  EAssert(pick_expr ~ctx)

let rand_EAssume : 'a expr_gen = fun ~ctx ->
  EAssume(pick_expr ~ctx)

let rand_EMultiArgFunction : 'a expr_gen = fun ~ctx ->
  EMultiArgFunction
    { params = pick_list ~ctx (fun () -> pick_ident ~ctx ());
      body = pick_expr ~ctx;
    }

let rand_ELetFun : 'a expr_gen = fun ~ctx ->
  ELetFun { func = pick_funsig ~ctx; body = pick_expr ~ctx }

let rand_ELetFunRec : 'a expr_gen = fun ~ctx ->
  ELetFunRec
    { funcs = pick_list ~ctx (fun () -> pick_funsig ~ctx);
      body = pick_expr ~ctx }

let rand_ETypeList : 'a expr_gen = fun ~ctx ->
  ignore ctx; ETypeList

let rand_ETypeIntersect : 'a expr_gen = fun ~ctx ->
  ETypeIntersect(
    pick_list ~ctx (fun () ->
        (pick_variant_type_label ~ctx (), pick_expr ~ctx, pick_expr ~ctx)))

(******************************************************************************)
(* This portion of the file defines random generators for statements. *)

let rand_SUntyped : 'a statement_gen = fun ~ctx ->
  SUntyped { var = pick_ident ~ctx (); defn = pick_expr ~ctx }

let rand_STyped : 'a typed_binding_opts_gen -> 'a statement_gen =
  fun typed_binding_opts_gen ~ctx ->
  STyped { typed_var = pick_typed_var ~ctx;
           defn = pick_expr ~ctx; 
           typed_binding_opts = typed_binding_opts_gen ~ctx
         }

let rand_SFun : 'a statement_gen = fun ~ctx ->
  SFun(pick_funsig ~ctx)

let rand_SFunRec : 'a statement_gen = fun ~ctx ->
  SFunRec(pick_list ~ctx (fun () -> pick_funsig ~ctx))

(******************************************************************************)
(* This portion of the file groups generators based upon their suitability for
   leaf or non-leaf construction in the various languages. *)

type 'lang generator_parts = {
  leaf_pattern_generators : 'lang pattern_gen list;
  nonleaf_pattern_generators : 'lang pattern_gen list;
  leaf_expression_generators : 'lang expr_gen list;
  nonleaf_expression_generators : 'lang expr_gen list;
  leaf_statement_generators : 'lang statement_gen list;
  nonleaf_statement_generators : 'lang statement_gen list;
}

let bluejay_generator_parts : bluejay generator_parts =
  let typed_binding_opts_gen ~ctx = ignore ctx; TBBluejay in
  {
    leaf_pattern_generators = [
      rand_PAny;
      rand_PVariable;
      rand_PVariant;
      rand_PEmptyList;
      rand_PDestructList;
    ];
    nonleaf_pattern_generators = [];
    leaf_expression_generators = [
      rand_EUnit;
      rand_EInt;
      rand_EBool;
      rand_EVar;
      rand_EInput;
      rand_EType;
      rand_ETypeInt;
      rand_ETypeBool;
      rand_ETypeTop;
      rand_ETypeBottom;
      rand_ETypeUnit;
      rand_ETypeSingle;
      rand_EAssert;
      rand_EAssume;
      rand_ETypeList;
    ];
    nonleaf_expression_generators = [
      rand_EBinop;
      rand_EIf;
      rand_ELet;
      rand_EAppl;
      rand_EMatch;
      rand_EProject;
      rand_ERecord;
      rand_EModule;
      rand_ENot;
      rand_EFunction;
      rand_EVariant;
      rand_EDefer;
      rand_ETypeRecord;
      rand_ETypeModule;
      rand_ETypeFun;
      rand_ETypeRefinement;
      rand_ETypeMu;
      rand_ETypeVariant;
      rand_ELetTyped typed_binding_opts_gen;
      rand_EList;
      rand_EListCons;
      rand_EMultiArgFunction;
      rand_ELetFun;
      rand_ELetFunRec;
      rand_ETypeIntersect;
    ];
    leaf_statement_generators = [];
    nonleaf_statement_generators = [
      rand_SUntyped;
      rand_STyped typed_binding_opts_gen;
      rand_SFun;
      rand_SFunRec;
    ]
  }

let desugared_generator_parts : desugared generator_parts =
  let typed_binding_opts_gen ~ctx =
    TBDesugared { do_wrap = pick_bool ~ctx; do_check = pick_bool ~ctx }
  in
  {
    leaf_pattern_generators = [
      rand_PAny;
      rand_PVariable;
      rand_PVariant;
    ];
    nonleaf_pattern_generators = [];
    leaf_expression_generators = [
      rand_EUnit;
      rand_EInt;
      rand_EBool;
      rand_EVar;
      rand_EInput;
      rand_EAbort;
      rand_EVanish;
      rand_EType;
      rand_ETypeInt;
      rand_ETypeBool;
      rand_ETypeTop;
      rand_ETypeBottom;
      rand_ETypeUnit;
      rand_ETypeSingle;
    ];
    nonleaf_expression_generators = [
      rand_EBinop;
      rand_EIf;
      rand_ELet;
      rand_EAppl;
      rand_EMatch;
      rand_EProject;
      rand_ERecord;
      rand_EModule;
      rand_ENot;
      rand_EFunction;
      rand_EVariant;
      rand_EDefer;
      rand_EGen;
      rand_ETypeRecord;
      rand_ETypeModule;
      rand_ETypeFun;
      rand_ETypeRefinement;
      rand_ETypeMu;
      rand_ETypeVariant;
      rand_ELetTyped typed_binding_opts_gen;
    ];
    leaf_statement_generators = [];
    nonleaf_statement_generators = [
      rand_SUntyped;
      rand_STyped typed_binding_opts_gen;
    ];
  }

let embedded_generator_parts : embedded generator_parts = {
  leaf_pattern_generators = [
    rand_PAny;
    rand_PVariable;
    rand_PVariant;
    rand_PInt;
    rand_PBool;
    rand_PType;
    rand_PRecord;
    rand_PModule;
    rand_PFun;
    rand_PUnit;
    rand_PUntouchable;
  ];
  nonleaf_pattern_generators = [];
  leaf_expression_generators = [
    rand_EUnit;
    rand_EInt;
    rand_EBool;
    rand_EVar;
    rand_EPick_i;
    rand_EPick_b;
    rand_EId;
    rand_ETable;
    rand_EAbort;
    rand_EVanish;
  ];
  nonleaf_expression_generators = [
    rand_EBinop;
    rand_EIf;
    rand_ELet;
    rand_EAppl;
    rand_EMatch;
    rand_EProject;
    rand_ERecord;
    rand_EModule;
    rand_ENot;
    rand_EFunction;
    rand_EVariant;
    rand_EDefer;
    rand_ECase;
    rand_EFreeze;
    rand_EThaw;
    rand_EId;
    rand_EIgnore;
    rand_ETable;
    rand_ETblAppl;
    rand_EDet;
    rand_EEscapeDet;
    rand_EIntensionalEqual;
    rand_EUntouchable;
  ];
  leaf_statement_generators = [];
  nonleaf_statement_generators = [
    rand_SUntyped;
  ];
}

(******************************************************************************)
(* This section provides tools to assemble the above pieces into a closed
   utility for generating depth-limited ASTs. *)

type 'lang generators = {
  gen_pattern :
    ?rand_state:Random.State.t -> depth_threshold:int -> unit ->
    'lang Pattern.t;
  gen_expr :
    ?rand_state:Random.State.t -> depth_threshold:int -> unit ->
    'lang Expr.t;
  gen_statement :
    ?rand_state:Random.State.t -> depth_threshold:int -> unit ->
    'lang statement;
}

let build_generators (parts : 'lang generator_parts)
  : 'lang generators =
  let create_depth_sensitive_gen
      (type a b)
      (leaf_generators : (ctx:a context -> b) list)
      (nonleaf_generators : (ctx:a context -> b) list)
    : depth_threshold:int -> ctx:a context -> b =
    (* Determine whether to use a leaf or non-leaf. *)
    if List.is_empty nonleaf_generators then
      fun ~depth_threshold ~ctx ->
        ignore depth_threshold;
        let generator = pick_from ~ctx leaf_generators in
        let ctx' = { ctx with depth = ctx.depth + 1 } in
        generator ~ctx:ctx'
    else if List.is_empty leaf_generators then
      fun ~depth_threshold ~ctx ->
        ignore depth_threshold;
        let generator = pick_from ~ctx nonleaf_generators in
        let ctx' = { ctx with depth = ctx.depth + 1 } in
        generator ~ctx:ctx'
    else
      fun ~depth_threshold ~ctx ->
        let generator =
          let k = pick_int ~ctx ~min:0 ~max:(1+ctx.depth+depth_threshold) in
          if k >= depth_threshold then
            (* Choose a leaf. *)
            pick_from ~ctx leaf_generators
          else
            (* Choose a non-leaf *)
            pick_from ~ctx nonleaf_generators
        in
        let ctx' = { ctx with depth = ctx.depth + 1 } in
        generator ~ctx:ctx'
  in
  let gen_pattern =
    create_depth_sensitive_gen
      parts.leaf_pattern_generators parts.nonleaf_pattern_generators
  in
  let gen_expr =
    create_depth_sensitive_gen
      parts.leaf_expression_generators parts.nonleaf_expression_generators
  in
  let gen_statement =
    create_depth_sensitive_gen
      parts.leaf_statement_generators parts.nonleaf_statement_generators
  in
  let rand_state = Random.State.make_self_init () in
  let make_context
      ~(rand_state:Random.State.t) ~(depth_threshold:int) (depth : int)
    : 'lang context =
    { rand_state;
      ctx_gen_pattern = gen_pattern ~depth_threshold;
      ctx_gen_expr = gen_expr ~depth_threshold;
      ctx_gen_statement = gen_statement ~depth_threshold;
      depth;
    }
  in
  { gen_pattern = (fun ?(rand_state=rand_state) ~depth_threshold () ->
        gen_pattern
          ~depth_threshold
          ~ctx:(make_context ~rand_state ~depth_threshold 0));
    gen_expr = (fun ?(rand_state=rand_state) ~depth_threshold () ->
        gen_expr
          ~depth_threshold
          ~ctx:(make_context ~rand_state ~depth_threshold 0));
    gen_statement = (fun ?(rand_state=rand_state) ~depth_threshold () ->
        gen_statement
          ~depth_threshold
          ~ctx:(make_context ~rand_state ~depth_threshold 0));
  }

let bluejay_generators : bluejay generators =
  build_generators bluejay_generator_parts

let desugared_generators : desugared generators =
  build_generators desugared_generator_parts

let embedded_generators : embedded generators =
  build_generators embedded_generator_parts

(******************************************************************************)
(* This section utilizes the above framework to create some unit tests. *)

let make_test_from_generators
    ~(test_name:string)
    ~(test_index:int)
    ~(rand_state:Random.State.t)
    (parser : string -> 'lang statement list)
    (generators : 'lang generators)
  : unit Alcotest.test_case =
  let num = 1 + Random.State.int rand_state 3 in
  let ast_gen =
    (fun () ->
       make_list num
         (fun _ -> generators.gen_statement ~rand_state ~depth_threshold:3 ())
    )
  in
  make_test_case_from_ast
    (Printf.sprintf "%s_%04d" test_name test_index)
    parser
    ast_gen


let make_tests_from_generators
    ~(test_name:string)
    ~(rand_state:Random.State.t)
    (generators : 'lang generators)
    (parser : string -> 'a statement list)
    (test_count : int)
  : unit Alcotest.test =
  let test_cases =
    make_list test_count
      (fun i ->
         make_test_from_generators
           ~test_name ~test_index:i ~rand_state parser generators)
  in
  (test_name, test_cases)

let make_rand_tests (trees_per_language : int) : unit Alcotest.test list =
  [ make_tests_from_generators
      ~test_name:"generated_bluejay_pp"
      ~rand_state:(Random.State.make (Array.init 0 ~f:(fun _ -> 0)))
      bluejay_generators
      Lang.Parser.Bluejay.parse_single_pgm_string
      trees_per_language;
    make_tests_from_generators
      ~test_name:"generated_desugared_pp"
      ~rand_state:(Random.State.make (Array.init 0 ~f:(fun _ -> 0)))
      desugared_generators
      Lang.Parser.Desugared.parse_single_pgm_string
      trees_per_language;
    make_tests_from_generators
      ~test_name:"generated_embedded_pp"
      ~rand_state:(Random.State.make (Array.init 0 ~f:(fun _ -> 0)))
      embedded_generators
      Lang.Parser.Embedded.parse_single_pgm_string
      trees_per_language;
  ]
