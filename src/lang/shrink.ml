(**
   This module contains an algorithm for greedily shrinking Bluejay ASTs to
   reduce examples.  It operates by mutating the AST at a variety of points in
   parameterized ways.  If a mutation preserves the behavior of a predicate
   function, then it is retained.  A result is returned when no further
   mutations preserve the predicate.
*)

open Core

open Ast

(* TODO: delete this logging chunk *)
let evil_log (s : string) : unit =
  ignore s
  (* let oc = Out_channel.create ~append:true "ztemp.log" in
  Out_channel.fprintf oc "%s" s;
  Out_channel.close oc *)

(** A witness for the types of nodes in the AST that we will allow to be
    punched out. *)
type ('lang, _) node_type =
  | NodeTypeExpr : ('lang, 'lang Expr.t) node_type
  | NodeTypeStatement : ('lang, 'lang Expr.statement) node_type
  | NodeTypePattern : ('lang, 'lang Pattern.t) node_type
  | NodeTypeList : ('lang, 'a) node_type -> ('lang, 'a list) node_type

(** A generalization over [node_type]. *)
type 'lang some_node =
  | SomeNode : ('lang, 'node) node_type * 'node -> 'lang some_node

(** A representation of a point in an AST which has been punched out.  This
    constructor carries an AST with a hole (in the form of a function), the
    subtree which was punched out to form the hole, and type witnesses for
    each. *)
type ('lang, 'encloser, 'hole) transform_point =
  | TransformPoint of {
      hole_type : ('lang, 'hole) node_type;
      encloser_type : ('lang, 'encloser) node_type;
      hole_node : 'hole;
      context_fn : 'hole -> 'encloser;
    }

(** A generalization of [transformation_point]. *)
type ('lang, 'encloser) some_transform_point =
  | SomeTransformPoint :
      ('lang, 'encloser, 'hole) transform_point ->
      ('lang, 'encloser) some_transform_point

(** A debugging tool to examine transformation points *)
let some_transform_point_to_approximate_string (type lang encloser)
    (some_transform_point : (lang, encloser) some_transform_point) : string =
  let SomeTransformPoint (type hole) (TransformPoint {
      hole_type; encloser_type; hole_node=_; context_fn
    } : (lang, encloser, hole) transform_point) = some_transform_point in
  let rec mk_dummy : 'a. (lang, 'a) node_type -> int -> 'a =
    fun (type a) (ty : (lang, a) node_type) (n : int) : a ->
      let hole_ident =
        if n = 0 then
          Ident.Ident "__HOLE__"
        else
          Ident.Ident (Printf.sprintf "__HOLE_%d__" n)
      in
      match ty with
      | NodeTypeExpr -> Expr.EVar hole_ident
      | NodeTypePattern -> Pattern.PVariable hole_ident
      | NodeTypeStatement ->
        Expr.SUntyped { var = hole_ident; defn = Expr.EVar hole_ident }
      | NodeTypeList ty' -> [mk_dummy ty' (n + 1)]
  in
  let dummy = mk_dummy hole_type 0 in
  let plugged_ast = context_fn dummy in
  let rec mk_str : 'a. (lang, 'a) node_type -> 'a -> string =
    fun (type a) (ty : (lang, a) node_type) (x : a) : string ->
      match ty with
      | NodeTypeExpr -> Expr.to_string x
      | NodeTypePattern -> Pattern.to_string x
      | NodeTypeStatement -> Expr.statement_to_string x
      | NodeTypeList ty' ->
        String.concat ~sep:"\n\n" @@ List.map ~f:(mk_str ty') x
  in
  mk_str encloser_type plugged_ast

(** Composes two transformation points by having one fill in the hole of the
    other.  (Note that this operation is asymmetric.) *)
let transform_compose (type lang node_a node_b node_c)
    (p1 : (lang, node_a, node_b) transform_point)
    (p2 : (lang, node_b, node_c) transform_point)
  : (lang, node_a, node_c) transform_point =
  match p1, p2 with
  | TransformPoint { hole_type = _;
                     encloser_type = encloser_type_1;
                     hole_node = _;
                     context_fn = context_fn_1; },
    TransformPoint { hole_type = hole_type_2;
                     encloser_type = _;
                     hole_node = hole_node_2;
                     context_fn = context_fn_2; } ->
    TransformPoint { hole_type = hole_type_2;
                     encloser_type = encloser_type_1;
                     hole_node = hole_node_2;
                     context_fn = fun x -> (context_fn_1 (context_fn_2 x))
                   }

(** A helper routine to produce information about how holes might be punched
    into a [funsig]. *)
let mk_holes_for_funsig (type lang) (funsig : lang Expr.funsig)
  : (lang Expr.t * (lang Expr.t -> lang Expr.funsig)) list =
  match funsig with
  | Expr.FUntyped { func_id; params; defn } ->
    [ (defn, fun defn -> Expr.FUntyped { func_id; params; defn }) ]
  | Expr.FTyped { type_vars; func_id; params; ret_type; defn } ->
    (defn,
     fun defn -> Expr.FTyped { type_vars; func_id; params; ret_type; defn }) ::
    Utils.List_utils.map_in_context params ~f:(
      fun ~before ~after ~idx:_ ~item:param ->
        match param with
        | Expr.TVar { var; tau } ->
          (tau, (fun tau ->
               let param' = Expr.TVar { var; tau } in
               Expr.FTyped { type_vars; func_id;
                             params = before @ (param' :: after);
                             ret_type; defn }
             ))
        | Expr.TVarDep { var; tau } ->
          (tau, (fun tau ->
               let param' = Expr.TVarDep { var; tau } in
               Expr.FTyped { type_vars; func_id;
                             params = before @ (param' :: after);
                             ret_type; defn }
             ))
    )

(** Produces the shallow transformation points for an expression.  These are all
    the holes which are a depth of exactly one from the root. *)
let enumerate_shallow_expr_points (type lang) (e : lang Expr.t)
  : (lang, lang Expr.t) some_transform_point list =
  let mk_expr_hole
      (hole_node : lang Expr.t) (context_fn : lang Expr.t -> lang Expr.t)
    : (lang, lang Expr.t) some_transform_point =
    SomeTransformPoint (TransformPoint {
        hole_type = NodeTypeExpr; encloser_type = NodeTypeExpr;
        hole_node; context_fn;
      })
  in
  let mk_pattern_hole
      (hole_node : lang Pattern.t) (context_fn : lang Pattern.t -> lang Expr.t)
    : (lang, lang Expr.t) some_transform_point =
    SomeTransformPoint (TransformPoint {
        hole_type = NodeTypePattern; encloser_type = NodeTypeExpr;
        hole_node; context_fn;
      })
  in
  let mk_statement_hole
      (hole_node : lang Expr.statement)
      (context_fn : lang Expr.statement -> lang Expr.t)
    : (lang, lang Expr.t) some_transform_point =
    SomeTransformPoint (TransformPoint {
        hole_type = NodeTypeStatement; encloser_type = NodeTypeExpr;
        hole_node; context_fn;
      })
  in
  match e with
  | Expr.EUnit -> []
  | Expr.EInt _ -> []
  | Expr.EBool _ -> []
  | Expr.EVar _ -> []
  | Expr.EBinop { left; binop; right } ->
    [ mk_expr_hole left (fun left -> Expr.EBinop { left; binop; right });
      mk_expr_hole right (fun right -> Expr.EBinop { left; binop; right });
    ]
  | Expr.EIf { cond; true_body; false_body } -> 
    [ mk_expr_hole cond
        (fun cond -> Expr.EIf { cond; true_body; false_body });
      mk_expr_hole true_body
        (fun true_body -> Expr.EIf { cond; true_body; false_body });
      mk_expr_hole false_body
        (fun false_body -> Expr.EIf { cond; true_body; false_body });
    ]
  | Expr.ELet { var; defn; body } ->
    [ mk_expr_hole defn (fun defn -> Expr.ELet { var; defn; body });
      mk_expr_hole body (fun body -> Expr.ELet { var; defn; body });
    ]
  | Expr.EAppl { func; arg } ->
    [ mk_expr_hole func (fun func -> Expr.EAppl { func; arg });
      mk_expr_hole arg (fun arg -> Expr.EAppl { func; arg });
    ]
  | Expr.EMatch { subject; patterns } ->
    ( mk_expr_hole subject (fun subject -> Expr.EMatch { subject; patterns })
    ) ::
    ( List.concat @@ Utils.List_utils.map_in_context patterns ~f:(
          fun ~before ~after ~idx:_ ~item:(pat,body) ->
            [ mk_pattern_hole pat
                (fun pat ->
                   Expr.EMatch { subject;
                                 patterns = before @ [(pat,body)] @ after
                               }
                );
              mk_expr_hole body
                (fun body ->
                   Expr.EMatch { subject;
                                 patterns = before @ [(pat,body)] @ after
                               }
                )
            ]
        )
    )
  | Expr.EProject { record; label } ->
    [ mk_expr_hole record (fun record -> Expr.EProject { record; label }) ]
  | Expr.ERecord m ->
    Map.to_alist m
    |> Utils.List_utils.map_in_context ~f:(
      fun ~before ~after ~idx:_ ~item:(lbl,body) ->
        mk_expr_hole body
          (fun body ->
             Expr.ERecord (
               RecordLabel.Map.of_alist_exn (before@((lbl,body)::after))))
    )
  | Expr.EModule statements ->
    Utils.List_utils.map_in_context statements ~f:(
      fun ~before ~after ~idx:_ ~item ->
        mk_statement_hole item
          (fun item -> Expr.EModule (before @ (item :: after))))
  | Expr.ENot e' ->
    [ mk_expr_hole e' (fun e' -> Expr.ENot e') ]
  | Expr.EInput -> []
  | Expr.EFunction { param; body } ->
    [ mk_expr_hole body (fun body -> Expr.EFunction { param ; body }) ]
  | Expr.EVariant { label; payload } ->
    [ mk_expr_hole payload (fun payload -> Expr.EVariant { label; payload }) ]
  | Expr.EDefer e' ->
    [ mk_expr_hole e' (fun e' -> Expr.EDefer e') ]
  | Expr.EPick_i -> []
  | Expr.EPick_b -> []
  | Expr.ECase { subject; cases; default } ->
    [ mk_expr_hole subject
        (fun subject -> Expr.ECase { subject; cases; default });
      mk_expr_hole default
        (fun default -> Expr.ECase { subject; cases; default });
    ] @
    Utils.List_utils.map_in_context cases ~f:(
      fun ~before ~after ~idx:_ ~item:(n, body) ->
        mk_expr_hole body (fun body ->
            Expr.ECase { subject;
                         cases = before @ ((n, body) :: after);
                         default; })
    )
  | Expr.EFreeze e' ->
    [ mk_expr_hole e' (fun e' -> Expr.EFreeze e') ]
  | Expr.EThaw e' ->
    [ mk_expr_hole e' (fun e' -> Expr.EThaw e') ]
  | Expr.EId ->
    []
  | Expr.EIgnore { ignored; body } ->
    [ mk_expr_hole ignored (fun ignored -> Expr.EIgnore { ignored; body });
      mk_expr_hole body (fun body -> Expr.EIgnore { ignored; body });
    ]
  | Expr.ETableCreate -> []
  | Expr.ETableAppl { tbl; gen; arg } ->
    [ mk_expr_hole tbl (fun tbl -> Expr.ETableAppl { tbl; gen; arg });
      mk_expr_hole gen (fun gen -> Expr.ETableAppl { tbl; gen; arg });
      mk_expr_hole arg (fun arg -> Expr.ETableAppl { tbl; gen; arg });
    ]
  | Expr.EDet e' ->
    [ mk_expr_hole e' (fun e' -> Expr.EDet e') ]
  | Expr.EEscapeDet e' ->
    [ mk_expr_hole e' (fun e' -> Expr.EEscapeDet e') ]
  | Expr.EIntensionalEqual { left; right } ->
    [ mk_expr_hole left
        (fun left -> Expr.EIntensionalEqual { left; right });
      mk_expr_hole right
        (fun right -> Expr.EIntensionalEqual { left; right });
    ]
  | Expr.EUntouchable e' ->
    [ mk_expr_hole e' (fun e' -> Expr.EUntouchable e') ]
  | Expr.EAbort _ -> []
  | Expr.EVanish _ -> []
  | Expr.EGen e' ->
    [ mk_expr_hole e' (fun e' -> Expr.EGen e') ]
  | Expr.EType -> []
  | Expr.ETypeInt -> []
  | Expr.ETypeBool -> []
  | Expr.ETypeTop -> []
  | Expr.ETypeBottom -> []
  | Expr.ETypeUnit -> []
  | Expr.ETypeRecord m ->
    Map.to_alist m
    |> Utils.List_utils.map_in_context ~f:(
      fun ~before ~after ~idx:_ ~item:(lbl,body) ->
        mk_expr_hole body
          (fun body ->
             Expr.ETypeRecord (
               RecordLabel.Map.of_alist_exn ((lbl,body)::before@after)))
    )
  | Expr.ETypeModule decls ->
    Utils.List_utils.map_in_context decls ~f:(
      fun ~before ~after ~idx:_ ~item:(lbl,body) ->
        mk_expr_hole body
          (fun body ->
             Expr.ETypeRecord (
               RecordLabel.Map.of_alist_exn ((lbl,body)::before@after)))
    )
  | Expr.ETypeFun { domain; codomain; dep; det } ->
    [ mk_expr_hole codomain
        (fun codomain -> Expr.ETypeFun { domain; codomain; dep; det });
      mk_expr_hole codomain
        (fun codomain -> Expr.ETypeFun { domain; codomain; dep; det });
    ]
  | Expr.ETypeRefinement { tau; predicate } ->
    [ mk_expr_hole tau
        (fun tau -> Expr.ETypeRefinement { tau; predicate });
      mk_expr_hole predicate
        (fun predicate -> Expr.ETypeRefinement { tau; predicate });
    ]
  | Expr.ETypeMu { var; params; body } ->
    [ mk_expr_hole body (fun body -> Expr.ETypeMu { var; params; body })]
  | Expr.ETypeVariant xs ->
    Utils.List_utils.map_in_context xs ~f:(
      fun ~before ~after ~idx:_ ~item:(l,t) ->
        mk_expr_hole t (fun t ->
            Expr.ETypeVariant (before @ ((l,t) :: after))))
  | Expr.ELetTyped
      { typed_var = { var; tau };
        defn; body; typed_binding_opts } ->
    [ mk_expr_hole tau
        (fun tau ->
           Expr.ELetTyped
             { typed_var = { var; tau };
               defn; body; typed_binding_opts });
      mk_expr_hole defn
        (fun defn ->
           Expr.ELetTyped
             { typed_var = { var; tau };
               defn; body; typed_binding_opts });
      mk_expr_hole body
        (fun body ->
           Expr.ELetTyped
             { typed_var = { var; tau };
               defn; body; typed_binding_opts });
    ]
  | Expr.ETypeSingle -> []
  | Expr.EList es ->
    Utils.List_utils.map_in_context es ~f:(
      fun ~before ~after ~idx:_ ~item ->
        mk_expr_hole item
          (fun item -> Expr.EList (before @ (item :: after)))
    )
  | Expr.EListCons(e1, e2) ->
    [ mk_expr_hole e1 (fun e1 -> Expr.EListCons(e1, e2));
      mk_expr_hole e2 (fun e2 -> Expr.EListCons(e1, e2));
    ]
  | Expr.EAssert e' ->
    [ mk_expr_hole e' (fun e' -> Expr.EAssert e') ]
  | Expr.EAssume e' ->
    [ mk_expr_hole e' (fun e' -> Expr.EAssume e') ]
  | Expr.EMultiArgFunction { params; body } ->
    [ mk_expr_hole body
        (fun body -> Expr.EMultiArgFunction { params; body }) ]
  | Expr.ELetFun { func; body } ->
    (mk_expr_hole body (fun body -> Expr.ELetFun { func; body })) ::
    (mk_holes_for_funsig func
     |> List.map ~f:(fun (e', mk) ->
         mk_expr_hole e' (fun e' -> Expr.ELetFun { func = (mk e'); body } )
       ))
  | Expr.ELetFunRec { funcs; body } ->
    (mk_expr_hole body (fun body -> Expr.ELetFunRec { funcs; body })) ::
    (List.concat @@ Utils.List_utils.map_in_context funcs ~f:(
        fun ~before ~after ~idx:_ ~item:func ->
          mk_holes_for_funsig func
          |> List.map ~f:(fun (e', mk) ->
              mk_expr_hole e' (fun e' ->
                  let funcs' = before @ (mk e' :: after) in
                  Expr.ELetFunRec { funcs = funcs'; body }
                )
            )            
      ))
  | Expr.ETypeList -> []
  | Expr.ETypeIntersect intersectands ->
    List.concat @@ Utils.List_utils.map_in_context intersectands ~f:(
      fun ~before ~after ~idx:_ ~item:(lbl, payload, result) ->
        [ mk_expr_hole payload (fun payload ->
              let intersectand = (lbl, payload, result) in
              Expr.ETypeIntersect (before @ (intersectand :: after))
            );
          mk_expr_hole result (fun result ->
              let intersectand = (lbl, payload, result) in
              Expr.ETypeIntersect (before @ (intersectand :: after))
            );
        ]
    )

(** Produces the shallow transformation points for a pattern.  These are all
    the holes which are a depth of exactly one from the root. *)
let enumerate_shallow_pattern_points (type lang) (p : lang Pattern.t)
  : (lang, lang Pattern.t) some_transform_point list =
  (* As of this writing, patterns can't have holes in them because they are
     not nested and don't contain types. *)
  ignore p; []

(** Produces the shallow transformation points for a statement.  These are all
    the holes which are a depth of exactly one from the root. *)
let enumerate_shallow_statement_points (type lang) (s : lang Expr.statement)
  : (lang, lang Expr.statement) some_transform_point list =
  let mk_expr_hole
      (hole_node : lang Expr.t)
      (context_fn : lang Expr.t -> lang Expr.statement)
    : (lang, lang Expr.statement) some_transform_point =
    SomeTransformPoint (TransformPoint {
        hole_type = NodeTypeExpr; encloser_type = NodeTypeStatement;
        hole_node; context_fn;
      })
  in
  match s with
  | Expr.SUntyped { var; defn } ->
    [ mk_expr_hole defn (fun defn -> Expr.SUntyped { var; defn }) ]
  | Expr.STyped { typed_var = { var; tau }; defn; typed_binding_opts } ->
    [ mk_expr_hole tau
        (fun tau ->
           Expr.STyped { typed_var = { var; tau }; defn; typed_binding_opts });
      mk_expr_hole defn
        (fun defn ->
           Expr.STyped { typed_var = { var; tau }; defn; typed_binding_opts });
    ]
  | Expr.SFun funsig ->
    (mk_holes_for_funsig funsig
     |> List.map ~f:(fun (e', mk) ->
         mk_expr_hole e' (fun e' -> Expr.SFun (mk e') )
       ))
  | Expr.SFunRec funsigs ->
    (List.concat @@ Utils.List_utils.map_in_context funsigs ~f:(
        fun ~before ~after ~idx:_ ~item:funsig ->
          mk_holes_for_funsig funsig
          |> List.map ~f:(fun (e', mk) ->
              mk_expr_hole e' (fun e' ->
                  Expr.SFunRec (before @ (mk e' :: after))
                )
            )            
      ))

let enumerate_shallow_list_points
    (type lang a)
    (el_ty : (lang, a) node_type) (xs : a list)
  : (lang, a list) some_transform_point list =
  Utils.List_utils.map_in_context xs ~f:(
    fun ~before ~after ~idx:_ ~item ->
      (SomeTransformPoint (TransformPoint(
           { hole_type = el_ty; encloser_type = NodeTypeList el_ty;
             hole_node = item; context_fn = (fun item -> before@(item::after));
           }))
      )
  )

(** Given an AST node, enumerates all of the points in the AST at which a hole
    can be punched.  A [Sequence.t] is used here to allow a caller to consume
    only some of those points. *)
let enumerate_deep_points
    (type lang encloser)
    (ty : (lang, encloser) node_type) (x : encloser)
  : (lang, encloser) some_transform_point Sequence.t =
  let identity_point = SomeTransformPoint (TransformPoint(
      { hole_type = ty; encloser_type = ty;
        hole_node = x; context_fn = (fun x -> x)
      }))
  in
  Sequence.unfold
    ~init:([[`Destruct identity_point]])
    ~f:(let rec handle
           (queue : [`Result of (lang, encloser) some_transform_point |
                     `Destruct of (lang, encloser) some_transform_point ]
                list list)
         : ((lang, encloser) some_transform_point *
            [`Result of (lang, encloser) some_transform_point |
             `Destruct of (lang, encloser) some_transform_point ]
              list list) option =
          (* TODO: delete this logging chunk *)
          let rec count q results destructs =
            match q with
            | [] -> results, destructs
            | []::q' -> count q' results destructs
            | (`Result _ :: xs)::q' ->
              count (xs::q') (results+1) destructs
            | (`Destruct _ :: xs)::q' ->
              count (xs::q') results (destructs+1)
          in
          let results, destructs = count queue 0 0 in
          evil_log (Printf.sprintf "\n\n!! %d,%d\n\n" results destructs);
          let evil_log_list msg xs =
            let txt = String.concat ~sep:"\n\n######\n\n" @@ List.map xs ~f:(fun item ->
                match item with
                | `Result x ->
                  "(* result   *)\n" ^ some_transform_point_to_approximate_string x
                | `Destruct x ->
                  "(* destruct *)\n" ^ some_transform_point_to_approximate_string x
                | `Wrapper x ->
                  some_transform_point_to_approximate_string x
              ) in
            evil_log (Printf.sprintf "\n\n###%s################\n\n%s\n\n###################\n\n" msg txt);
          in
          evil_log_list "queue" (Stdlib.List.flatten queue);
          match queue with
          | []::queue' ->
            evil_log "\n~~ empty segment\n\n";
            handle queue'
          | (`Result x :: segment) :: queue' ->
            evil_log "\n~~ result\n\n";
            Some(x, segment :: queue')
          | [] ->
            evil_log "\n~~ end of queue\n\n";
            None
          | (`Destruct x :: segment) :: queue' ->
            evil_log (
              Printf.sprintf "\n~~ destruct:\n%s\n\n"
                (some_transform_point_to_approximate_string x)
            );
            let new_queue = segment :: queue' in
            match x with
            | SomeTransformPoint (TransformPoint(
                { hole_type; encloser_type = _; hole_node; context_fn = _ } )
               as x_inner) ->
              let new_items =
                begin
                  (
                    match hole_type with
                    | NodeTypeExpr ->
                      evil_log (
                        Printf.sprintf "~~ (hole node):\n%s\n\n"
                          (Expr.to_string hole_node);
                      );
                      evil_log_list "hole_points"
                        (enumerate_shallow_expr_points hole_node
                         |> List.map ~f:(fun x -> `Wrapper x));
                      enumerate_shallow_expr_points hole_node
                      |> List.map ~f:(fun y ->
                          match y with
                          | SomeTransformPoint y_inner ->
                            SomeTransformPoint (
                              transform_compose x_inner y_inner)
                        )
                    | NodeTypePattern ->
                      enumerate_shallow_pattern_points hole_node
                      |> List.map ~f:(fun y ->
                          match y with
                          | SomeTransformPoint y_inner ->
                            SomeTransformPoint (
                              transform_compose x_inner y_inner)
                        )
                    | NodeTypeStatement ->
                      enumerate_shallow_statement_points hole_node
                      |> List.map ~f:(fun y ->
                          match y with
                          | SomeTransformPoint y_inner ->
                            SomeTransformPoint (
                              transform_compose x_inner y_inner)
                        )
                    | NodeTypeList el_ty ->
                      enumerate_shallow_list_points el_ty hole_node
                      |> List.map ~f:(fun y ->
                          match y with
                          | SomeTransformPoint y_inner ->
                            SomeTransformPoint (
                              transform_compose x_inner y_inner)
                        )
                  )
                  |> List.map ~f:(fun x -> `Destruct x)
                end
              in
              let () = evil_log_list "new_items" new_items in
              handle (new_items :: [`Result x] :: new_queue)
        in handle)

(** The type of a replacement filter as used by [shrink]. *)
type 'lang replacement_filter = { 
  replacement_filter :
    'node. ('lang, 'node) node_type -> old_node:'node -> new_node:'node -> bool
}

(** A structure carrying the transformation routines used by [shrink].  Each
    routine takes an AST node and offers a list of potential replacements. *)
type 'lang transformers = {
  expr_transformers : ('lang Expr.t -> 'lang Expr.t list) list;
  pattern_transformers : ('lang Pattern.t -> 'lang Pattern.t list) list;
  statement_transformers :
    ('lang Expr.statement -> 'lang Expr.statement list) list;
  list_transformers : 'a. ('a list -> 'a list list) list
}

(** A collection of default transformers for the AST. *)
let default_transformers = {
  expr_transformers = [
    (fun _ -> [EInt 0]);
  ];
  pattern_transformers = [];
  statement_transformers = [];
  list_transformers = [
    (fun lst ->
       Utils.List_utils.map_in_context lst ~f:(
         fun ~before ~after ~idx:_ ~item:_ ->
           before @ after
       )
    )
  ];
}

(**
   Given an AST node, this function will attempt to find a smaller example which
   satisfies a given predicate.  This is accomplished by randomly transforming
   the AST at each point in the tree and testing it against the provided
   predicate.  This algorithm is greedy: if a transformation applies, then it
   is used going forward.

   The user is expected to provide a collection of transformations for each AST
   node type, the predicate which should be maintained, and the AST node to
   start with (as well as a witness of its type).  The returned AST node is the
   last node found which obeys the provided predicate.

   It is the responsibility of the caller to ensure that the transformation
   functions lead to a fixed point.  This function will recognize
   transformations which do not change the AST (e.g. replacing a subtree with
   itself) but will not recognize other modifications.  Optionally, a function
   may be supplied to determine whether a particular transformed AST should be
   used (whether or not it ascribes to the predicate).
*)
let shrink (type lang node)
    (predicate : node -> bool)
    ?(filter : lang replacement_filter option)
    ?(transformers : lang transformers = default_transformers)
    (node_type : (lang, node) node_type)
    (node : node)
  : node =
  (* TODO: delete this logging chunk *)
  begin
    let rec show : 'a. (lang, 'a) node_type -> 'a -> string =
      fun (type a) (xt : (lang, a) node_type) (x : a) ->
        match xt with
        | NodeTypeExpr -> Expr.to_string x
        | NodeTypePattern -> Pattern.to_string x
        | NodeTypeStatement -> Expr.statement_to_string x
        | NodeTypeList et ->
          String.concat ~sep:"\n\n" (List.map x ~f:(fun e -> show et e))
    in
    evil_log (Printf.sprintf "----------------------------------\n\n%s" (show node_type node));
  end;
  let rec loop
      ~(predicate : node -> bool)
      ~(filter : lang replacement_filter option)
      ~(transformers : lang transformers)
      ~(node_type : (lang, node) node_type)
      ~(node : node)
    : node =
    let open Container.Continue_or_stop in
    let rec node_compare : 'a. (lang, 'a) node_type -> 'a -> 'a -> int =
      fun (type a) (node_type : (lang, a) node_type) (x : a) (y : a) ->
        match node_type with
        | NodeTypeExpr -> Expr.compare Ident.compare x y
        | NodeTypePattern -> Pattern.compare x y
        | NodeTypeStatement -> Expr.compare_statement Ident.compare x y
        | NodeTypeList (type el) (el_ty : (lang, el) node_type) ->
          List.compare (node_compare el_ty) x y
    in
    let points = enumerate_deep_points node_type node in
    let next : node option =
      Sequence.fold_until points
        ~init:()
        ~finish:(fun () -> None)
        ~f:(fun () point ->
            let SomeTransformPoint (type hole) (TransformPoint {
                hole_type;
                encloser_type=_;
                hole_node;
                context_fn;
              } : (lang, node, hole) transform_point) = point in
            let transforms : (hole -> hole list) list =
              match hole_type with
              | NodeTypeExpr -> transformers.expr_transformers
              | NodeTypePattern -> transformers.pattern_transformers
              | NodeTypeStatement -> transformers.statement_transformers
              | NodeTypeList _ -> transformers.list_transformers
            in
            let reduced_ast_opt =
              Utils.List_utils.take_first_mapped transforms ~f:(fun transform ->
                  Utils.List_utils.take_first_mapped (transform hole_node) ~f:(
                    fun transformed_hole_node ->
                      let transformed_encloser =
                        context_fn transformed_hole_node
                      in
                      let accept =
                        node_compare node_type node transformed_encloser <> 0 &&
                        predicate transformed_encloser &&
                        match filter with
                        | None -> true
                        | Some filter ->
                          filter.replacement_filter node_type
                            ~old_node:node ~new_node:transformed_encloser
                      in
                      if accept then
                        Some transformed_encloser
                      else
                        None
                  )
                )
            in
            match reduced_ast_opt with
            | None -> Continue ()
            | Some ast -> Stop (Some ast)
          )
    in
    match next with
    | None -> node
    | Some new_node ->
      loop
        ~predicate
        ~filter
        ~transformers
        ~node_type
        ~node:new_node
  in
  loop
    ~predicate
    ~filter
    ~transformers
    ~node_type
    ~node
