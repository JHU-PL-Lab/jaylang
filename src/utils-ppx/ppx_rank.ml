(**
   This PPX creates a deriver called "jay_rank" which creates ranking functions
   for variant types in mutually recursive type declarations.  Ranking functions
   produce a unique integer for each constructor of a variant type, allowing for
   e.g. quick, type-safe comparison of constructors by integer in comparison
   functions.  Other types in the mutually recursive type declarations are
   handled differently: record types are ignored (as they have only one
   constructor) while abstract and open types produce compilation errors (as no
   ranking function can be generated for them).
*)

open Core
open Ppxlib;;

open Ast_builder.Default;;

let[@inline always] llocident (x : label loc) : Longident.t loc =
  Loc.map ~f:(fun s -> Lident s) x

let constructor_arguments_are_empty (args : constructor_arguments) : bool =
  match args with
  | Pcstr_tuple xs -> List.is_empty xs
  | Pcstr_record xs -> List.is_empty xs

let make_rank_function_for_variant_type_decl
    (type_name : label loc)
    (type_params : (core_type * (variance * injectivity)) list)
    (constructors : constructor_declaration list)
  : structure =
  let loc = type_name.loc in
  let function_name =
    Loc.make ~loc @@
    if String.equal type_name.txt "t" then
      "to_rank"
    else
      type_name.txt ^ "_to_rank"
  in
  let function_name_pattern =
    ppat_var ~loc function_name
  in
  let fn_type_params =
    List.mapi type_params
      ~f:(fun n _ -> Loc.make ~loc @@ "a" ^ string_of_int n)
  in
  let subject_ident = Loc.make ~loc "x" in
  let body_expr =
    pexp_match ~loc (pexp_ident ~loc (llocident subject_ident)) @@
    List.mapi constructors ~f:(fun n constructor ->
        let loc = constructor.pcd_loc in
        let pattern =
          if constructor_arguments_are_empty constructor.pcd_args then
            ppat_construct ~loc (llocident constructor.pcd_name) None
          else
            ppat_construct ~loc (llocident constructor.pcd_name)
              (Some (ppat_any ~loc))
        in
        let body = pexp_constant ~loc (Pconst_integer(string_of_int n, None)) in
        case ~lhs:pattern ~guard:None ~rhs:body)
  in
  let function_param_pattern =
    ppat_constraint ~loc (ppat_var ~loc subject_ident) @@
    ptyp_constr ~loc (llocident type_name) @@
    List.map ~f:(fun a -> ptyp_constr ~loc (llocident a) []) fn_type_params
  in
  let function_expr =
    let inner_expr =
      pexp_function ~loc [
        case
          ~lhs:function_param_pattern
          ~guard:None
          ~rhs:body_expr
      ]
    in
    List.fold_right fn_type_params ~init:inner_expr ~f:(fun type_param e ->
        pexp_newtype ~loc type_param e
      ) 
  in
  [ pstr_value ~loc Nonrecursive [
        value_binding ~loc
          ~pat:function_name_pattern
          ~expr:function_expr
      ] ]
;;

let make_rank_function_for_type_decl ~ctxt (type_decl : type_declaration)
  : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match type_decl.ptype_kind with
  | Ptype_variant constructors ->
    make_rank_function_for_variant_type_decl
      type_decl.ptype_name type_decl.ptype_params constructors
  | Ptype_record _label_decls ->
    (* Ignore record types as there is only one constructor by definition. *)
    []
  | Ptype_open ->
    (* Complain: we can't generate ranking functions for this type. *)
    [%str [%ocaml.error "jay_rank deriver cannot be applied to open types"]]
  | Ptype_abstract ->
    (* Complain: we can't generate ranking functions for abstract types. *)
    [%str [%ocaml.error "jay_rank deriver cannot be applied to abstract types"]]
;;

let make_rank_functions_for_type_decl_list ~ctxt
    ((_ : rec_flag), (type_decl_list : type_declaration list))
  : structure =
  (* Given a list of type declarations, we intend to produce a ranking function
     for those types. *)
  List.concat_map type_decl_list ~f:(make_rank_function_for_type_decl ~ctxt)
;;

let rank_generator () =
  Deriving.Generator.V2.make
    Deriving.Args.empty
    make_rank_functions_for_type_decl_list
;;

let gcompare_deriver =
  Deriving.add
    ~str_type_decl:(rank_generator ())
    "jay_rank"
;;
