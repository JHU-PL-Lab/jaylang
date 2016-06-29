(**
   Contains data type definitions for the AST of the Swam language.
*)

open Core_ast;;
open Uid;;

let pp_ident = Core_ast_pp.pp_ident;;
let pp_unary_operator = Core_ast_pp.pp_unary_operator;;
let pp_binary_operator = Core_ast_pp.pp_binary_operator;;


(** Expressions in the nested language. *)
type expr =
  | Record_expr of uid * expr Ident_map.t
  [@printer
     fun formatter map ->
      Format.fprintf formatter "Swan_ast.Record_expr(\"%a\")"
        (Pp_utils.pp_map Core_ast_pp.pp_ident pp_expr Ident_map.enum) map
     ]
  | Function_expr of uid * function_value
  | Int_expr of uid * int
  | Bool_expr of uid * bool
  | String_expr of uid * string
  | Ref_expr of uid * expr
  | Var_expr of uid * egg_var
  | Appl_expr of uid * expr * expr
  | Conditional_expr of uid * expr * pattern * function_value * function_value
  | If_expr of uid * expr * expr * expr
  | Deref_expr of uid * expr
  | Update_expr of uid * expr * expr
  | Binary_operation_expr of uid * expr * binary_operator * expr
  | Unary_operation_expr of uid * unary_operator * expr
  | Indexing_expr of uid * expr * expr
  | Let_expr of uid * egg_var * expr * expr
  | Projection_expr of uid * expr * ident
  | Match_expr of uid * expr * match_pair list
  [@@deriving eq, ord]

(** Function values in the nested language. *)
and function_value =
  | Function of uid * egg_var * expr
  [@@deriving eq, ord]


(** Patterns in the nested language. *)
and pattern =
  | Record_pattern of uid * pattern Ident_map.t
  (* [@printer
     fun formatter map ->
      Format.fprintf formatter "Swan_ast.Record_pattern(\"%a\")"
        (pp_map pp_ident pp_pattern Ident_map.enum) map
     ] *)
  | Fun_pattern of uid
  | Ref_pattern of uid
  | Int_pattern of uid
  | Bool_pattern of uid * bool
  | String_pattern of uid
  | Any_pattern of uid
  [@@deriving eq, ord]

and match_pair =
  | Match_pair of uid * pattern * expr
  [@@deriving eq, ord]

and egg_var =
  | Egg_var of uid * ident
  [@@deriving eq, ord]

let rec pp_expr : Format.formatter -> expr -> Ppx_deriving_runtime.unit=
  let __46 () = pp_match_pair
  and __45 () = pp_expr
  and __44 () = pp_uid
  and __43 () = pp_ident
  and __42 () = pp_expr
  and __41 () = pp_uid
  and __40 () = pp_expr
  and __39 () = pp_expr
  and __38 () = pp_egg_var
  and __37 () = pp_uid
  and __36 () = pp_expr
  and __35 () = pp_expr
  and __34 () = pp_uid
  and __33 () = pp_expr
  and __32 () = pp_unary_operator
  and __31 () = pp_uid
  and __30 () = pp_expr
  and __29 () = pp_binary_operator
  and __28 () = pp_expr
  and __27 () = pp_uid
  and __26 () = pp_expr
  and __25 () = pp_expr
  and __24 () = pp_uid
  and __23 () = pp_expr
  and __22 () = pp_uid
  and __21 () = pp_expr
  and __20 () = pp_expr
  and __19 () = pp_expr
  and __18 () = pp_uid
  and __17 () = pp_function_value
  and __16 () = pp_function_value
  and __15 () = pp_pattern
  and __14 () = pp_expr
  and __13 () = pp_uid
  and __12 () = pp_expr
  and __11 () = pp_expr
  and __10 () = pp_uid
  and __9 () = pp_egg_var
  and __8 () = pp_uid
  and __7 () = pp_expr
  and __6 () = pp_uid
  and __5 () = pp_uid
  and __4 () = pp_uid
  and __3 () = pp_uid
  and __2 () = pp_function_value
  and __1 () = pp_uid
  and __0 () =
    ((let fprintf = Format.fprintf in
      fun formatter  ->
      fun (uid, map)  ->
        Format.fprintf formatter "Swan_ast.Record_expr(\"%a, %a\")"
          (Uid.pp_uid) uid
          (Pp_utils.pp_map Core_ast_pp.pp_ident pp_expr Ident_map.enum) map)
       [@ocaml.warning "-26"]) in
  ((let open! Ppx_deriving_runtime in
     fun fmt  ->
       function
       | Record_expr (a0, a1) -> (__0 ()) fmt (a0, a1)
       | Function_expr (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Function_expr (@,";
          (((__1 ()) fmt) a0; Format.fprintf fmt ",@ "; ((__2 ()) fmt) a1);
          Format.fprintf fmt "@])")
       | Int_expr (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Int_expr (@,";
          (((__3 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           (Format.fprintf fmt "%d") a1);
          Format.fprintf fmt "@])")
       | Bool_expr (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Bool_expr (@,";
          (((__4 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           (Format.fprintf fmt "%B") a1);
          Format.fprintf fmt "@])")
       | String_expr (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.String_expr (@,";
          (((__5 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           (Format.fprintf fmt "%S") a1);
          Format.fprintf fmt "@])")
       | Ref_expr (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Ref_expr (@,";
          (((__6 ()) fmt) a0; Format.fprintf fmt ",@ "; ((__7 ()) fmt) a1);
          Format.fprintf fmt "@])")
       | Var_expr (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Var_expr (@,";
          (((__8 ()) fmt) a0; Format.fprintf fmt ",@ "; ((__9 ()) fmt) a1);
          Format.fprintf fmt "@])")
       | Appl_expr (a0,a1,a2) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Appl_expr (@,";
          ((((__10 ()) fmt) a0;
            Format.fprintf fmt ",@ ";
            ((__11 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((__12 ()) fmt) a2);
          Format.fprintf fmt "@])")
       | Conditional_expr (a0,a1,a2,a3,a4) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Conditional_expr (@,";
          ((((((__13 ()) fmt) a0;
              Format.fprintf fmt ",@ ";
              ((__14 ()) fmt) a1);
             Format.fprintf fmt ",@ ";
             ((__15 ()) fmt) a2);
            Format.fprintf fmt ",@ ";
            ((__16 ()) fmt) a3);
           Format.fprintf fmt ",@ ";
           ((__17 ()) fmt) a4);
          Format.fprintf fmt "@])")
       | If_expr (a0,a1,a2,a3) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.If_expr (@,";
          (((((__18 ()) fmt) a0;
             Format.fprintf fmt ",@ ";
             ((__19 ()) fmt) a1);
            Format.fprintf fmt ",@ ";
            ((__20 ()) fmt) a2);
           Format.fprintf fmt ",@ ";
           ((__21 ()) fmt) a3);
          Format.fprintf fmt "@])")
       | Deref_expr (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Deref_expr (@,";
          (((__22 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           ((__23 ()) fmt) a1);
          Format.fprintf fmt "@])")
       | Update_expr (a0,a1,a2) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Update_expr (@,";
          ((((__24 ()) fmt) a0;
            Format.fprintf fmt ",@ ";
            ((__25 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((__26 ()) fmt) a2);
          Format.fprintf fmt "@])")
       | Binary_operation_expr (a0,a1,a2,a3) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Binary_operation_expr (@,";
          (((((__27 ()) fmt) a0;
             Format.fprintf fmt ",@ ";
             ((__28 ()) fmt) a1);
            Format.fprintf fmt ",@ ";
            ((__29 ()) fmt) a2);
           Format.fprintf fmt ",@ ";
           ((__30 ()) fmt) a3);
          Format.fprintf fmt "@])")
       | Unary_operation_expr (a0,a1,a2) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Unary_operation_expr (@,";
          ((((__31 ()) fmt) a0;
            Format.fprintf fmt ",@ ";
            ((__32 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((__33 ()) fmt) a2);
          Format.fprintf fmt "@])")
       | Indexing_expr (a0,a1,a2) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Indexing_expr (@,";
          ((((__34 ()) fmt) a0;
            Format.fprintf fmt ",@ ";
            ((__35 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((__36 ()) fmt) a2);
          Format.fprintf fmt "@])")
       | Let_expr (a0,a1,a2,a3) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Let_expr (@,";
          (((((__37 ()) fmt) a0;
             Format.fprintf fmt ",@ ";
             ((__38 ()) fmt) a1);
            Format.fprintf fmt ",@ ";
            ((__39 ()) fmt) a2);
           Format.fprintf fmt ",@ ";
           ((__40 ()) fmt) a3);
          Format.fprintf fmt "@])")
       | Projection_expr (a0,a1,a2) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Projection_expr (@,";
          ((((__41 ()) fmt) a0;
            Format.fprintf fmt ",@ ";
            ((__42 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((__43 ()) fmt) a2);
          Format.fprintf fmt "@])")
       | Match_expr (a0,a1,a2) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Match_expr (@,";
          ((((__44 ()) fmt) a0;
            Format.fprintf fmt ",@ ";
            ((__45 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((fun x  ->
               Format.fprintf fmt "[@[<hov>";
               ignore
                 (List.fold_left
                    (fun sep  ->
                       fun x  ->
                         if sep then Format.fprintf fmt ";@ ";
                         ((__46 ()) fmt) x;
                         true) false x);
               Format.fprintf fmt "@]]")) a2);
          Format.fprintf fmt "@])"))[@ocaml.warning "-A"])
and show_expr : expr -> Ppx_deriving_runtime.string=
  fun x  -> Format.asprintf "%a" pp_expr x
and pp_function_value :
  Format.formatter -> function_value -> Ppx_deriving_runtime.unit=
  let __2 () = pp_expr
  and __1 () = pp_egg_var
  and __0 () = pp_uid in
  ((let open! Ppx_deriving_runtime in
     fun fmt  ->
       function
       | Function (a0,a1,a2) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Function (@,";
          ((((__0 ()) fmt) a0; Format.fprintf fmt ",@ "; ((__1 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((__2 ()) fmt) a2);
          Format.fprintf fmt "@])"))[@ocaml.warning "-A"])
and show_function_value : function_value -> Ppx_deriving_runtime.string=
  fun x  -> Format.asprintf "%a" pp_function_value x
and pp_pattern : Format.formatter -> pattern -> Ppx_deriving_runtime.unit=
  let __8 () = pp_uid
  and __7 () = pp_uid
  and __6 () = pp_uid
  and __5 () = pp_uid
  and __4 () = pp_uid
  and __3 () = pp_uid
  and __2 () = Pp_utils.pp_map Core_ast_pp.pp_ident pp_pattern Ident_map.enum
  and __1 () = pp_pattern
  and __0 () = pp_uid in
  ((let open! Ppx_deriving_runtime in
     fun fmt  ->
       function
       | Record_pattern (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Record_pattern (@,";
          (((__0 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           ((__2 ()) fmt) a1);
          Format.fprintf fmt "@])")
       | Fun_pattern a0 ->
         (Format.fprintf fmt "(@[<hov2>Egg_ast.Fun_pattern@ ";
          ((__3 ()) fmt) a0;
          Format.fprintf fmt "@])")
       | Ref_pattern a0 ->
         (Format.fprintf fmt "(@[<hov2>Egg_ast.Ref_pattern@ ";
          ((__4 ()) fmt) a0;
          Format.fprintf fmt "@])")
       | Int_pattern a0 ->
         (Format.fprintf fmt "(@[<hov2>Egg_ast.Int_pattern@ ";
          ((__5 ()) fmt) a0;
          Format.fprintf fmt "@])")
       | Bool_pattern (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Bool_pattern (@,";
          (((__6 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           (Format.fprintf fmt "%B") a1);
          Format.fprintf fmt "@])")
       | String_pattern a0 ->
         (Format.fprintf fmt "(@[<hov2>Egg_ast.String_pattern@ ";
          ((__7 ()) fmt) a0;
          Format.fprintf fmt "@])")
       | Any_pattern a0 ->
         (Format.fprintf fmt "(@[<hov2>Egg_ast.Any_pattern@ ";
          ((__8 ()) fmt) a0;
          Format.fprintf fmt "@])"))[@ocaml.warning "-A"])
and show_pattern : pattern -> Ppx_deriving_runtime.string=
  fun x  -> Format.asprintf "%a" pp_pattern x
and pp_match_pair :
  Format.formatter -> match_pair -> Ppx_deriving_runtime.unit=
  let __2 () = pp_expr
  and __1 () = pp_pattern
  and __0 () = pp_uid in
  ((let open! Ppx_deriving_runtime in
     fun fmt  ->
       function
       | Match_pair (a0,a1,a2) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Match_pair (@,";
          ((((__0 ()) fmt) a0; Format.fprintf fmt ",@ "; ((__1 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((__2 ()) fmt) a2);
          Format.fprintf fmt "@])"))[@ocaml.warning "-A"])
and show_match_pair : match_pair -> Ppx_deriving_runtime.string=
  fun x  -> Format.asprintf "%a" pp_match_pair x
and pp_egg_var : Format.formatter -> egg_var -> Ppx_deriving_runtime.unit=
  let __1 () = pp_ident
  and __0 () = pp_uid in
  ((let open! Ppx_deriving_runtime in
     fun fmt  ->
       function
       | Egg_var (a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Egg_ast.Egg_var (@,";
          (((__0 ()) fmt) a0; Format.fprintf fmt ",@ "; ((__1 ()) fmt) a1);
          Format.fprintf fmt "@])"))[@ocaml.warning "-A"])
and show_egg_var : egg_var -> Ppx_deriving_runtime.string=
  fun x  -> Format.asprintf "%a" pp_egg_var x


;;
