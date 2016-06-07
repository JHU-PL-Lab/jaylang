(**
   Contains data type definitions for the AST of the *nested* toy language.
*)

open Ast;;
open Ast_pp;;
open Pp_utils;;
open Uid;;

(** Expressions in the nested language. *)
type expr =
  | Record_expr of uid * expr Ident_map.t
        (* [@printer
          fun formatter map ->
            Format.fprintf formatter "Nested_ast.Record_expr(\"%a\")"
              (pp_map pp_ident pp_expr Ident_map.enum) map
        ] *)
  | Function_expr of uid * function_value
  | Int_expr of uid * int
  | Bool_expr of uid * bool
  | String_expr of uid * string
  | Ref_expr of uid * expr
  | Var_expr of uid * var
  | Appl_expr of uid * expr * expr
  | Conditional_expr of uid * expr * pattern * function_value * function_value
  | Deref_expr of uid * expr
  | Update_expr of uid * expr * expr
  | Binary_operation_expr of uid * expr * binary_operator * expr
  | Unary_operation_expr of uid * unary_operator * expr
  | Indexing_expr of uid * expr * expr
  | Let_expr of uid * var * expr * expr
  | Projection_expr of uid * expr * ident
  [@@deriving eq, ord]

(** Function values in the nested language. *)
and function_value =
  | Function of uid * var * expr
  [@@deriving eq, ord]

(** Patterns in the nested language. *)
and pattern =
  | Record_pattern of uid * pattern Ident_map.t
        (* [@printer
          fun formatter map ->
            Format.fprintf formatter "Nested_ast.Record_pattern(\"%a\")"
              (pp_map pp_ident pp_pattern Ident_map.enum) map
        ] *)
  | Fun_pattern of uid
  | Ref_pattern of uid
  | Int_pattern of uid
  | Bool_pattern of uid * bool
  | String_pattern of uid
  [@@deriving eq, ord]
;;

(*Hand-corrected generated code*)
let rec pp_expr : Format.formatter -> expr -> Ppx_deriving_runtime.unit=
  let __24 () = pp_ident
  and __23 () = pp_expr
  and __22 () = pp_expr
  and __21 () = pp_expr
  and __20 () = pp_var
  and __19 () = pp_expr
  and __18 () = pp_expr
  and __17 () = pp_expr
  and __16 () = pp_unary_operator
  and __15 () = pp_expr
  and __14 () = pp_binary_operator
  and __13 () = pp_expr
  and __12 () = pp_expr
  and __11 () = pp_expr
  and __10 () = pp_expr
  and __9 () = pp_function_value
  and __8 () = pp_function_value
  and __7 () = pp_pattern
  and __6 () = pp_expr
  and __5 () = pp_expr
  and __4 () = pp_expr
  and __3 () = pp_var
  and __2 () = pp_expr
  and __1 () = pp_function_value
  and __0 () =
    ((let fprintf = Format.fprintf in
      fun formatter  ->
      fun uid map  ->
        Format.fprintf formatter "Nested_ast.Record_expr(%a \"%a\")"
          Uid.pp_uid uid (pp_map pp_ident pp_expr Ident_map.enum) map)[@ocaml.warning
       "-26"]) in
  ((let open! Ppx_deriving_runtime in
     fun fmt  ->
       function
       | Record_expr (u, a) -> (__0 ()) fmt u a
       | Function_expr (u, a0) ->
         (Format.fprintf fmt "(@[<hov2>Nested_ast.Function_expr@ ";
          Uid.pp_uid fmt u;
          ((__1 ()) fmt) a0;
          Format.fprintf fmt "@])")
       | Int_expr (u, a0) ->
         (Format.fprintf fmt "(@[<hov2>Nested_ast.Int_expr@ ";
          Uid.pp_uid fmt u;
          (Format.fprintf fmt "%d") a0;
          Format.fprintf fmt "@])")
       | Bool_expr (u, a0) ->
         (Format.fprintf fmt "(@[<hov2>Nested_ast.Bool_expr@ ";
          Uid.pp_uid fmt u;
          (Format.fprintf fmt "%B") a0;
          Format.fprintf fmt "@])")
       | String_expr (u, a0) ->
         (Format.fprintf fmt "(@[<hov2>Nested_ast.String_expr@ ";
          Uid.pp_uid fmt u;
          (Format.fprintf fmt "%S") a0;
          Format.fprintf fmt "@])")
       | Ref_expr (u, a0) ->
         (Format.fprintf fmt "(@[<hov2>Nested_ast.Ref_expr@ ";
          Uid.pp_uid fmt u;
          ((__2 ()) fmt) a0;
          Format.fprintf fmt "@])")
       | Var_expr (u, a0) ->
         (Format.fprintf fmt "(@[<hov2>Nested_ast.Var_expr@ ";
          Uid.pp_uid fmt u;
          ((__3 ()) fmt) a0;
          Format.fprintf fmt "@])")
       | Appl_expr (u,a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Nested_ast.Appl_expr (@,";
          Uid.pp_uid fmt u;
          (((__4 ()) fmt) a0; Format.fprintf fmt ",@ "; ((__5 ()) fmt) a1);
          Format.fprintf fmt "@])")
       | Conditional_expr (u,a0,a1,a2,a3) ->
         (Format.fprintf fmt "@[<hov2>Nested_ast.Conditional_expr (@,";
          Uid.pp_uid fmt u;
          (((((__6 ()) fmt) a0;
             Format.fprintf fmt ",@ ";
             ((__7 ()) fmt) a1);
            Format.fprintf fmt ",@ ";
            ((__8 ()) fmt) a2);
           Format.fprintf fmt ",@ ";
           ((__9 ()) fmt) a3);
          Format.fprintf fmt "@])")
       | Deref_expr (u,a0) ->
         (Format.fprintf fmt "(@[<hov2>Nested_ast.Deref_expr@ ";
          Uid.pp_uid fmt u;
          ((__10 ()) fmt) a0;
          Format.fprintf fmt "@])")
       | Update_expr (u,a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Nested_ast.Update_expr (@,";
          Uid.pp_uid fmt u;
          (((__11 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           ((__12 ()) fmt) a1);
          Format.fprintf fmt "@])")
       | Binary_operation_expr (u,a0,a1,a2) ->
         (Format.fprintf fmt
            "@[<hov2>Nested_ast.Binary_operation_expr (@,";
          Uid.pp_uid fmt u;
          ((((__13 ()) fmt) a0;
            Format.fprintf fmt ",@ ";
            ((__14 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((__15 ()) fmt) a2);
          Format.fprintf fmt "@])")
       | Unary_operation_expr (u,a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Nested_ast.Unary_operation_expr (@,";
          Uid.pp_uid fmt u;
          (((__16 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           ((__17 ()) fmt) a1);
          Format.fprintf fmt "@])")
       | Indexing_expr (u,a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Nested_ast.Indexing_expr (@,";
          Uid.pp_uid fmt u;
          (((__18 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           ((__19 ()) fmt) a1);
          Format.fprintf fmt "@])")
       | Let_expr (u,a0,a1,a2) ->
         (Format.fprintf fmt "@[<hov2>Nested_ast.Let_expr (@,";
          Uid.pp_uid fmt u;
          ((((__20 ()) fmt) a0;
            Format.fprintf fmt ",@ ";
            ((__21 ()) fmt) a1);
           Format.fprintf fmt ",@ ";
           ((__22 ()) fmt) a2);
          Format.fprintf fmt "@])")
       | Projection_expr (u,a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Nested_ast.Projection_expr (@,";
          Uid.pp_uid fmt u;
          (((__23 ()) fmt) a0;
           Format.fprintf fmt ",@ ";
           ((__24 ()) fmt) a1);
          Format.fprintf fmt "@])"))[@ocaml.warning "-A"])
and show_expr : expr -> Ppx_deriving_runtime.string=
  fun x  -> Format.asprintf "%a" pp_expr x
and pp_function_value :
  Format.formatter -> function_value -> Ppx_deriving_runtime.unit=
  let __1 () = pp_expr
  and __0 () = pp_var in
  ((let open! Ppx_deriving_runtime in
     fun fmt  ->
       function
       | Function (u,a0,a1) ->
         (Format.fprintf fmt "@[<hov2>Nested_ast.Function (@,";
          Uid.pp_uid fmt u;
          (((__0 ()) fmt) a0; Format.fprintf fmt ",@ "; ((__1 ()) fmt) a1);
          Format.fprintf fmt "@])"))[@ocaml.warning "-A"])
and show_function_value : function_value -> Ppx_deriving_runtime.string=
  fun x  -> Format.asprintf "%a" pp_function_value x
and pp_pattern : Format.formatter -> pattern -> Ppx_deriving_runtime.unit=
  let __0 () =
    ((let fprintf = Format.fprintf in
      fun formatter  ->
      fun map  ->
        Format.fprintf formatter "Nested_ast.Record_pattern(\"%a\")"
          (pp_map pp_ident pp_pattern Ident_map.enum) map)[@ocaml.warning
       "-26"]) in
  ((let open! Ppx_deriving_runtime in
     fun fmt  ->
       function
       | Record_pattern (u,a) -> Uid.pp_uid fmt u; (__0 ()) fmt a
       | Fun_pattern u -> Uid.pp_uid fmt u; Format.pp_print_string fmt "Nested_ast.Fun_pattern"
       | Ref_pattern u -> Uid.pp_uid fmt u; Format.pp_print_string fmt "Nested_ast.Ref_pattern"
       | Int_pattern u -> Uid.pp_uid fmt u; Format.pp_print_string fmt "Nested_ast.Int_pattern"
       | Bool_pattern (u,a0) ->
         (Format.fprintf fmt "(@[<hov2>Nested_ast.Bool_pattern@ ";
          Uid.pp_uid fmt u;
          (Format.fprintf fmt "%B") a0;
          Format.fprintf fmt "@])")
       | String_pattern u  ->
         Uid.pp_uid fmt u;
         Format.pp_print_string fmt "Nested_ast.String_pattern")[@ocaml.warning
     "-A"])
and show_pattern : pattern -> Ppx_deriving_runtime.string=
  fun x  -> Format.asprintf "%a" pp_pattern x
