open Core
open Jhupllib
open Jayil
open Jay
open Bluejay
open Jay_instrumentation

(* let _show_expr' = Pp_utils.pp_to_string Bluejay_ast_internal_pp.pp_expr;; *)

exception Parse_failure of string

module type Error_ident = sig
  type t

  val equal : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

module type Error_value = sig
  type t

  val equal : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

module type Error_binop = sig
  type t

  val equal : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

module type Error_type = sig
  type t

  val equal : t -> t -> bool
  val subtype : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

let replace_linebreaks (str : string) : string =
  String.tr ~target:'\n' ~replacement:' ' str

module type Error = sig
  module Error_ident : Error_ident
  module Error_value : Error_value
  module Error_binop : Error_binop
  module Error_type : Error_type

  type ident
  type value
  type binop
  type type_sig

  type error_binop = {
    err_binop_left_aliases : ident list;
    err_binop_right_aliases : ident list;
    err_binop_left_val : value;
    err_binop_right_val : value;
    err_binop_operation : binop;
  }

  type error_match = {
    err_match_aliases : ident list;
    err_match_val : value;
    err_match_expected : type_sig;
    err_match_actual : type_sig;
  }

  type error_value = { err_value_aliases : ident list; err_value_val : value }

  type t =
    | Error_binop of error_binop
    | Error_match of error_match
    | Error_value of error_value

  val equal : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

module Make
    (Ident : Error_ident)
    (Value : Error_value)
    (Binop : Error_binop)
    (Type : Error_type) :
  Error
    with type ident = Ident.t
     and type value = Value.t
     and type binop = Binop.t
     and type type_sig = Type.t = struct
  module Error_ident = Ident
  module Error_value = Value
  module Error_binop = Binop
  module Error_type = Type

  type ident = Ident.t
  type value = Value.t
  type binop = Binop.t
  type type_sig = Type.t

  type error_binop = {
    err_binop_left_aliases : Ident.t list;
    err_binop_right_aliases : Ident.t list;
    err_binop_left_val : Value.t;
    err_binop_right_val : Value.t;
    err_binop_operation : Binop.t;
  }
  [@@deriving equal, to_yojson]

  type error_match = {
    err_match_aliases : Ident.t list;
    err_match_val : Value.t;
    err_match_expected : Type.t;
    err_match_actual : Type.t;
  }
  [@@deriving equal, to_yojson]

  type error_value = {
    err_value_aliases : Ident.t list;
    err_value_val : Value.t;
  }
  [@@deriving equal, to_yojson]

  type t =
    | Error_binop of error_binop
    | Error_match of error_match
    | Error_value of error_value
  [@@deriving equal, to_yojson]

  let equal = equal

  let pp_alias_list formatter aliases =
    Pp_utils.pp_concat_sep " ="
      (fun formatter x -> Ident.pp formatter x)
      formatter
      (Batteries.List.enum aliases)

  let pp_error_binop formatter err =
    let pp_left_value formatter err =
      let l_aliases = err.err_binop_left_aliases in
      let l_value = err.err_binop_left_val in
      if List.length l_aliases > 0
      then
        Format.fprintf formatter "@[* Left Val   : @[%a@ =@ %a@]@]@,"
          pp_alias_list l_aliases Value.pp l_value
      else Format.pp_print_string formatter ""
    in
    let pp_right_value formatter err =
      let r_aliases = err.err_binop_right_aliases in
      let r_value = err.err_binop_right_val in
      if List.length r_aliases > 0
      then
        Format.fprintf formatter "@[* Right Val  : @[%a@ =@ %a@]@]@,"
          pp_alias_list r_aliases Value.pp r_value
      else Format.pp_print_string formatter ""
    in
    let pp_constraint formatter err =
      Format.fprintf formatter "@[* Constraint : @[%a@]@]" Binop.pp
        err.err_binop_operation
    in
    Format.fprintf formatter "@[<v 0>%a%a%a@]" pp_left_value err pp_right_value
      err pp_constraint err

  let pp_error_match formatter err =
    let pp_value formatter err =
      let aliases = err.err_match_aliases in
      let value = err.err_match_val in
      if not @@ List.is_empty aliases
      then
        Format.fprintf formatter "@[* Value    : @[%a@ =@ %a@]@]@,"
          pp_alias_list aliases Value.pp value
      else Format.fprintf formatter "@[* Value    : @[%a@]@]@," Value.pp value
    in
    let pp_expected formatter err =
      Format.fprintf formatter "@[* Expected : @[%a@]@]@," Type.pp
        err.err_match_expected
    in
    let pp_actual formatter err =
      Format.fprintf formatter "@[* Actual   : @[%a@]@]" Type.pp
        err.err_match_actual
    in
    Format.fprintf formatter "@[<v 0>%a%a%a@]" pp_value err pp_expected err
      pp_actual err

  let pp_error_value formatter err =
    let pp_value formatter err =
      let aliases = err.err_value_aliases in
      let value = err.err_value_val in
      if not @@ List.is_empty aliases
      then
        Format.fprintf formatter "@[* Value : @[%a@ =@ %a@]@]" pp_alias_list
          aliases Value.pp value
      else Format.fprintf formatter "@[* Value : @[%a@]@]" Value.pp value
    in
    Format.fprintf formatter "@[<v 0>%a@]" pp_value err

  let pp formatter error =
    match error with
    | Error_binop err -> pp_error_binop formatter err
    | Error_match err -> pp_error_match formatter err
    | Error_value err -> pp_error_value formatter err

  let show = Pp_utils.pp_to_string pp
end

(* **** Jayil modules **** *)

module Jayil_ident :
  Error_ident with type t = Dbmc.Interpreter.Ident_with_stack.t = struct
  type t = Dbmc.Interpreter.Ident_with_stack.t

  let equal = Dbmc.Interpreter.Ident_with_stack.equal
  let pp = Dbmc.Interpreter.Ident_with_stack.pp
  let show = Dbmc.Interpreter.Ident_with_stack.show
  let to_yojson ident = `String (replace_linebreaks @@ show ident)
end

module Jayil_value : Error_value with type t = Ast.clause_body = struct
  type t = Ast.clause_body

  let equal = Ast.equal_clause_body

  (* We use the brief version in order to avoid printing out function
     bodies (which may have clauses added during instrumentation. *)
  let pp = Ast_pp_brief.pp_clause_body
  let show = Ast_pp_brief.show_clause_body
  let to_yojson value = `String (replace_linebreaks @@ show value)
end

module Jayil_binop :
  Error_binop
    with type t = Ast.clause_body * Ast.binary_operator * Ast.clause_body =
struct
  type t = Ast.clause_body * Ast.binary_operator * Ast.clause_body
  [@@deriving equal, to_yojson]

  let equal = equal

  let pp formatter (binop : t) =
    let left, op, right = binop in
    Format.fprintf formatter "%a %a %a" Ast_pp.pp_clause_body left
      Ast_pp.pp_binary_operator op Ast_pp.pp_clause_body right

  let show binop = Pp_utils.pp_to_string pp binop
  let to_yojson binop = `String (replace_linebreaks @@ show binop)
end

module Jayil_type : Error_type with type t = Ast.type_sig = struct
  type t = Ast.type_sig

  let equal = Ast.equal_type_sig
  let subtype = Ast.Type_signature.subtype
  let pp = Ast_pp.pp_type_sig
  let show = Ast_pp.show_type_sig
  let to_yojson typ = `String (Ast_pp.show_type_sig typ)
end

module Jayil_error = Make (Jayil_ident) (Jayil_value) (Jayil_binop) (Jayil_type)

(* **** Jay modules **** *)

module Jay_ident : Error_ident with type t = Jay_ast.ident = struct
  type t = Jay_ast.ident

  let equal = Jay_ast.equal_ident
  let pp = Jay_ast_pp.pp_ident
  let show = Jay_ast_pp.show_ident
  let to_yojson ident = `String (replace_linebreaks @@ show ident)
end

module Jay_value : Error_value with type t = Jay_ast.expr_desc = struct
  type t = Jay_ast.expr_desc

  let equal = Jay_ast.equal_expr_desc
  let pp = Jay_ast_pp.pp_expr_desc_without_tag
  let show = Jay_ast_pp.show_expr_desc
  let to_yojson value = `String (replace_linebreaks @@ show value)
end

module Jay_binop : Error_binop with type t = Jay_ast.expr_desc = struct
  type t = Jay_ast.expr_desc

  let equal = Jay_ast.equal_expr_desc
  let pp = Jay_ast_pp.pp_expr_desc_without_tag
  let show = Jay_ast_pp.show_expr_desc
  let to_yojson binop = `String (replace_linebreaks @@ show binop)
end

module Jay_type : Error_type with type t = Jay_ast.type_sig = struct
  type t = Jay_ast.type_sig

  let equal = Jay_ast.equal_type_sig
  let subtype _ _ = false
  let pp = Jay_ast_pp.pp_on_type
  let show = Jay_ast_pp.show_on_type
  let to_yojson typ = `String (replace_linebreaks @@ show typ)
end

module On_error = Make (Jay_ident) (Jay_value) (Jay_binop) (Jay_type)

(* **** Bluejay modules **** *)

module Bluejay_ident : Error_ident with type t = Bluejay_ast.ident = struct
  type t = Bluejay_ast.ident

  let equal = Bluejay_ast.equal_ident
  let pp = Bluejay_ast_pp.pp_ident
  let show = Bluejay_ast_pp.show_ident
  let to_yojson ident = `String (replace_linebreaks @@ show ident)
end

module Bluejay_value : Error_value with type t = Bluejay_ast.expr_desc = struct
  type t = Bluejay_ast.expr_desc

  let equal = Bluejay_ast.equal_expr_desc
  let pp = Bluejay_ast_pp.pp_expr_desc_without_tag
  let show = Bluejay_ast_pp.show_expr_desc
  let to_yojson value = `String (replace_linebreaks @@ show value)
end

module Bluejay_binop : Error_binop with type t = Bluejay_ast.expr_desc = struct
  type t = Bluejay_ast.expr_desc

  let equal = Bluejay_ast.equal_expr_desc
  let pp = Bluejay_ast_pp.pp_expr_desc_without_tag
  let show = Bluejay_ast_pp.show_expr_desc
  let to_yojson binop = `String (replace_linebreaks @@ show binop)
end

module Bluejay_type : Error_type with type t = Bluejay_ast.type_sig = struct
  type t = Bluejay_ast.type_sig

  let equal = Bluejay_ast.equal_type_sig
  let subtype _ _ = false
  let pp = Bluejay_ast_pp.pp_bluejay_type
  let show = Bluejay_ast_pp.show_bluejay_type
  let to_yojson typ = `String (replace_linebreaks @@ show typ)
end

module Bluejay_error = struct
  module Error_base =
    Make (Bluejay_ident) (Bluejay_value) (Bluejay_binop) (Bluejay_type)

  type error_type = {
    err_type_variable : Bluejay_value.t;
    err_type_expected : Bluejay_value.t;
    err_type_actual : Bluejay_value.t;
  }
  [@@deriving equal, to_yojson]

  type t =
    | Error_binop of Error_base.error_binop
    | Error_match of Error_base.error_match
    | Error_value of Error_base.error_value
    | Error_bluejay_type of error_type
  (* [@@ deriving equal, to_yojson] *)

  let pp_error_type formatter err =
    let pp_value formatter err =
      let value = err.err_type_variable in
      Format.fprintf formatter "@[* Value    : @[%a@]@]@," Bluejay_value.pp
        value
    in
    let pp_expected formatter err =
      Format.fprintf formatter "@[* Expected : @[%a@]@]@," Bluejay_value.pp
        err.err_type_expected
    in
    let pp_actual formatter err =
      Format.fprintf formatter "@[* Actual   : @[%a@]@]" Bluejay_value.pp
        err.err_type_actual
    in
    Format.fprintf formatter "@[<v 0>%a%a%a@]" pp_value err pp_expected err
      pp_actual err

  let to_yojson : t -> Yojson.Safe.t =
   fun error ->
    match error with
    | Error_binop err -> Error_base.to_yojson (Error_base.Error_binop err)
    | Error_match err -> Error_base.to_yojson (Error_base.Error_match err)
    | Error_value err -> Error_base.to_yojson (Error_base.Error_value err)
    | Error_bluejay_type err -> error_type_to_yojson err

  let pp formatter error =
    match error with
    | Error_binop err -> Error_base.pp formatter (Error_base.Error_binop err)
    | Error_match err -> Error_base.pp formatter (Error_base.Error_match err)
    | Error_value err -> Error_base.pp formatter (Error_base.Error_value err)
    | Error_bluejay_type err -> pp_error_type formatter err

  let show = Pp_utils.pp_to_string pp
end

(* **** Jayil error cleanup **** *)

let jayil_error_remove_instrument_vars
    (jayil_jay_maps : Jayil_instrumentation_maps.t) (error : Jayil_error.t) :
    Jayil_error.t =
  let remove_instrument_aliases aliases =
    (* let () = print_endline "This is aliases pre-transform" in
       let () =
         aliases
         |> List.iter ~f:(fun x -> print_endline @@ Dbmc.Interpreter.show_ident_with_stack x)
       in *)
    List.filter
      ~f:(fun alias_with_stack ->
        let alias =
          Dbmc.Interpreter.ident_from_id_with_stack alias_with_stack
        in
        not
        @@ Jayil_instrumentation_maps.is_var_instrumenting jayil_jay_maps alias)
      aliases
  in
  match error with
  | Error_binop err ->
      let left_aliases' =
        remove_instrument_aliases err.err_binop_left_aliases
      in
      let right_aliases' =
        remove_instrument_aliases err.err_binop_right_aliases
      in
      let _, op, _ = err.err_binop_operation in
      let l_operand' =
        match left_aliases' with
        | [] -> err.err_binop_left_val
        | (v, _) :: _ -> Ast.Var_body (Var (v, None))
      in
      let r_operand' =
        match right_aliases' with
        | [] -> err.err_binop_right_val
        | (v, _) :: _ -> Ast.Var_body (Var (v, None))
      in
      Error_binop
        {
          err with
          err_binop_left_aliases = left_aliases';
          err_binop_right_aliases = right_aliases';
          err_binop_operation = (l_operand', op, r_operand');
        }
  | Error_match err ->
      let match_aliases = err.err_match_aliases in
      Error_match
        { err with err_match_aliases = remove_instrument_aliases match_aliases }
  | Error_value err ->
      let aliases = err.err_value_aliases in
      Error_value
        { err with err_value_aliases = remove_instrument_aliases aliases }

(* **** Jayil to jay error translation **** *)

(* Helper function that returns a jay binop, depending on the jayil
   binary operator. *)
let jayil_to_jay_binop (jayil_binop : Ast.binary_operator) :
    Jay_ast.expr_desc -> Jay_ast.expr_desc -> Jay_ast.expr =
  match jayil_binop with
  | Ast.Binary_operator_plus -> fun e1 e2 -> Jay_ast.Plus (e1, e2)
  | Ast.Binary_operator_minus -> fun e1 e2 -> Jay_ast.Minus (e1, e2)
  | Ast.Binary_operator_times -> fun e1 e2 -> Jay_ast.Times (e1, e2)
  | Ast.Binary_operator_divide -> fun e1 e2 -> Jay_ast.Divide (e1, e2)
  | Ast.Binary_operator_modulus -> fun e1 e2 -> Jay_ast.Modulus (e1, e2)
  | Ast.Binary_operator_equal_to -> fun e1 e2 -> Jay_ast.Equal (e1, e2)
  | Ast.Binary_operator_not_equal_to -> fun e1 e2 -> Jay_ast.Neq (e1, e2)
  | Ast.Binary_operator_less_than -> fun e1 e2 -> Jay_ast.LessThan (e1, e2)
  | Ast.Binary_operator_less_than_or_equal_to ->
      fun e1 e2 -> Jay_ast.Leq (e1, e2)
  | Ast.Binary_operator_and -> fun e1 e2 -> Jay_ast.And (e1, e2)
  | Ast.Binary_operator_or -> fun e1 e2 -> Jay_ast.Or (e1, e2)

let jayil_to_jay_error (jayil_inst_maps : Jayil_instrumentation_maps.t)
    (jayil_jay_maps : Jay_to_jayil_maps.t)
    (interp_session : Dbmc.Interpreter.session)
    (final_env : Dbmc.Interpreter.denv) (jayil_err : Jayil_error.t) :
    On_error.t list =
  (* Helper functions *)
  let open Jay_ast in
  let get_pre_inst_id x =
    Jayil_instrumentation_maps.get_pre_inst_var_opt jayil_inst_maps x
    |> Option.value ~default:x
  in
  let jayil_to_jay_expr =
    Jay_to_jayil_maps.get_natodefa_equivalent_expr_exn jayil_jay_maps
  in
  let jayil_to_jay_aliases aliases =
    aliases
    |> List.filter_map ~f:(fun alias ->
           let alias' = get_pre_inst_id alias in
           let e_desc = jayil_to_jay_expr alias' in
           match e_desc.body with Var _ | Error _ -> Some e_desc | _ -> None)
    |> Batteries.List.unique
  in
  let get_idents_from_aliases (aliases : expr_desc list) =
    aliases
    |> List.filter_map ~f:(fun ed ->
           match ed.body with Var x -> Some x | _ -> None)
  in
  let jayil_to_jay_value (aliases : Ast.ident list) : expr_desc =
    let last_var =
      try List.last_exn aliases
      with Invalid_argument _ ->
        raise @@ Jhupllib.Utils.Invariant_failure "Can't have empty alias list!"
    in
    jayil_to_jay_expr @@ get_pre_inst_id last_var
  in
  let jayil_to_jay_type (typ : Ast.type_sig) : Jay_ast.type_sig =
    match typ with
    | Ast.Top_type -> TopType
    | Ast.Int_type -> IntType
    | Ast.Bool_type -> BoolType
    | Ast.Fun_type -> FunType
    | Ast.Rec_type lbls ->
        Jay_to_jayil_maps.get_type_from_idents jayil_jay_maps lbls
    | Ast.Bottom_type ->
        raise
        @@ Jhupllib.Utils.Invariant_failure
             (Printf.sprintf "Bottom type not in jay")
  in
  (* Jayil to jay *)
  match jayil_err with
  | Jayil_error.Error_binop err ->
      let l_aliases =
        err.err_binop_left_aliases
        |> List.map ~f:Dbmc.Interpreter.ident_from_id_with_stack
      in
      let r_aliases =
        err.err_binop_right_aliases
        |> List.map ~f:Dbmc.Interpreter.ident_from_id_with_stack
      in
      let l_aliases_jay = jayil_to_jay_aliases l_aliases in
      let r_aliases_jay = jayil_to_jay_aliases r_aliases in
      let _, op, _ = err.err_binop_operation in
      let l_value = jayil_to_jay_value l_aliases in
      let r_value = jayil_to_jay_value r_aliases in
      let constraint_expr =
        let left_expr =
          if List.is_empty l_aliases_jay
          then l_value
          else List.hd_exn l_aliases_jay
        in
        let right_expr =
          if List.is_empty r_aliases_jay
          then r_value
          else List.hd_exn r_aliases_jay
        in
        new_expr_desc @@ jayil_to_jay_binop op left_expr right_expr
      in
      [
        Error_binop
          {
            err_binop_left_aliases = get_idents_from_aliases l_aliases_jay;
            err_binop_right_aliases = get_idents_from_aliases r_aliases_jay;
            err_binop_left_val = l_value;
            err_binop_right_val = r_value;
            err_binop_operation = constraint_expr;
          };
      ]
  | Jayil_error.Error_match err ->
      let aliases =
        err.err_match_aliases
        |> List.map ~f:Dbmc.Interpreter.ident_from_id_with_stack
      in
      (* let () = print_endline "Printing aliases" in
         let () = List.iter (fun a -> print_endline @@ Ast.show_ident a) aliases in
         let () = print_endline @@ show_expr ((jayil_to_jay_value aliases).body) in *)
      [
        Error_match
          {
            err_match_aliases =
              get_idents_from_aliases @@ jayil_to_jay_aliases aliases;
            err_match_val = jayil_to_jay_value aliases;
            err_match_expected = jayil_to_jay_type err.err_match_expected;
            err_match_actual = jayil_to_jay_type err.err_match_actual;
          };
      ]
  | Jayil_error.Error_value err -> (
      let aliases =
        err.err_value_aliases
        |> List.map ~f:Dbmc.Interpreter.ident_from_id_with_stack
      in
      (* let () = print_endline "Printing aliases" in
         let () = List.iter (fun a -> print_endline @@ Ast.show_ident a) aliases in *)
      let err_val_edesc = jayil_to_jay_value aliases in
      match err_val_edesc.body with
      | Match (subj, pat_ed_lst) -> (
          let jayil_var_opt =
            Jay_to_jayil_maps.get_odefa_var_opt_from_natodefa_expr
              jayil_jay_maps subj
          in
          match jayil_var_opt with
          | Some (Var (x, _)) ->
              let dv1, stk = Ast.Ident_map.find x final_env in
              let v = Dbmc.Interpreter.value_of_dvalue dv1 in
              let alias_graph = interp_session.alias_graph in
              let jayil_aliases_raw =
                Sato_tools.find_alias alias_graph (x, stk)
              in
              let jayil_aliases =
                jayil_aliases_raw |> List.map ~f:(fun (x, _) -> x) |> List.rev
              in
              let actual_type = Sato_tools.get_value_type v in
              let errors =
                let mapper (pat, _) =
                  let expected_type =
                    Sato_tools.get_expected_type_from_pattern pat
                  in
                  On_error.Error_match
                    {
                      err_match_aliases =
                        get_idents_from_aliases
                        @@ jayil_to_jay_aliases jayil_aliases;
                      err_match_val = jayil_to_jay_value jayil_aliases;
                      err_match_expected = expected_type;
                      err_match_actual = jayil_to_jay_type actual_type;
                    }
                in
                List.map ~f:mapper pat_ed_lst
              in
              errors
          | None -> failwith "Should have found an jayil var!")
      | _ ->
          [
            Error_value
              {
                err_value_aliases =
                  get_idents_from_aliases @@ jayil_to_jay_aliases aliases;
                err_value_val = err_val_edesc;
              };
          ])

let jayil_to_bluejay_error_simple
    (jayil_inst_maps : Jayil_instrumentation_maps.t)
    (jayil_jay_maps : Jay_to_jayil_maps.t)
    (bluejay_jay_maps : Bluejay_to_jay_maps.t)
    (interp_session : Dbmc.Interpreter.session)
    (final_env : Dbmc.Interpreter.denv) (jayil_err : Jayil_error.t) :
    Bluejay_error.t list =
  let open Bluejay in
  let jay_errors =
    jayil_to_jay_error jayil_inst_maps jayil_jay_maps interp_session final_env
      jayil_err
  in
  let transform_type_sig (t : Jay_ast.type_sig) : Bluejay_ast.type_sig =
    match t with
    | TopType -> TopType
    | IntType -> IntType
    | BoolType -> BoolType
    | FunType -> FunType
    | RecType r -> RecType r
    | ListType -> ListType
    | VariantType (Variant_label variant_label) ->
        VariantType (Variant_label variant_label)
  in
  let jay_expr_to_bluejay nat =
    nat |> Bluejay_ast_internal.from_jay_expr_desc
    |> Bluejay_to_jay_maps.sem_bluejay_from_core_bluejay bluejay_jay_maps
    |> Bluejay_to_jay_maps.syn_bluejay_from_sem_bluejay bluejay_jay_maps
    |> Bluejay_ast_internal.from_internal_expr_desc
  in
  let transform_one_error (nat_err : On_error.t) : Bluejay_error.t =
    match nat_err with
    | Error_binop
        {
          err_binop_left_aliases;
          err_binop_right_aliases;
          err_binop_left_val;
          err_binop_right_val;
          err_binop_operation;
        } ->
        let left_val = jay_expr_to_bluejay err_binop_left_val in
        let right_val = jay_expr_to_bluejay err_binop_right_val in
        let binop = jay_expr_to_bluejay err_binop_operation in
        Error_binop
          {
            err_binop_left_aliases;
            err_binop_right_aliases;
            err_binop_left_val = left_val;
            err_binop_right_val = right_val;
            err_binop_operation = binop;
          }
    | Error_match
        {
          err_match_aliases;
          err_match_val;
          err_match_expected;
          err_match_actual;
        } ->
        Error_match
          {
            err_match_aliases;
            err_match_val = jay_expr_to_bluejay err_match_val;
            err_match_expected = transform_type_sig err_match_expected;
            err_match_actual = transform_type_sig err_match_actual;
          }
    | Error_value { err_value_aliases; err_value_val } ->
        Error_value
          {
            err_value_aliases;
            err_value_val = jay_expr_to_bluejay err_value_val;
          }
  in
  List.map ~f:transform_one_error jay_errors

let jayil_to_bluejay_error (jayil_inst_maps : Jayil_instrumentation_maps.t)
    (jayil_jay_maps : Jay_to_jayil_maps.t)
    (bluejay_jay_maps : Bluejay_to_jay_maps.t)
    (interp_session : Dbmc.Interpreter.session)
    (err_source : Bluejay_ast.expr_desc) (jayil_err : Jayil_error.t) :
    Bluejay_error.t list =
  (* Helper functions *)
  let open Jay_ast in
  let get_pre_inst_id x =
    Jayil_instrumentation_maps.get_pre_inst_var_opt jayil_inst_maps x
    |> Option.value ~default:x
  in
  let jayil_to_jay_expr =
    Jay_to_jayil_maps.get_natodefa_equivalent_expr jayil_jay_maps
  in
  let jayil_to_jay_aliases aliases =
    aliases
    |> List.filter_map ~f:(fun alias ->
           let alias' = get_pre_inst_id alias in
           match jayil_to_jay_expr alias' with
           | None -> None
           | Some e_desc -> (
               match e_desc.body with
               | Var _ | Error _ -> Some e_desc
               | _ -> None))
    |> Batteries.List.unique
  in
  (* Jayil to jay *)
  match jayil_err with
  | Jayil_error.Error_binop _ | Jayil_error.Error_match _ ->
      failwith "Should have been handled by jayil_to_bluejay_error_simple"
  | Jayil_error.Error_value err ->
      let jayil_aliases_with_stack = err.err_value_aliases in
      (* let () =
           List.iter
             ~f:(fun is -> print_endline @@ Dbmc.Interpreter.show_ident_with_stack is)
             jayil_aliases_with_stack
         in *)
      (* This is the stack of the "false" value that indicates where the error
         is located. *)
      let relevant_stk =
        match List.last jayil_aliases_with_stack with
        | Some (_, final_stk) -> final_stk
        | None -> failwith "Should have at least one element in the list!"
      in
      (* Restoring the Jay version of the error indicator (the false value) *)
      let core_nat_aliases =
        jayil_aliases_with_stack
        |> List.map ~f:Dbmc.Interpreter.ident_from_id_with_stack
        |> jayil_to_jay_aliases
      in
      let sem_nat_aliases =
        core_nat_aliases
        |> List.map ~f:Bluejay_ast_internal.from_jay_expr_desc
        |> List.map
             ~f:
               (Bluejay_to_jay_maps.sem_bluejay_from_core_bluejay
                  bluejay_jay_maps)
      in
      (* let () =
           List.iter
             ~f:(fun ed -> print_endline @@ show_expr' ed.body)
             sem_nat_aliases
         in *)
      (* Getting the expression that triggered the error *)
      let sem_val_exprs =
        sem_nat_aliases
        |> List.filter_map
             ~f:
               (Bluejay_to_jay_maps.get_value_expr_from_sem_expr
                  bluejay_jay_maps)
      in
      (* let () =
           List.iter
             ~f:(fun ed -> print_endline @@ show_expr' ed.body)
             sem_val_exprs
         in *)
      (* Getting the jayil variable corresponding to the error-triggering value *)
      let jayil_vars =
        sem_val_exprs
        |> List.filter_map
             ~f:
               (Bluejay_to_jay_maps.get_core_expr_from_sem_expr bluejay_jay_maps)
        |> List.map ~f:Bluejay_ast_internal.to_jay_expr_desc
        |> List.filter_map
             ~f:
               (Jay_to_jayil_maps.get_odefa_var_opt_from_natodefa_expr
                  jayil_jay_maps)
      in
      (* let () =
           List.iter
             ~f:(fun (Var (x, _)) -> print_endline @@ Ast.show_ident x)
             jayil_vars
         in *)
      (* TODO: This is hacky. There has got to be a better way of doing this *)
      (* let stacks =
           jayil_aliases_with_stack
           |> List.map ~f:Dbmc.Interpreter.stack_from_id_with_stack
         in *)
      let alias_graph = interp_session.alias_graph in
      (* let jayil_vars_with_stack =
           jayil_vars
           |> List.map ~f:(fun (Var (x, _)) -> x)
           |> List.map
               ~f:(fun x -> List.map ~f:(fun stk -> (x, stk)) stacks)
           |> List.concat
           |> List.map ~f:(Sato_tools.find_alias alias_graph)
           |> List.concat
         in *)
      let jayil_vars_with_stack : Dbmc.Interpreter.Ident_with_stack.t list =
        jayil_vars
        |> List.map ~f:(fun (Var (x, _)) -> x)
        |> List.map ~f:(Sato_tools.find_alias_without_stack alias_graph)
        |> List.concat |> List.concat
        (* TODO: This might be buggy; we're assuming that all values that might
           trigger the error must have an alias that is defined within the same
           block as the error indicator (i.e. the said alias and the indicator
           will have the same stack). *)
        |> List.filter ~f:(fun (_, stk) ->
               Dbmc.Concrete_stack.equal stk relevant_stk)
        |> List.map ~f:(Sato_tools.find_alias alias_graph)
        |> List.concat
      in
      (* let keys = Batteries.List.of_enum @@ Ast.Ident_map.keys final_env in
         let () = List.iter ~f:(fun k -> print_endline @@ show_ident k) keys in *)
      (* let () = failwith @@ string_of_bool @@ List.is_empty @@ List.concat jayil_vars_with_stack in *)
      (* let () =
         List.iter
           ~f:(fun x -> print_endline @@ Dbmc.Interpreter.show_ident_with_stack x)
           jayil_vars_with_stack in *)
      let rec find_val
          (vdef_mapping :
            ( Dbmc.Interpreter.Ident_with_stack.t,
              Ast.clause_body * Dbmc.Interpreter.dvalue )
            Hashtbl.t) (xs : Dbmc.Interpreter.Ident_with_stack.t list) :
          Dbmc.Interpreter.dvalue =
        (* : Dbmc.Interpreter.dvalue * Dbmc.Interpreter.Ident_with_stack.t = *)
        match xs with
        | [] -> failwith "Should at least find one value!"
        | hd :: tl -> (
            (* let () = print_endline @@ Dbmc.Interpreter.show_ident_with_stack hd in *)
            let found = Hashtbl.find vdef_mapping hd in
            match found with
            | Some (_, dv) -> dv
            | None -> find_val vdef_mapping tl)
      in
      let err_val = find_val interp_session.val_def_map jayil_vars_with_stack in
      (* let val_exprs =
           let relevant_tags = bluejay_jay_maps.syn_tags in
           jayil_vars_with_stack
           |> List.map ~f:(fun (x, _) -> x)
           |> List.filter_map ~f:jayil_to_jay_expr
           |> List.map ~f:Bluejay_ast_internal.from_jay_expr_desc
           |> List.map ~f:(Bluejay_to_jay_maps.sem_bluejay_from_core_bluejay bluejay_jay_maps)
           |> List.map ~f:(Bluejay_to_jay_maps.syn_bluejay_from_sem_bluejay bluejay_jay_maps)
           |> List.filter ~f:(fun ed -> List.mem relevant_tags ed.tag ~equal:(=))
           |> List.map ~f:Bluejay_ast_internal.from_internal_expr_desc
           |> Batteries.List.unique
         in *)
      (* let () = List.iter ~f:(fun x -> print_endline @@ Bluejay_ast_pp.show_expr_desc x) sem_val_expr_lst in *)
      let v = Dbmc.Interpreter.value_of_dvalue err_val in
      (* Here we need to refine the expected type; since they could be aliases to
         the type value rather than the types themselves.
         e.g. let x = bool in let (y : x) = true in y and false *)
      let expected_type, err_var =
        match err_source.body with
        | LetWithType (x, _, _, t) ->
            let ret =
              Var x |> Bluejay_ast_internal.new_expr_desc
              |> Bluejay_ast_internal.from_internal_expr_desc
            in
            (t, ret)
        | LetFunWithType (Funsig (x, _, _), _, t) ->
            let ret =
              Var x |> Bluejay_ast_internal.new_expr_desc
              |> Bluejay_ast_internal.from_internal_expr_desc
            in
            (t, ret)
        | LetRecFunWithType (fsigs, _, ts) -> (
            let precise_lookup =
              core_nat_aliases
              |> List.filter_map ~f:(fun alias ->
                     match alias.body with Error idnt -> Some idnt | _ -> None)
              |> List.filter_map ~f:(fun x ->
                     Ident_map.find_opt x bluejay_jay_maps.error_to_rec_fun_type)
            in
            let precise_type =
              if List.is_empty precise_lookup
              then failwith "No type found!"
              else
                List.hd_exn precise_lookup
                |> Bluejay_to_jay_maps.syn_bluejay_from_sem_bluejay
                     bluejay_jay_maps
                (* let () = failwith @@ On_to_jayil.show_expr_desc x
                   in *)
                (* let () = failwith "1" in *)
                |> Bluejay_ast_internal.from_internal_expr_desc
            in
            let fsig_with_types = List.zip_exn fsigs ts in
            let var_opt =
              List.fold
                ~f:(fun acc (Funsig (x, _, _), t) ->
                  if Bluejay_ast.equal_expr_desc t precise_type
                  then Some x
                  else acc)
                ~init:None fsig_with_types
            in
            match var_opt with
            | Some x ->
                let ret =
                  Var x |> Bluejay_ast_internal.new_expr_desc
                  |> Bluejay_ast_internal.from_internal_expr_desc
                in
                (precise_type, ret)
            | None -> failwith "Should have found the type signature!")
        | _ -> failwith "Shouldn't be here!"
      in
      let expected_type_internal =
        expected_type |> Bluejay_ast_internal.to_internal_expr_desc
      in
      (* TODO: How to handle this? Is this worth the effort? *)
      (* let rec type_resolution (ed : Bluejay_ast_internal.syn_bluejay_edesc) :
             Bluejay_ast_internal.syn_bluejay_edesc =
           let open Bluejay_ast_internal in
           let tag = ed.tag in
           let e = ed.body in
           match e with
           | TypeVar _ | TypeInt | TypeBool -> ed
           | TypeRecord r ->
               let body' = TypeRecord (Ident_map.map type_resolution r) in
               { tag; body = body' }
           | TypeList led ->
               let body' = TypeList (type_resolution led) in
               { tag; body = body' }
           | TypeArrow (ed1, ed2) ->
               let ed1' = type_resolution ed1 in
               let ed2' = type_resolution ed2 in
               let body' = TypeArrow (ed1', ed2') in
               { tag; body = body' }
           | TypeArrowD ((x, ed1), ed2) ->
               let ed1' = type_resolution ed1 in
               let ed2' = type_resolution ed2 in
               let body' = TypeArrowD ((x, ed1'), ed2') in
               { tag; body = body' }
           | TypeUnion (ed1, ed2) ->
               let ed1' = type_resolution ed1 in
               let ed2' = type_resolution ed2 in
               let body' = TypeUnion (ed1', ed2') in
               { tag; body = body' }
           | TypeIntersect (ed1, ed2) ->
               let ed1' = type_resolution ed1 in
               let ed2' = type_resolution ed2 in
               let body' = TypeIntersect (ed1', ed2') in
               { tag; body = body' }
           | TypeSet (ed, pred) ->
               let ed' = type_resolution ed in
               let body' = TypeSet (ed', pred) in
               { tag; body = body' }
           | TypeRecurse (rec_id, ed) ->
               let ed' = type_resolution ed in
               let body' = TypeRecurse (rec_id, ed') in
               { tag; body = body' }
           | _ -> (
               (* Potential FIXME: A lot of things could go wrong here... *)
               let jayil_vars =
                 ed
                 |> Bluejay_to_jay_maps.sem_from_syn bluejay_jay_maps
                 |> Bluejay_to_jay_maps.get_core_expr_from_sem_expr
                      bluejay_jay_maps
                 |> Option.value_exn |> Bluejay_ast_internal.to_jay_expr_desc
                 |> Jay_to_jayil_maps.get_odefa_var_opt_from_natodefa_expr
                      jayil_jay_maps
                 |> Option.value_exn
                 |> (fun (Ast.Var (x, _)) ->
                      Sato_tools.find_alias_without_stack alias_graph x)
                 |> List.concat
                 |> List.filter ~f:(fun (_, stk) ->
                        Dbmc.Concrete_stack.equal stk relevant_stk)
                 |> List.map ~f:(Sato_tools.find_alias alias_graph)
                 |> List.concat
               in
               let val_exprs =
                 let relevant_tags = bluejay_jay_maps.syn_tags in
                 jayil_vars
                 |> List.map ~f:(fun (x, _) -> x)
                 |> List.filter_map ~f:jayil_to_jay_expr
                 |> List.map ~f:Bluejay_ast_internal.from_jay_expr_desc
                 |> List.map
                      ~f:
                        (Bluejay_to_jay_maps.sem_bluejay_from_core_bluejay
                           bluejay_jay_maps)
                 |> List.map
                      ~f:
                        (Bluejay_to_jay_maps.syn_bluejay_from_sem_bluejay
                           bluejay_jay_maps)
                 |> List.filter ~f:(fun ed ->
                        List.mem relevant_tags ed.tag ~equal:( = ))
                 |> Batteries.List.unique
               in
               let type_exprs =
                 val_exprs |> List.filter ~f:Bluejay_ast_internal.is_type_expr
               in
               (* let () =
                    List.iter ~f:(fun ed -> print_endline @@ Bluejay_ast_pp.show_expr_desc (Bluejay_ast_internal.from_internal_expr_desc ed)) val_exprs
                  in *)
               let val_expr_cleansed_opt =
                 type_exprs |> List.map ~f:type_resolution |> List.hd
               in
               match val_expr_cleansed_opt with
               | None -> List.hd_exn val_exprs
               | Some l -> l)
         in *)
      (* NOTE: The case where a variable might take on different types for 
         different instances might be problematic here. *)
      let check_aliases_for_type (ed : Bluejay_ast_internal.syn_bluejay_edesc) :
          Bluejay_ast_internal.syn_bluejay_edesc option =
        let jayil_vars =
          ed
          |> Bluejay_to_jay_maps.sem_from_syn bluejay_jay_maps
          |> Bluejay_to_jay_maps.get_core_expr_from_sem_expr bluejay_jay_maps
          |> Option.value_exn |> Bluejay_ast_internal.to_jay_expr_desc
          |> Jay_to_jayil_maps.get_odefa_var_opt_from_natodefa_expr
               jayil_jay_maps
          |> Option.value_exn
          |> (fun (Ast.Var (x, _)) ->
               Sato_tools.find_alias_without_stack alias_graph x)
          |> List.concat
          (* TODO: Rethink the strategy here *)
          |> List.filter ~f:(fun (_, stk) ->
                 Dbmc.Concrete_stack.equal stk relevant_stk)
          |> List.map ~f:(Sato_tools.find_alias alias_graph)
          |> List.concat
        in
        let val_exprs =
          let relevant_tags = bluejay_jay_maps.syn_tags in
          jayil_vars
          |> List.map ~f:(fun (x, _) -> x)
          |> List.filter_map ~f:jayil_to_jay_expr
          |> List.map ~f:Bluejay_ast_internal.from_jay_expr_desc
          |> List.map
               ~f:
                 (Bluejay_to_jay_maps.sem_bluejay_from_core_bluejay
                    bluejay_jay_maps)
          |> List.map
               ~f:
                 (Bluejay_to_jay_maps.syn_bluejay_from_sem_bluejay
                    bluejay_jay_maps)
          |> List.filter ~f:(fun ed ->
                 List.mem relevant_tags ed.tag ~equal:( = ))
          |> Batteries.List.unique
        in
        let type_exprs =
          val_exprs |> List.filter ~f:Bluejay_ast_internal.is_type_expr
        in
        List.hd type_exprs
      in
      let rec type_resolution (ed : Bluejay_ast_internal.syn_bluejay_edesc) :
          Bluejay_ast_internal.syn_bluejay_edesc =
        let open Bluejay_ast_internal in
        let type_expr_opt = check_aliases_for_type ed in
        let resolve_type ted = 
          let tag = ted.tag in
          let e = ted.body in
          match e with
          | TypeVar _ | TypeInt | TypeBool -> ted
          | TypeRecord r ->
              let body' = TypeRecord (Ident_map.map type_resolution r) in
              { tag; body = body' }
          | TypeList led ->
              let body' = TypeList (type_resolution led) in
              { tag; body = body' }
          | TypeArrow (ed1, ed2) ->
              let ed1' = type_resolution ed1 in
              let ed2' = type_resolution ed2 in
              let body' = TypeArrow (ed1', ed2') in
              { tag; body = body' }
          | TypeArrowD ((x, ed1), ed2) ->
              let ed1' = type_resolution ed1 in
              let ed2' = type_resolution ed2 in
              let body' = TypeArrowD ((x, ed1'), ed2') in
              { tag; body = body' }
          | TypeUnion (ed1, ed2) ->
              let ed1' = type_resolution ed1 in
              let ed2' = type_resolution ed2 in
              let body' = TypeUnion (ed1', ed2') in
              { tag; body = body' }
          | TypeIntersect (ed1, ed2) ->
              let ed1' = type_resolution ed1 in
              let ed2' = type_resolution ed2 in
              let body' = TypeIntersect (ed1', ed2') in
              { tag; body = body' }
          | TypeSet (ed, pred) ->
              let ed' = type_resolution ed in
              let body' = TypeSet (ed', pred) in
              { tag; body = body' }
          | TypeRecurse (rec_id, ed) ->
              let ed' = type_resolution ed in
              let body' = TypeRecurse (rec_id, ed') in
              { tag; body = body' }
          | _ -> failwith "Should be working with a type expression!" 
        in        
        let resolve_non_type ed = 
          let tag = ed.tag in
          let e = ed.body in
          let transform_funsig (f_sig : syntactic_only funsig) = 
            let (Funsig (f, args, f_body)) = f_sig in
            let f_body' = type_resolution f_body in
            Funsig (f, args, f_body')
          in
          match e with
          | Int _ | Bool _ | Var _ | Input -> ed
          | Function (args, fed) ->
            {tag; body = Function (args, type_resolution fed)}
          | Appl (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Appl (ed1', ed2')}
          | Let (x, ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Let (x, ed1', ed2')}
          | LetRecFun (f_sigs, ed) ->
            let f_sigs' = List.map ~f:transform_funsig f_sigs in
            let ed' = type_resolution ed in
            {tag; body = LetRecFun (f_sigs', ed')}
          | LetFun (f_sig, ed) ->
            let f_sig' = transform_funsig f_sig in
            let ed' = type_resolution ed in
            {tag; body = LetFun (f_sig', ed')}
          | LetWithType (x, ed1, ed2, ed3) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            let ed3' = type_resolution ed3 in
            {tag; body = LetWithType (x, ed1', ed2', ed3')}
          | LetRecFunWithType (f_sigs, ed, ts) ->
            let f_sigs' = List.map ~f:transform_funsig f_sigs in
            let ed' = type_resolution ed in
            let ts' = List.map ~f:type_resolution ts in
            {tag; body = LetRecFunWithType (f_sigs', ed', ts')}
          | LetFunWithType (f_sig, ed, t) ->
            let f_sig' = transform_funsig f_sig in
            let ed' = type_resolution ed in
            let t' = type_resolution t in
            {tag; body = LetFunWithType (f_sig', ed', t')}
          | Plus (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Plus (ed1', ed2')}
          | Minus (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Minus (ed1', ed2')}
          | Times (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Times (ed1', ed2')}
          | Divide (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Divide (ed1', ed2')}
          | Modulus (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Modulus (ed1', ed2')}
          | Equal (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Equal (ed1', ed2')}
          | Neq (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Neq (ed1', ed2')}
          | LessThan (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = LessThan (ed1', ed2')}
          | Leq (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Leq (ed1', ed2')}
          | GreaterThan (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = GreaterThan (ed1', ed2')}
          | Geq (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Geq (ed1', ed2')}
          | And (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = And (ed1', ed2')}
          | Or (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = Or (ed1', ed2')}
          | Not ed ->
            let ed' = type_resolution ed in
            {tag; body = Not ed'}
          | If (ed1, ed2, ed3) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            let ed3' = type_resolution ed3 in
            {tag; body = If (ed1', ed2', ed3')}
          | Record r ->
            let r' = Ident_map.map type_resolution r in
            {tag; body = Record r'}
          | RecordProj (ed, l) ->
            let ed' = type_resolution ed in
            {tag; body = RecordProj (ed', l)}
          | Match (ed, pat_ed_lst) ->
            let ed' = type_resolution ed in
            let pat_ed_lst' = 
              List.map 
                ~f:(fun (pat, ed) -> (pat, type_resolution ed))
                pat_ed_lst
            in
            {tag; body = Match (ed', pat_ed_lst')}
          | VariantExpr (v_lbl, ed) ->
            let ed' = type_resolution ed in
            {tag; body = VariantExpr (v_lbl, ed')}
          | List eds ->
            let eds' = List.map ~f:type_resolution eds in
            {tag; body = List eds'}
          | ListCons (ed1, ed2) ->
            let ed1' = type_resolution ed1 in
            let ed2' = type_resolution ed2 in
            {tag; body = ListCons (ed1', ed2')}
          | Assert ed ->
            let ed' = type_resolution ed in
            {tag; body = Assert ed'}
          | Assume ed ->
            let ed' = type_resolution ed in
            {tag; body = Assume ed'}
          | TypeError _ -> failwith "resolve_non_type: I'm not sure why you're here..."
          | _ -> failwith "resolve_non_type: Should be working with a non-type expression!" 
        in
        match type_expr_opt with
        | Some t -> 
          let () = print_endline "Found type aliases!" in
          (* let () = 
            print_endline @@ Bluejay_ast.show_expr_desc 
            @@ Bluejay_ast_internal.from_internal_expr_desc t in *)
          resolve_type t
        | None -> 
          let () = print_endline "No type aliases!" in
          resolve_non_type ed
      in
      let resolved_expected_type = type_resolution expected_type_internal in
      let find_tag =
        sem_nat_aliases
        |> List.filter_map ~f:(fun alias ->
               Bluejay_to_jay_maps.Intermediate_expr_desc_map.find_opt alias
                 bluejay_jay_maps.error_to_expr_tag)
      in
      let tag =
        if List.is_empty find_tag
        then failwith "No tag found!"
        else List.hd_exn find_tag
      in
      let new_t =
        match v with
        | Value_int _ -> Bluejay_ast_internal.new_expr_desc @@ TypeInt
        | Value_bool _ -> Bluejay_ast_internal.new_expr_desc @@ TypeBool
        | _ -> failwith "TBI!"
      in
      (* let expected_type_internal =
           expected_type
           |> Bluejay_ast_internal.to_internal_expr_desc
         in *)
      let actual_type =
        (* let () = print_endline @@ "expected: " ^ string_of_int expected_type_internal.tag in *)
        (* let () = print_endline @@ "actual: " ^ show_expr_desc new_t in *)
        (* let () = print_endline @@ string_of_int tag in *)
        let replaced =  
          Bluejay_to_jay_maps.replace_type resolved_expected_type new_t tag
        in
        if Bluejay_ast_internal.equal_expr_desc replaced resolved_expected_type
          then
            Bluejay_ast_internal.new_expr_desc @@ TypeError (Ident "Type unknown")
          else 
            replaced
      in
      let actual_type_external = 
        actual_type |> Bluejay_ast_internal.from_internal_expr_desc
      in
      let resolved_expected_type_external =
        resolved_expected_type |> Bluejay_ast_internal.from_internal_expr_desc
      in
      (* let () = 
        print_endline @@ Bluejay_ast.show_expr_desc resolved_expected_type_external
      in *)
      (* let () = print_endline @@ Ast_pp.show_value v in *)
      (* let vs =
           dvs_lst
           |> List.map
               ~f:(fun (dv, _) ->
                   let res = Dbmc.Interpreter.value_of_dvalue dv in
                   res
                   )
         in *)
      [
        Bluejay_error.Error_bluejay_type
          {
            err_type_variable = err_var;
            err_type_expected = resolved_expected_type_external;
            err_type_actual = actual_type_external;
          };
      ]
