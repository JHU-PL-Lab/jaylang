open Batteries
open Jhupllib

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
  String.replace_chars (function '\n' -> " " | c -> String.of_char c) str

module Ident : Error_ident with type t = Ast.ident = struct
  type t = Ast.ident

  let equal = Ast.equal_ident
  let pp = Ast_pp.pp_ident
  let show = Ast_pp.show_ident
  let to_yojson ident = `String (replace_linebreaks @@ show ident)
end

module Value : Error_value with type t = Ast.clause_body = struct
  type t = Ast.clause_body

  let equal = Ast.equal_clause_body

  (* We use the brief version in order to avoid printing out function
     bodies (which may have clauses added during instrumentation. *)
  let pp = Ast_pp_brief.pp_clause_body
  let show = Ast_pp_brief.show_clause_body
  let to_yojson value = `String (replace_linebreaks @@ show value)
end

module Binop :
  Error_binop
    with type t = Ast.clause_body * Ast.binary_operator * Ast.clause_body =
struct
  type t = Ast.clause_body * Ast.binary_operator * Ast.clause_body
  [@@deriving eq, to_yojson]

  let equal = equal

  let pp formatter (binop : t) =
    let left, op, right = binop in
    Format.fprintf formatter "%a %a %a" Ast_pp.pp_clause_body left
      Ast_pp.pp_binary_operator op Ast_pp.pp_clause_body right

  let show binop = Pp_utils.pp_to_string pp binop
  let to_yojson binop = `String (replace_linebreaks @@ show binop)
end

module Type : Error_type with type t = Ast.type_sig = struct
  type t = Ast.type_sig

  let equal = Ast.equal_type_sig
  let subtype = Ast.Type_signature.subtype
  let pp = Ast_pp.pp_type_sig
  let show = Ast_pp.show_type_sig
  let to_yojson typ = `String (Ast_pp.show_type_sig typ)
end

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
    with type ident := Ident.t
     and type value := Value.t
     and type binop := Binop.t
     and type type_sig := Type.t = struct
  module Error_ident = Ident
  module Error_value = Value
  module Error_binop = Binop
  module Error_type = Type

  type error_binop = {
    err_binop_left_aliases : Ident.t list;
    err_binop_right_aliases : Ident.t list;
    err_binop_left_val : Value.t;
    err_binop_right_val : Value.t;
    err_binop_operation : Binop.t;
  }
  [@@deriving eq, to_yojson]

  type error_match = {
    err_match_aliases : Ident.t list;
    err_match_val : Value.t;
    err_match_expected : Type.t;
    err_match_actual : Type.t;
  }
  [@@deriving eq, to_yojson]

  type error_value = {
    err_value_aliases : Ident.t list;
    err_value_val : Value.t;
  }
  [@@deriving eq, to_yojson]

  type t =
    | Error_binop of error_binop
    | Error_match of error_match
    | Error_value of error_value
  [@@deriving eq, to_yojson]

  let equal = equal

  let pp_alias_list formatter aliases =
    Pp_utils.pp_concat_sep " ="
      (fun formatter x -> Ident.pp formatter x)
      formatter (List.enum aliases)

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

module Odefa_error = Make (Ident) (Value) (Binop) (Type)
