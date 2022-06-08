open Jhupllib;;

open Odefa_ast;;

(* Natodefa modules *)

module type Error_ident = sig
  type t;;
  val equal : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

module type Error_value = sig
  type t;;
  val equal : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

module type Error_binop = sig
  type t;;
  val equal : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

module type Error_type = sig
  type t;;
  val equal : t -> t -> bool;;
  val subtype : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

module type Error_natodefa_type = sig
  type t;;
  val equal : t -> t -> bool;;
  val subtype : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

module Ident : Error_ident with type t = On_ast.ident;;

module Value : Error_value with type t = On_ast.core_natodefa;;

module Binop : Error_binop with type t = On_ast.core_natodefa;;

module Type : Error_type with type t = On_ast.type_sig;;

module NatodefaType : Error_natodefa_type with type t = On_ast.syn_type_natodefa;;

module type Error = sig
  module Error_ident : Error_ident;;
  module Error_value : Error_value;;
  module Error_binop : Error_binop;;
  module Error_type : Error_type;;
  module Error_natodefa_type : Error_natodefa_type;;

  type ident;;
  type value;;
  type binop;;
  type type_sig;;
  type natodefa_type;;

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

  type error_value = {
    err_value_aliases : ident list;
    err_value_val : value;
  }

  type error_type = {
    err_type_aliases : ident list;
    err_type_val : value;
    err_type_expected : natodefa_type;
    err_type_actual : natodefa_type;
  }

  type t =
    | Error_binop of error_binop
    | Error_match of error_match
    | Error_value of error_value
    | Error_natodefa_type of error_type

  val equal : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

(** Functor of an error based on if it uses odefa or natodefa-related types *)
module Make
    (Ident : Error_ident)
    (Value : Error_value)
    (Binop : Error_binop)
    (PrimitiveType : Error_type)
    (NatodefaType : Error_natodefa_type)
  : (Error
      with type ident := Ident.t
      and type value := Value.t
      and type binop := Binop.t
      and type type_sig := PrimitiveType.t
      and type natodefa_type := NatodefaType.t)

(** An error produced by an odefa program *)
module On_error
  : (Error
      with type ident := Ident.t
      and type value := Value.t
      and type binop := Binop.t
      and type type_sig := Type.t
      and type natodefa_type := NatodefaType.t)
;;

(** Given an odefa/natodefa mapping, removes variables that were added during
    error instrumentation. *)
val odefa_error_remove_instrument_vars :
  On_to_odefa_maps.t -> Error.Odefa_error.t -> Error.Odefa_error.t
;;

(** Given an odefa/natodefa mapping, converts an odefa error into a natodefa
    error. *)
val odefa_to_natodefa_error :
  On_to_odefa_maps.t -> Ton_to_on_maps.t -> On_ast.syn_natodefa_edesc option -> Ast.ident list -> Ast.value On_ast.Ident_map.t -> Error.Odefa_error.t -> On_error.t
;;