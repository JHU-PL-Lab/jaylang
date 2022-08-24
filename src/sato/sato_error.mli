open Jhupllib;;

(** Raised when a string cannot be parsed into an error *)
exception Parse_failure of string;;

(* **** Module type signatures **** *)

(** Representation of a variable ident in an error *)
module type Error_ident = sig
  type t;;
  val equal : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

(** Representation of a value-assigning clause in an error *)
module type Error_value = sig
  type t;;
  val equal : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

(** Representation of a binary operation in an error *)
module type Error_binop = sig
  type t;;
  val equal : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

(** Representation of a type annotation in an error *)
module type Error_type = sig
  type t;;
  val equal : t -> t -> bool;;
  val subtype : t -> t -> bool;;
  val pp : t Pp_utils.pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

(* **** Error modules **** *)

module type Error = sig
  module Error_ident : Error_ident;;
  module Error_value : Error_value;;
  module Error_binop : Error_binop;;
  module Error_type : Error_type;;

  (** The type of a variable identifier in the error *)
  type ident;;
  (** The type of a value-assigning clause in the error *)
  type value;;
  (** The type representing a binary operation in a binop error*)
  type binop;;
  (** The type of a type annotation in the error *)
  type type_sig;;

  (** Type of an error caused by a binary operation that returns false *)
  type error_binop = {
    (** The alias chain leading up to the left value. *)
    err_binop_left_aliases : ident list;
    (** The alias chain leading up to the right value. *)
    err_binop_right_aliases : ident list;
    (** The value of the left side of the binop. *)
    err_binop_left_val : value;
    (** The value of the right side of the binop. *)
    err_binop_right_val : value;
    (** The binary operation *)
    err_binop_operation : binop;
  }

  (** Type of an error caused by a false pattern match (e.g. type errors) *)
  type error_match = {
    (** The alias chain of the ident begin matched upon. *)
    err_match_aliases : ident list;
    (** The value of the ident that is being matched. *)
    err_match_val : value;
    (** The expected type of the symbol being matched. *)
    err_match_expected : type_sig;
    (** The actual type of the symbol being matched. *)
    err_match_actual : type_sig;
  }

  (** Type of an error caused by a singular false value (e.g. in an assert) *)
  type error_value = {
    (** The alias chain defining the boolean value. *)
    err_value_aliases : ident list;
    (** The boolean value (should always be false). *)
    err_value_val : value;
  }

  (** Type of an error *)
  type t =
    | Error_binop of error_binop
    | Error_match of error_match
    | Error_value of error_value
  ;;

  (** Returns true if two errors are equal, false otherwise. *)
  val equal : t -> t -> bool;;

  (** Pretty-printer for an error. *)
  val pp : t Pp_utils.pretty_printer;;

  (** Show the error as a string. *)
  val show : t -> string;;

  val to_yojson : t -> Yojson.Safe.t;;
end;;

(** Functor of an error based on if it uses odefa or natodefa-related types *)
module Make
    (Ident : Error_ident)
    (Value : Error_value)
    (Binop : Error_binop)
    (Type : Error_type)
  : (Error
      with type ident = Ident.t
      and type value = Value.t
      and type binop = Binop.t
      and type type_sig = Type.t)


(* **** Odefa Error Modules **** *)
module Odefa_ident : Error_ident with type t = Odefa_ast.Ast.ident;;
module Odefa_value : Error_value with type t = Odefa_ast.Ast.clause_body;;
module Odefa_binop : Error_binop with type t =
  (Odefa_ast.Ast.clause_body * Odefa_ast.Ast.binary_operator * Odefa_ast.Ast.clause_body);;
module Odefa_type : Error_type with type t = Odefa_ast.Ast.type_sig;;

(** An error produced by an odefa program *)
module Odefa_error
  : (Error
      with type ident = Odefa_ident.t
      and type value = Odefa_value.t
      and type binop = Odefa_binop.t
      and type type_sig = Odefa_type.t)
;;

(* Natodefa modules *)

module Natodefa_ident : Error_ident with type t = Odefa_natural.On_ast.ident;;
module Natodefa_value : Error_value with type t = Odefa_natural.On_ast.expr_desc;;
module Natodefa_binop : Error_binop with type t = Odefa_natural.On_ast.expr_desc;;
module Natodefa_type : Error_type with type t = Odefa_natural.On_ast.type_sig;;

(** An error produced by a natodefa program *)
module On_error
  : (Error
      with type ident = Natodefa_ident.t
      and type value = Natodefa_value.t
      and type binop = Natodefa_binop.t
      and type type_sig = Natodefa_type.t)
;;

(* Typed Natodefa modules *)

module Ton_ident : Error_ident with type t = Typed_odefa_natural.Ton_ast.ident;;
module Ton_value : Error_value with type t = Typed_odefa_natural.Ton_ast.expr_desc;;
module Ton_binop : Error_binop with type t = Typed_odefa_natural.Ton_ast.expr_desc;;
module Ton_type : Error_type with type t = Typed_odefa_natural.Ton_ast.type_sig;;

(** An error produced by a natodefa program *)
module Ton_error
  : (Error
      with type ident = Ton_ident.t
      and type value = Ton_value.t
      and type binop = Ton_binop.t
      and type type_sig = Ton_type.t)
;;
(** Given an odefa/natodefa mapping, removes variables that were added during
    error instrumentation. *)
val odefa_error_remove_instrument_vars :
  Odefa_instrumentation.Odefa_instrumentation_maps.t -> Odefa_error.t -> Odefa_error.t
;;

(** Given an odefa/natodefa mapping, converts an odefa error into a natodefa
    error. *)
val odefa_to_natodefa_error :
  Odefa_instrumentation.Odefa_instrumentation_maps.t -> Odefa_natural.On_to_odefa_maps.t -> Dbmc.Interpreter.session -> 
  Dbmc.Interpreter.denv -> Odefa_error.t -> On_error.t list
;;

val odefa_to_ton_error_simple :
  Odefa_instrumentation.Odefa_instrumentation_maps.t -> 
  Odefa_natural.On_to_odefa_maps.t -> Typed_odefa_natural.Ton_to_on_maps.t ->
  Dbmc.Interpreter.session -> 
  Dbmc.Interpreter.denv -> Odefa_error.t -> Ton_error.t list
;;