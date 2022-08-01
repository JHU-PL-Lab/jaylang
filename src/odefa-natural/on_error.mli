open Odefa_ast;;

(* Natodefa modules *)

module Ident : Error.Error_ident with type t = On_ast.ident;;
module Value : Error.Error_value with type t = On_ast.expr;;
module Binop : Error.Error_binop with type t = On_ast.expr;;
module Type : Error.Error_type with type t = On_ast.type_sig;;

(** An error produced by a natodefa program *)
module On_error
  : (Error.Error
      with type ident := Ident.t
      and type value := Value.t
      and type binop := Binop.t
      and type type_sig := Type.t)
;;

(** Given an odefa/natodefa mapping, removes variables that were added during
    error instrumentation. *)
val odefa_error_remove_instrument_vars :
  On_to_odefa_maps.t -> Error.Odefa_error.t -> Error.Odefa_error.t
;;

(** Given an odefa/natodefa mapping, converts an odefa error into a natodefa
    error. *)
(* val odefa_to_natodefa_error :
  On_to_odefa_maps.t -> Error.Odefa_error.t -> On_error.t
;; *)