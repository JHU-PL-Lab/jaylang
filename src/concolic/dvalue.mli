
open Jayil.Ast

module rec T :
  sig
    type t =
      | Direct of value (* record, function, int, or bool *)
      | FunClosure of Ident.t * function_value * Denv.t
      | RecordClosure of record_value * Denv.t
      | AbortClosure of Denv.t

    val value_of_t : t -> value

    val pp : t -> string

    val is_int_or_bool : t -> bool
  end
and Denv :
  sig
    type t

    val empty : t

    val add : t -> Ident_new.t -> T.t -> Concolic_key.t -> t

    val fetch : t -> var -> T.t * Concolic_key.t

    val fetch_val : t -> var -> T.t

    val fetch_key : t -> var -> Concolic_key.t
  end

include module type of T with type t = T.t
