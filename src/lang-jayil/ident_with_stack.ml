(* open Core

module T = struct
  type t = Dbmc.Id.t * Concrete_stack.t
  [@@deriving sexp_of, compare, equal, hash]
;;
end;;

type ident_with_stack_set = Ident_with_stack.t Hash_set.t

let show_ident_with_stack (x, stk) = 
  (show_ident x) ^ "@" ^ Concrete_stack.to_string stk *)