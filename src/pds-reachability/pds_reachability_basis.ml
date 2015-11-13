(**
   This module defines a module type signature used as the basis for the PDS
   reachability functor. 
*)

open Batteries;;

(**
   A module type which serves as the basis for the functor which builds the
   PDS reachability implementation.
*)
module type Basis =
sig
  type state
  type stack_element

  module State_ord : Interfaces.OrderedType with type t = state
  module Stack_element_ord : Interfaces.OrderedType with type t = stack_element

  val pp_state : state -> string
  val pp_stack_element : stack_element -> string
  
  val ppa_state : state -> string
  val ppa_stack_element : stack_element -> string
end;;
