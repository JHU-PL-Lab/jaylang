open Batteries;;

(** This exception should be raised when code has not yet been implemented. *)
exception Not_yet_implemented of string;;

(** This exception should be raised if a defensive runtime check determines that
    an invariant has been violated. *)
exception Invariant_failure of string;;

val natural_compare_seq : ((unit -> int) list) -> int

val uniq_enum : ('a -> 'a -> int) -> 'a Enum.t -> 'a Enum.t

(**
   Expands a list of lists as a Cartesian product.  That is, the list
   {[
     [[1;2;3];[4];[5;6]]
   ]}
   would yield the list
   {[
     [[1;4;5];[2;4;5];[3;4;5];[1;4;6];[2;4;6];[3;4;6]]
   ]}
   the first element being the result of selecting [1], [4], and [5] from the
   original three lists.
*)
val cartesian_product_of_list : 'a list list -> 'a list list

val pairwise_enum_fold : ('a -> 'a -> 'b) -> 'a Enum.t -> 'b Enum.t
