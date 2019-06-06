(**
   This module contains type declarations used in the test generator to avoid
   duplicating them between .ml and .mli files.
*)

open Odefa_ast;;

open Ast;;

(**
   A test generator is a tool which attempts to find an input sequence to lead
   to a particular point in a program.
*)
type test_generator =
  {
    tg_program : expr;
    (** The program being analyzed.  This is stored for reference purposes. *)

    tg_target : Ident.t;
    (** The program point we are trying to reach.  This is stored for reference
        purposes. *)

    tg_generator_fn : (int -> test_generation_result) option
    (** A function which, given a maximum number of steps to take, attempts to
        reach the point in question.  Here, "steps" are not of any fixed amount
        of effort; however, some maximum step count is required as test
        generation is not decidable.

        If this function is None, then it is known that no additional paths will
        reach the program point in question.  Note that this field is not
        guaranteed to take on the value None regardless of how many steps of
        computation are performed, as test generation is undecidable. *)
  }

(**
   The result of a test generation pass.  Test generation *may* produce an input
   sequence which will reach the point we are attempting to locate.
*)
and test_generation_result =
  {
    tgr_inputs : int list option;
    (** The input sequence which will lead to the point in question.  If None,
        then this result did not find an input sequence within the provided
        number of steps. *)

    tgr_steps : int;
    (** The number of steps which were taken from the called test generator to
        reach this result. *)

    tgr_total_steps : int;
    (** The total number of steps taken to reach this result.  This includes the
        most recent call to the test generator and the total steps required to
        produce that test generator. *)

    tgr_generator : test_generator;
    (** A test generator which will only produce results for paths not
        previously discovered in this result's ancestry.  If it is known that no
        such paths exists, this value is None.  Note that this value may be Some
        even if no such paths exist as test generation is undecidable. *)
  }
;;
