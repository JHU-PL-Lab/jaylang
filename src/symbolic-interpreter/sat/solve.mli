(**
   This module defines a routine which performs satisfiability checks on a
   collection of formulae.  This module does not check complex structures such
   as functions and relies upon the Formulae module to detect immediate
   inconsistencies (such as when functions are declared equal to integers).  It
   will, however, discover cases in which e.g. integer variables are constrained
   to have no solutions.
*)

(**
   Given a collection of formulae, determines whether or not a solution exists.
*)
val solve : Formulae.t -> bool
