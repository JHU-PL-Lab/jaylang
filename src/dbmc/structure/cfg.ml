open Core

(*
    CFG: The cfg itself is already for the possible running programs.
    (* CFG can have loops *)

    Dynamic CFG: One graph, that should be a sub-graph of the CFG.
    (* No! Any Dynamic CFG must not have loops, or its not a CFG *)

    On the other leve,
    CFG is a language, while Dynamic CFG is a string.

    Maybe I can just call it Control-Flow Trace temporarily.

    Now the question is we hope to compose piece-wise traces together when
    they are consistent. 

    Formally, it's a question on _context-sensitive language concatenation_. 
    btw, concatenating regular languages may be much easier.

    Empirically, if one remembers all the choices made in one trace, whether 
    two traces are compatible or not can be checked by whether they made
    different choices on the same choice.
*)
