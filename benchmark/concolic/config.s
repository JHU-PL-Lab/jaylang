
(
  (testcases_to_time
    (
    ; Goals with these tests:
    ;   * Have ill-typed programs for as many different features as possible
    ;   * Correct the ill-typed programs to see if we can exhaust the tree quickly (or a pruned tree)
    ;   * Combine features into bigger programs to see if we can still refute their typing
    ;   * Have ill-typed programs where the type error won't be found because it is too deep

    ; trees
    "bst_type_well_typed.jil" ; is well-typed version of set_type_10
    "set_type_10.jil" ; checks if some tree is valid bst. It's not
    "recursive_type_3.jil" ; tree type where the tree children are ill-typed. The well-typed case is covered in bst
    "set_type_5_well_typed.jil" ; is balanced tree
    "set_type_5.jil" ; checks if binary tree is balanced. It's not

    ; recursive programs
    "recursive_fun_1.jil" ; ill-typed recursive program
    "recursive_fun_1_well_typed.jil" ; well-typed recursive program
    "mutually_recursive_dep_types_1.jil" ; ill-typed. combine mutually rec and dependent types, but well-typed is not a trivial extension on the program

    ; dependent types
    "dependent_type_test_1_well_typed.jil" ; well-typed dependent types
    "dependent_type_test_1.jil" ; ill-typed dependent types

    ; records and modules
    "record_7.jil" ; ill-typed record type. No nested records
    "record_7_well_typed.jil" ; well-typed record type. No nested records
    "module_5_1.jil" ; ill-typed `remove` in the module and in type of `equal` function parameter in module signature
    ;"module_5_well_typed.jil" ; converts to massive jil file (> 10MB). Times out


    ; other
    "let_fun_test_9.jil" ; ill-typed 
    "let_fun_test_9_well_typed.jil" ; well-typed functions for types
    "polymorphic_map.jil" ; ill-typed polymorphism and lists
    ;"polymorphic_map_well_typed.jil" ; well-typed polymorphism and lists. Times out
    "flow_sensitive_1.jil" ; two typed-declared functions where one calls the other
    "flow_sensitive_1_well_typed.jil" ; 

    ; limitations
    "expected_timeout.jil" ; type error is too deep, so we expect that we can't hit it

    )
  )
  (testcases_not_time
    ()
  )
  (repeat 5)
  (timeout "30m")
)
