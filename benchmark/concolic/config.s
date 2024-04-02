
(
  (testcases_to_time
    (
    ; Goals with these tests:
    ;   * Have ill-typed programs for as many different features as possible
    ;   * Correct the ill-typed programs to see if we can exhaust the tree quickly (or a pruned tree)
    ;   * Combine features into bigger programs to see if we can still refute their typing
    ;   * Have ill-typed programs where the type error won't be found because it is too deep

    ; trees
    "bst_well_typed.jil" ; is well-typed version of bst
    "bst.jil" ; checks if some tree is valid bst. It's not
    "rec_type.jil" ; tree type where the tree children are ill-typed. The well-typed case is covered in bst
    "balanced_tree_well_typed.jil" ; is balanced tree -- is simpler case of braun_tree from racket
    "balanced_tree.jil" ; checks if binary tree is balanced. It's not

    ; recursive programs
    "rec_fun_well_typed.jil" ; well-typed recursive program
    "rec_fun.jil" ; ill-typed recursive program
    "mutually_rec_dep_types.jil" ; ill-typed. combine mutually rec and dependent types, but well-typed is not a trivial extension on the program
    "insertion_sort1.jil"
    "insertion_sort2.jil"

    ; dependent types
    "dep_type_well_typed.jil" ; well-typed dependent types
    "dep_type.jil" ; ill-typed dependent types

    ; records and modules
    "record_constraint_well_typed.jil" ; well-typed record type. No nested records
    "record_constraint.jil" ; ill-typed record type. No nested records
    "module.jil" ; ill-typed `remove` in the module and in type of `equal` function parameter in module signature
    ;"module_well_typed.jil" ; converts to massive jil file (> 10MB) when wrapped. Times out


    ; other
    "polymorphic_map.jil" ; ill-typed polymorphism and lists
    "flow_sensitive_well_typed.jil" ; 
    "flow_sensitive.jil" ; two typed-declared functions where one calls the other

    ; limitations
    "deep_abort.jil" ; type error is too deep, so we expect that we can't hit it

    )
  )
  (testcases_not_time
    ()
  )
  (test_path "benchmark/concolic/jil_wrap")
  (repeat 5)
  (timeout "30m")
)
