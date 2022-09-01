(
  ; higher-order bmc
  ; no better than forward

  ; higher-order runtime contract
  ; 

  ; rosette

  (testcases_to_time (
    ; recursive flip a boolean
    ; "input_blur.natodefa"

    ; the sum of two products
    ; "facehugger.natodefa"

    
    ; "input_flatten.natodefa"
    ; "input_k_cfa_2.natodefa"
    ; "input_k_cfa_3.natodefa"
    ; "input_map.natodefa"
    ; "input_mj09.natodefa"
    ; "input_sat_1.natodefa"
    ; "input_sat_1_direct.natodefa"
    ; "smbc_fold0s.natodefa"
    ; "smbc_gen_list_len.natodefa"
    ; "smbc_long_rev_sum3.natodefa"
    ; "smbc_pigeon.natodefa"
    ; "smbc_sorted_sum.natodefa"

    ;;  
    ;; "input_eta.natodefa"
    )
  )
  (testcases_not_time (
    ; "input_list_sum_add_build.natodefa"
    )
  )
  (test_path "benchmark/cases")
  (repeat 1)
  (timeout "20m")
)
