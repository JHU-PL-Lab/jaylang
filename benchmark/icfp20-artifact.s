(
  (testcases_to_time (
    "input_blur.natodefa"
    "input_eta.natodefa"
    "input_facehugger.natodefa"
    "input_flatten.natodefa"
    "input_k_cfa_2.natodefa"
    "input_k_cfa_3.natodefa"
    "input_map.natodefa"
    "input_mj09.natodefa"
    "input_sat_1.natodefa"
    "input_sat_1_direct.natodefa"
    "smbc_fold0s.natodefa"
    "smbc_gen_list_len.natodefa"
    "smbc_long_rev_sum3.natodefa"
    "smbc_pigeon.natodefa"
    "smbc_sorted_sum.natodefa"
    )
  )
  (testcases_not_time (
    "input_list_sum_add_build.natodefa"
    )
  )
  (test_path "vendor/icfp20-artifact/cases")
  (repeat 3)
  (timeout "20m")
)
