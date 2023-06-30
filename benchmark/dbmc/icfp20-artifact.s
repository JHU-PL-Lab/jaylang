(
  (testcases_to_time (
    "input_blur.jay"
    "input_eta.jay"
    "input_facehugger.jay"
    "input_flatten.jay"
    "input_k_cfa_2.jay"
    "input_k_cfa_3.jay"
    "input_map.jay"
    "input_mj09.jay"
    "input_sat_1.jay"
    "input_sat_1_direct.jay"
    "smbc_fold0s.jay"
    "smbc_gen_list_len.jay"
    "smbc_long_rev_sum3.jay"
    "smbc_pigeon.jay"
    "smbc_sorted_sum.jay"
    )
  )
  (testcases_not_time (
    "input_list_sum_add_build.jay"
    )
  )
  (test_path "vendor/icfp20-artifact/cases")
  (repeat 3)
  (timeout "20m")
)
