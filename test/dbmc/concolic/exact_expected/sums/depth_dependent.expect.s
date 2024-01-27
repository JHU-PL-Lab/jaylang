(
  ; solved correctly
  (
    (r Hit Hit)
    (sum_val_is_desired_branch Hit Hit)
    (call_fun_branch Hit Hit)
  )
  ; hit max step many times, so thinks desired branch is unsatisfiable
  (
    (r Hit Hit)
    (sum_val_is_desired_branch Unknown Hit)
    (call_fun_branch Hit Hit)
  )
)