(
  ; all good
  (
    (r Hit Hit)
    (call_fun_branch Hit Hit)
    (sum_val_is_desired_branch Hit Hit)
  )
  ; solver times out when solving
  (
    (r Hit Hit)
    (call_fun_branch Hit Hit)
    (sum_val_is_desired_branch Unknown Hit)
  )
  ; reaches max step too many times
  (
    (r Hit Reach_max_step)
    (call_fun_branch Hit Hit)
    (sum_val_is_desired_branch Unknown Hit)
  )
  (
    (r Hit Reach_max_step)
    (call_fun_branch Hit Hit)
    (sum_val_is_desired_branch Unknown Unknown)
  )
)