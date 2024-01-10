(
  ; success
  (
    (r (Hit) (Hit))
    (sum_val_is_desired_branch (Hit) (Hit))
    (call_fun_branch (Hit) (Hit))
  )
  ; recursed too far, so the expressions take too long to solve
  (
    (r (Hit) (Hit))
    (sum_val_is_desired_branch Unknown (Hit))
    (call_fun_branch (Hit) (Hit))
  )
  ; recursed too far too many times, so can't solve or runs out of time
  (
    (r (Hit) (Hit))
    (sum_val_is_desired_branch Unreachable_because_max_step (Reach_max_step 0))
    (call_fun_branch (Hit) (Hit))
  )
  (
    (r (Hit) (Hit))
    (sum_val_is_desired_branch Unknown (Reach_max_step 0))
    (call_fun_branch (Hit) (Hit))
  )
)