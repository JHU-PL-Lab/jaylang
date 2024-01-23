(
  ; correctly solved
  (
    (r Unknown Reach_max_step)  
    (first_branch Hit Hit)
    (x_is_50_branch Hit Hit)
    (e Hit Hit)
  )
  ; runs out of time because expressions take a while
  (
    (r Unknown Hit)  
    (first_branch Hit Hit)
    (x_is_50_branch Hit Hit)
    (e Hit Hit)
  )
  ; if times out every time, then last branch is unfound, so unknown
  (
    (r Unknown Hit)  
    (first_branch Hit Hit)
    (x_is_50_branch Hit Hit)
    (e Unknown Unknown)
  )
)