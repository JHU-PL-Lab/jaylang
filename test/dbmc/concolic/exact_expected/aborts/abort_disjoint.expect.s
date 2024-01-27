(
  (
    (abort_on_false_side_branch Hit Hit)
    (no_abort_inner_branch Unsatisfiable Hit)
    (inner_branch Found_abort Hit)
  )
  ; if abort is hit first
  (
    (abort_on_false_side_branch Hit Hit)
    (no_abort_inner_branch Unknown Hit)
    (inner_branch Found_abort Hit)
  )
)