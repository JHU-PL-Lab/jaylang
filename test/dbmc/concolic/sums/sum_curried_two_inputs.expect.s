(
  ; success
  (
    (r (Hit) (Hit))
    (w1 (Hit) (Hit))
    (e (Hit) (Hit))
  )
  ; too large of input means solve takes too long
  (
    (r (Hit) (Hit))
    (w1 (Hit) (Hit))
    (e (Unknown 0) (Hit))
  )
  (
    (r (Unknown 0) (Hit))
    (w1 (Hit) (Hit))
    (e (Unknown 0) (Hit))
  )
  ; I expect that it should try small inputs and hit all branches, but this is just in case
)