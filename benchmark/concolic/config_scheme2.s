
(
  (testcases_to_time
    (
    "all.jil"
    "append.jil"
    "boolflip_e.jil"
    "braun_tree.jil"
    "flatten.jil"
    "fold_fun_list.jil"
    "foldl.jil"
    "foldl1.jil"
    "foldr.jil"
    "foldr1.jil"
    "hors.jil"
    "hrec.jil"
    "intro1.jil"
    "intro3.jil"
    "last.jil"
    "lastpair.jil"
    "max.jil"
    "mem.jil"
    "member.jil"
    "mult_all_e.jil"
    "mult_cps_e.jil"
    "mult_e.jil"
    "mult.jil"
    "nth0.jil"
    "r_lock.jil"
    "reverse.jil"
    "sum_acm_e.jil"
    "sum_all_e.jil"
    "sum_e.jil"
    "tree_depth.jil"
    )
  )
  (testcases_not_time
    ()
  )
  (test_path "test/dbmc/concolic/scheme-pldi-2015")
  (repeat 5)
  (timeout "30m")
)
