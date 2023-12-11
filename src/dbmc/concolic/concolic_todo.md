I need to accept a list of AST branches and try to hit them.
* The best I can do is try randomly at first, so I think I keep the same flow
* If a parent is unsatisfiable, then it is unreachable
  * Instead of analyzing parents, I could just track if either side is reached to determine reachability vs unsatisfiability
* If satisfied, then track the input that got there
* Also track the previous session because I might want to return to previous targets
  * If our target is not satisfiable, then go to previously assigned target
  * We might like a target stack, or maybe a target priority queue
    * Add targets according to time, and if they are a duplicate, then overwrite the time
    * Prioritize by most recent
* Use variants so we can finish the session in case we need to return to it with a different target
  * We need to clean up after exiting a branch, so if an exception is thrown, then we can't clean up
  * It's more annoying for me to catch an exception than to deal with variants
  * It's also a good chance to just make a session monad.

Other:
* Use a logger instead of printing
  * Or set verbosity in session
* 


I have these two outputs, one with it working, and one with it not. I cannot find the difference that causes one to work.

Now I rebuilt and it IS working. But the output doesn't change to justify it. This is some C++ sort of stuff...

The working one:
```
Starting concolic execution...
------------------------------
Running program...

Branch Information:
always_false_branch: True=UNHIT; False=UNHIT
both_reachable: True=UNHIT; False=UNHIT
unreachable_branch: True=UNHIT; False=UNHIT

Target branch: None
Feed 2 to x
ADD GLOBAL FORMULA (= x_ x_)
ADD GLOBAL FORMULA (= one_ (Int 1))
ADD GLOBAL FORMULA (let ((a!1 (= cx_ (Bool (< (i one_) (i x_))))))
  (and a!1 ((_ is Int) one_) ((_ is Int) x_)))
Hitting: both_reachable: true
Hitting: always_false_branch: false
ADD PICK FORMULA: (=> |P_always_false_branch_-(both_reachable,$tt);| (and (= cx_ (Bool true))))
ADD GLOBAL FORMULA (=> |P_always_false_branch_-(both_reachable,$tt);| (and (= cx_ (Bool true))))
ADD GLOBAL FORMULA (let ((a!1 (=> (and (= |always_false_cond_-(both_reachable,$tt);| (Bool false)))
               (= both_reachable_
                  |ret_true_both_reachable_-(both_reachable,$tt);|)))
      (a!2 (= |ret_true_both_reachable_-(both_reachable,$tt);|
              (Bool (< (i one_)
                       (i |always_false_branch_-(both_reachable,$tt);|)))))
      (a!4 (=> (= |always_false_cond_-(both_reachable,$tt);| (Bool false))
               (and (= |always_false_branch_-(both_reachable,$tt);|
                       |must_get_here_-(always_false_branch,$ff);-(both_reachable,$tt);|)
                    (= |must_get_here_-(always_false_branch,$ff);-(both_reachable,$tt);|
                       (Int 10)))))
      (a!5 (= |always_false_cond_-(both_reachable,$tt);|
              (Bool (< (i x_) (i one_))))))
(let ((a!3 (=> (and (= |always_false_cond_-(both_reachable,$tt);| (Bool false)))
               (and a!2
                    ((_ is Int) one_)
                    ((_ is Int) |always_false_branch_-(both_reachable,$tt);|)))))
  (=> (= cx_ (Bool true))
      (and a!1 a!3 a!4 a!5 ((_ is Int) x_) ((_ is Int) one_)))))
ADD PICK FORMULA: (=> P_both_reachable_ and)
ADD GLOBAL FORMULA (=> P_both_reachable_ and)
Evaluated to: true
------------------------------
Running program...

Branch Information:
always_false_branch: True=UNHIT; False=HIT
both_reachable: True=HIT; False=UNHIT
unreachable_branch: True=UNHIT; False=UNHIT

Target branch: always_false_branch_-(both_reachable,$tt);; condition: always_false_cond_-(both_reachable,$tt); = true
Solving for target branch:
Branch to pick: |P_always_false_branch_-(both_reachable,$tt);|
Branch condition: (= |always_false_cond_-(both_reachable,$tt);| (Bool true))
```


The failing one:
```

Starting concolic execution...
------------------------------
Running program...

Branch Information:
always_false_branch: True=Unhit; False=Unhit
both_reachable: True=Unhit; False=Unhit
unreachable_branch: True=Unhit; False=Unhit

Target branch: None
Feed -9 to x
ADD GLOBAL FORMULA (= x_ x_)
ADD GLOBAL FORMULA (= one_ (Int 1))
ADD GLOBAL FORMULA (let ((a!1 (= cx_ (Bool (< (i one_) (i x_))))))
  (and a!1 ((_ is Int) one_) ((_ is Int) x_)))
Hitting: both_reachable: false
ADD GLOBAL FORMULA (=> (= cx_ (Bool false))
    (and (= both_reachable_ |ret_false_both_reachable_-(both_reachable,$ff);|)
         (= |ret_false_both_reachable_-(both_reachable,$ff);| (Int (- 10)))))
ADD PICK FORMULA: (=> P_both_reachable_ and)
ADD GLOBAL FORMULA (=> P_both_reachable_ and)
Evaluated to: -10
------------------------------
Running program...

Branch Information:
always_false_branch: True=Unhit; False=Unhit
both_reachable: True=Unhit; False=Hit
unreachable_branch: True=Unhit; False=Unhit

Target branch: both_reachable_; condition: cx_ = true
Solving for target branch:
Branch to pick: P_both_reachable_
Branch condition: (= cx_ (Bool true))
Feed 2 to x
ADD GLOBAL FORMULA (= x_ x_)
ADD GLOBAL FORMULA (= one_ (Int 1))
ADD GLOBAL FORMULA (let ((a!1 (= cx_ (Bool (< (i one_) (i x_))))))
  (and a!1 ((_ is Int) one_) ((_ is Int) x_)))
Hitting: both_reachable: true
Hitting: always_false_branch: false
ADD PICK FORMULA: (=> |P_always_false_branch_-(both_reachable,$tt);| (and (= cx_ (Bool true))))
ADD GLOBAL FORMULA (=> |P_always_false_branch_-(both_reachable,$tt);| (and (= cx_ (Bool true))))
ADD GLOBAL FORMULA (let ((a!1 (=> (and (= |always_false_cond_-(both_reachable,$tt);| (Bool false)))
               (= both_reachable_
                  |ret_true_both_reachable_-(both_reachable,$tt);|)))
      (a!2 (= |ret_true_both_reachable_-(both_reachable,$tt);|
              (Bool (< (i one_)
                       (i |always_false_branch_-(both_reachable,$tt);|)))))
      (a!4 (=> (= |always_false_cond_-(both_reachable,$tt);| (Bool false))
               (and (= |always_false_branch_-(both_reachable,$tt);|
                       |must_get_here_-(always_false_branch,$ff);-(both_reachable,$tt);|)
                    (= |must_get_here_-(always_false_branch,$ff);-(both_reachable,$tt);|
                       (Int 10)))))
      (a!5 (= |always_false_cond_-(both_reachable,$tt);|
              (Bool (< (i x_) (i one_))))))
(let ((a!3 (=> (and (= |always_false_cond_-(both_reachable,$tt);| (Bool false)))
               (and a!2
                    ((_ is Int) one_)
                    ((_ is Int) |always_false_branch_-(both_reachable,$tt);|)))))
  (=> (= cx_ (Bool true))
      (and a!1 a!3 a!4 a!5 ((_ is Int) x_) ((_ is Int) one_)))))
ADD PICK FORMULA: (=> P_both_reachable_ and)
ADD GLOBAL FORMULA (=> P_both_reachable_ and)
Evaluated to: true
------------------------------
Running program...

Branch Information:
always_false_branch: True=Unhit; False=Hit
both_reachable: True=Hit; False=Hit
unreachable_branch: True=Unhit; False=Unhit

Target branch: always_false_branch_-(both_reachable,$tt);; condition: always_false_cond_-(both_reachable,$tt); = true
Solving for target branch:
Branch to pick: |P_always_false_branch_-(both_reachable,$tt);|
Branch condition: (= |always_false_cond_-(both_reachable,$tt);| (Bool true))
Solving for target branch:
Branch to pick: P_both_reachable_
Branch condition: (= cx_ (Bool true))
Feed 2 to x
```