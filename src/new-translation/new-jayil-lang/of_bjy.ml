(*
Translations:
  [[int]] =
    { gen = freeze pick_i ; check = fun e -> match e with int -> true | _ -> false }

  [[bool]] =
    { gen = freeze pick_b ; check = fun e -> match e with bool -> true | _ -> false }

  [[tau1 -> tau2]] =
    { gen = freeze @@
      fun arg ->
        if [[tau1]].check arg (* shouldn't we only do this when we have wrap flag on? This behavior is inconsistent with the desired behavior when the user doesn't have the wrap flag on *) (* scott says to leave out of implementation, but put a note in the spec *)
        then thaw [[tau2]].gen
        else abort
    ; check = fun e ->
        match e with
        | fun ->
          let arg = thaw [[tau1]].gen in
          [[tau2]].check (e arg) (* this now evaluates e for a second time, which is unecessary computation *)
        | _ -> false

    }
  
    (* I think the paper has a bug and we need the final outer thaw *)
    [[Mu B. tau]] =
      thaw @@
      Y (fun self -> freeze 
        { gen = freeze @@
            (fun B -> thaw [[tau]].gen) (thaw self)
          
        ; check = fun e ->
            (fun B -> [[tau]].check e) (thaw self)
        }
      )


    [[V_1 of tau_1 | ... | V_n of tau_n]] =
      { gen = freeze @@
          case pick_i on
          | 0 -> V_1 (thaw [[tau_1]].gen) (* TODO: make this actually in il with ints instead of variants *)
          | ...
          | n-2 -> V_(n-1) (thaw [[tau_(n-1)]].gen)
          | _ -> V_n (thaw [[tau_n]].gen)
          end
      ; check = fun e ->
          match e with
          | V_1 v_1 -> [[tau_1]].check v_1
          | ...
          | V_n v_n -> [[tau_n]].check v_n
          end
      }

  



*)

(*
  Question:
    Do we ever need to run the checker without aborting when it evaluates to false?
    If yes:
      Then we need to make sure that we return false in erroring cases
    If no:
      Then we can skip the false cases and always abort, which means fewer match cases (e.g. don't need to check that e is a function. Simply )

  My guess:
    We always abort on `false` return because of definition of TC. We abort when not TC, so we abort when not true.

  Also:
    We will want an identifier that is "ignore" so that we don't need to capture a closure (is no problem for functional environment, but in case of snapshottable stores, we'll want it)
*)