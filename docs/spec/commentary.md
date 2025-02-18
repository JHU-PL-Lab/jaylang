
# Translation commentary

We didn't just snap our fingers, and the translation specification popped into existence. No, we had to think and talk through a lot of the definitions. Here is a file containing some of that commentary, if it makes sense to keep around. Otherwise, the `desugar` and `embed` specifications are relatively to-the-point.

## 20-sided die

The "20-sided die" idea is to roll a die before we start evaluating the program, and the roll determines which checker we run; other types become only "wrap", and the one we rolled gets a "check". This way, we don't have to run all checkers beforehand (which may diverge) in order to check the statement at the end of the program.

But how do we handle types that are inside the body of let-expressions? For example:

```ocaml
let f x =
  let g (y : int) : int = y in (* only ever here after calling f *)
  x
in
let h (x : int) : int =
  let _ = f x in (* pulls the check of g into here *)
  x
in
h
```

Because of this example we cannot simply give a single rolled value to each checker--`g`'s checker could never get run because it only gets run in sequence with `h`, which would have a different roll value. This is not an easy problem to solve, so we instead avoid it.

It's for this reason that Bluejay programs are a sequence of statements, where we only "roll a die" to decide which statement to check. All other typed let-expressions that are **not** statements always have their checkers turned on, and all "non-rolled" statements are only wrapped in their type, not checked. Thus, that example program actually looks like this, a statement list:


```ocaml
let f x =
  let g (y : int) : int = y in (* only ever here after calling f *)
  x

let h (x : int) : int =
  let _ = f x in (* pulls the check of g into here *)
  x
```

In fact, while we can actually do a dice roll and let the concolic evaluator pick which checker to run, we feel it is likely smarter to produce `n` separate target programs, each with a different checker, that can be run in parallel. The user can choose to run the checks in parallel this way with the `-p` flag to the `./ceval.exe` concolic evaluator executable.

## Recursive variants

The variant type embedding is a little bit confusing at first glance. Why do we partition the variant constructors into two cases?

We do this because the generation of some recursive variant types are unlikely to terminate if all variant constructors are equally likely to be picked. Imagine any AST type. Most cases split, and there are few leaves. Probabilistically, with no smart choices, the generation won't terminate.

This termination problem is solved by splitting the variants into those that are recursive and those that are not, and we make it extremely unlikely to pick a recursive variant. The concolic evaluator, however, doesn't care about "unlikely"; it can pick unlikely events with ease. Thus, these constructors are still easy to pick when they are wanted, and they are only hard to pick randomly. This encourages termination while still allowing exploration of the full program.

It is not included in the specification what happens if all constructors are recursive or all constructors are nonrecursive. We assume the reader understands that in either of these cases, the constructors are not partitioned at all, but rather they are joined under one case. This is handled in the implementation but is somewhat ignored in the spec. I think it is worthy of this comment here, however.
