# Abstract

This writeup is to solve questions:

1. Where to put the `complete` state?
2. How does the `complete` state help?

# Data Structures

We uses data structures `rule` `rule_action` `node` `edge` `lookup_key` `lookup_detail` `job` and modules `Lookup` `Scheduler` `Unroll` for the lookup functionality. Furthermore, we also have `riddler` `solver` `checker` that used in the project but not the interests of this writeup.

It may not have to be complex like this now, however, we need to document it clearly before any refactoring.

At a high level, the first `lookup` can spawn infinite other (sub-)lookups. `Scheduler` arranges the order of lookups. A `lookup` is treated as a `job` here. Stepping close, one `lookup` creates other `lookup`s and waits for their results. `Unroll` is a module to process the results. Therefore, we can treat a `lookup` contains two parts:

1. how to create other lookups.
2. how to deal with the results of other lookups and compose them into its own result.

For part 1, A created `lookup` doesn't launch immediately. It's also delegated in the `Scheduler`. The main consideration is the current asycronous library in use a.k.a. `Lwt` doesn't support customized priority so we have to manually operate it.

For part 2, there may be multiple lookups waiting for multiple other lookups. The current solution is to maintain a result list for each lookup. The waiting-for becomes a subscription. When a target lookup subscribes a source lookup, it can get all the results no matter in the past or in the future. Current implementation benefits from `Lwt_stream`.

Source lookup procudes `result` stored in the corresponding `Lwt_stream` and notify the target lookup via a `message`. Currently, `result` and `message` are equivalent in all use-cases.

A `rule` describes what part 1 and part 2 should be. `rule_action` are components for rule. The design for `rule_action` is for flexibility. You can image the implementation of each semantics rules are large and share similar logics, so we define a few basic `rule_actions` so that any rule can be composed by `rule_action`s.

# Problem & Solution


(**TODO** for optmization: move granularities from node to block)

The current problem is we need to add a new state for each lookup's running state, `is_complete`. Once a lookup is complete, it should never report any new message. If a lookup's dependencies are all complete, itself is complete.

How should we add `is_complete` to the current system? Should it be in `message`?

A natural thought is to make the message a variant of either a real message or a control message. Here the control message is the `complete`.

Where should I put the `complete` state? Before answering this question, let's investigate a few examples to see how we can use the completeness state.

## eg1

```jil
x = 1
t = x + x
```

```phis
p_x -> (x = 1)
p_t -> (t = x + x; p_x)
```

When the result can be **constants** (this is discussed before), we can have

```phis
p_x -> (x = 1)
p_t -> (t = 1 + 1; p_x)
```

## eg2

```jil
x = 1
z = 0
g = fun x -> x
t = g z
```

Then

```phi
p_x -> (x = 1)
p_t -> (OR_INF(g = g, t = x); p_g; p_z)
```

When `x` `z` `g` `t` are complete, the phis can be simplified as

```phi
p_x -> (x = 1)
p_t -> (g = g, t = x; p_g; p_z)
```

## eg3

```jil
x = 1
z = 0
f1 = fun x1 -> x1
f2 = fun x2 -> z
(g <- f1 or f2)
t = g z
```

When `f1` is complete, it doesn't change much.

```phi
p_x -> x == 1
p_z -> 0
p_f1 -> true, f1 = f1
p_f2 -> true, f2 = f2
p_g -> g = f1 or f2, p_f1 or p_f2
p_t -> (OR_INF([g = f1, t = x, p_f1, p_x], [g = f2, t = z, p_f2, p_z]), p_g, p_z)
```

When both `f1` and `f2` are complete, `g` and `t` are also complete, then

```phi
p_x -> x == 1
p_z -> 0
p_f1 -> true, f1 = f1
p_f2 -> true, f2 = f2
p_g -> g = f1 or f2, p_f1 or p_f2
p_t -> (OR([g = f1, t = x, p_f1, p_x], [g = f2, t = z, p_f2, p_z]), p_g, p_z)
```

# Remove complete node after UNSAT

t.b.c.



cover this case first
```
c = FALSE;
t = c ? e1 : e2;
```
