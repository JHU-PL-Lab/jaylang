# Invalidate a control-flow

(`locality`)

or shut down a node.

The motivation is to focus on some part of the giant formula. The problem is 

- how to only check part of the formula and
- how the checking result is useful globally

The checking scheme is depended on the current implementation for choices, specifically, on how the function applications and conditions are looked up.

(Despite the _or_ path we are using) the search graph, whose nodes are lookups, and the edges are dependencies, has a root node which is the `target` node.

Function applications `t = f a` will have different nodes on `f`'s choices e.g. `f1` `f2`. The global formula records `if picked t, then f==f1 or f==f2`. At the viewpoint of `f1`, what should `f1` know?

Conditions are a bit difference. For Rule CondBottom, `t = c ? e1.. : e2..`, we have nodes `t`, `c`, `e1`, `e2`. The global formula records nothing for `c`. At the viewpoint of  `c`, it see nothing.

`c`'s implication is used for Rule CondTop. e.g. when you are in `e1`, you will create a lookup `c` and you will only care about whether `c` can be true.

# SMT side

One problem of a bounded checking is how to arrange the meaning of smt-check result (`SAT`/`UNSAT`) with respect to complete pathes.

If `SAT` is strong (finding a feasible path), `UNSAT` must be either infeasible or incomplete. Currenly, the completeness is an ephemerous state. By _set to false global f_, all the pending states won't be picked. 

The same trick is used in the _eager check_, when they are _set to hotspot_, they also won't be picked. It's easy when it's immediately checked in that clause.

```
c1 = ...
r1 = c1 ? ( 
          c2 = ...
          r2 = c2 ? ( t ) : ( ... )
        )
        : ( ... )
```

Consider the case `t -> c2 -> c1`. `c2` is the pre-conditions for all the lookup that `c2` depends. The same for `c1`. 

Case 1: if the pre-conditions fail (never satisfiable)?

If `c2` fails, `t` fails.
If `c1` fails, and in this case `c1` is immediately depends by `c2` so we need to check `c1 && c2`. If this fails, `c1` and `c2` fails.


```
c1 = ...
...
  r1 = c1a ? ( e1a ) : ( ... )
...
  r1 = c1b ? ( e1b ) : ( ... )
...

c2 <- e1a or e1b
r2 = c2 ? ( t ) : ( ... )
```

Case 2: if some possible precondition fails?

If `c1a` (not with `c2`) fails, `c2` is still possible
If `c1a && c2` fails, `c2` is still possible.
Therefore, the difference is to check `c1a`, we need more information. e.g. if `c1a` is possible while `c1a && c2` fails, checking `c1a` without `c2` gives us inaccurate result.
If `(c1a || c2a) && c2` fails, all possible choice of `c2` fails, so does `c2`.



...

## Problem of non-immediate eager check
 
for `c2`:

```
p_c2 -> p_c1a ...
p_c1a -> c2
``` 




## To-do

gc the phi