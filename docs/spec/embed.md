
# Embed types as expressions

We take the desugared target and embed it into a low-level language. We use the term "embed" because we most of the logic done in this step is taking types and embedding them as expressions in the target code.

Definitions:
* Double square brackets `[[ . ]]` is `mapsto` under embedding the desugared code.
* A tilde `~` prefixes a reserved label that the programmer cannot create.
* A dollar sign `$` prefixes a fresh name.


# Target language

The target of the embedding can be found pushed in the `main` branch in the Jaylang repo at `src/lang/ast.ml`

It is simply desugared Bluejay without the types, but it has gained freeze, thaw, id, and casing (which could be sugar for nested conditionals, but we like that they easily give n-ary branches).

## Statements

Programs are statement lists. A statement is a let-expression without a continuation.

Statements are embedded exactly like their corresponding let-expression as it is defined in this document.

## Types

### Int and bool

```ocaml
[[int]] =
  { ~gen = freeze pick_i
  ; ~check = fun $e -> let _ = 0 + $e in {} (* attempt to use as integer to check it *)
  ; ~wrap = fun $e -> $e
  }

[[bool]] =
  { ~gen = freeze pick_b
  ; ~check = fun $e -> let _ = not $e in {} (* attempt to use as bool to check it *)
  ; ~wrap = fun $e -> $e
  }
```

### Arrow

```ocaml
[[tau1 -> tau2]]
  { ~gen = freeze @@
    fun $arg -> 
      let _ = [[tau1]].~check $arg in
      thaw [[tau2]].~gen
  ; ~check = fun $e ->
    [[tau2]].~check ($e (thaw [[tau1]].~gen))
  ; ~wrap = fun $e ->
    fun $x ->
      let _ = [[tau1]].~check $x in
      [[tau2]].~wrap ($e ([[tau1]].~wrap $x))
  }
```

### Mu

```ocaml
[[Mu B. tau]] =
  Y (fun $self -> fun $dummy ->
    { ~gen = freeze @@
      (fun B -> thaw [[tau]].~gen) ($self {})
    ; ~check = fun $e ->
      (fun B -> [[tau]].~check $e) ($self {})
    ; ~wrap = fun $e ->
      (fun B -> [[tau]].~wrap $e) ($self {})
    }
  ) 0

Y = 
  fun f ->
    (fun x -> fun dummy -> f (x x) {})
    (fun x -> fun dummy -> f (x x) {})
```

Notes:
* Use a dummy argument instead of freeze/thaw for simplicity with combinator

### Variants

```ocaml
[[V_0 of tau_0 | ... | V_n of tau_n]] =
  { ~gen = freeze @@
    if pick_i == 123456789 (* unlikely number to pick *)
    then
      case pick_i on
      | 1 -> V_i1 (thaw [[tau_i1]].~gen)
      | ...
      | n -> V_im (thaw [[tau_im]].~gen)
      | _ -> V_i0 (thaw [[tau_i0]].~gen)
    else
      case pick_i on
      | 1 -> V_j1 (thaw [[tau_j1]].~gen)
      | ...
      | n -> V_jl (thaw [[tau_jl]].~gen)
      | _ -> V_j0 (thaw [[tau_j0]].~gen)
    (*
      Where i0,...,im, j0,...,jl are a permutation of 0,...,n, and
      tau_i0,...,tau_im contain in their AST the identifier of some
      in-scope Mu type variable, and tau_j0,...,tau_jl do not.
    *)
  ; ~check = fun $e ->
    match $e with
    | V_0 $v -> [[tau_0]].~check $v
    | ...
    | V_n $v -> [[tau_n]].~check $v
    end
  ; ~wrap = fun $e ->
    match $e with
    | V_0 $v -> V_0 ([[tau_0]].~wrap $v)
    | ...
    | V_n $v -> V_n ([[tau_n]].~wrap $v)
    end
  }
```

### Records

```ocaml
[[{ l_0 : tau_0 ; ... ; l_n : tau_n }]] =
  { ~gen = freeze @@
    { l_0 = thaw [[tau_0]].~gen ; ... ; l_n = thaw [[tau_n]].~gen }
  ; ~check = fun $e ->
    let _ = [[tau_0]].~check $e.l_0 in
    ...
    let _ = [[tau_n]].~check $e.l_n in
    {}
  ; ~wrap = fun $e ->
    { l_0 = [[tau_0]].~wrap $e.l_0 ; ... ; l_n = [[tau_n]].~wrap $e.l_n }
  }
```

### Refinement types

```ocaml
[[{ tau | e_p }]] =
  { ~gen = freeze @@
    let $gend = thaw [[tau]].~gen in
    if [[ e_p ]] $gend
    then $gend
    else diverge (* i.e. safely quit *)
  ; ~check = fun $e ->
    let _ = [[tau]].~check $e in
    if [[ e_p ]] $e
    then {}
    else (`~Predicate_failed $e) e_p (* unsafely quit with a type mismatch *)
  ; ~wrap = fun $e ->
    [[tau]].~wrap $e
  }
```

### Dependent types

```ocaml
[[(x : tau_1) -> tau_2]] =
  { ~gen = freeze @@
    fun $x' -> 
      let _ = [[tau1]].~check $x' in
      thaw [[tau_2[$x'/x]]].~gen
  ; ~check = fun $e ->
    let $arg = thaw [[tau_1]].~gen in
    [[tau_2[$arg/x]]].~check ($e $arg)
  ; ~wrap = fun $e ->
    fun $x' ->
      let _ = [[tau1]].~check $x' in
      [[tau_2[$x'/x]]].~wrap ($e ([[tau_1]].~wrap $x'))
  }
```

Or alternatively, if we do the substitution at interpretation time (which is indeed how we do it in the implementation):

```ocaml
[[(x : tau_1) -> tau_2]] =
  { ~gen = freeze @@
    fun $x' -> 
      let _ = [[tau1]].check $x' in
      (fun x -> thaw [[tau_2]].~gen) $x'
  ; ~check = fun $e ->
    let $arg = thaw [[tau_1]].~gen in
    (fun x -> [[tau_2]].~check) $arg ($e $arg)
  ; ~wrap = fun $e ->
    fun $x' ->
      let _ = [[tau1]].~check $x' in
      (fun x -> [[tau_2]].~wrap) $x' ($e ([[tau_1]].~wrap $x'))
  }
```

### Polymorphic functions / type

Because of the desugaring, we only have types and dependent types instead of polymorphic functions. See the desugaring, and then see the definition of `type` here.

```ocaml
[[ type ]] =
  { ~gen = freeze @@
    let i = pick_i in
    { ~gen = freeze @@ `~Untouched i
    ; ~check = fun $e ->
      match $e with
      | `~Untouched v ->
        if v == i
        then {}
        else $e == `~Untouched i
    ; ~wrap = fun $e -> $e
    }
  , ~check = fun $e ->
    let _ = $e.~gen in
    let _ = $e.~check in
    let _ = $e.~wrap in
    {}
  , ~wrap = fun $e -> $e
  }
```

Notes:
* The `else` case (the only one in the code above) is a nice way to fail unsafely with a type mismatch instead of an `abort`, and if the error finder is instrumented with a way to show the reason for the type mismatch, then this yields a nice error message. `$e` is `~Untouched v`, and `==` only works on int and bool, so this causes a type mismatch suredly.

### Intersection types

```ocaml
[[((V_0 of tau_0) -> tau_0') && ... && ((V_n of tau_n) -> tau_n')]] =
  { ~gen = freeze @@ fun $arg ->
    match $arg with
    | V_0 $v ->
      let _ = [[tau_0]].~check $v in
      thaw [[tau_0']].~gen
    | ...
    | V_n $v ->
      let _ = [[tau_n]].~check $v in
      thaw [[tau_n']].~gen
    end
  ; ~check = fun $e ->
    case pick_i on
    | 1 -> [[(V_1 of tau_1) -> tau_1']].~check $e
    | ...
    | n -> [[(V_n of tau_n) -> tau_n']].~check $e
    | _ -> [[(V_0 of tau_0) -> tau_0']].~check $e
    end
  ; ~wrap = fun $e -> fun $arg ->
    match $arg with
    | V_0 $v ->
      let _ = [[tau_0]].~check $v in
      [[tau_0']].~wrap ($e (V_0 ([[tau_0]].~wrap v)))
    | ...
    | V_n $v ->
      let _ = [[tau_n]].~check $v in
      [[tau_n']].~wrap ($e (V_n ([[tau_n]].~wrap v)))
    end
  }
```

### List type

The `List` type has been desugared into a variant, so nothing is needed here.


## Extensions

Here we have a few ideas for extensions, but they are not fully thought through. They are not in the implementation yet.

#### Top/bottom

```ocaml
[[ top ]] = 
  { ~gen = freeze @@ `~Top 0
  , ~check = fun _ -> {} (* anything is in top *)
  , ~wrap = fun $e -> $e
  }

[[ bottom ]] =
  { ~gen = freeze @@ diverge (* can't make a value of type bottom, so exit safely *)
  , ~check = fun _ -> (`~Bottom {}) (`~Bottom {}) (* nothing is in bottom *)
  , ~wrap = fun $e -> $e
  }
```

### Dependent records

```ocaml
[[ {: l_0 : tau_0 , ... , l_n : tau_n :} ]] =
  { ~gen = freeze @@
    let l_0 = thaw [[tau_0]].~gen in (* use the name l_0 to put it in scope *)
    ...
    let l_(n-1) = thaw [[tau_(n-1)]].~gen in (* use the name l_(n-1) to put it in scope *)
    { l_0 = l_0 , ... , l_(n-1) = l_(n-1) , l_n = thaw [[tau_n]].~gen }
  , ~check = fun $e ->
    let _ = [[tau_0]].~check $e.l_0 in
    let l_0 = $e.l_0 in (* put the name l_0 in scope *) 
    ...
    let _ = [[tau_(n-1)]].~check $e.l_(n-1) in
    let l_(n-1) = $e.l_(n-1) in (* put the name l_(n-1) in scope *)
    let _ = [[tau_n]].~check $e.l_n in
    {}
  , ~wrap = fun $e ->
    let l_0 = = [[tau_0]].~wrap $e.l_0 in (* put the name l_0 in scope *)
    ...
    let l_(n-1) = = [[tau_(n-1)]].~wrap $e.l_(n-1) in (* put the name l_(n-1) in scope *)
    { l_0 = l_0 , ..., l_(n-1) = l_(n-1) , l_n = [[tau_n]].~wrap $e.l_n }
  }
```

Notes:
* Is there a potential issue here that the tau may be a closure, and then the labels don't get put in scope correctly? Does this problem maybe also occur with dependent functions?

### Singleton

The singleton of a type is just the singleton set containing that type.

```ocaml
[[singlet tau]] =
  { ~gen = freeze @@ [[tau]]
  , ~check = [[type]].~check
  , ~wrap = [[type]].~wrap
  }
```

Note:
* The word `singlet` is used instead of `singleton` because it only works on types. e.g. `singlet 5` is bad, whereas a programmer might expect `singleton 5` to work.

### Refinement types as dependent records

```ocaml
[[ { tau | e_p }]] =
  let $r = [[ {: ~value : tau , ~dummy = if e_p value then unit else bottom :} ]] in (* note this is a dependent record *)
  { ~gen = freeze @@
    (thaw $r.~gen).~value
  , ~check = fun $e ->
    $r.~check { ~value = $e , ~dummy = {} }
  , ~wrap = fun $e ->
    $r.~wrap { ~value = $e , ~dummy = {} }
  }
```

### Intersection types as dependent functions

This is really just a desugaring and should be done during the desugar pass, but because it is an extension we're just thinking about, I put it temporarily here.

```ocaml
[[((V_0 of tau_0) -> tau_0') && ... && ((V_n of tau_n) -> tau_n')]] =
  [[ ($x : V_0 of tau_0 | ... | V_n of tau_n) ->
      match $x with
      | V_0 _ -> tau_0'
      | ...
      | V_n _ -> tau_n'
  ]]
```

## Semantic expressions

### Let

```ocaml
[[ let (x : tau) = e in e' ]] =
  let x = 
    let $r = [[tau]] in
    let $v = [[ e ]] in
    let _ = $r.~check $v in
    $r.~wrap $v
  in
  [[ e' ]]
```

```ocaml
(* with this flag, we don't run the checker--only wrap *)
[[ let (x : tau (no check)) = e in e' ]] =
  let x =
    [[tau]].~wrap [[ e ]]
  in
  [[ e' ]]

(* with this flag, tau needs to know the value of x *)
[[ let (x : tau (tau knows binding)) = e in e' ]] =
  let x =
    let x = [[ e ]] in (* use x so that tau can know about it *)
    let $r = [[tau]] in
    let _ = $r.~check x in
    $r.~wrap x
  in
  [[ e' ]]

(* combine both of the above flags *)
[[ let (x : tau (no check, tau knows binding)) = e in e' ]] =
  let x =
    let x = [[ e ]] in (* use x so that tau can know about it *)
    [[tau]].~wrap x
  in
  [[ e' ]]

```

### Binary operations

```ocaml
[[ e binop e' ]] =
  [[ e ]] binop [[ e' ]]
```

### Application

```ocaml
[[ e e' ]] =
  [[ e ]]  [[ e' ]]
```

### Records

```ocaml
[[ { l1 = e1 ; ... ; ln = en } ]] =
  { l1 = [[ e1 ]] ; ... ; ln = [[ en ]] }
```

Note:
* Nothing really to do here because `wrap` handles record subtyping.

### Values

```ocaml
[[ v ]] = v
```