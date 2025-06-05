
# Embed types as expressions

We take the desugared target and embed it into a low-level language. We use the term "embed" because we most of the logic done in this step is taking types and embedding them as expressions in the target code.

Definitions:
* Double square brackets `[[ . ]]` is a mapping to embed the desugared code.
* A tilde `~` prefixes a reserved label that the programmer cannot create.
* A dollar sign `$` prefixes a fresh name.

Anything that is not explicitly mapped in this specification is handled trivially by recursively embedding the components and reconstructing the same structure again.


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

### Function type

There is a lot of overlap between all four variations created by two toggles:
1. Dependent
2. Deterministic

We are wordy here and write out the full definition for each, but the implementation makes use of the overlap, though the code is not necessarily that clean because of it.

Note that `det` forces any execution inside of it to not use any nondeterminism, unless that nondeterminism is used in an `escape_det`.

`tbl_appl` uses the table to lookup up the associated value, and the table is mutated to map to a new generated value if the key didn't exist.

Nonces are used to differentiate generated functions.

#### Basic function type

```ocaml
[[tau1 -> tau2]] =
  { ~gen = freeze @@
    let $nonce = pick_i in
    fun $arg -> 
      let _ = $nonce in
      let _ = [[tau1]].~check $arg in
      thaw [[tau2]].~gen
  ; ~check = fun $e ->
    [[tau2]].~check ($e (escape_det(thaw [[tau1]].~gen)))
  ; ~wrap = fun $e ->
    fun $arg ->
      let _ = [[tau1]].~check $arg in
      [[tau2]].~wrap ($e ([[tau1]].~wrap $arg))
  }
```

#### Deterministic function type

```ocaml
[[tau1 --> tau2]] =
  { ~gen = freeze @@ 
    let $nonce = pick_i in
    let $tb = table in
    fun $arg ->
      let _ = $nonce in
      let _ = [[tau1]].check $arg in
      tbl_appl($tb, thaw [[tau2]].~gen, $arg)
  ; ~check = fun $e ->
    [[tau2]].~check (det($e (escape_det(thaw [[tau1]].~gen))))
  ; ~wrap = fun $e ->
    fun $arg ->
      let _ = [[tau1]].~check $arg in
      [[tau2]].~wrap ($e ([[tau1]].~wrap $arg))
  }
```

Notes:
* The `det` ensures that the application of `$e` is deterministic.

#### Dependent function type

```ocaml
[[(x : tau_1) -> tau_2]] =
  { ~gen = freeze @@
    let $nonce = pick_i in
    fun $arg -> 
      let _ = $nonce in
      let _ = [[tau1]].check $arg in
      let x = $arg in
      thaw [[tau_2]].~gen
  ; ~check = fun $e ->
    let x = escape_det(thaw [[tau_1]].~gen) in
    [[tau_2]].~check ($e x)
  ; ~wrap = fun $e ->
    fun $arg ->
      let _ = [[tau_1]].~check $arg in
      let x = [[tau_1]].~wrap $arg in
      [[tau_2]].~wrap ($e x)
  }
```

#### Dependent deterministic function type

```ocaml
[[(x : tau_1) -> tau_2]] =
  { ~gen = freeze @@
    let $nonce = pick_i in
    let $tb = table in
    fun $arg -> 
      let _ = $nonce in
      let _ = [[tau1]].check $arg in
      let x = $arg in
      tbl_appl($tb, thaw [[tau_2]].~gen, x)
  ; ~check = fun $e ->
    let x = escape_det(thaw [[tau_1]].~gen) in
    [[tau_2]].~check det($e x)
  ; ~wrap = fun $e ->
    fun $arg ->
      let _ = [[tau_1]].~check $arg in
      let x = [[tau_1]].~wrap $arg in
      [[tau_2]].~wrap ($e x)
  }
```

### Recursive types

#### Standard

```ocaml
[[Mu B x1 ... xn. tau]] =
  thaw @@
  Y (fun $self -> freeze @@ fun x1 -> ... -> fun xn ->
    { ~gen = freeze @@
      let B = thaw $self in thaw [[tau]].~gen
    ; ~check = fun $e ->
      let B = thaw $self in [[tau]].~check $e
    ; ~wrap = fun $e ->
      let B = thaw $self in [[tau]].~wrap $e
    }
  )

Y = 
  fun f ->
    (fun x -> freeze (thaw (f (x x))))
    (fun x -> freeze (thaw (f (x x))))
```

Notes:
* This Y-combinator is special-made with freezing and thawing because this is its only use case.

#### With type-splaying

```ocaml
[[Mu B x1 ... xn. tau]] =
  Y (fun $self -> fun $depth -> fun x1 -> ... fun xn ->
    { ~gen = freeze @@
      if $depth == 0
      then let B = 0 in `Stub [[tau]].~gen (* need to make some B so that [[tau]] is closed *)
      else let B = $self ($depth - 1) in thaw [[tau]].~gen (* still have some depth allowed, so continue normally *)
    ; ~check = fun $e ->
      match $e with
      | `Stub $gen ->
        let B = 0 in (* needed to allow intensional equality to work--must simulate the generator at depth 0 *)
        if $gen === [[tau]].~gen (* intensional equality *)
        then {}
        else abort "Recursive type stub has different nonce than expected"
      | _ -> (* run the normal checker *)
        let B = $self ($depth - 1) in [[tau]].~check $e 
    ; ~wrap = fun $e ->
      let B = $self ($depth - 1) in [[tau]].~wrap $e
    }
  ) 3 (* allowed depth is three by default *)

Y = 
  fun f ->
    (fun x -> fun i -> f (x x) i)
    (fun x -> fun i -> f (x x) i)
```

Notes:
* If the user is type-splaying their recursive functions, then they'll also expect that recursive types can be cut short
* This translation is only valid if the original Bluejay program contains no `input`. It is unsound in that case.
  * Therefore, before we run this translation, we first scan the source code for any `input` and fail as a "parse error" in the case one exists.
* Notice that this match comes after the desugaring phase, so it is allowed to catch literally anything, including untouchable values

### Variant declarations

```ocaml
[[V_0 of tau_0 | ... | V_n of tau_n]] =
  { ~gen = freeze @@
    if pick_i == 123456789 (* unlikely number to pick *)
    then
      (* generate a constructor that is potentially recursive *)
      case pick_i on
      | 1 -> V_i1 (thaw [[tau_i1]].~gen)
      | ...
      | m -> V_im (thaw [[tau_im]].~gen)
      | _ -> V_i0 (thaw [[tau_i0]].~gen)
    else
      (* generate a constructor that is more likely to be terminal *)
      case pick_i on
      | 1 -> V_j1 (thaw [[tau_j1]].~gen)
      | ...
      | l -> V_jl (thaw [[tau_jl]].~gen)
      | _ -> V_j0 (thaw [[tau_j0]].~gen)
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

Notes:
* `i0,...,im, j0,...,jl` are a permutation of `0,...,n`, and `tau_i0,...,tau_im` contain in their AST the identifier of some in-scope Mu type variable while `tau_j0,...,tau_jl` do not.
* No `case` in the generation is needed when there is only one variant constructor. In such a scenario, the entire `gen` is replaced the `default` case.

### Record types

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
    let $candidate = thaw [[tau]].~gen in
    if [[ e_p ]] $candidate
    then $candidate
    else diverge (* i.e. safely quit *)
  ; ~check = fun $e ->
    let _ = [[tau]].~check $e in
    if [[ e_p ]] $e
    then {}
    else abort "Failed predicate"
  ; ~wrap = fun $e ->
    [[tau]].~wrap $e
  }
```

### Type / polymorphic types

Because of the desugaring, we only have types and dependent types instead of polymorphic functions. See the desugaring, and then see the definition of `type` here.

```ocaml
[[ type ]] =
  { ~gen = freeze @@
    let i = pick_i in
    { ~gen = freeze @@ `~Untouched { ~i = i ; ~nonce = pick_i }
    ; ~check = fun $e ->
      match $e with
      | `~Untouched v ->
        if v.~i == i
        then {}
        else abort "Non-equal untouchable values"
    ; ~wrap = fun $e -> $e
    }
  ; ~check = fun $e ->
    let _ = $e.~gen in
    let _ = $e.~check in
    let _ = $e.~wrap in
    {}
  ; ~wrap = fun $e -> $e
  }
```

### Top/bottom

```ocaml
[[ top ]] = 
  { ~gen = freeze @@ `~Top { ~nonce = pick_i }
  ; ~check = fun _ -> {} (* anything is in top *)
  ; ~wrap = fun $e -> $e
  }

[[ bottom ]] =
  { ~gen = freeze @@ diverge (* can't make a value of type bottom, so exit safely *)
  ; ~check = fun _ -> abort "Nothing is in bottom"
  ; ~wrap = fun $e -> $e
  }
```

### Module types

```ocaml
[[ sig val l_0 : tau_0 ... val l_n : tau_n end ]] =
  { ~gen = freeze @@
    let l_0 = thaw [[tau_0]].~gen in (* use the name l_0 to put it in scope *)
    ...
    let l_(n-1) = thaw [[tau_(n-1)]].~gen in (* use the name l_(n-1) to put it in scope *)
    { l_0 = l_0 ; ... ; l_(n-1) = l_(n-1) ; l_n = thaw [[tau_n]].~gen }
  ; ~check = fun $e ->
    let _ = [[tau_0]].~check $e.l_0 in
    let l_0 = $e.l_0 in (* put the name l_0 in scope *) 
    ...
    let _ = [[tau_(n-1)]].~check $e.l_(n-1) in
    let l_(n-1) = $e.l_(n-1) in (* put the name l_(n-1) in scope *)
    let _ = [[tau_n]].~check $e.l_n in
    {}
  ; ~wrap = fun $e ->
    let l_0 = [[tau_0]].~wrap $e.l_0 in (* put the name l_0 in scope *)
    ...
    let l_(n-1) = [[tau_(n-1)]].~wrap $e.l_(n-1) in (* put the name l_(n-1) in scope *)
    { l_0 = l_0 ; ...; l_(n-1) = l_(n-1) ; l_n = [[tau_n]].~wrap $e.l_n }
  }
```

Notes:
* This is the same as dependent records with the `{: :}` syntax.

### Singleton type

The singleton of a type is just the singleton set containing that type.

```ocaml
[[singlet tau]] =
  { ~gen = freeze @@ [[tau]]
  ; ~check = fun $t ->
      let _ =  [[tau]].~check (thaw $t.~gen) in
      $t.~check (thaw [[tau]].~gen)
  ; ~wrap = fun $t -> $t
  }
```

Note:
* The word `singlet` is used instead of `singleton` because it only works on types. e.g. `singlet 5` is bad, whereas a programmer might expect `singleton 5` to work.
* The check goes in both directions: it first asks if `t` is a subtype of `tau` (anything that `t` generates is in `tau`) and then that `tau` is a subtype of `t`, giving equality.

### Intersection types

The "intersection" type has been desugared into a dependent function, so nothing is needed here.

### List type

The `list` type has been desugared into a variant, so nothing is needed here.

#### Other encodings

These ideas are not in the implementation. The only one here (refinement types as dependent records) is just an encoding we could do, but it is not a simple desugar because it requires nontrivial projection of the `~value` label.

### Refinement types as dependent records

```ocaml
[[ { tau | e_p }]] =
  let $r = [[ {: ~value : tau ; ~dummy = if e_p value then unit else bottom :} ]] in (* note this is a dependent record *)
  { ~gen = freeze @@
    (thaw $r.~gen).~value
  ; ~check = fun $e ->
    $r.~check { ~value = $e ; ~dummy = {} }
  ; ~wrap = fun $e ->
    $r.~wrap { ~value = $e ; ~dummy = {} }
  }
```

## Semantic expressions

### Let

```ocaml
[[ let (x : tau) = e in e' ]] =
  let x = 
    let $v = [[ e ]] in
    let _ = [[tau]].~check $v in
    [[tau]].~wrap $v
  in
  [[ e' ]]
```

```ocaml
(* with this flag, we don't run the checker--only wrap *)
[[ let_no_check (x : tau) = e in e' ]] =
  let x =
    [[tau]].~wrap [[ e ]]
  in
  [[ e' ]]

(* with this flag, we don't wrap--only run the checker *)
[[ let_no_wrap (x : tau) = e in e' ]] =
  let x =
    let $v = [[ e ]] in
    let _ = [[tau]].~check $v in
    $v
  in
  [[ e' ]]
```
