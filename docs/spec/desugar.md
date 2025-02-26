
# Desugar

We desugar into a subset of Bluejay so that we have fewer translation cases.

Definitions:
* "OCaml-array" brackets `[| . |]` is `mapsto` under desugaring.
* A tilde `~` prefixes a reserved record label that the programmer cannot create.
* A dollar sign `$` prefixes a fresh name.

# Desugared target

The target of the desugaring can be found pushed in the `main` branch in the Jaylang repo at `src/lang/ast.ml`.

Note that we add `abort`, and `diverge` in this pass, and everything else exists already in Bluejay.

## Statements

Programs are statement lists. Each statement is a let-expression without a continuation.

Statements are desugared exactly like their corresponding let-expressions, except each top-level resulting `let` (e.g. there are many with a recursive function) is its own statement. We need no special cases, and the spec to desugar a statement is inferred from all definitions below and this simple transformation described here.

## Functions

Note that untyped functions are all the same, but we just don't used typed let expressions.

### Let functions

```ocaml
[| let f (x_1 : tau_1) ... (x_n : tau_n) : tau =
  e
  in
  e' |] =
let (f : [| tau_1 -> ... -> tau_n -> tau |]) = 
  fun x_1 -> ... -> fun x_n ->
    [| e |]
in
[| e' |] 
```

### Let poly

```ocaml
[| let f (type a_1 ... a_n) (x_1 : tau_1) ... (x_m : tau_m) : tau =
    e
  in
  e' |] =
let (f : [| forall a_1, ..., a_n. tau_1 -> ... -> tau_m -> tau |]) =
  fun a_1 -> ... -> fun a_n ->
    fun x_1 -> ... -> fun x_m ->
      [| e |]
in
[| e' |]

[| forall alpha_1, ..., alpha_n. tau1 -> tau2 |] =
  (alpha_1 : type) -> ... -> (alpha_n : type) -> [| tau1 -> tau2 |] 
```

Notes:
* We have created a `type` in the target language of the desugar, so this is actually not just a subset of Bluejay. See the translation for the definition of `type`.


### Let dependent

```ocaml
[| letd f (x : tau_1) : tau_2 =
    e
  in
  e' |] =
let (f : [| (x : tau_1 ) -> tau_2 |]) = fun x -> [| e |]
in
[| e' |]
```

### Let dependent poly

```ocaml
[| let f (type a_1 ... a_n) (x : tau_1) : tau_2 =
    e
  in
  e' |] =
let (f : [| forall a_1, ..., a_n. (x : tau_1) -> tau_2 |]) =
  fun a_1 -> ... -> fun a_n ->
    fun x ->
      [| e |]
in
[| e' |]

(* to be clear *)
[| forall alpha_1, ..., alpha_n. (x : tau1) -> tau2 |] =
  (alpha_1 : type) -> ... -> (alpha_n : type) -> [| (x : tau1) -> tau2 |] 
```

### Let rec

Here is an initial example for how recursive functions get desugared.

```ocaml
[| let rec f1 (x1_1 : tau1_1) ... (x1_m1 tau1_m1) : tau1 = e1
  with ...
  with fn (xn_1 : taun_1) ... (xn_mn : taun_mn) : taun = en
  in
  e |] =
let $f1 =
  fun $f1 -> fun $f2 -> ... -> fun $fn -> 
    fun x1_1 -> ... -> fun x1_m1 ->
      let (f1 : [| tau_f1 |] (flag: no check, tau knows binding)) = $f1 $f1 $f2 ... $fn in (* these are the actual names of the functions so that e1 uses them as normal *)
      let (f2 : [| tau_f2 |]) = $f2 $f1 $f2 ... $fn in (* no check here as well and as in similar cases *)
      ...
      let (fn : [| tau_fn |]) = $fn $f1 $f2 ... $fn in
      [| e1 |] (* this uses the f1, ..., fn as normal *)
in
...
let $fn =
  fun $f1 -> fun $f2 -> ... -> fun $fn ->
    fun xn_1 -> ... -> fun xn_mn ->
      let f1 = $f1 $f1 $f2 ... $fn in (* need the types on these too *)
      let f2 = $f2 $f1 $f2 ... $fn in
      ...
      let fn = $fn $f1 $f2 ... $fn in
      [| en |]
in
let (f1 : [| tau1_1 -> ... -> tau1_m1 -> tau1 |] (flag: tau knows binding)) = $f1 $f1 $f2 ... $fn in
...
let (fn : [| taun_1 -> ... -> taun_mn -> taun |]) = $fn $f1 $f2 ... $fn in (* same flags *)
[| e |]
```

Note:
* This doesn't wrap the recursive calls. We need to creatively wrap without checking the functions we use for recursion.
* We need to specially handle each type of function, which is a mess. This motivates a two-step process which I describe below.

The process of the general desugar is as follows:
* We can send any mutally recursive functions to a list of their id, type, parameters, and body.
* We take that list and map it as in the following code block.

```ocaml
[ (f1, tau_f1_opt, [ x1_1 , ... , x1_m1 ], e1)
; ...
; (fn, tau_fn_opt, [ xn_1 , ... , xn_mn ], en)
], e |->
let $f1 =
  fun $f1 -> ... -> fun $fn ->
    fun x1_1 -> ... -> fun x1_m1 ->
      let (f1 : [| tau_f1_opt |] (flag: no check)) = $f1 $f1 $f2 ... $fn in
      ...
      let (fn : [| tau_fn_opt |] (flag: no check)) = $fn $f1 $f2 ... $fn in
      [| e1 |]
in
...
let $fn =
  fun $f1 -> ... -> fun $fn ->
    fun xn_1 -> ... -> fun xn_mn ->
      let (f1 : [| tau_f1_opt |] (flag: no check)) = $f1 $f1 $f2 ... $fn in
      ...
      let (fn : [| tau_fn_opt |] (flag: no check)) = $fn $f1 $f2 ... $fn in
      [| en |]
in
let (f1 : [| tau_f1_opt |]) = $f1 $f2 $f2 ... $fn in
...
let (fn : [| tau_fn_opt |]) = $fn $f2 $f2 ... $fn in
[| e |]
```

Notes:
* If the tau is None, then we do an untyped let-expression. I leave this as a note for conciseness because I feel it is clear from context what we mean in the definition above.
* Notice the obvious parallel between the first few let-expressions in each function body and the top-level let-expressions we make at the end to actually define the functions for later use. We make use of this in the implementation.

We get the id, type, parameters, and body using the let rules we've already seen. Here is an example for let-poly functions.

```ocaml
let f1 (type a1_1 ... a1_n1) (x1_1 : tau1_1) ... (x1_m1 : tau1_m1) : tau1 =
  e1
with
...
fn (type an_1 ... a1_nn) (xn_1 : taun_1) ... (xn_mn : taun_mn) : taun =
  en
in
e
|->
[ (f1, Some ((a1_1 : type) -> ... -> (a1_n1 : type) -> tau1_1 -> ... -> tau1_m1 -> tau1), [ a1_1 , ... , a1_n1 , x1_1 , .... , x1_m1 ], e1)
; ...
; (fn, Some ((an_1 : type) -> ... -> (an_nn : type) -> taun_1 -> ... -> taun_mn -> taun), [ an_1 , ... , an_nn , xn_1 , .... , xn_mn ], en)
], e
```


### Multi-arg functions

```ocaml
[| fun x1 ... xn -> e |] =
  fun x1 -> ... -> fun xn -> [| e |]
```

## List

```ocaml
let filter_list x =
  match x with
  | `Nil _ -> x
  | `Cons _ -> x
  end

[| List tau |] =
  Mu $t.
  ``Nil unit (* This is a unique variant name that the user cannot create, and unit is a dummy payload *)
  || ``Cons {: ~hd : [| tau |] , ~tl : $t :} (* so is this *)

[| x :: xs |] =
  `Cons { ~hd = x , ~tl = filter_list [| x |] }

[| [] |] =
  `Nil {} 

[| [ x1 , ... , xn ] |] =
  [| x1 :: ... :: xn :: [] |]

(* pattern *)
[| [] -> e |] =
  `Nil -> [| e |]

(* pattern *)
[| hd :: tl -> e |] =
  `Cons $r -> let hd = $r.~hd in let tl = $r.~tl in [| e |]
```

Notes:
* We need to instrument cons because otherwise it always looks well-typed. So we filter all things on the right of `(::)` to be lists.

## Variant

```ocaml
[| V_i e |] =
  V_i [| e |]
```

## Pattern matching

```ocaml
[| match e with
  | p1 -> e1
  | ...
  | pi -> ei
  | (any or variable) -> eany
  | ...
  | pn -> en
  end |] =
match [| e |] with
| [| p1 -> e1 |] 
| ...
| [| pi -> ei |]
| `~Untouched $ignore -> abort
| (any or variable) ->
  [| eany |]
(* the patterns following `any` or a catchall variable are not reachable *)
end

(* patterns. See List for these cases, too. They are copied here *)
[| p -> e |] =
  p -> [| e |]

(* Empty list pattern *)
[| [] -> e |] =
  `Nil -> [| e |]

(* Cons pattern *)
[| hd :: tl -> e |] =
  `Cons $r -> let hd = $r.~hd in let tl = $r.~tl in [| e |]
```

Notes:
* This involves desugaring list patterns, which is why we capture the whole `pat -> expr` instead of pattern and expression separately.
* We assume that Untouched is a unique variant name the user cannot create.

## Intersection types

```ocaml
[| ((V_0 of tau_0) -> tau_0') && ... && ((V_n of tau_n) -> tau_n') |] =
  [| ($x : V_0 of tau_0 | ... | V_n of tau_n)  ->
        match $x with
        | V_0 _ -> tau_0'
        | ...
        | V_n _ -> tau_n' |]
```

## Assert/assume

```ocaml
[| assert e |] =
  if [| e |]
  then {}
  else abort

[| assume e |]
  if [| e |]
  then {}
  else diverge
```

## And/or

We need to desugar and/or to short-circuit, and this is required so that we can try to solve for the case of the left expression that causes us to evaluate the right expression. This way, in interpretation of the target language, we don't have to think about how the short-circuiting of the boolean operations affects branches.

```ocaml
[| e and e' |] =
  if [| e |]
  then [| e' |]
  else false

[| e or e' |] = 
  if [| e |]
  then true
  else [| e' |]
``` 

I am still unsure if we want to do this. The alternative (which is the old solution) is to not short-circuit at all. Adding the short-circuit introduces branches (but it may help us identify solves to skip that are pinned). I will for now NOT do this desugar here, and instead I will leave and/or as binops in the target language which we might later remove with this. Note, though, that we often add this branch ourselves if we want to short-circuit (which is often) because we want to skip all of the branches in `e'`. I am likely to benchmark how this affects performance.

## Division/modulus

Division and modulus can go wrong if right expression is `0`. We instrument during the desugar process to add this as a branch in the program.

```ocaml
[| e / e' |] =
  let $v = [| e' |] in (* only evaluate once *)
  if $v == 0
  then abort
  else [| e |] / $v

[| e % e' |] =
  let $v = [| e' |] in (* only evaluate once *)
  if $v == 0
  then abort
  else [| e |] % $v
```

## Arrow

```ocaml
[| tau1 -> tau2 |] =
  [| tau1 |] -> [| tau2 |]

[| (x : tau1) -> tau2 |] =
  (x : [| tau1 |]) -> [| tau2 |]
```

## Monadic syntax

```ocaml
[| let%bind x = e in e' |] =
  [| bind e (fun x -> e') |]
```

Notes:
* This implicitly fails if there is no appropriate function called `bind` in scope

## Conclusion

Unless it was explicitly mentioned in this file, everything else is desugared by just recursively desugaring its components.