
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

Note that untyped functions are all the same, and we would use untyped let expressions.

### Non-rec functions

```ocaml
[| let f (type a_1 ... a_n) (x_1 : tau_1) ... (x_m : tau_m) : tau =
  e
  in
  e' |] =
let (f : [| (a_1 : type) -> ... -> (a_n : type) -> tau_1 -> ... -> tau_m -> tau |]) = 
  fun a_1 -> ... -> fun a_n ->
    fun x_1 -> ... -> fun x_m ->
      [| e |]
in
[| e' |] 
```

Notes:
* The back arrow is our notation for a dependent parameter.
* Any dependent parameter `(x_i <- tau_i)` is instead in the final arrow type as `... -> (x_1 : tau_i) -> ...`, i.e. the dependent parameters are translated naturally into dependent arrows.
* `n` may be zero (i.e. no type variables), and `m` is positive, as is enforced in the parser.

### Recursive functions

```ocaml
[| let f1 (type a1_1 ... a1_n1) (x1_1 : tau1_1) ... (x1_m1 : tau1_m1) : tau1 =
  e1
  and ...
  and fn (type an_1 ... an_nn) (xn_1 : taun_1) ... (xn_mn : taun_mn) : taun =
    en
  in
  e |] =
(* this record is never accessible to the user, so we don't need to create unusable labels *)
let $r = Y_n
  (fun f1 -> ... -> fun fn ->
    let_no_check f1 : (a1_1 : type) -> ... -> (a1_n1 : type) -> tau1_1 -> ... -> tau1_m1 -> tau1 =
      fun a1_1 -> ... -> fun a1_n1 ->
        fun x1_1 -> ... -> fun x1_m1 ->
          [| e1 |]
    in
    f1)
  ...
  (fun f1 -> ... -> fun fn ->
    let_no_check fn : (an_1 : type) -> ... -> (an_nn : type) -> taun_1 -> ... -> taun_mn -> taun =
      fun an_1 -> ... -> fun an_nn ->
        fun xn_1 -> ... -> fun xn_mn ->
          [| en |]
    in
    fn)

(* first expose the names so that the types can use them *)
let f1 = $r.f1
...
let fn = $r.fn
(* then run the checkers on the types, but don't wrap because that would be a double wrap *)
let_no_wrap f1 : (a1_1 : type) -> ... -> (a1_n1 : type) -> tau1_1 -> ... -> tau1_m1 -> tau1 = f1
...
let_no_wrap fn : (an_1 : type) -> ... -> (an_nn : type) -> taun_1 -> ... -> taun_mn -> taun = fn
```

for `Y_n` the fixed point combinator on `n` mutually recursive functions.

```ocaml
Y_n = fun f1 ... fn ->
  Y (fun self f1 ... fn ->
    { l1 = fun x ->
      let r = self f1 ... fn in
      f1 r.l1 ... r.ln x
    ; ...
    ; ln = fun x ->
      let r = self f1 ... fn in
      fn r.l1 ... r.ln x
    }
  ) f1 ... fn

Y = fun f ->
  (fun s -> fun x -> f (s s) x)
  (fun s -> fun x -> f (s s) x)
```

Notes:
* The same as non-rec functions...

### Multi-arg functions

```ocaml
[| fun x1 ... xn -> e |] =
  fun x1 -> ... -> fun xn -> [| e |]
```

## List

```ocaml
let filter_list x =
  match x with
  | `~Nil _ -> x
  | `~Cons _ -> x
  end

[| list tau |] =
  Mu $t.
  | `~Nil of unit (* This is a unique variant name that the user cannot create, and unit is a dummy payload *)
  | `~Cons of { ~hd : [| tau |] ; ~tl : $t } (* so is this *)

[| x :: xs |] =
  `~Cons { ~hd = [| x |] ; ~tl = filter_list [| xs |] }

[| [] |] =
  `~Nil {} 

[| [ x1 ; ... ; xn ] |] =
  [| x1 :: ... :: xn :: [] |]
```

Notes:
* We need to instrument cons because otherwise it always looks well-typed. So we filter all things on the right of `(::)` to be lists.

## Dependent record / module

```ocaml
[| struct let l_1 : tau_1 = e_1 ... let l_n : tau_n = e_n end |] =
  [| let l_1 = e_1 in
    ...
    let l_n = e_n in 
    { l_1 = l_1 ; ... ; l_n = l_n } |]
```

## Pattern matching

```ocaml
[| match e with
  | p_1 -> e_1
  | ...
  | p_n -> e_n
  | l_ident -> e_l_ident
  end |] =
match [| e |] with
| [| p_1 -pat-> e_1 |] 
| ...
| [| p_n -pat-> e_n |]
| `~Untouched _ -> abort "Matched untouchable value"
| l_ident -> [| e_l_ident |]
end

(* patterns. See List for these cases, too. They are copied here *)
[| p -pat-> e |] =
  p -pat-> [| e |]

(* Empty list pattern *)
[| [] -pat-> e |] =
  `~Nil -pat-> [| e |]

(* Cons pattern *)
[| hd :: tl -pat-> e |] =
  `~Cons $r -pat-> let hd = $r.~hd in let tl = $r.~tl in [| e |]
```

Notes:
* This involves desugaring list patterns, which is why we capture the whole `pat -> expr` instead of pattern and expression separately.
* We assume that Untouched is a unique variant name the user cannot create.
* It is a parse error to have any patterns following a catchall pattern (any/_ or an identifier)

## Intersection types

```ocaml
[| ((`V_0 of tau_0) -> tau_0') & ... & ((`V_n of tau_n) -> tau_n') |] =
  [| ($x : `V_0 of tau_0 | ... | `V_n of tau_n) ->
        match $x with
        | `V_0 _ -> tau_0'
        | ...
        | `V_n _ -> tau_n' |]
```

## Assert/assume

```ocaml
[| assert e |] =
  if [| e |]
  then {}
  else abort "Failed assertion"

[| assume e |]
  if [| e |]
  then {}
  else diverge
```

## And/or

We need to desugar and/or to short-circuit, and this is required so that we can try to solve for the case of the left expression that causes us to evaluate the right expression. This way, in interpretation of the target language, we don't have to think about how the short-circuiting of the boolean operations affects branches.

```ocaml
[| e && e' |] =
  if [| e |]
  then [| e' |]
  else false

[| e || e' |] = 
  if [| e |]
  then true
  else [| e' |]
``` 

I am still unsure if we want to do this. The alternative (which is the old solution) is to not short-circuit at all. Adding the short-circuit introduces branches (but it may help us identify solves to skip that are pinned). I will for now NOT do this desugar here, and instead I will leave and/or as binops in the target language which we might later remove with this. Note, though, that we often add this branch ourselves if we want to short-circuit (which is often) because we want to skip all of the branches in `e'`. I am likely to benchmark how this affects performance.

## Division/modulo

Division and modulus can go wrong if right expression is `0`. We instrument during the desugar process to add this as a branch in the program.

```ocaml
[| e / e' |] =
  let $v = [| e' |] in (* only evaluate once *)
  if $v == 0
  then abort "Divide by 0"
  else [| e |] / $v

[| e % e' |] =
  let $v = [| e' |] in (* only evaluate once *)
  if $v == 0
  then abort "Modulo by 0"
  else [| e |] % $v
```

## Arrow

```ocaml
[| tau1 -> tau2 |] =
  [| tau1 |] -> [| tau2 |]

[| (x : tau1) -> tau2 |] =
  (x : [| tau1 |]) -> [| tau2 |]
```

## Parse-time

The following desugaring steps are trivial and are done while the program is parsed, so there is no language construct for them.

This is certainly a slight hack, but the scope is small enough that it's fine for now.

### Monadic syntax

```ocaml
[| let%bind x = e in e' |] =
  [| bind e (fun x -> e') |]
```

Notes:
* This implicitly fails if there is no appropriate function called `bind` in scope

### Pipelining

```ocaml
[| e |> e' |] =
  [|  e' e |]
```

Notes:
* This means `e'` is evaluated first, which is a somewhat unintuitive evaluation order, and it motivates actually having the pipelining operator in the language, or desugaring to `(fun a b -> b a) e e'`

### Singlet in module types

```ocaml
(* as a line in a module type *)
[| val t = tau |] =
  [| val t : singlet (tau) |]
```

Notes:
* This is like OCaml's `type t = tau` in a module type. It is simple sugar for the singleton type.

## Conclusion

Unless it was explicitly mentioned in this file, everything else is desugared by just recursively desugaring its components and building back up the same expression.