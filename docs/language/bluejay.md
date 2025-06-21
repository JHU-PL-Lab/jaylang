
# Bluejay language

This document aims to informally describe the syntax and semantics of Bluejay. It has last been updated on June 12, 2025.

As a general rule, Bluejay syntax should be considered similar to OCaml. Many characteristics carry over.

## High level

Bluejay has types as values, and expressions as types. There is no separate type language and semantic language; they are both one.

Bluejay uses semantic types. The type system is not syntactic. Only explicitly typed statements are checked, and they are checked semantically: an expression `e` has type `tau` if for all values `v` where `e` evaluates to `v`, it must be that `v` is in `tau`. This is checked using concolic evaluation to enumerate the evaluations of `e` and look for a resulting value `v` that is **not** in `tau`. On recursive well-typed programs, and it possible (and likely) that the search does not terminate.

Remember: only explicitly typed statements are checked. All code is otherwise completely untyped.

Programs are statement lists. Statements are let-expressions without the `in`. All valid statements are also valid as let-expressions by adding an `in`.

Bluejay's semantics are eager: expressions are evaluated as soon as possible.

## Quick start

Want a program that demonstrates some of what Bluejay can do?

Bluejay has refinement and dependent types:

```ocaml
let rec gcd (dependent a : int | a > 0) (dependent b : int | b > 0) : { x : int | x > 0 && x <= a && x <= b } =
  let rem = a % b in
  if rem == 0
  then b
  else gcd b rem

let lcm (dependent n : int | n > 0) (dependent m : int | m > 0) : { x : int | x >= n && x >= m } =
  n * m / gcd n m
```

Types as values, singleton types, and first class modules give us OCaml-style functors:

```ocaml
let MONAD = sig
  val m : type --> type
  val map  : (a : type) -> (b : type) -> m a -> (a -> b) -> m b
  val join : (a : type) -> m (m a) -> m a
  val return : (a : type) -> a -> m a
end

let Derive_bind (dep M : MONAD) : sig
  val m : (a : type) -> singlet (M.m a)
  val bind : (a : type) -> (b : type) -> m a -> (a -> m b) -> m b
end = struct
  let m = M.m
  let bind = fun a b x f -> M.join b (M.map a (M.m b) x f)
end
```

Read on ahead to figure out what else Bluejay can do!

## Features

The full feature list of Bluejay is the following.

**First class values:**
* Integers
* Booleans
* Functions
* Lists
* Records
* Variants
* Modules
* Types

**Types:**
* `int`
* `bool`
* `list tau` for a type `tau`. The type function `list` is not first class.
* `singlet tau` for a type `tau`. The type function `singlet` is not first class. `singlet tau` is the type whose only member is the type `tau`.
* `type`, the type of a type.
* `top`, the type of everything, whose generated member is an usable value, but everthing checks against `top`.
* `bottom`, the subtype of everything, which has no members.
* `{ l_1 : tau_1 ; ... ; l_n : tau_n }`, record types.
* `sig val l_1 : tau1 ... val l_n : tau_n end`, module types.
* `tau1 -> tau2`, function types.
* `(x : tau1) -> tau2`, dependent function types, where `tau2` may use `x`.
* `{ x : tau | pred }` or `{ tau | fun x -> pred }`, refinement types. `tau` is refined to only the members `x` that pass the predicate, i.e. the predicate `pred` evaluates to `true`.
* `mu t. tau` where `tau` uses `t`. This is a recursive type, where `t` is bound to `tau`.
* ```V_1 of tau_1 | ... | `V_n of tau_n``, variant types.
* ``((`V_1 of tau_1) -> tau_1') & ... & ((`V_n of tau_n) -> tau_n')``, function intersection types.
* Any expression used in a place that expects a type must evaluate to a type.

**Operations:**
* `not`, the boolean unary operation.
* Typical integer binary operations. Notably, equality is `==` and works on integers and booleans.
* Record label projection.
* List cons with `(::)` infix operation.
* Function application.

**Control flow:**
* if/then/else
* `assert e` exits the program as an error in the case `e` evaluates to `false`.
* `assume e` exits the program safely in the case `e` evalutes to `false`. It is as if the program diverges detectably, and evaluation ends then and there.
* Pattern matching.

**Language constructs:**
* Let expressions (with and without type annotations).
* Recursive function definitions.

**Patterns:**
* Variant patterns.
* Identifiers patterns, e.g. `x` or `_`. These catch all values.
* List patterns: `[]` or `hd :: tl` for identifiers `hd` and `tl`.
* Patterns may not nest.

**Of note:**
* Identifiers in Bluejay may be uppercase or lowercase. After the first character, they are alphanumeric and can contain underscores.
* The `_` identifier is a valid l-value.
* All variant constructors are identifiers that begin with a backtick.
* All variant constructors are of arity one. That is, they have exactly one value as a payload: no more, no less.
* Bluejay has no side effects besides the `input` construct that evaluates to an unconstrained integer.
* Anything not mentioned here or in the syntax below does not exist. That's right: no strings, floats, arrays, or mutation.

## Syntax

### Literals

```ocaml
42     (* integer literal *)
true   (* boolean literal *)
false  (* boolean literal *)
```

Negative integers must be written as subtractions from `0`.

### Lists

```ocaml
[]                    (* empty list *)
[ 1 ; 2 ; 3 ]         (* list with elements *)
1 :: 2 :: []          (* cons construction *)
[ 1 ; 2 ; 1 + 1 + 1 ] (* list of expressions *)
```

By eager semantics, the fourth list above evaluates to the same value as the second.

### Statements

#### Let Statements

Basic let statements bind a value to a variable:

```ocaml
let x = 2 + 2
```

#### Typed Let Statements

Typed let statements bind a value to a variable and then check the type.

```ocaml
let x : int = 2 + 2
```

### Expressions

Let expressions bind variables in bodies. They may be typed or untyped

```ocaml
let _ =                   (* untyped statement *)
  let x = 2 + 2 in        (* untyped let expression *)
  let y : int = 2 + x in  (* typed let expression *)
  x + y                   (* body of the above `let y` expression *)
```

### Functions

#### Standard functions

```ocaml
let f = fun x -> x + 1    (* anonymous functions *)
let f = fun x y -> x + y  (* anonymous multi-arg functions *)

let f x y = x + y                       (* function definition sugar *)
let f (x : int) (y : int) : int = x + y (* type function definition sugar *)

let r = f 0 1  (* function call *)
```

#### Recursive functions

Single recursive functions must use the function definition sugar and the `rec` keyword. They may be typed or untyped.

```ocaml
let rec fact (n : int) : int =
  if n == 0 then 1 else n * fact (n - 1)
```

Mutually recursive functions are separated by `and`. They need not all be typed or untyped.

```ocaml
let rec even (n : int) : bool =
  n == 0 || odd (n - 1)

and odd n =
  n <> 0 && even (n - 1)
```

#### Polymorphic functions

The `type` keyword declares polymorphic types to be used in a function. Polymorphic type are locally abstract and are not existential. They cannot be inspected.

```ocaml
let appl (type a b) (x : a) (f : a -> b) : b =
  f x
```

There is no type inference, so types must be applied as arguments to instantiate them in the use of a polymorphic function. Type variables declared as above are the first few parameters.

```ocaml
let r = appl int bool 100 (fun x -> x > 0)
```

#### Type functions

Types are values, so parametric types are written as type functions. The type of a list of integers is `list int`.

```ocaml
let ls : list int = [ 1 ; 2 ; 3 ]
```

#### Dependent parameters

Dependent parameters use the `dependent` keyword, or `dep` for short.

```ocaml
let f (dependent b : bool) (dep c : int) : if b || c > 0 then bool else int =
  if b || c > 0
  then b
  else c
```

Types as values and dependent parameters can accomplish the same thing as the `type` syntax for polymorphic functions. This allows type parameters to be in any order. Note that `b` is still locally abstract and cannot be inspected.

```ocaml
let rec map (type a) (ls : list a) (dependent b : type) (f : a -> b) : list b =
  match ls with
  | [] -> []
  | hd :: tl -> f hd :: map a tl b f
  end
```

The above example uses pattern matching. There is more to come on pattern matching.

### Pattern matching

Pattern matches always are terminated with `end` and can combine any sorts of patterns. There are patterns for lists, variants, and identifiers. There are no record patterns. The following is valid.

```ocaml
let _ = 
  match x with
  | [] -> []                  (* empty list pattern *)
  | hd :: tl -> hd + sum tl   (* list cons pattern *)
  | `V y -> 0 - y             (* variant pattern *)
  | `Some z -> z * 2          (* another variant pattern *)
  | _ -> 10                   (* catchall pattern *)
  end
```

It is a parse error to have patterns after a catchall pattern. Catchall patterns are underscore (`_`) and identifiers.

Some examples of invalid patterns are as follows:

```ocaml
let _ =
  match x with
  | a :: [] -> a       (* INVALID: nested patterns because [] is itself a pattern *)
  | [ a ] -> a         (* INVALID: list patterns are only empty and cons *)
  | `None -> 0         (* INVALID: variant pattern without a payload *)
  | `X (`Y i) -> i     (* INVALID: nested patterns again *)
  | y -> y             (* valid identifier pattern catches everything *)
  | `Some i -> i       (* INVALID: pattern after a catchall *)
  end
```

### Types

#### Built-in types

```ocaml
int           (* type of integers *)
bool          (* type of booleans *)
unit          (* the terminal type whose only member is () *)
type          (* type of types *)
top           (* everything has type top *)
bottom        (* nothing has type bottom *)
singlet int   (* the type containing only int *)
list bool     (* the type of lists of booleans )
```

`singlet` and `list` are not first class: they must be applied with a type argument where they are written.

#### Refinement types

Refinement types are written like sets.

```ocaml
let t = { i : int | i > 0 }      (* positive integers *)

let t = { int | fun i -> i > 0 } (* also the positive integers *)

let is_pos = fun i -> i > 0
let t = { int | is_pos }         (* again, the positive integers *)
```

There is sugar for refined function parameters. The following two are equivalent.

```ocaml
let f (x : { i : int | i < 10 }) : bool =
  x > 0

let f (x : int | x < 10) : bool =
  x > 0
```

#### Recursive types

Recursive types use the `mu` keyword.

```ocaml
let peano = mu t. (* t is bound to the following type body *)
  | `Succ of t
  | `Zero of unit
```

Parametrized recursive types are written with additional identifiers before the `.` as seen in the following example.

```ocaml
let ls2 = mu l a b. (* `l` binds the body with parameters `a` and `b` *)
  | `Cons of     (* recursive case *)
    { hd_a : a       (* uses `a` parameter *)
    ; hd_b : b       (* uses `b` parameter *)
    ; tl   : l b a } (* uses `l` and applies `b` and `a` to it *)
  | `Nil of unit (* base case )
```

Uses of `l` in the type body must apply the arguments. In this example, the arguments swap every time.

#### Deterministic function types

Function types are by default assumed to contain nondeterministic function members.

The type of deterministic functions is written with a long arrow `-->`. Members of this type must be deterministic up to intensional equality of the argument.

```ocaml
let det_id : int --> int = fun x -> x     (* deterministic identity function *)

let bad_id : int --> int = fun _ -> input (* this is ill-typed *)
```

One of the only programming patterns that requires this type is type functions. Otherwise, **regular function types with `->` should be used as much as possible** because they are almost always sufficient.

```ocaml
let appl (type a b) (dep det_tf : type --> type) (x : det_tf a) (f : det_tf a -> det_tf b) : det_tf b =
  f x

let _ =
  appl int bool (fun t -> `Id of t) (`Id 0) (fun x ->
    match x with 
    | `Id i -> `Id (i == 0)
    end
  )
```

#### Other types

Other types like variant, record, module, function, and list types are described elsewhere in this file.

### Variants

Variant constructors are polymorphic and need not be declared prior to use.

```ocaml
let some_42 = `Some 42   (* polymorphic variant constructor *)
```

Variant types can be declared and used.

```ocaml
let option a = `Some of a | `None of unit  (* variant type *)
let some_42 : option int = `Some 42        (* value with the option type *)

let some_or_diverge (x : option int) : int =
  match x with
  | `Some i -> i
  | `None _ -> assume false
  end
```

They can also be defined in-place.

```ocaml
let some_or_diverge (x : `Some of int | `None of unit) : int =
  match x with
  | `Some i -> i
  | `None _ -> assume false
  end
```

### Records

Records are also polymorphic, and their types need not be declared prior to use.

```ocaml
let r = { x = 0 ; y = true }  (* record with x and y fields *)
let x_label = r.x             (* project from records with . *)
```

Record types are written with colons. They, too, can be defined in place.

```ocaml
let t = { x : int ; y : bool }   (* record type *)
let r : t = { x = 0 ; y = true } (* record of type t *)

let f (r : t) : bool =
  r.x == 0 && r.y

let f (r : { x : int ; y : bool }) : bool =
  r.x == 0 && r.y
```

Records are reconstructed to remove undeclared labels.

```ocaml
let r : { x : int } = { x = 0 ; y = true } (* this succeeds due to subtyping *)
let _ = r.y (* this fails *)

let f : { x : int } -> { x : int } = fun r -> r
let r = f { x = 0 ; y = true } (* this succeeds due to subtyping *)
let _ = r.y (* this fails just like above *)
```

To write the type of the empty record, use the following syntax.

```ocaml
let mt_type = {:} (* empty record type *)
let mt = {}       (* empty record value *)

let _ : mt_type = mt (* the is well-typed *)
```

### Modules

Modules are first class values. They are containers of statements, just like programs.

Any statement that is valid in a program is valid in a module.

```ocaml
let M = struct
  let t = int         (* can use types as values *)

  let zero = 0        (* untyped statement *)

  let one = zero + 1  (* statements can depend on prior statements *)

  let two : t = 2     (* typed statement *)

  let one = two - one (* later statements shadow earlier ones *)

  let rec sum (ls : list t) : t =
    match ls with
    | [] -> 0
    | hd :: tl -> hd + sum tl
    end
end
```

Module types are just like other types, and they can be seen as dependent record types.

```ocaml
let S = sig
  val t : type   (* types as values. t is any type *)
  val zero : t   (* uses t from above *)
  val one : t    
  val two : t
  val sum : list t -> t
end

let _ : S = M   (* use the above module type to check against M *)

let _ : sig
  val t = int   (* sugar for val t : singlet int, i.e. t is int *)
end = M

let _ : sig
  val t : singlet int (* this is identical to the above *)
end = M
```

Duplicate labels are not allowed in module types, but they are allowed in modules.

### Side effects

The only side effect is random integer generation.

```ocaml
let rand_int = input   (* assigns a random integer to rand_int *)
```

Monads can be used to encode side effects. There is sugar for monadic binding. The following are the same:

```ocaml
let a1 = 
  let%bind x = e in
  e'

let a2 =
  bind e (fun x -> e')
```

If a polymorphic `bind` is in scope, then this sugar is not useful.

## Conclusion

The above syntax can be combined almost indiscriminately to create neat programming patterns.

The test suite has over 8000 lines of Bluejay code. Find it at `jaylang/test/bjy/`.

Look at the following test directories for simple applictions
* `sato-bjy-well-typed` (and `-ill-typed`)
* `oopsla-tests-well-typed` (and `-ill-typed`)
* `edge-cases-well-typed` ..
* `type-splayed-recursion` ..
* `deterministic-functions-well-typed` ..

For many refinement types, see
* `soft-contract-well-typed` (and `-ill-typed`)

For slightly more involved programs, see
* `oopsla-24-benchmarks-well-typed` (and `-ill-typed`)

And for tests that push the limits as we currently know them, see
* `functors-well-typed` (and `-ill-typed`)
* `post-oopsla-well-typed` ..
