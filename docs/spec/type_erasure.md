
# Erase types from Bluejay programs

Bluejay programs are actually run (after the type-checking phase) in "type erased" mode. All types are sent to unusable unit values `()`, and type functions are consequently sent to functions that return `()`.

Type erasure defines the *meaning* of *running* a Bluejay program after the program has been type checked. The meaning of types in Bluejay are defined by their embeddings (see `embed.md`), and they do not have a semantics otherwise--hence we erase them.

Definitions:
* Angle brackets with pipes `<| . |> : Bluejay -> TypeErased` is a mapping to erase types from Bluejay code.
* Anything that is not explicitly mapped in this specification is implicitly mapped homomorphically; structure is preserved, and components are recursively mapped.

## Types

All translations of types are the same: they are destroyed and sent to unit values.

The categorization here is just for simple organization.

### Primitive types

```ocaml
<| type |> = ()

<| int |> = ()

<| bool |> = ()

<| top |> = ()

<| bottom |> = ()

<| unit |> = ()
```

### Record and module types

```ocaml
<| { l_1 : tau_1 ; ... ; l_n : tau_n } |> = ()

<| sig val l_1 : tau_1 ... val l_n : tau_n end |> = ()
```

### Function types

```ocaml
<| tau_1 -> tau_2 |> = ()

<| tau_1 --> tau_2 |> = ()

<| (x : tau_1) -> tau_2 |> = ()

<| (x : tau_1) --> tau_2 |> = ()
```

### Refinement types

```ocaml
<| { tau | e } |> = ()
```

### Variant and function intersection types

```ocaml
<| `V_1 of tau_1 | ... | `V_n of tau_n |> = ()

<| ((`V_0 of tau_0) -> tau_0') & ... & ((`V_n of tau_n) -> tau_n') |> = ()
```

### Type functions

Type functions need to accept parameters to preserve the semantics of their meaning. The parameters are discarded, and the type-erased version returns `()`.

```ocaml
<| list |> = fun _ -> ()

<| singlet |> = fun _ -> ()
```

## Recursive types

```ocaml
<| mu t. tau |> = ()

<| mu t a_1 a_2 ... a_n. tau |> = 
  fun _ _ ... _ -> ()
```

Notes:
* Without any parameters, a recursive type can just be `()`.
* For `n` parameters on `t`, there are `n` ignored function parameters in the type-erased target.

## Let expressions

```ocaml
<| let x : tau = e in e' |> =
  let x = <| e |> in <| e' |>

<| let (rec) f (type a_1 ... a_n) (x_1 : tau_1) ... (x_m : tau_m) : tau =
    e
   in
   e' |>
  let (rec) f a_1 ... a_n x_1 ... x_m =
    <| e |>
  in
  <| e' |>
```

## Statements

Statements are type erased correspondingly to let expressions.

## Everything else

Everything else is type erased homomorphically as stated above.

For example, with addition, type erasure is defined as follows.

```ocaml
<| e + e' |> =
  <| e |> + <| e' |>
```
