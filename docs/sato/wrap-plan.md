Wrap:

*** Design Questions ***
- When should wrap happen?
  - After type checking?
  - Meaningful to wrap a unchecked function?
  - Complication of combining type-checking and wrapping?
  - Probably should still happen with type-checking? (less work, more code vs more work, less code) <- right now just do it as part of bluejay-to-jay
  - Maybe turn sato off to avoid detecting runtime error eagerly (when it's actually a type error)

~~- Where should wrap happen?
  - At call site vs. At definition~~

- Potential difficulties:
  - Currying <- eta-conversion should solve this problem
  - Need explicit syntactic signal to "wrap"
  - Union & intersection

- "Wrap" in type checking vs actual wrap for runtime: how are they different?
  - Separation of definition vs. use error
  - The "wrap" we did in function generation isn't really "wrap" in the sense 
  that it still very much checks for "definition" rather than "usage". 
  (TODO: Right now our tool doesn't necessarily have the best error msg for this)
  - I still think it might be ideal that our wrapping and error checking occurs
  independently.

*** Example transformation ***

```ocaml
let add (x : int) (y : int) : int = x + y in add
```
==>
```ocaml
let add (x : int) (y : int) : int = 
    (fun x' -> 
        let c1 = (int.check x') in
        if (int.check x') 
        then
            fun y' -> (
            let c2 = (int.check y') in 
            if c2
            then
                (x + y)
            else
                assert c2) y
        else 
            assert c1) x
        
```

```ocaml
let add x y = 
  x + y in
let check_add = 
  let arg1 = input in
  let arg2 = input in
  if int.check (add arg1 arg2) then add else type_error!
in 
check_add
```