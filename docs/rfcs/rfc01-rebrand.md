---
title: JHU PL Lab Demand-driven Project Rebranding
date: Jun 2022
---

We (JHU PL Lab) have several done and ongoing projects related to the demand-driven approach to features in functional languages.

We use these abbreviations currently:

- ODEFA: Order-DEpendent Functional Analysis
- NATODEFA: Natural ODEFA
- DDPA: Demand-driven Program Analysis
- DDSE: Demand-driven Symbolic Evaluation
- DBMC: Demand-driven Bounded Model Checking
- SATO: Symbolic Analysis Typechecker for Odefa
- and more

Now we're thinking of giving them a family of related names. My thoughts are:

1. Language-centric: we pick a language, and the related tools are named after the standard convention as any other language, e.g., OCaml or Java.
2. One research project can have an arbitrary name, as the leader's will. 
3. Tool name doesn't need to be the same as the project name.
4. Don't use a fancy name when there is no research project for it.

# Language names

The language is called **Jay**. For all the _uninteresting_ parts of the language toolchains, we don't give it a special name. This includes:

- The _untyped_ _natural_ syntax is **Jay** `.jy` (**Natodefa**).
- The intermediate language is **JayIL** `.jil` (**Odefa**)
- The _typed_ _natural_ language is **BlueJay** `.bjy` (**TypedNatodefa**)

I prefer Jay file extension `.jy` rather than `.jay` because BlueJay extension `.bjy` can be a super-string of `.jy`.

<!-- The _typed_ language is open to name `BlueJay` `AzureJay` `TypeJay`. -->

<!-- The intermediate language is open to JayIL, JayIR, JayNF, CoreJay. No `DwarfJay` yet because no specific project is on this level. -->

# Project names

**DBMC** and **TypeSato** are two research projects. The research project can have a fancy name, and it should also have a descriptive explanation in simple English.

Since **DBMC** and **Sato** are not languages, it's fine that their new names don't follow -Jay pattern. They are just techniques or applications.

The symbolic execution driver is called **DeeJay**, Demand-driven symbolic ExEcutor on JAY. It sounds like DJ, so the tool is `dj`.

# Tool examples

```shell
# open the REPL
jay

# interpret a program
jay echo.jy

# generate inputs that run to the end
dj birdie.bjy

# genereate inputs that run to `target`
dj -t target birdie.jy

# genereate inputs that run to last clause
dj -t target wing.jil

# genereate inputs that trigger a failure assert
dj --assert birdie.jy

# genereate inputs that trigger a type-error
dj --check-type full.jy
dj-type full.jy
```

# Research project names and paper names

`dj` is the symbolic execution driver. Each project (if it's still on the symbolic land) can have an alias binary. This binary is equivalent to invode `dj` with a specific argument.

I mention paper titles for completeness, but they can be plain, so they're totally in another land.

Project **DBMC** is just called project **DeeJay** since it focuses on the symbolic engine of this language while BMC is the working model.

Its name on paper need not be fancy: Higher-Order DBMC is still good.

Project **TypeSato** is called **Jayzz** (sounds _jazz_)

Its name on paper need not be fancy too: Refutation for Higher-Order Set-Type is still good.

# Summary

My general idea is to treat `Jay` as a normal language. Languages can have fancy names, and language tools can have derived names, unless a specific tool becomes a research project. A project can have a fancy name, but the tool doesn't.

Under my scheme, the language is `BlueJay` `Jay` and `JayIL` respectively, corresponding to previous `TypeNatodefa` `Natodefa` `Odefa`.

For tools,

**ODEFA** will be unsed.
**DDPA** is only used as a program analysis model. `jay-analysis --ddpa0 source.jay`.
**DDSE** is only used as a symbolic execution model. `dy --model=ddse source.jay`
**DMBC** is only used as the default symbolic execution model. `dy <--model=dbmc> source.jay`
**SATO** will be unsed. `dy --check-type full.jay` or `jayzz full.jay`.

# reference

https://expeditedsecurity.com/aws-in-plain-english/
[POLA]: https://en.wikipedia.org/wiki/Principle_of_least_astonishment