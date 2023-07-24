# How to implement a fmt for a langage

See what [OCamlFormat] does on [Fmt.ast.ml](https://github.com/ocaml-ppx/ocamlformat/blob/main/lib/Fmt_ast.ml).

The entry functions are `fmt_ast` `fmt_file` at the end of the file, and the central functions are `fmt_exression` in the above part.

A quick glimpse is the code mainly uses `hvbox` and `hovbox`. So what are they?

# OCaml box models

In short, OCaml has five boxes used in format printing. 

1. hbox, the horizontal box, never break
2. vbox, the vertical box, always break
3. hvbox, vertical/horizontal box, either one line or the most lines, all-or-nothing. So a hvbox is hbox if possible, or vbox.
4. hov (vertical or horizontal box), do break only when there is no enough line space. Two boxes model for this line after break. The difference can be found on a `b)]` (break,right-parenthesis,box-closure)
4.1. box (named after `open_box`), the structural hov, whose corresponding right parenthesis will appear on a new line and share the same indentation of the left parenthesis.
4.2 hovbox (named after `open_hovbox`), the packed structural hov,  whose corresponding right parenthesis will appear at the end of line.

# Indentation

The indentation is computed as the sum of the box indentation for the **newline** and the one-shot indentation of the previous break (`n` in `print_break _ n`). The box indentation is accumulated along all the wrapping boxes. You won't have the indentation immedately when a box is opened. The box indentation will only be effective after the **newline**.


The example is from this tutorial:

_For instance, if b is break 1 0 in the output "--b--b--", we get_

```text
# hbox 
-- -- --

# vbox (vertical)
-- 
-- 
--

# hvbox  or  hovbox (strutrual or packed) (enough margin)
-- -- --

# hvbox  or  hovbox (strutrual or packed) (insuffient margin)
--
--
--

```

