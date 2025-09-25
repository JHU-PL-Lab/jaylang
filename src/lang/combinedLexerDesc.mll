(*
This file is a combined description of the various Bluejay-related languages.
During the build process, Dune runs the "uncombine.py" descript to split it
into multiple different files which are then processed as usual by ocamllex.
The "uncombine.py" script includes instructions describing how it operates but,
generally, special comments which include the commands "scope" and "endscope"
are used to describe which lines of this file are preserved in each version of
the language and which lines are erased.
*)

{
  (*! scope bluejay !*)
  open BluejayParserDesc
  (*! endscope !*)
  (*! scope desugared !*)
  open DesugaredParserDesc
  (*! endscope !*)
  (*! scope embedded !*)
  open EmbeddedParserDesc
  (*! endscope !*)
  open Lexing
  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let alpha_upper = ['A'-'Z']
let whitespace = [' ' '\t']
let newline = '\n'
let string_contents = [^'"']*

let ident_start = alpha
let ident_cont = alpha | digit | '_'

rule token = parse
| eof                  { EOF }
| "(*"                 { multi_line_comment 1 lexbuf }
| whitespace           { token lexbuf }
| newline              { incr_lineno lexbuf; token lexbuf }
| "{"                  { OPEN_BRACE }
| "}"                  { CLOSE_BRACE }
| "("                  { OPEN_PAREN }
| ")"                  { CLOSE_PAREN }
| ";"                  { SEMICOLON }
| "`"                  { BACKTICK }
| "="                  { EQUALS }
| "."                  { DOT }
(*! scope bluejay desugared !*) (* must stay here vs. :: *)
| ":"                  { COLON }
(*! endscope !*)
| "_"                  { UNDERSCORE }
| "|"                  { PIPE }
| "||"                 { DOUBLE_PIPE }
(*! scope bluejay !*) (* must stay here vs. && *)
| "&"                  { AMPERSAND }
(*! endscope !*)
| "&&"                 { DOUBLE_AMPERSAND }
| "not"                { NOT }
| "fun"                { FUNCTION }
| "function"           { FUNCTION }
| "with"               { WITH }
| "if"                 { IF }
| "then"               { THEN }
| "else"               { ELSE }
| "let"                { LET }
| "let%bind"           { LET_BIND}
| "in"                 { IN }
| "->"                 { ARROW }
| "false"              { BOOL false }
| "true"               { BOOL true }
| "match"              { MATCH }
| "end"                { END }
| "struct"             { STRUCT }
| "defer"              { DEFER }
| "+"                  { PLUS }
| "-"                  { MINUS }
| "*"                  { ASTERISK }
| "/"                  { SLASH }
| "%"                  { PERCENT }
| "=="                 { EQUAL_EQUAL }
| "<>"                 { NOT_EQUAL }
| "<"                  { LESS }
| "<="                 { LESS_EQUAL }
| ">"                  { GREATER }
| ">="                 { GREATER_EQUAL }
| "|>"                 { PIPELINE }
(*! scope bluejay desugared !*)
| "-->"                { LONG_ARROW }
| "bool"               { BOOL_KEYWORD }
| "bottom"             { BOTTOM_KEYWORD }
| "input"              { INPUT }
| "int"                { INT_KEYWORD }
| "mu"                 { MU }
| "of"                 { OF }
| "sig"                { SIG }
| "singletype"         { SINGLETYPE_KEYWORD }
| "top"                { TOP_KEYWORD }
| "type"               { TYPE }
| "unit"               { UNIT_KEYWORD }
| "val"                { VAL }
(*! endscope !*)
(*! scope bluejay !*)
| "["                  { OPEN_BRACKET }
| "]"                  { CLOSE_BRACKET }
| "::"                 { DOUBLE_COLON }
| "and"                { AND }
| "assert"             { ASSERT }
| "assume"             { ASSUME }
| "dependent"          { DEPENDENT }
| "dep"                { DEP }
| "list"               { LIST }
| "rec"                { REC }
| "abstract"           { ABSTRACT }
(*! endscope !*)
(*! scope desugared !*)
| "#abort"             { ABORT }
| "#no_check"          { NO_CHECK }
| "#no_wrap"           { NO_WRAP }
| "#vanish"            { VANISH }
| "#gen"               { GEN }
(*! endscope !*)
(*! scope embedded !*)
| ","                  { COMMA }
| "#pick_i"            { PICK_I }
| "#pick_b"            { PICK_B }
| "#case"              { CASE }
| "#default"           { DEFAULT }
| "#freeze"            { FREEZE }
| "#thaw"              { THAW }
| "#id"                { ID }
| "#ignore"            { IGNORE }
| "#tableCreate"       { TABLE_CREATE }
| "#tableAppl"         { TABLE_APPL }
| "#det"               { DET }
| "#escapedet"         { ESCAPEDET }
| "#intensionalEqual"  { INTENSIONAL_EQUAL }
| "#untouchable"       { UNTOUCHABLE }
(*! endscope !*)
| digit+ as n          { INT (int_of_string n) }
| ident_start ident_cont* as s     { IDENTIFIER s }

and multi_line_comment depth = parse
| "(*" { multi_line_comment (depth + 1) lexbuf }
| "*)" { if depth = 1 then token lexbuf else multi_line_comment (depth - 1) lexbuf }
| newline { incr_lineno lexbuf; multi_line_comment depth lexbuf }
| eof { failwith "Lexer - unexpected EOF in multi-line comment" }
| _ { multi_line_comment depth lexbuf }

{}
