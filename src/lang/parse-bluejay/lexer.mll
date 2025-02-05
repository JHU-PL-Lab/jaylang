
{
  open Parser
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
let comment = '#' [^'\n']* '\n'
let string_contents = [^'"']*

let ident_start = alpha
let ident_cont = alpha | digit | '_'

rule token = parse
| eof                  { EOF }
| "(*" ([^'*']|('*'[^')']))* ("*)"|"**)") {token lexbuf} (* OCaml-like comments *)
| comment              { incr_lineno lexbuf; token lexbuf }
| whitespace           { token lexbuf }
| newline              { incr_lineno lexbuf; token lexbuf }
| "{"                  { OPEN_BRACE }
(* | "{:"                 { OPEN_BRACE_COLON } *)
| "}"                  { CLOSE_BRACE }
(* | ":}"                 { CLOSE_BRACE_COLON } *)
| "("                  { OPEN_PAREN }
| ")"                  { CLOSE_PAREN }
| "["                  { OPEN_BRACKET }
| "]"                  { CLOSE_BRACKET }
(* | ","                  { COMMA } *)
| ";"                  { SEMICOLON }
| "`"                  { BACKTICK }
(* | "'"                  { APOSTROPHE } *)
| "="                  { EQUALS }
| "."                  { DOT }
| ":"                  { COLON }
| "::"                 { DOUBLE_COLON }
| "_"                  { UNDERSCORE }
| "|"                  { PIPE }
| "||"                 { DOUBLE_PIPE }
| "&&"                 { DOUBLE_AMPERSAND }
(* | "$"                  { DOLLAR } *)
(* | "[|"                 { OPEN_OBRACKET }
| "|]"                 { CLOSE_OBRACKET } *)
| "and"                { AND }
| "or"                 { OR }
| "not"                { NOT }
| "int"                { INT_KEYWORD }
| "bool"               { BOOL_KEYWORD }
| "unit"               { UNIT_KEYWORD }
| "fun"                { FUNCTION }
| "function"           { FUNCTION }
(* | "record"             { RECORD } *)
| "with"               { WITH }
| "if"                 { IF }
| "then"               { THEN }
| "else"               { ELSE }
| "let"                { LET }
| "letd"               { LET_D }
| "rec"                { REC }
| "in"                 { IN }
| "->"                 { ARROW }
| "false"              { BOOL false }
| "true"               { BOOL true }
| "input"              { INPUT }
| "match"              { MATCH }
| "end"                { END }
| "assert"             { ASSERT }
| "assume"             { ASSUME }
| "type"               { TYPE }
| "Mu"                 { MU }
| "List"               { LIST }
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
| digit+ as n          { INT (int_of_string n) }
| ident_start ident_cont* as s     { IDENTIFIER s }

{}
