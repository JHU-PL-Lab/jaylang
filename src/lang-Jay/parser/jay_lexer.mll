{
  open Jay_parser;;
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
| comment              { incr_lineno lexbuf; token lexbuf }
| whitespace           { token lexbuf }
| newline              { incr_lineno lexbuf; token lexbuf }
| "{"                  { OPEN_BRACE }
| "}"                  { CLOSE_BRACE }
| "("                  { OPEN_PAREN }
| ")"                  { CLOSE_PAREN }
| "["                  { OPEN_BRACKET }
| "]"                  { CLOSE_BRACKET }
| ","                  { COMMA }
| "`"                  { BACKTICK }
| "="                  { EQUALS }
| "."                  { DOT }
| "::"                 { DOUBLE_COLON }
| "_"                  { UNDERSCORE }
| "|"                  { PIPE }
| "and"                { AND }
| "or"                 { OR }
| "not"                { NOT }
| "int"                { INT }
| "bool"               { BOOL_KEYWORD }
| "fun"                { FUNCTION }
| "function"           { FUNCTION }
| "with"               { WITH }
| "if"                 { IF }
| "then"               { THEN }
| "else"               { ELSE }
| "let"                { LET }
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
| digit+ as n          { INT_LITERAL (int_of_string n) }
| ident_start ident_cont* as s     { IDENTIFIER s }

{}
