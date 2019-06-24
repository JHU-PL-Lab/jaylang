{
  open Onparser;;
}

let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
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
| blank+               { token lexbuf }
| "{"                  { OPEN_BRACE }
| "}"                  { CLOSE_BRACE }
| "("                  { OPEN_PAREN }
| ")"                  { CLOSE_PAREN }
| ","                  { COMMA }
| "="                  { EQUALS }
| "and"                { AND }
| "or"                 { OR }
| "not"                { NOT }
| "fun"                { FUNCTION }
| "function"           { FUNCTION }
| "if"                 { IF }
| "then"               { THEN }
| "else"               { ELSE }
| "let"                { LET }
| "rec"                { REC }
| "in"                 { IN }
| "->"                 { GOESTO }
| "false"              { BOOL false }
| "true"               { BOOL true }
| ";;"                 { EOEX }
| "+"                  { PLUS }
| "-"                  { MINUS }
| "<"                  { BINOP_LESS }
| "<="                 { BINOP_LESS_EQUAL }
| "-"? digit+ as n     { INT_LITERAL (int_of_string n) }
| "\"" (string_contents as s) "\"" { STRING_LITERAL s }
| ident_start ident_cont* as s     { IDENTIFIER s }

{}
