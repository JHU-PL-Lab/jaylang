
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
let string_contents = [^'"']*

let ident_start = alpha
let ident_cont = alpha | digit | '_'

rule token = parse
| eof                  { EOF }
| "(*"                 { multi_line_comment 1 lexbuf }
| whitespace           { token lexbuf }
| newline              { incr_lineno lexbuf; token lexbuf }
| "{"                  { OPEN_BRACE }
| "{:"                 { OPEN_BRACE_COLON }
| "}"                  { CLOSE_BRACE }
| ":}"                 { COLON_CLOSE_BRACE }
| "("                  { OPEN_PAREN }
| ")"                  { CLOSE_PAREN }
| "["                  { OPEN_BRACKET }
| "]"                  { CLOSE_BRACKET }
| ";"                  { SEMICOLON }
| "`"                  { BACKTICK }
| "="                  { EQUALS }
| "."                  { DOT }
| ":"                  { COLON }
| "::"                 { DOUBLE_COLON }
| "_"                  { UNDERSCORE }
| "|"                  { PIPE }
| "||"                 { DOUBLE_PIPE }
| "&"                  { AMPERSAND }
| "&&"                 { DOUBLE_AMPERSAND }
| "and"                { AND }
(* | "or"                 { OR } *)
| "not"                { NOT }
| "int"                { INT_KEYWORD }
| "bool"               { BOOL_KEYWORD }
| "unit"               { UNIT_KEYWORD }
| "top"                { TOP_KEYWORD }
| "bottom"             { BOTTOM_KEYWORD }
| "singlet"            { SINGLET_KEYWORD }
| "fun"                { FUNCTION }
| "function"           { FUNCTION }
| "with"               { WITH }
| "if"                 { IF }
| "then"               { THEN }
| "else"               { ELSE }
| "let"                { LET }
| "let%bind"           { LET_BIND}
| "rec"                { REC }
| "in"                 { IN }
| "->"                 { ARROW }
| "-->"                { LONG_ARROW }
| "<-"                 { BACK_ARROW }
| "false"              { BOOL false }
| "true"               { BOOL true }
| "input"              { INPUT }
| "match"              { MATCH }
| "end"                { END }
| "assert"             { ASSERT }
| "assume"             { ASSUME }
| "type"               { TYPE }
| "Mu"                 { MU }
| "list"               { LIST }
| "sig"                { SIG }
| "struct"             { STRUCT }
| "val"                { VAL }
| "of"                 { OF }
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
| digit+ as n          { INT (int_of_string n) }
| ident_start ident_cont* as s     { IDENTIFIER s }

and multi_line_comment depth = parse
| "(*" { multi_line_comment (depth + 1) lexbuf }
| "*)" { if depth = 1 then token lexbuf else multi_line_comment (depth - 1) lexbuf }
| newline { incr_lineno lexbuf; multi_line_comment depth lexbuf }
| eof { failwith "Lexer - unexpected EOF in multi-line comment" }
| _ { multi_line_comment depth lexbuf }

{}
