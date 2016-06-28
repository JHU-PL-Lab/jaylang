%{
open Core_ast;;
open Nested_ast;;
open Parser_support;;

module List = BatList;;
%}

%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token OPEN_BRACE
%token CLOSE_BRACE
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token OPEN_PAREN
%token CLOSE_PAREN
%token COMMA
%token ARROW
%token QUESTION_MARK
%token TILDE
%token COLON
%token EQUALS
%token DOT
%token BANG
%token LEFT_ARROW
%token KEYWORD_FUN
%token KEYWORD_LET
%token KEYWORD_IN
%token KEYWORD_REF
%token KEYWORD_INT
%token KEYWORD_TRUE
%token KEYWORD_FALSE
%token KEYWORD_AND
%token KEYWORD_OR
%token KEYWORD_NOT
%token KEYWORD_STRING
%token BINOP_PLUS
%token BINOP_MINUS
%token BINOP_LESS
%token BINOP_LESS_EQUAL
%token BINOP_EQUAL
%token DOUBLE_SEMICOLON
%token EOF

%left LAM
%right KEYWORD_IN
%nonassoc TILDE
%left LEFT_ARROW
%nonassoc BINOP_LESS BINOP_LESS_EQUAL BINOP_EQUAL KEYWORD_OR KEYWORD_AND KEYWORD_NOT OPEN_BRACKET
%left BINOP_PLUS BINOP_MINUS
%right BANG
%left DOT
%right KEYWORD_REF

%start <Nested_ast.expr> prog
%start <Nested_ast.expr option> delim_expr

%%

prog:
  | expr EOF
      { $1 }
  ;

delim_expr:
  | EOF
      { None }
  | expr DOUBLE_SEMICOLON
      { Some($1) }
  | expr EOF
      { Some($1) }
  ;

expr:
  | KEYWORD_LET identifier EQUALS expr KEYWORD_IN expr
      { Let_expr(next_uid $startpos $endpos,Nested_var(next_uid $startpos $endpos,$2),$4,$6) }
  | expr TILDE pattern QUESTION_MARK function_value COLON function_value
      { Conditional_expr(next_uid $startpos $endpos,$1,$3,$5,$7) }
  | expr LEFT_ARROW expr
      { Update_expr(next_uid $startpos $endpos,$1,$3) }
  | expr BINOP_PLUS expr
      { Binary_operation_expr(next_uid $startpos $endpos,$1,Binary_operator_plus,$3) }
  | expr BINOP_MINUS expr
      { Binary_operation_expr(next_uid $startpos $endpos,$1,Binary_operator_int_minus,$3) }
  | expr BINOP_LESS expr
      { Binary_operation_expr(next_uid $startpos $endpos,$1,Binary_operator_int_less_than,$3) }
  | expr BINOP_LESS_EQUAL expr
      { Binary_operation_expr(next_uid $startpos $endpos,$1,Binary_operator_int_less_than_or_equal_to,$3) }
  | expr BINOP_EQUAL expr
      { Binary_operation_expr(next_uid $startpos $endpos,$1,Binary_operator_equal_to,$3) }
  | expr KEYWORD_AND expr
      { Binary_operation_expr(next_uid $startpos $endpos,$1,Binary_operator_bool_and,$3) }
  | expr KEYWORD_OR expr
      { Binary_operation_expr(next_uid $startpos $endpos,$1,Binary_operator_bool_or,$3) }
  | KEYWORD_NOT expr
      { Unary_operation_expr(next_uid $startpos $endpos,Unary_operator_bool_not,$2) }
  | expr OPEN_BRACKET expr CLOSE_BRACKET
      { Indexing_expr(next_uid $startpos $endpos,$1,$3) }
  | function_value
      { Function_expr(next_uid $startpos $endpos,$1) }
  | application_expr
      { $1 }
  ;

application_expr:
  | application_expr primary_expr
      { Appl_expr(next_uid $startpos $endpos,$1,$2) }
  | primary_expr
      { $1 }
  ;

primary_expr:
  | primary_expr DOT identifier
      { Projection_expr(next_uid $startpos $endpos,$1, $3) }
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, record_element) CLOSE_BRACE
      { Record_expr(next_uid $startpos $endpos,Ident_map.of_enum @@ List.enum $2) }
  | OPEN_BRACE CLOSE_BRACE
      { Record_expr(next_uid $startpos $endpos,Ident_map.empty) }
  | int_value
      { Int_expr(next_uid $startpos $endpos,$1) }
  | bool_value
      { Bool_expr(next_uid $startpos $endpos,$1) }
  | string_value
      { String_expr(next_uid $startpos $endpos,$1) }
  | identifier
      { Var_expr(next_uid $startpos $endpos, Nested_var(next_uid $startpos $endpos, $1)) }
  | KEYWORD_REF primary_expr
      { Ref_expr(next_uid $startpos $endpos,$2) }
  | BANG primary_expr
      { Deref_expr(next_uid $startpos $endpos,$2) }
  | OPEN_PAREN expr CLOSE_PAREN
      { $2 }
  ;

record_element:
  | identifier EQUALS expr
      { ($1,$3) }
  ;

pattern:
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, record_pattern_element) CLOSE_BRACE
      { Nested_ast.Record_pattern(next_uid $startpos $endpos,Ident_map.of_enum @@ List.enum $2) }
  | OPEN_BRACE CLOSE_BRACE
      { Nested_ast.Record_pattern(next_uid $startpos $endpos,Ident_map.empty) }
  | KEYWORD_FUN
      { Nested_ast.Fun_pattern(next_uid $startpos $endpos) }
  | KEYWORD_REF
      { Nested_ast.Ref_pattern(next_uid $startpos $endpos) }
  | KEYWORD_INT
      { Nested_ast.Int_pattern(next_uid $startpos $endpos) }
  | bool_pattern
      { Nested_ast.Bool_pattern(next_uid $startpos $endpos,$1) }
  | KEYWORD_STRING
      { Nested_ast.String_pattern(next_uid $startpos $endpos) }
  ;

record_pattern_element:
  | identifier EQUALS pattern
      { ($1,$3) }
  ;

bool_pattern:
  | KEYWORD_TRUE
      { true }
  | KEYWORD_FALSE
      { false }
  ;

function_value:
  | KEYWORD_FUN identifier ARROW expr %prec LAM
      { Function(next_uid $startpos $endpos, Nested_var(next_uid $startpos $endpos, $2),$4) }
  ;

int_value:
  | INT_LITERAL
      { $1 }
  ;

bool_value:
  | KEYWORD_TRUE
      { true }
  | KEYWORD_FALSE
      { false }
  ;

string_value:
  | STRING_LITERAL
      { $1 }
  ;

/*variable:
  | identifier
      { Var($1,None) }
  ;*/

identifier:
  | IDENTIFIER
      { Ident($1) }
  ;

separated_nonempty_trailing_list(separator, rule):
  | nonempty_list(terminated(rule, separator))
      { $1 }
  | separated_nonempty_list(separator,rule)
      { $1 }
  ;
