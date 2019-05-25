%{
open Odefa_ast;;
open Ast;;
module List = BatList;;
%}

%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token EOF
%token OPEN_PAREN
%token CLOSE_PAREN
%token SEMICOLON
%token EQUALS
%token ARROW
%token QUESTION_MARK
%token COLON
%token KEYWORD_INPUT
%token KEYWORD_FUN
%token KEYWORD_TRUE
%token KEYWORD_FALSE
%token KEYWORD_AND
%token KEYWORD_OR
%token KEYWORD_XOR
%token BINOP_PLUS
%token BINOP_MINUS
%token BINOP_LESS
%token BINOP_LESS_EQUAL
%token BINOP_EQUAL
%token DOUBLE_SEMICOLON

%start <Odefa_ast.Ast.expr> prog
%start <Odefa_ast.Ast.expr option> delim_expr

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
  | separated_nonempty_trailing_list(SEMICOLON, clause)
      { Expr($1) }
  ;

clause:
  | variable EQUALS clause_body
      { Clause($1,$3) }
  ;

variable:
  | identifier
      { Var($1,None) }
  ;

identifier:
  | IDENTIFIER
      { Ident $1 }
  ;

clause_body:
  | value
      { Value_body($1) }
  | KEYWORD_INPUT
      { Input_body }
  | variable
      { Var_body($1) }
  | variable variable
      { Appl_body($1,$2) }
  | variable QUESTION_MARK
        OPEN_PAREN expr CLOSE_PAREN COLON
        OPEN_PAREN expr CLOSE_PAREN
      { Conditional_body($1,$4,$8) }
  | variable BINOP_PLUS variable
      { Binary_operation_body($1,Binary_operator_plus,$3) }
  | variable BINOP_MINUS variable
      { Binary_operation_body($1,Binary_operator_minus,$3) }
  | variable BINOP_LESS variable
      { Binary_operation_body($1,Binary_operator_less_than,$3) }
  | variable BINOP_LESS_EQUAL variable
      { Binary_operation_body($1,Binary_operator_less_than_or_equal_to,$3) }
  | variable BINOP_EQUAL variable
      { Binary_operation_body($1,Binary_operator_equal_to,$3) }
  | variable KEYWORD_AND variable
      { Binary_operation_body($1,Binary_operator_and,$3) }
  | variable KEYWORD_OR variable
      { Binary_operation_body($1,Binary_operator_or,$3) }
  | variable KEYWORD_XOR variable
      { Binary_operation_body($1,Binary_operator_xor,$3) }
  ;

value:
  | function_value
      { Value_function($1) }
  | int_value
      { Value_int($1) }
  | bool_value
      { Value_bool($1) }
  ;

function_value:
  | KEYWORD_FUN variable ARROW OPEN_PAREN expr CLOSE_PAREN
      { Function_value($2,$5) }
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

separated_nonempty_trailing_list(separator, rule):
  | nonempty_list(terminated(rule, separator))
      { $1 }
  | separated_nonempty_list(separator,rule)
      { $1 }
  ;
