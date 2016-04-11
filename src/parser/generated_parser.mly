%{
open Ast;;
module List = BatList;;
%}

%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token EOF 
%token OPEN_BRACE 
%token CLOSE_BRACE 
%token OPEN_BRACKET 
%token CLOSE_BRACKET 
%token OPEN_PAREN 
%token CLOSE_PAREN 
%token SEMICOLON
%token COMMA
%token EQUALS 
%token ARROW 
%token QUESTION_MARK 
%token TILDE 
%token COLON
%token LEFT_ARROW
%token BANG
%token DOT
%token KEYWORD_FUN 
%token KEYWORD_INT
%token KEYWORD_REF
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

%start <Ast.expr> prog
%start <Ast.expr option> delim_expr

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
  | variable
      { Var_body($1) }
  | variable variable
      { Appl_body($1,$2) }
  | variable TILDE pattern QUESTION_MARK function_value COLON function_value
      { Conditional_body($1,$3,$5,$7) }
  | variable DOT identifier
      { Projection_body($1,$3) }
  | BANG variable
      { Deref_body($2) }
  | variable LEFT_ARROW variable
      { Update_body($1,$3) }
  | variable BINOP_PLUS variable
      { Binary_operation_body($1,Binary_operator_plus,$3) }
  | variable BINOP_MINUS variable
      { Binary_operation_body($1,Binary_operator_int_minus,$3) }
  | variable BINOP_LESS variable
      { Binary_operation_body($1,Binary_operator_int_less_than,$3) }
  | variable BINOP_LESS_EQUAL variable
      { Binary_operation_body($1,Binary_operator_int_less_than_or_equal_to,$3) }
  | variable BINOP_EQUAL variable
      { Binary_operation_body($1,Binary_operator_equal_to,$3) }
  | variable KEYWORD_AND variable
      { Binary_operation_body($1,Binary_operator_bool_and,$3) }
  | variable KEYWORD_OR variable
      { Binary_operation_body($1,Binary_operator_bool_or,$3) }
  | KEYWORD_NOT variable
      { Unary_operation_body(Unary_operator_bool_not,$2) }
  | variable OPEN_BRACKET variable CLOSE_BRACKET
      { Indexing_body($1,$3) }
  ;

value:
  | record_value
      { Value_record($1) }
  | function_value
      { Value_function($1) }
  | ref_value
      { Value_ref($1) }
  | int_value
      { Value_int($1) }
  | string_value
      { Value_string($1) }
  | bool_value
      { Value_bool($1) }
  ;

record_value:
  | OPEN_BRACE CLOSE_BRACE
      { Record_value(Ident_map.empty) }
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, record_element) CLOSE_BRACE
      { Record_value(Ident_map.of_enum @@ List.enum $2) }
  ;
  
record_element:
  | identifier EQUALS variable
      { ($1,$3) }
  ;
  
function_value:
  | KEYWORD_FUN variable ARROW OPEN_PAREN expr CLOSE_PAREN
      { Function_value($2,$5) }
  ;
  
ref_value:
  | KEYWORD_REF variable
      { Ref_value($2) }
  ;

int_value:
  | INT_LITERAL
      { $1 }
  ;

string_value:
  | STRING_LITERAL
      { $1 }
  ;

bool_value:
  | KEYWORD_TRUE
      { true }
  | KEYWORD_FALSE
      { false }
  ;

pattern:
  | record_pattern
      { $1 }
  | KEYWORD_INT
      { Int_pattern }
  | bool_pattern
      { Bool_pattern($1) }
  | KEYWORD_STRING
      { String_pattern }
  ;

record_pattern:
  | OPEN_BRACE CLOSE_BRACE
      { Record_pattern(Ident_map.empty) }
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, record_pattern_element) CLOSE_BRACE
      { Record_pattern(Ident_map.of_enum @@ List.enum $2) }
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

separated_nonempty_trailing_list(separator, rule):
  | nonempty_list(terminated(rule, separator))
      { $1 }
  | separated_nonempty_list(separator,rule)
      { $1 }
  ;
