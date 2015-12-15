%{
open Ast;;
%}

%token <string> IDENTIFIER
%token EOF 
%token OPEN_BRACE 
%token CLOSE_BRACE 
%token OPEN_PAREN 
%token CLOSE_PAREN 
%token SEMICOLON
%token COMMA
%token EQUALS 
%token ARROW 
%token QUESTION_MARK 
%token TILDE 
%token COLON 
%token KEYWORD_FUN 
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
  ;

value:
  | record_value
      { Value_record($1) }
  | function_value
      { Value_function($1) }
  ;

record_value:
  | OPEN_BRACE CLOSE_BRACE
  |   { Record_value(Ident_set.empty) }
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, identifier) CLOSE_BRACE
      { Record_value(Ident_set.of_list $2) }
  ;
  
function_value:
  | KEYWORD_FUN variable ARROW OPEN_PAREN expr CLOSE_PAREN
      { Function_value($2,$5) }
  ;

pattern:
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, identifier) CLOSE_BRACE
      { Record_pattern(Ident_set.of_list $2) }
  ;

separated_nonempty_trailing_list(separator, rule):
  | nonempty_list(terminated(rule, separator))
      { $1 }
  | separated_nonempty_list(separator,rule)
      { $1 }
  ;
