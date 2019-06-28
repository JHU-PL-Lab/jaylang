%{
open On_ast;;
module List = BatList;;
%}

%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token <bool> BOOL
%token EOF
%token OPEN_PAREN
%token CLOSE_PAREN
%token EQUALS
%token ARROW
%token FUNCTION
%token WITH
%token LET
%token IN
%token REC
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token NOT
%token BINOP_PLUS
%token BINOP_MINUS
%token BINOP_LESS
%token BINOP_LESS_EQUAL
%token BINOP_EQUAL

/*
 * Precedences and associativities.  Lower precedences come first.
 */
%right prec_let                         /* Let ... In ... */
%right prec_fun                         /* function declaration */
%right prec_if                          /* If ... Then ... Else */
%right OR                               /* Or */
%right AND                              /* And */
%left BINOP_EQUAL BINOP_LESS BINOP_LESS_EQUAL  /* = */
%left BINOP_PLUS BINOP_MINUS            /* + - */

%start <On_ast.expr> prog

%%

prog:
  | expr EOF
      { $1 }
  ;

expr:
  | unary_expr
      { $1 }
  | expr BINOP_PLUS expr
      { Plus($1, $3) }
  | expr BINOP_MINUS expr
      { Minus($1, $3) }
  | expr BINOP_LESS expr
      { LessThan($1, $3) }
  | expr BINOP_LESS_EQUAL expr
      { Leq($1, $3) }
  | expr AND expr
      { And($1, $3) }
  | expr OR expr
      { Or($1, $3) }
  | expr BINOP_EQUAL expr
      { Equal($1, $3) }
  | FUNCTION param_list ARROW expr %prec prec_fun
      { Function($2, $4) }
  | LET REC fun_sig_list IN expr %prec prec_fun
      { LetRecFun($3, $5) }
  | IF expr THEN expr ELSE expr %prec prec_if
      { If($2, $4, $6) }
  | LET ident_decl EQUALS expr IN expr %prec prec_let
      { Let($2, $4, $6) }
  | LET fun_sig IN expr %prec prec_fun
      { LetFun($2, $4)}
;

fun_sig:
  | ident_decl param_list EQUALS expr
    { Funsig ($1, $2, $4) }

fun_sig_list:
  | fun_sig { [$1] }
  | fun_sig WITH fun_sig_list { $1 :: $3 }

unary_expr:
  | NOT simple_expr { Not($2) }
  | appl_expr { $1 }

appl_expr:
  | appl_expr simple_expr
    { Appl($1, $2) }
  | simple_expr { $1 }
;

simple_expr:
  | INT_LITERAL
      { Int $1 }
  | BOOL
      { Bool $1 }
  | ident_usage
      { $1 }
  | OPEN_PAREN expr CLOSE_PAREN
      { $2 }
;

param_list:
  | ident_decl param_list { $1 :: $2 }
  | ident_decl { [$1] }
;

ident_usage:
  | ident_decl { Var $1 }
;

ident_decl:
  | IDENTIFIER { Ident $1 }
;
