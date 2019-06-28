%{
open On_ast;;
module List = BatList;;
exception On_Parse_error of string;;
%}

%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL
%token EOF
%token OPEN_BRACE
%token CLOSE_BRACE
%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token COMMA
%token EQUALS
%token ARROW
%token DOT
%token FUNCTION
%token WITH
%token LET
%token IN
%token REC
%token MATCH
%token PIPE
%token END
%token BACKTICK
%token DOUBLE_COLON
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token NOT
%token ANY
%token INT
%token STRING
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
%right DOUBLE_COLON                     /* head :: tail */
%right OR                               /* Or */
%right AND                              /* And */
%left BINOP_EQUAL BINOP_LESS BINOP_LESS_EQUAL  /* = */
%left BINOP_PLUS BINOP_MINUS            /* + - */
%right prec_label                       /* `A expr */
%left DOT                               /* record access */

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
  | expr DOT label
      { RecordProj($1, $3) }
  | MATCH expr WITH match_expr_list END
      { Match($2, $4) }
  | BACKTICK variant_label expr %prec prec_label
      { VariantExpr($2, $3)  }
  | OPEN_BRACKET expr_list CLOSE_BRACKET
      { List ($2) }
  | OPEN_BRACKET CLOSE_BRACKET { List ([]) }
  | expr DOUBLE_COLON expr
      { ListCons($1, $3) }
;

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }

pattern:
  | ANY { AnyPat }
  | INT { IntPat }
  | BOOL { if $1 then TruePat else FalsePat}
  | FUNCTION { FunPat }
  | STRING { StringPat }
  | OPEN_BRACE record_pattern_body CLOSE_BRACE { RecPat($2) }
  | OPEN_BRACE CLOSE_BRACE { RecPat (Ident_map.empty) }
  | BACKTICK variant_label pattern
    { VariantPat (Variant($2,$3)) } %prec prec_label
  | IDENTIFIER { VarPat (Ident($1)) }
  | OPEN_BRACKET CLOSE_BRACKET { EmptyLstPat }
  | pattern DOUBLE_COLON pattern { LstDestructPat($1, $3) }
;

record_pattern_body:
  | label EQUALS pattern
      { let (Label k) = $1 in
        let key = Ident k in
        Ident_map.singleton key $3 }
  | label EQUALS pattern COMMA record_pattern_body
      { let (Label k) = $1 in
        let key = Ident k in
        let old_map = $5 in
        let dup_check = Ident_map.mem key old_map in
        if dup_check then raise (On_Parse_error "Duplicate label names in record!")
        else
        let new_map = Ident_map.add key $3 old_map in
        new_map
      }
;

match_expr:
  | PIPE pattern ARROW expr
    { ($2, $4) }

match_expr_list:
  | match_expr { [$1] }
  | match_expr match_expr_list { $1 :: $2 }

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
  | STRING_LITERAL
      { String $1 }
  | BOOL
      { Bool $1 }
  | ident_usage
      { $1 }
  | OPEN_BRACE record_body CLOSE_BRACE
      { Record $2 }
  | OPEN_BRACE CLOSE_BRACE
      { Record (Ident_map.empty) }
  | OPEN_PAREN expr CLOSE_PAREN
      { $2 }
;

record_body:
  | label EQUALS expr
      { let (Label k) = $1 in
        let key = Ident k in
        Ident_map.singleton key $3 }
  | label EQUALS expr COMMA record_body
      { let (Label k) = $1 in
        let key = Ident k in
        let old_map = $5 in
        let dup_check = Ident_map.mem key old_map in
        if dup_check then raise (On_Parse_error "Duplicate label names in record!")
        else
        let new_map = Ident_map.add key $3 old_map in
        new_map
      }
;

param_list:
  | ident_decl param_list { $1 :: $2 }
  | ident_decl { [$1] }
;

variant_label:
  | IDENTIFIER { Variant_label $1 }
;

label:
  | IDENTIFIER { Label $1 }
;

ident_usage:
  | ident_decl { Var $1 }
;

ident_decl:
  | IDENTIFIER { Ident $1 }
;
