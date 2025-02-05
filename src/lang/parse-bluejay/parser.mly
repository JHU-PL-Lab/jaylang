
%{
  module Langs = struct end (* Ugly fix for Menhir issue with nested modules *)
  (* Note: because AST uses GADTs and constraints, I must do some type annotation in this file *)
  open Ast
  open Binop
  open Pattern
  open Expr
  open Parsing_tools
%}

%token <string> IDENTIFIER
%token <int> INT
%token <bool> BOOL
%token EOF
%token OPEN_BRACE
%token CLOSE_BRACE
%token SEMICOLON
%token BACKTICK
%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token EQUALS
%token ARROW
%token DOT
%token COLON
%token DOUBLE_COLON
%token UNDERSCORE
%token PIPE
%token DOUBLE_PIPE
%token DOUBLE_AMPERSAND
%token FUNCTION
%token WITH
%token LET
%token LET_D
%token IN
%token REC
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token NOT
%token INT_KEYWORD
%token BOOL_KEYWORD
%token UNIT_KEYWORD
%token INPUT
%token MATCH
%token END
%token ASSERT
%token ASSUME
%token TYPE
%token MU
%token LIST
%token PLUS
%token MINUS
%token ASTERISK
%token SLASH
%token PERCENT
%token LESS
%token LESS_EQUAL
%token GREATER
%token GREATER_EQUAL
%token EQUAL_EQUAL
%token NOT_EQUAL

%token OPEN_BRACE_COLON
%token COLON_CLOSE_BRACE

/*
 * Precedences and associativities.  Lower precedences come first.
 */
%nonassoc prec_let prec_fun   /* Let-ins and functions */
%nonassoc prec_if             /* Conditionals */
%nonassoc prec_mu             /* mu types */
%nonassoc prec_list_type      /* list types */
%left OR                      /* Or */
%left AND                     /* And */
%right NOT                    /* Not */
/* == <> < <= > >= */
%left EQUAL_EQUAL NOT_EQUAL LESS LESS_EQUAL GREATER GREATER_EQUAL
%right DOUBLE_COLON           /* :: */
%left PLUS MINUS              /* + - */
%left ASTERISK SLASH PERCENT  /* * / % */
%right ASSERT ASSUME prec_variant    /* Asserts, Assumes, and variants */
%right ARROW                  /* -> for type declaration */
%right DOUBLE_AMPERSAND      /* && for type intersection */

%start <Bluejay.statement list> prog
%start <Bluejay.statement list option> delim_expr

%%

prog:
  | statement_list EOF { $1 }
  ;

delim_expr:
  | EOF
      { None }
  | prog EOF
      { Some ($1) }
  ;

statement_list:
  | statement { [ $1 ] }
  | statement statement_list { $1 :: $2 }

statement:
  | LET ident_decl EQUALS expr
      { SUntyped { var = $2 ; body = $4 } : Bluejay.statement }
  | LET OPEN_PAREN ident_decl COLON expr CLOSE_PAREN EQUALS expr
      { STyped { typed_var = { var = $3 ; tau = $5 } ; body = $8 } : Bluejay.statement }
  | letfun_rec
      { SFunRec $1 : Bluejay.statement }
  | letfun
      { SFun $1 : Bluejay.statement }

/* **** Expressions **** */


expr:
  | appl_expr /* Includes primary expressions */
      { $1 : Bluejay.t }
  | ASSERT expr
      { EAssert $2 : Bluejay.t }
  | ASSUME expr
      { EAssume $2 : Bluejay.t }
  | variant_label expr %prec prec_variant
      { EVariant { label = $1 ; payload = $2 } : Bluejay.t }
  | expr ASTERISK expr
      { EBinop { left = $1 ; binop = BTimes ; right = $3 } : Bluejay.t }
  | expr SLASH expr
      { EBinop { left = $1 ; binop = BDivide ; right = $3 } : Bluejay.t }
  | expr PERCENT expr
      { EBinop { left = $1 ; binop = BModulus ; right = $3 } : Bluejay.t }
  | expr PLUS expr
      { EBinop { left = $1 ; binop = BPlus ; right = $3 } : Bluejay.t }
  | expr MINUS expr
      { EBinop { left = $1 ; binop = BMinus ; right = $3 } : Bluejay.t }
  | expr DOUBLE_COLON expr
      { EListCons ($1, $3) : Bluejay.t }
  | expr EQUAL_EQUAL expr
      { EBinop { left = $1 ; binop = BEqual ; right = $3 } : Bluejay.t }
  | expr NOT_EQUAL expr
      { EBinop { left = $1 ; binop = BNeq ; right = $3 } : Bluejay.t }
  | expr GREATER expr
      { EBinop { left = $1 ; binop = BGreaterThan ; right = $3 } : Bluejay.t }
  | expr GREATER_EQUAL expr
      { EBinop { left = $1 ; binop = BGeq ; right = $3 } : Bluejay.t }
  | expr LESS expr
      { EBinop { left = $1 ; binop = BLessThan ; right = $3 } : Bluejay.t }
  | expr LESS_EQUAL expr
      { EBinop { left = $1 ; binop = BLeq ; right = $3 } : Bluejay.t }
  | NOT expr
      { ENot $2 : Bluejay.t }
  | expr AND expr
      { EBinop { left = $1 ; binop = BAnd ; right = $3 } : Bluejay.t }
  | expr OR expr
      { EBinop { left = $1 ; binop = BOr ; right = $3 } : Bluejay.t }
  | IF expr THEN expr ELSE expr %prec prec_if
      { EIf { cond = $2 ; true_body = $4 ; false_body = $6 } : Bluejay.t }
  | FUNCTION ident_decl ARROW expr %prec prec_fun 
      { EFunction { param = $2 ; body = $4 } : Bluejay.t }
  | FUNCTION param_list ARROW expr %prec prec_fun
      { EMultiArgFunction { params = $2 ; body = $4 } : Bluejay.t }
  // Let
  | LET ident_decl EQUALS expr IN expr %prec prec_let
      { ELet { var = $2 ; body = $4 ; cont = $6 } : Bluejay.t }
  | LET OPEN_PAREN ident_decl COLON expr CLOSE_PAREN EQUALS expr IN expr %prec prec_let
      { ELetTyped { typed_var = { var = $3 ; tau = $5 } ; body = $8 ; cont = $10 } : Bluejay.t }
  // Functions
  | letfun_rec IN expr %prec prec_fun
      { ELetFunRec { funcs = $1 ; cont = $3 } : Bluejay.t }
  | letfun IN expr %prec prec_fun
      { ELetFun { func = $1 ; cont = $3 } : Bluejay.t }
  // Match
  | MATCH expr WITH PIPE? separated_nonempty_list(PIPE, match_expr) END
      { EMatch { subject = $2 ; patterns = $5 } : Bluejay.t }
  // Types expressions
  | INT_KEYWORD
      { ETypeInt : Bluejay.t }
  | BOOL_KEYWORD
      { ETypeBool : Bluejay.t }
  | UNIT_KEYWORD
      { ETypeRecord empty_record : Bluejay.t }
  | LIST expr %prec prec_list_type
      { ETypeList $2 : Bluejay.t } 
  | MU ident_decl DOT expr %prec prec_mu
      { ETypeMu { var = $2 ; body = $4 } : Bluejay.t}
  | expr ARROW expr
      { ETypeArrow { domain = $1 ; codomain = $3 } : Bluejay.t }
  | OPEN_PAREN ident_decl COLON expr CLOSE_PAREN ARROW expr
      { ETypeArrowD { binding = $2 ; domain = $4 ; codomain = $7 } : Bluejay.t }
  | OPEN_BRACE expr PIPE expr CLOSE_BRACE
      { ETypeRefinement { tau = $2 ; predicate = $4 } : Bluejay.t }
  | variant_type_body
      { ETypeVariant $1 : Bluejay.t }
  | intersection_type_body
      { ETypeIntersect $1 : Bluejay.t } 
;

(* doesn't *really* need parens, but without them we would never get a meaningful intersection type *)
intersection_type_body:
  | OPEN_PAREN OPEN_PAREN variant_type_label expr CLOSE_PAREN ARROW expr CLOSE_PAREN
      { [ ($3, $4, $7) ] }
  | OPEN_PAREN OPEN_PAREN variant_type_label expr CLOSE_PAREN ARROW expr CLOSE_PAREN DOUBLE_AMPERSAND intersection_type_body
      { ($3, $4, $7) :: $10 }
;

variant_type_body:
  | variant_type_label expr { [($1, $2)] }
  | variant_type_label expr DOUBLE_PIPE variant_type_body { ($1, $2) :: $4 }

record_type:
  | OPEN_BRACE record_type_body CLOSE_BRACE
      { ETypeRecord $2 : Bluejay.t }
  | OPEN_BRACE_COLON separated_nonempty_list(SEMICOLON, record_type_item) COLON_CLOSE_BRACE
      { ETypeRecordD $2 : Bluejay.t }

record_type_item:
  | record_label COLON expr
      { $1, $3 : RecordLabel.t * Bluejay.t }

record_type_body:
  | record_label COLON expr
      { new_record $1 $3 }
  | record_label COLON expr SEMICOLON record_type_body
      { add_record_entry $1 $3 $5 }

// basic_types:

/* **** Functions **** */

letfun:
  | LET fun_sig_non_dep
  | LET_D fun_sig_dependent { $2 }

letfun_rec:
  | LET REC separated_nonempty_list(WITH, fun_sig_non_dep)
  | LET_D REC separated_nonempty_list(WITH, fun_sig_dependent) { $3 }

/* let foo x = ... */
/* let foo (x : int) ... : int = ... */
/* let foo (type a b) (x : int) ... : t = ... */
fun_sig_non_dep:
  | ident_decl param_list EQUALS expr
      { FUntyped { func_id = $1 ; params = $2 ; body = $4 } : Bluejay.funsig }
  | ident_decl param_list_with_type COLON expr EQUALS expr
      { FTyped { type_vars = [] ; func_id = $1 ; params = $2 ; ret_type = $4 ; body = $6 } : Bluejay.funsig }
  | ident_decl OPEN_PAREN TYPE param_list CLOSE_PAREN param_list_with_type COLON expr EQUALS expr 
      { FTyped { type_vars = $4 ; func_id = $1 ; params = $6 ; ret_type = $8 ; body = $10 } : Bluejay.funsig }

/* letd foo (x : int) ... : t = ... */
/* letd foo (type a b) (x : int) ... : t = ... */
fun_sig_dependent:
  | ident_decl param_with_type COLON expr EQUALS expr
      { FDepTyped { type_vars = [] ; func_id = $1 ; params = $2 ; ret_type = $4 ; body = $6 } : Bluejay.funsig }
  | ident_decl OPEN_PAREN TYPE param_list CLOSE_PAREN param_with_type COLON expr EQUALS expr
      { FDepTyped { type_vars = $4 ; func_id = $1 ; params = $6 ; ret_type = $8 ; body = $10 } : Bluejay.funsig }

/* **** Primary expressions **** */

/* (fun x -> x) y */
appl_expr:
  | appl_expr primary_expr { EAppl { func = $1 ; arg = $2 } : Bluejay.t }
  | primary_expr { $1 : Bluejay.t }
;

/* In a primary_expr, only primitives, vars, records, and lists do not need
   surrounding parentheses. */
primary_expr:
  | INT
      { EInt $1 : Bluejay.t }
  | BOOL
      { EBool $1 : Bluejay.t }
  | INPUT
      { EPick_i : Bluejay.t }
  | TYPE
      { EType : Bluejay.t }
  | ident_usage
      { $1 : Bluejay.t }
  | OPEN_BRACE record_body CLOSE_BRACE
      { ERecord $2 : Bluejay.t }
  | OPEN_BRACE CLOSE_BRACE
      { ERecord empty_record : Bluejay.t }
  | OPEN_BRACKET separated_nonempty_list(SEMICOLON, expr) CLOSE_BRACKET
      { EList $2 : Bluejay.t }
  | OPEN_BRACKET CLOSE_BRACKET
      { EList [] : Bluejay.t }
  | OPEN_PAREN expr CLOSE_PAREN
      { $2 }
  | record_type
      { $1 : Bluejay.t }
  | primary_expr DOT record_label
      { EProject { record = $1 ; label = $3} : Bluejay.t }
;

/* **** Idents + labels **** */

param_list_with_type:
  | param_with_type param_list_with_type { $1 :: $2 }
  | param_with_type { [$1] }
;

param_with_type:
  | OPEN_PAREN ident_decl COLON expr CLOSE_PAREN
      { { var = $2 ; tau = $4 } : Bluejay.typed_var }
;

param_list:
  | ident_decl param_list { $1 :: $2 }
  | ident_decl { [ $1 ] }
;

record_label:
  | ident_decl { RecordLabel.RecordLabel $1 }
;

ident_usage:
  | ident_decl { EVar $1 : Bluejay.t }
;

ident_decl:
  | IDENTIFIER { Ident.Ident $1 }
;

/* **** Records, lists, and variants **** */

/* {x = 1, y = 2, z = 3} */
record_body:
  | record_label EQUALS expr
      { new_record $1 $3 }
  | record_label EQUALS expr SEMICOLON record_body
      { add_record_entry $1 $3 $5 }
;

/* e.g. `Variant 0 */
variant_label:
  | BACKTICK ident_decl { VariantLabel.VariantLabel $2 }

/* e.g. ``Variant int */ 
variant_type_label:
  | BACKTICK BACKTICK ident_decl { VariantTypeLabel.VariantTypeLabel $3 }

/* **** Pattern matching **** */

match_expr:
  | pattern ARROW expr
      { ($1, $3) : Bluejay.pattern * Bluejay.t }

pattern:
  | UNDERSCORE { PAny }
  | ident_decl { PVariable $1 }
  | variant_label ident_decl { PVariant { variant_label = $1 ; payload_id = $2 } }
  | variant_label OPEN_PAREN ident_decl CLOSE_PAREN { PVariant { variant_label = $1 ; payload_id = $3 } }
  | OPEN_BRACKET CLOSE_BRACKET { PEmptyList }
  | ident_decl DOUBLE_COLON ident_decl { PDestructList { hd_id = $1 ; tl_id = $3 } }
  | OPEN_PAREN pattern CLOSE_PAREN { $2 }
;
