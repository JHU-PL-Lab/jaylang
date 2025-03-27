
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
%token BACK_ARROW
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
%token LET_BIND
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
%token TOP_KEYWORD
%token BOTTOM_KEYWORD
%token SINGLET_KEYWORD
%token INPUT
%token MATCH
%token END
%token ASSERT
%token ASSUME
%token TYPE
%token MU
%token LIST
%token SIG
%token STRUCT
%token VAL
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
// %left DOUBLE_PIPE             /* || for variant type */
%left OR                      /* Or */
%left AND                     /* And */
%right NOT                    /* Not */
/* == <> < <= > >= */
%left EQUAL_EQUAL NOT_EQUAL LESS LESS_EQUAL GREATER GREATER_EQUAL
%right DOUBLE_COLON           /* :: */
%left PLUS MINUS              /* + - */
%left ASTERISK SLASH PERCENT  /* * / % */
%right ASSERT ASSUME          /* Asserts, Assumes */
%right ARROW                  /* -> for type declaration */
%right prec_variant   /* variants, lists */
// %right DOUBLE_AMPERSAND      /* && for type intersection */

%start <Bluejay.statement list> prog
%start <Bluejay.statement list option> delim_expr

%%

(* [separated_at_least_two_list(separator, X)] recognizes a list with at least two
   [X]'s, separated by [separator]'s. It produces a value of type ['a list] if
   [X] produces a value of type ['a]. *)

%public separated_at_least_two_list(separator, X):
  x1 = X; separator; x2 = X
    { [ x1; x2 ] }
    [@name two]
| x1 = X; separator; x2 = X; separator; xs = separated_nonempty_list(separator, X)
    { x1 :: x2 :: xs }
    [@name more]

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
  | LET l_ident EQUALS expr
      { SUntyped { var = $2 ; body = $4 } : Bluejay.statement }
  | LET OPEN_PAREN l_ident COLON expr CLOSE_PAREN EQUALS expr
      { STyped { typed_var = { var = $3 ; tau = $5 } ; body = $8 ; do_wrap = true ; do_check = true } : Bluejay.statement }
  | letfun_rec
      { SFunRec $1 : Bluejay.statement }
  | letfun
      { SFun $1 : Bluejay.statement }

/* **** Expressions **** */


expr:
  | appl_expr /* Includes primary expressions */
      { $1 : Bluejay.t }
  | op_expr
      { $1 : Bluejay.t }
  | type_expr
      { $1 }
  | IF expr THEN expr ELSE expr %prec prec_if
      { EIf { cond = $2 ; true_body = $4 ; false_body = $6 } : Bluejay.t }
  | FUNCTION l_ident ARROW expr %prec prec_fun 
      { EFunction { param = $2 ; body = $4 } : Bluejay.t }
  | FUNCTION l_ident param_list ARROW expr %prec prec_fun
      { EMultiArgFunction { params = $2 :: $3 ; body = $5 } : Bluejay.t }
  // Let
  | LET l_ident EQUALS expr IN expr %prec prec_let
      { ELet { var = $2 ; body = $4 ; cont = $6 } : Bluejay.t }
  | LET_BIND l_ident EQUALS expr IN expr %prec prec_let
      { ELetBind { var = $2 ; body = $4 ; cont = $6 } : Bluejay.t }
  | LET OPEN_PAREN l_ident COLON expr CLOSE_PAREN EQUALS expr IN expr %prec prec_let
      { ELetTyped { typed_var = { var = $3 ; tau = $5 } ; body = $8 ; cont = $10 ; do_wrap = true ; do_check = true } : Bluejay.t }
  // Functions
  | letfun_rec IN expr %prec prec_fun
      { ELetFunRec { funcs = $1 ; cont = $3 } : Bluejay.t }
  | letfun IN expr %prec prec_fun
      { ELetFun { func = $1 ; cont = $3 } : Bluejay.t }
  // Match
  | MATCH expr WITH PIPE? separated_nonempty_list(PIPE, match_expr) END
      { EMatch { subject = $2 ; patterns = $5 } : Bluejay.t }
;

type_expr:
  | MU l_ident DOT expr %prec prec_mu
      { ETypeMu { var = $2 ; body = $4 } : Bluejay.t}
  | expr ARROW expr
      { ETypeArrow { domain = $1 ; codomain = $3 } : Bluejay.t }
  | OPEN_PAREN l_ident COLON expr CLOSE_PAREN ARROW expr
      { ETypeArrowD { binding = $2 ; domain = $4 ; codomain = $7 } : Bluejay.t }
  | separated_nonempty_list(DOUBLE_PIPE, single_variant_type)
      { ETypeVariant $1 : Bluejay.t }
  (* we need at least two here because otherwise it is just an arrow with a single variant type *)
  | separated_at_least_two_list(DOUBLE_AMPERSAND, single_intersection_type)
      { ETypeIntersect $1 : Bluejay.t } 

(* Doesn't *really* need parens I think, but without them we would never get a meaningful intersection type *)
(* This gives us a shift/reduce conflict that *can* be resolved because it is a valid expression in itself *)
single_intersection_type:
  | OPEN_PAREN OPEN_PAREN single_variant_type CLOSE_PAREN ARROW expr CLOSE_PAREN
      { let (a, b) = $3 in (a, b, $6) }

single_variant_type:
  | variant_type_label expr %prec prec_variant { $1, $2 }

record_type_or_refinement:
  (* exactly one label *)
  | OPEN_BRACE record_label COLON expr CLOSE_BRACE
      { ETypeRecord (new_record $2 $4)}
  (* more than one label *)
  | OPEN_BRACE record_label COLON expr SEMICOLON record_type_body CLOSE_BRACE
      { ETypeRecord (add_record_entry $2 $4 $6) }
  (* refinement type with binding for tau, which looks like a record type at first, so that's why we expand the rules above *)
  | OPEN_BRACE l_ident COLON expr PIPE expr CLOSE_BRACE
      { ETypeRefinement { tau = $4 ; predicate = EFunction { param = $2 ; body = $6 } } : Bluejay.t }
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
  | LET fun_sig { $2 }

letfun_rec:
  | LET REC separated_nonempty_list(WITH, fun_sig) { $3 }

/* let foo x = ... */
/* let foo (x : int) ... : int = ... */
/* let foo (type a b) (x : int) ... : t = ... */
fun_sig:
  | ident param_list EQUALS expr
      { FUntyped { func_id = $1 ; params = $2 ; body = $4 } : Bluejay.funsig }
  | ident param_list_with_type COLON expr EQUALS expr
      { FTyped { type_vars = [] ; func_id = $1 ; params = $2 ; ret_type = $4 ; body = $6 } : Bluejay.funsig }
  | ident OPEN_PAREN TYPE param_list CLOSE_PAREN param_list_with_type COLON expr EQUALS expr 
      { FTyped { type_vars = $4 ; func_id = $1 ; params = $6 ; ret_type = $8 ; body = $10 } : Bluejay.funsig }

/* **** Primary expressions **** */

/* (fun x -> x) y */
appl_expr:
  | appl_expr primary_expr { EAppl { func = $1 ; arg = $2 } : Bluejay.t }
  /* Give `singlet` and `list` keywords the same precedence as function application */
  | SINGLET_KEYWORD primary_expr { ETypeSingle $2 : Bluejay.t }
  | LIST primary_expr { ETypeList $2 : Bluejay.t } 
  | primary_expr { $1 : Bluejay.t }
;


/* In a primary_expr, only primitives, vars, records, and lists do not need
   surrounding parentheses. */
primary_expr:
  | INT
      { EInt $1 : Bluejay.t }
  | BOOL
      { EBool $1 : Bluejay.t }
  | ident_usage
      { $1 : Bluejay.t }
  (* keywords *)
  | INPUT
      { EPick_i : Bluejay.t }
  | TYPE
      { EType : Bluejay.t }
  | INT_KEYWORD
      { ETypeInt : Bluejay.t }
  | BOOL_KEYWORD
      { ETypeBool : Bluejay.t }
  | UNIT_KEYWORD
      { ETypeRecord empty_record : Bluejay.t }
  | TOP_KEYWORD
      { ETypeTop : Bluejay.t }
  | BOTTOM_KEYWORD
      { ETypeBottom : Bluejay.t }
  (* braces/parens *)
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
  | OPEN_BRACE expr PIPE expr CLOSE_BRACE
      { ETypeRefinement { tau = $2 ; predicate = $4 } : Bluejay.t }
  | SIG nonempty_list(val_item) END
      { ETypeRecordD $2 : Bluejay.t }
  | STRUCT nonempty_list(let_item) END
      { ERecordD $2 : Bluejay.t }
  | record_type_or_refinement
      { $1 : Bluejay.t }
  | primary_expr DOT record_label
      { EProject { record = $1 ; label = $3} : Bluejay.t }
;

op_expr:
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

/* **** Idents + labels **** */

param_list_with_type:
  | param_with_type param_list_with_type { $1 :: $2 }
  | param_with_type { [$1] }
;

param_with_type:
  | OPEN_PAREN l_ident COLON expr CLOSE_PAREN
      { TVar { var = $2 ; tau = $4 } : Bluejay.param }
  | OPEN_PAREN l_ident BACK_ARROW expr CLOSE_PAREN
      { TVarDep { var = $2 ; tau = $4 } : Bluejay.param }
;

param_list:
  | l_ident param_list { $1 :: $2 }
  | l_ident { [ $1 ] }
;

/* val x : t (* for module types *) */
val_item:
  | VAL record_type_item { $2 }

/* let x_1 = e_1 (* for modules *) */
let_item:
  | LET record_label EQUALS expr { ($2, $4) }

%inline record_label:
  | ident { RecordLabel.RecordLabel $1 }
;

%inline ident_usage:
  | ident { EVar $1 : Bluejay.t }
;

%inline l_ident: (* like "lvalue". These are idents that can be assigned to *)
  | ident { $1 }
  | UNDERSCORE { Ident.Ident "_"}

%inline ident: (* these are idents that can be used as values *)
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
  | BACKTICK ident { VariantLabel.VariantLabel $2 }

/* e.g. ``Variant int */ 
variant_type_label:
  | BACKTICK BACKTICK ident { VariantTypeLabel.VariantTypeLabel $3 }

/* **** Pattern matching **** */

match_expr:
  | pattern ARROW expr
      { ($1, $3) : Bluejay.pattern * Bluejay.t }

pattern:
  | UNDERSCORE { PAny } (* for efficiency later, handle underscore separately *)
  | ident { PVariable $1 } (* not l_ident because we handle underscore above *)
  | variant_label l_ident { PVariant { variant_label = $1 ; payload_id = $2 } }
  | variant_label OPEN_PAREN l_ident CLOSE_PAREN { PVariant { variant_label = $1 ; payload_id = $3 } }
  | OPEN_BRACKET CLOSE_BRACKET { PEmptyList }
  | l_ident DOUBLE_COLON l_ident { PDestructList { hd_id = $1 ; tl_id = $3 } }
  | OPEN_PAREN pattern CLOSE_PAREN { $2 }
;
