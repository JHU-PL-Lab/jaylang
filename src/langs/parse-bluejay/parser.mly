
%{
  open Ast
%}

%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token <bool> BOOL
%token EOF
%token OPEN_BRACE
%token CLOSE_BRACE
%token COMMA
%token BACKTICK
// %token APOSTROPHE
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
// %token DOLLAR
// %token OPEN_OBRACKET
// %token CLOSE_OBRACKET
%token FUNCTION
// %token RECORD
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
%token INT
%token BOOL_KEYWORD
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

// %token TYPEVAR
%token OPEN_BRACE_TYPE
%token CLOSE_BRACE_TYPE
// %token OPEN_PAREN_TYPE
// %token CLOSE_PAREN_TYPE

/*
 * Precedences and associativities.  Lower precedences come first.
 */
%nonassoc prec_let prec_fun   /* Let-ins and functions */
%nonassoc prec_if             /* Conditionals */
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

%start <Bluejay.t> prog
%start <Bluejay.t option> delim_expr

%%

prog:
  | expr EOF
      { $1 }
  ;

delim_expr:
  | EOF
      { None }
  | expr EOF
      { Some ($1) }
  ;

/* **** Expressions **** */

expr:
  | appl_expr /* Includes primary expressions */
      { $1 : bjy }
  | ASSERT expr
      { EAssert $2 }
  | ASSUME expr
      { EAssume $2 }
  | variant_label expr %prec prec_variant
      { EVariant { label = $1 ; payload = $2 } }
  | expr ASTERISK expr
      { EBinop { left = $1 ; binop = BTimes ; right = $3 } }
  | expr SLASH expr
      { EBinop { left = $1 ; binop = BDivide ; right = $3 } }
  | expr PERCENT expr
      { EBinop { left = $1 ; binop = BModulus ; right = $3 } }
  | expr PLUS expr
      { EBinop { left = $1 ; binop = BPlus ; right = $3 } }
  | expr MINUS expr
      { EBinop { left = $1 ; binop = BMinus ; right = $3 } }
  | expr DOUBLE_COLON expr
      { EListCons ($1, $3) }
  | expr EQUAL_EQUAL expr
      { EBinop { left = $1 ; binop = BEqual ; right = $3 } }
  | expr NOT_EQUAL expr
      { EBinop { left = $1 ; binop = BNeq ; right = $3 } }
  | expr GREATER expr
      { EBinop { left = $1 ; binop = BGreaterThan ; right = $3 } }
  | expr GREATER_EQUAL expr
      { EBinop { left = $1 ; binop = BGeq ; right = $3 } }
  | expr LESS expr
      { EBinop { left = $1 ; binop = BLessThan ; right = $3 } }
  | expr LESS_EQUAL expr
      { EBinop { left = $1 ; binop = BLeq ; right = $3 } }
  | NOT expr
      { ENot $2 }
  | expr AND expr
      { EBinop { left = $1 ; binop = BAnd ; right = $3 } }
  | expr OR expr
      { EBinop { left = $1 ; binop = BOr ; right = $3 } }
  | IF expr THEN expr ELSE expr %prec prec_if
      { EIf { cond = $2 ; true_body = $4 ; false_body = $6 } }
  | FUNCTION param_list ARROW expr %prec prec_fun
      { EMultiArgFunction { params = $2 ; body = $4 } }
  | LET REC fun_sig_list IN expr %prec prec_fun
      { ELetFunRec { funcs = $3 ; cont = $5 } } // TODO 
      // { LetRecFun($3, new_expr_desc $5) }
  | LET REC fun_sig_with_type_list IN expr %prec prec_let 
      { LetRecFunWithType ($3, new_expr_desc $5) }
  | LET REC fun_sig_poly_with_type_list IN expr %prec prec_let
      { LetRecFunWithType ($3, new_expr_desc $5) }
  | LET_D REC fun_sig_dependent_list IN expr %prec prec_let 
      { LetRecFunWithType ($3, new_expr_desc $5) }
  | LET_D REC fun_sig_poly_dep_list IN expr %prec prec_let 
      { LetRecFunWithType ($3, new_expr_desc $5) }
  | LET ident_decl EQUALS expr IN expr %prec prec_let
      { Let($2, new_expr_desc $4, new_expr_desc $6) }
  | LET OPEN_PAREN ident_decl COLON expr CLOSE_PAREN EQUALS expr IN expr %prec prec_let
      { LetWithType($3, new_expr_desc $8, new_expr_desc $10, new_expr_desc $5) }
  | LET fun_sig IN expr %prec prec_fun
      { LetFun($2, new_expr_desc $4) }
  | LET fun_sig_with_type IN expr %prec prec_fun
      { LetFunWithType ($2, new_expr_desc $4) }
  | LET fun_sig_poly_with_type IN expr %prec prec_fun 
      { LetFunWithType ($2, new_expr_desc $4) }
  | LET_D fun_sig_dependent IN expr %prec prec_fun
      { LetFunWithType ($2, new_expr_desc $4) }
  | LET_D fun_sig_poly_dep IN expr %prec prec_fun
      { LetFunWithType ($2, new_expr_desc $4) }
  | MATCH expr WITH PIPE? match_expr_list END
      { Match(new_expr_desc $2, $5) }
  // Types expressions
  | basic_types { $1 }
  // | type_parameter { $1 }
  | MU ident_decl DOT expr 
    { TypeRecurse ($2, build_recursive_type $2 (new_expr_desc $4)) }
  | expr ARROW expr { TypeArrow (new_expr_desc $1, new_expr_desc $3) }
  | OPEN_PAREN ident_decl COLON expr CLOSE_PAREN ARROW expr { TypeArrowD (($2, new_expr_desc $4), new_expr_desc $7) }
  // TODO: Change this to fancy curly
  | OPEN_BRACE DOT expr PIPE expr CLOSE_BRACE { TypeSet (new_expr_desc $3, new_expr_desc $5) } 
  | expr DOUBLE_AMPERSAND expr { TypeIntersect (new_expr_desc $1, new_expr_desc $3) }
  | variant_type_body { TypeVariant $1 }
;

// type_parameter:
//   | APOSTROPHE IDENTIFIER { TypeUntouched $2 }

variant_type_body:
  | variant_type_label expr { [($1, new_expr_desc $2)] }
  | variant_type_label expr DOUBLE_PIPE variant_type_body { ($1, new_expr_desc $2) :: $4 }

record_type:
  | OPEN_BRACE_TYPE record_type_body CLOSE_BRACE_TYPE
      { TypeRecord $2 }
  | OPEN_BRACE_TYPE CLOSE_BRACE_TYPE
      { TypeRecord (Ident_map.empty) }

record_type_body:
  | label COLON expr
      { new_record $1 (new_expr_desc $3) }
  | label COLON expr COMMA record_type_body
      { add_record_entry $1 (new_expr_desc $3) $5 }
;

basic_types:
  | INT { TypeInt }
  | BOOL_KEYWORD { TypeBool }
  | record_type { $1 }
  | LIST expr { TypeList (new_expr_desc $2) }

/* let foo x = ... */
fun_sig:
  | ident_decl param_list EQUALS expr
      { Funsig ($1, $2, new_expr_desc $4) }

/* let foo (x : int) ... : int = ... */
fun_sig_with_type:
  | ident_decl param_list_with_type COLON expr EQUALS expr
      { new_fun_with_type $1 $2 (new_expr_desc $4) (new_expr_desc $6) }

/* letd foo (x : int) ... : t = ... */
fun_sig_dependent:
  | ident_decl param_with_type COLON expr EQUALS expr
      { new_dependent_fun $1 $2 (new_expr_desc $4) (new_expr_desc $6) }

/* let foo (type a b) (x : int) ... : t = ... */
fun_sig_poly_with_type:
  | ident_decl OPEN_PAREN TYPE param_list CLOSE_PAREN param_list_with_type COLON expr EQUALS expr 
      { new_poly_fun_with_type $1 $4 $6 (new_expr_desc $8) (new_expr_desc $10) }

/* letd foo (type a b) (x : int) ... : t = ... */
fun_sig_poly_dep:
   ident_decl OPEN_PAREN TYPE param_list CLOSE_PAREN param_with_type COLON expr EQUALS expr
      { new_poly_dependent_fun $1 $4 $6 (new_expr_desc $8) (new_expr_desc $10) }

/* let rec foo x y = ... with bar a b = ... in ... */
fun_sig_list:
  | fun_sig { [$1] }
  | fun_sig WITH fun_sig_list { $1 :: $3 }

/* let rec foo (x : int) (y : bool) ... : (bool -> bool) = ... with bar (a : int) (b : int) : ... = ... in ... */
fun_sig_with_type_list:
  | fun_sig_with_type { [$1] }
  | fun_sig_with_type WITH fun_sig_with_type_list { $1 :: $3 }

/* let rec foo (type a b) (x : int) (y : bool) ... : (bool -> bool) = ... with bar (a : int) (b : int) : ... = ... in ... */
fun_sig_poly_with_type_list:
  | fun_sig_poly_with_type { [$1] }
  | fun_sig_poly_with_type WITH fun_sig_poly_with_type_list { $1 :: $3 }

/* letd rec foo (x : int) ... : (bool -> bool) = ... with bar (a : int) : ... = ... in ... */
fun_sig_dependent_list:
  | fun_sig_dependent { [$1] }
  | fun_sig_dependent WITH fun_sig_dependent_list { $1 :: $3 }

/* letd rec foo (type a b) (x : int) ... : t = ... with bar (a : int) : ... = ... in ... */
fun_sig_poly_dep_list:
  | fun_sig_poly_dep { [$1] }
  | fun_sig_poly_dep WITH fun_sig_poly_dep_list { $1 :: $3 } 

/* (fun x -> x) y */
appl_expr:
  | appl_expr primary_expr { Appl((new_expr_desc $1), (new_expr_desc $2)) }
  | primary_expr { $1 }
;

/* In a primary_expr, only primitives, vars, records, and lists do not need
   surrounding parentheses. */
primary_expr:
  | INT_LITERAL
      { Int $1 }
  | BOOL
      { Bool $1 }
  | INPUT
      { Input }
  | ident_usage
      { $1 }
  | OPEN_BRACE record_body CLOSE_BRACE
      { Record $2 }
  | OPEN_BRACE CLOSE_BRACE
      { Record Ident_map.empty }
  | OPEN_BRACKET list_body CLOSE_BRACKET
      { List $2 }
  | OPEN_BRACKET CLOSE_BRACKET
      { List [] }
  | OPEN_PAREN expr CLOSE_PAREN
      { $2 }
  | primary_expr DOT label
      { RecordProj((new_expr_desc $1), $3) }
;

/* **** Idents + labels **** */

param_list_with_type:
  | param_with_type param_list_with_type { $1 :: $2 }
  | param_with_type { [$1] }
;

param_with_type:
  | OPEN_PAREN ident_decl COLON expr CLOSE_PAREN { ($2, (new_expr_desc $4)) }
;

param_list:
  | ident_decl param_list { $1 :: $2 }
  | ident_decl { [ $1 ] }
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

/* **** Records, lists, and variants **** */

/* {x = 1, y = 2, z = 3} */
record_body:
  | label EQUALS expr
      { new_record $1 (new_expr_desc $3) }
  | label EQUALS expr COMMA record_body
      { add_record_entry $1 (new_expr_desc $3) $5 }
;

/* [1, 2, true] (Unlike ocaml, bluejay lists can be heterogenous) */
list_body:
  | expr COMMA list_body { (new_expr_desc $1) :: $3 }
  | expr { [new_expr_desc $1] }
;

/* `Variant 2 */
variant_label:
  | BACKTICK IDENTIFIER { VariantLabel.VariantLabel (Ident.Ident $2) }

variant_type_label:
  | BACKTICK BACKTICK IDENTIFIER { VariantLabel.VariantLabel (Ident.Ident $3) }

/* **** Pattern matching **** */

match_expr_list:
  | match_expr PIPE match_expr_list
      { $1 :: $3 }
  | match_expr
      { [$1] }
;

match_expr:
  | pattern ARROW expr
      { ($1, (new_expr_desc $3)) }

pattern:
  | UNDERSCORE { AnyPat }
  | INT { IntPat }
  | BOOL_KEYWORD { BoolPat }
  | FUNCTION { FunPat }
  | IDENTIFIER { VarPat(Ident($1)) }
  | variant_label ident_decl { VariantPat($1, $2) }
  | variant_label OPEN_PAREN ident_decl CLOSE_PAREN { VariantPat($1, $3) }
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, record_pattern_element) CLOSE_BRACE { StrictRecPat (record_from_list $2) }
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, record_pattern_element) UNDERSCORE CLOSE_BRACE { RecPat (record_from_list $2) }
  | OPEN_BRACE CLOSE_BRACE { StrictRecPat (Ident_map.empty) }
  | OPEN_BRACE UNDERSCORE CLOSE_BRACE { RecPat (Ident_map.empty) }
  | OPEN_BRACKET CLOSE_BRACKET { EmptyLstPat }
  | ident_decl DOUBLE_COLON ident_decl { LstDestructPat($1, $3) }
  | OPEN_PAREN pattern CLOSE_PAREN { $2 }
;

record_pattern_element:
  | label EQUALS ident_decl
      { ($1, Some $3) }
;

separated_nonempty_trailing_list(separator, rule):
  | nonempty_list(terminated(rule, separator))
      { $1 }
  | separated_nonempty_list(separator,rule)
      { $1 }
  ;
