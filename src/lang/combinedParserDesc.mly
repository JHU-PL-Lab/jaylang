(*
This file is a combined description of the various Bluejay-related languages.
During the build process, Dune runs the "uncombine.py" descript to split it
into multiple different files which are then processed as usual by menhir.  The
"uncombine.py" script includes instructions describing how it operates but,
generally, special comments which include the commands "scope" and "endscope"
are used to describe which lines of this file are preserved in each version of
the language and which lines are erased.
*)

%{
  (* Note: because AST uses GADTs and constraints, we must do some type
     annotation in this file *)
  open Ast
  open Binop
  open Pattern
  open Expr
  open Parsing_tools
  (*! scope bluejay !*)
  open Bluejay
  (*! endscope !*)
  (*! scope desugared !*)
  open Desugared
  (*! endscope !*)
  (*! scope embedded !*)
  open Embedded
  (*! endscope !*)
%}

(* everyone *)
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
%token EQUALS
%token ARROW
%token DOT
%token UNDERSCORE
%token PIPE
%token DOUBLE_PIPE
%token DOUBLE_AMPERSAND
%token FUNCTION
%token WITH
%token LET
%token LET_BIND
%token IN
%token IF
%token THEN
%token ELSE
%token NOT
%token MATCH
%token END
%token STRUCT
%token DEFER
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
%token PIPELINE
(*! scope bluejay desugared !*)
%token BOOL_KEYWORD
%token BOTTOM_KEYWORD
%token COLON
%token INPUT
%token INT_KEYWORD
%token LONG_ARROW
%token MU
%token OF
%token SIG
%token SINGLET_KEYWORD
%token TOP_KEYWORD
%token TYPE
%token UNIT_KEYWORD
%token VAL
(*! endscope !*)
(*! scope bluejay !*)
%token AMPERSAND
%token AND
%token ASSERT
%token ASSUME
%token CLOSE_BRACKET
%token DEPENDENT
%token DEP
%token DOUBLE_COLON
%token LIST
%token OPEN_BRACKET
%token REC
(*! endscope !*)
(*! scope desugared embedded !*)
%token ABORT
%token VANISH
(*! endscope !*)
(*! scope desugared !*)
%token GEN
%token NO_CHECK
%token NO_WRAP
(*! endscope !*)
(*! scope embedded !*)
%token COMMA
%token PICK_I
%token PICK_B
%token CASE
%token DEFAULT
%token FREEZE
%token THAW
%token ID
%token IGNORE
%token TABLE_CREATE
%token TABLE_APPL
%token DET
%token ESCAPEDET
%token INTENSIONAL_EQUAL
%token UNTOUCHABLE
(*! endscope !*)

/*
 * Precedences and associativities.  Lower precedences come first.
 */
%nonassoc prec_let prec_fun   /* Let-ins and functions */
%nonassoc prec_if             /* Conditionals */
(*! scope bluejay desugared !*)
%nonassoc prec_mu             /* mu types */
(*! endscope !*)
%left PIPELINE                /* |> */
%right DOUBLE_PIPE            /* || for boolean or */
%right DOUBLE_AMPERSAND       /* && for boolean and */
%right NOT                    /* Not */
/* == <> < <= > >= */
%left EQUAL_EQUAL NOT_EQUAL LESS LESS_EQUAL GREATER GREATER_EQUAL
(*! scope bluejay !*)
%right DOUBLE_COLON           /* :: */
(*! endscope !*)
%left PLUS MINUS              /* + - */
%left ASTERISK SLASH PERCENT  /* * / % */
(*! scope bluejay !*)
%left AMPERSAND
(*! endscope !*)
(*! scope bluejay desugared !*)
%right ARROW LONG_ARROW       /* -> for type declaration, and --> for deterministic */
(*! endscope !*)
%right prec_variant           /* variants, lists */

%start <statement list> prog
%start <statement list option> delim_expr

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
      { SUntyped { var = $2 ; defn = $4 } : statement }
  (* desugared lets may have #no_check #no_wrap but bluejay lets may not *)
  (*! scope desugared !*)
  | LET typed_binding NO_CHECK? NO_WRAP? EQUALS expr
      { STyped { typed_var = { var = fst $2 ; tau = snd $2 };
                 defn = $6;
                 typed_binding_opts =
                    TBDesugared { do_wrap = Option.is_none $3;
                                  do_check = Option.is_none $4;
                                }
               } : statement
      }
  (*! endscope !*)
  (*! scope bluejay !*)
  | LET typed_binding EQUALS expr
      { STyped { typed_var = { var = fst $2 ; tau = snd $2 };
                 defn = $4;
                 typed_binding_opts = TBBluejay
               } : statement
      }
  | letfun_rec
      { SFunRec $1 : statement }
  | letfun
      { SFun $1 : statement }
  (*! endscope !*)

/* **** Expressions **** */


expr:
  | appl_expr /* Includes primary expressions */
      { $1 : t }
  | op_expr
      { $1 : t }
  (*! scope bluejay desugared !*)
  | type_expr
      { $1 }
  (*! endscope !*)
  | IF expr THEN expr ELSE expr %prec prec_if
      { EIf { cond = $2 ; true_body = $4 ; false_body = $6 } : t }
  | FUNCTION l_ident ARROW expr %prec prec_fun 
      { EFunction { param = $2 ; body = $4 } : t }
  (*! scope bluejay !*)
  | FUNCTION l_ident param_list ARROW expr %prec prec_fun
      { EMultiArgFunction { params = $2 :: $3 ; body = $5 } : t }
  (*! endscope !*)
  // Let
  (* bluejay and desugared have different let syntax to allow for e.g. #no_check *)
  (*! scope bluejay !*)
  | LET typed_binding EQUALS expr IN expr %prec prec_let
      { ELetTyped { typed_var = { var = fst $2 ; tau = snd $2 };
                    defn = $4;
                    body = $6;
                    typed_binding_opts = TBBluejay
                  } : t
      }
  (*! endscope !*)
  (*! scope desugared !*)
  | LET typed_binding NO_CHECK? NO_WRAP? EQUALS expr IN expr %prec prec_let
      { ELetTyped { typed_var = { var = fst $2 ; tau = snd $2 };
                    defn = $6;
                    body = $8;
                    typed_binding_opts = TBDesugared
                        { do_wrap = Option.is_none $3;
                          do_check = Option.is_none $4;
                        }
                  } : t
      }
  (*! endscope !*)
  | LET l_ident EQUALS expr IN expr %prec prec_let
      { ELet { var = $2 ; defn = $4 ; body = $6 } : t }
  | LET_BIND l_ident EQUALS expr IN expr %prec prec_let (* this is desugared in place, which is a little ugly... *)
      { EAppl { func = EAppl { func = EVar (Ident "bind") ; arg = $4 } ; arg = EFunction { param = $2 ; body = $6 }} : t } 
  (*! scope embedded !*)
  | IGNORE expr IN expr %prec prec_let
      { EIgnore { ignored = $2; body = $4 } }
  (*! endscope !*)
  // Functions
  (*! scope bluejay !*)
  | letfun_rec IN expr %prec prec_fun
      { ELetFunRec { funcs = $1 ; body = $3 } : t }
  | letfun IN expr %prec prec_fun
      { ELetFun { func = $1 ; body = $3 } : t }
  (*! endscope !*)
  // Match
  | MATCH expr WITH PIPE? match_expr_list END
      { EMatch { subject = $2 ; patterns = $5 } : t }
  (*! scope embedded !*)
  | CASE expr WITH PIPE? case_expr_list DEFAULT expr END
      { ECase { subject = $2; cases = $5; default = $7 } }
  | TABLE_APPL OPEN_PAREN expr COMMA expr COMMA expr CLOSE_PAREN
      { ETableAppl { tbl = $3; gen = $5; arg = $7; } }
  | INTENSIONAL_EQUAL OPEN_PAREN expr COMMA expr CLOSE_PAREN
      { EIntensionalEqual { left = $3; right = $5 } }
  (*! endscope !*)
;

(*! scope bluejay desugared !*)
%inline typed_binding:
  | l_ident COLON expr
      { ($1, $3) }
  | OPEN_PAREN l_ident COLON expr CLOSE_PAREN
      { ($2, $4) }

%inline type_expr:
  | PIPE separated_nonempty_list(PIPE, single_variant_type) (* pipe optional before first variant *)
      { ETypeVariant $2 : t }
  | separated_nonempty_list(PIPE, single_variant_type)
      { ETypeVariant $1 : t }
  | type_expr_without_variant
      { $1 : t }

%inline type_expr_without_variant:
  | MU l_ident list(l_ident) DOT expr %prec prec_mu
      { ETypeMu { var = $2 ; params = $3 ; body = $5 } : t}
  | expr ARROW expr
      { ETypeFun { domain = $1 ; codomain = $3 ; dep = `No ; det = false } : t }
  | expr LONG_ARROW expr
      { ETypeFun { domain = $1 ; codomain = $3 ; dep = `No ; det = true } : t }
  | OPEN_PAREN l_ident COLON expr CLOSE_PAREN ARROW expr
      { ETypeFun { domain = $4 ; codomain = $7 ; dep = `Binding $2 ; det = false } : t }
  | OPEN_PAREN l_ident COLON expr CLOSE_PAREN LONG_ARROW expr
      { ETypeFun { domain = $4 ; codomain = $7 ; dep = `Binding $2 ; det = true } : t }
  (*! scope bluejay !*)
  | expr AMPERSAND expr
      { (* We need to restrict these expressions to the form
            `Foo τ -> τ'
           in order to build our AST.  This is a bit tricky, since Menhir
           doesn't have the lookahead necessary to do so.  We'll just let Menhir
           parse whatever expressions it likes here and then yell if we get
           something which doesn't match.  But to play nice, we need to make '&'
           a binary operator.  This means that the types to the left and right
           may either be a single type of this form or intersection types
           themselves.
      *)
        let extract (e : t) : (VariantTypeLabel.t * t * t) list =
          match e with
          | ETypeIntersect xs -> xs
          | ETypeFun { domain = ETypeVariant [(lbl,tIn)];
                       codomain = tOut;
                       dep = `No;
                       det = false;
                     } ->
            [(lbl, tIn, tOut)]
          | ETypeFun { det = true; _ } ->
            failwith "TODO: error message: deterministic variable"
          | ETypeFun { dep = `Binding _; _ } ->
            failwith "TODO: error message: dependent variable"
          | ETypeFun { domain = ETypeVariant (_::_::_); _ } ->
            failwith "TODO: error message: multiple arguments"
          | _ ->
            failwith "TODO: error message: not a function type"
        in
        ETypeIntersect (extract $1 @ extract $3) : t
      }
  (*! endscope !*)
(*! endscope !*)

(*! scope bluejay desugared !*)

single_variant_type:
  | variant_type_label OF expr %prec prec_variant { $1, $3 }

record_type_or_refinement:
  (* exactly one label *)
  | OPEN_BRACE record_label COLON expr CLOSE_BRACE
      { ETypeRecord (new_record $2 $4)}
  (* more than one label *)
  | OPEN_BRACE record_label COLON expr SEMICOLON record_type_body CLOSE_BRACE
      { ETypeRecord (add_record_entry $2 $4 $6) }
  (* refinement type with binding for tau, which looks like a record type at first, so that's why we expand the rules above *)
  | OPEN_BRACE l_ident COLON expr PIPE expr CLOSE_BRACE
      { ETypeRefinement { tau = $4 ; predicate = EFunction { param = $2 ; body = $6 } } : t }

record_type_item:
  | record_label COLON expr
      { $1, $3 : RecordLabel.t * t }

record_type_body:
  | record_label COLON expr
      { new_record $1 $3 }
  | record_label COLON expr SEMICOLON record_type_body
      { add_record_entry $1 $3 $5 }

(*! endscope !*)

// basic_types:

/* **** Functions **** */

(*! scope bluejay !*)

letfun:
  | LET fun_sig { $2 }

letfun_rec:
  | LET REC separated_nonempty_list(AND, fun_sig) { $3 }

/* let foo x = ... */
/* let foo (x : int) ... : int = ... */
/* let foo (type a b) (x : int) ... : t = ... */
fun_sig:
  | ident param_list EQUALS expr
      { FUntyped { func_id = $1 ; params = $2 ; defn = $4 } : funsig }
  | ident param_list_with_type COLON expr EQUALS expr
      { FTyped { type_vars = [] ; func_id = $1 ; params = $2 ; ret_type = $4 ; defn = $6 } : funsig }
  | ident OPEN_PAREN TYPE param_list CLOSE_PAREN param_list_with_type COLON expr EQUALS expr 
      { FTyped { type_vars = $4 ; func_id = $1 ; params = $6 ; ret_type = $8 ; defn = $10 } : funsig }

(*! endscope !*)

/* **** Primary expressions **** */

/* (fun x -> x) y */
appl_expr:
  | appl_expr primary_expr { EAppl { func = $1 ; arg = $2 } : t }
  | DEFER primary_expr { EDefer $2 : t }
  (*! scope bluejay !*)
  | ASSERT primary_expr
      { EAssert $2 : t }
  | ASSUME primary_expr
      { EAssume $2 : t }
  (*! endscope !*)
  (*! scope desugared embedded !*)
  | ABORT ident
      { let Ident s = $2 in EAbort s : t }
  | VANISH primary_expr
      { EVanish () : t }
  (*! endscope !*)
  (*! scope desugared !*)
  | GEN primary_expr
      { EGen $2 : t }
  (*! endscope !*)
  (*! scope embedded !*)
  | FREEZE primary_expr
      { EFreeze $2 : t }
  | THAW primary_expr
      { EThaw $2 : t }
  | DET primary_expr
      { EDet $2 : t }
  | ESCAPEDET primary_expr
      { EEscapeDet $2 : t }
  | UNTOUCHABLE primary_expr
      { EUntouchable $2 : t }
  (*! endscope !*)
  | primary_expr { $1 : t }
;


/* In a primary_expr, only primitives, vars, records, and lists do not need
   surrounding parentheses. */
primary_expr:
  | INT
      { EInt $1 : t }
  | BOOL
      { EBool $1 : t }
  | ident_usage
      { $1 : t }
  (* keywords *)
  (*! scope bluejay desugared !*)
  | INPUT
      { EInput : t }
  (*! endscope !*)
  (*! scope embedded !*)
  | PICK_I
      { EPick_i : t }
  | PICK_B
      { EPick_b : t }
  | ID
      { EId : t }
  | TABLE_CREATE
      { ETableCreate : t }
  (*! endscope !*)
  (*! scope bluejay desugared !*)
  | TYPE
      { EType : t }
  | INT_KEYWORD
      { ETypeInt : t }
  | BOOL_KEYWORD
      { ETypeBool : t }
  | UNIT_KEYWORD
      { ETypeUnit : t }
  | TOP_KEYWORD
      { ETypeTop : t }
  | BOTTOM_KEYWORD
      { ETypeBottom : t }
  (*! endscope !*)
  (*! scope bluejay !*)
  | LIST
      { ETypeList : t }
  (*! endscope !*)
  (*! scope bluejay desugared !*)
  | SINGLET_KEYWORD
      { ETypeSingle : t }
  (*! endscope !*)
  (* braces/parens *)
  | OPEN_PAREN CLOSE_PAREN
      { EUnit : t }
  (*! scope bluejay desugared !*)
  | OPEN_BRACE COLON CLOSE_BRACE
      { ETypeRecord empty_record : t }
  (*! endscope !*)
  | OPEN_BRACE record_body CLOSE_BRACE
      { ERecord $2 : t }
  | OPEN_BRACE CLOSE_BRACE
      { ERecord empty_record : t }
  (*! scope bluejay !*)
  | OPEN_BRACKET separated_nonempty_list(SEMICOLON, expr) CLOSE_BRACKET
      { EList $2 : t }
  | OPEN_BRACKET CLOSE_BRACKET
      { EList [] : t }
  (*! endscope !*)
  | OPEN_PAREN expr CLOSE_PAREN
      { $2 }
  (*! scope bluejay desugared !*)
  | OPEN_BRACE expr PIPE expr CLOSE_BRACE
      { ETypeRefinement { tau = $2 ; predicate = $4 } : t }
  (*! endscope !*)
  | STRUCT statement_list END
      { EModule $2 : t }
  (*! scope bluejay desugared !*)
  | SIG nonempty_list(val_item) END
      { ETypeModule $2 : t }
  | record_type_or_refinement
      { $1 : t }
  (*! endscope !*)
  | primary_expr DOT record_label
      { EProject { record = $1 ; label = $3} : t }
;

op_expr:
  | variant_label expr %prec prec_variant
      { EVariant { label = $1 ; payload = $2 } : t }
  | expr ASTERISK expr
      { EBinop { left = $1 ; binop = BTimes ; right = $3 } : t }
  | expr SLASH expr
      { EBinop { left = $1 ; binop = BDivide ; right = $3 } : t }
  | expr PERCENT expr
      { EBinop { left = $1 ; binop = BModulus ; right = $3 } : t }
  | expr PLUS expr
      { EBinop { left = $1 ; binop = BPlus ; right = $3 } : t }
  | expr MINUS expr
      { EBinop { left = $1 ; binop = BMinus ; right = $3 } : t }
  (*! scope bluejay !*)
  | expr DOUBLE_COLON expr
      { EListCons ($1, $3) : t }
  (*! endscope !*)
  | expr EQUAL_EQUAL expr
      { EBinop { left = $1 ; binop = BEqual ; right = $3 } : t }
  | expr NOT_EQUAL expr
      { EBinop { left = $1 ; binop = BNeq ; right = $3 } : t }
  | expr GREATER expr
      { EBinop { left = $1 ; binop = BGreaterThan ; right = $3 } : t }
  | expr GREATER_EQUAL expr
      { EBinop { left = $1 ; binop = BGeq ; right = $3 } : t }
  | expr LESS expr
      { EBinop { left = $1 ; binop = BLessThan ; right = $3 } : t }
  | expr LESS_EQUAL expr
      { EBinop { left = $1 ; binop = BLeq ; right = $3 } : t }
  | NOT expr
      { ENot $2 : t }
  | expr DOUBLE_AMPERSAND expr
      { EBinop { left = $1 ; binop = BAnd ; right = $3 } : t }
  | expr DOUBLE_PIPE expr
      { EBinop { left = $1 ; binop = BOr ; right = $3 } : t }
  | expr PIPELINE expr (* Note: evaluation order is that e' is evaluated first in e |> e' *)
      { EAppl { func = $3 ; arg = $1 } }

/* **** Idents + labels **** */

(*! scope bluejay !*)

param_list_with_type:
  | param_with_type param_list_with_type { $1 :: $2 }
  | param_with_type { [$1] }
;

param_with_type:
  | OPEN_PAREN l_ident COLON expr CLOSE_PAREN
      { TVar { var = $2 ; tau = $4 } : param }
  | OPEN_PAREN l_ident COLON expr PIPE expr CLOSE_PAREN
      { TVar { var = $2 ; tau = ETypeRefinement { tau = $4 ; predicate = EFunction { param = $2 ; body = $6 } } } : param }
  | OPEN_PAREN DEP l_ident COLON expr CLOSE_PAREN
  | OPEN_PAREN DEPENDENT l_ident COLON expr CLOSE_PAREN
      { TVarDep { var = $3 ; tau = $5 } : param }
  | OPEN_PAREN DEP l_ident COLON expr PIPE expr CLOSE_PAREN
  | OPEN_PAREN DEPENDENT l_ident COLON expr PIPE expr CLOSE_PAREN
      { TVarDep { var = $3 ; tau = ETypeRefinement { tau = $5 ; predicate = EFunction { param = $3 ; body = $7 } } } : param }
;

param_list:
  | l_ident param_list { $1 :: $2 }
  | l_ident { [ $1 ] }
;

(*! endscope !*)

(*! scope bluejay desugared !*)

/* val x : t (* for module types *) */
/* val t = tau (* pure simple sugar for val t : singlet tau *) */
val_item:
  | VAL record_type_item { $2 }
  | VAL record_label EQUALS expr { $2, EAppl { func = ETypeSingle ; arg = $4 } : RecordLabel.t * t }

(*! endscope !*)

%inline record_label:
  | ident { RecordLabel.RecordLabel $1 }
;

%inline ident_usage:
  | ident { EVar $1 : t }
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

(*! scope bluejay desugared !*)
/* e.g. ``Variant int */ 
variant_type_label:
  | BACKTICK ident { VariantTypeLabel.VariantTypeLabel $2 }
(*! endscope !*)

/* **** Pattern matching **** */

match_expr_list:
  | terminating_pattern ARROW expr
      { [ $1, $3 ] : (pattern * t) list }
  | pattern ARROW expr PIPE match_expr_list (* does not include terminating patterns *)
      { ($1, $3) :: $5 : (pattern * t) list }
  | pattern ARROW expr
      { [ $1, $3 ] : (pattern * t) list }

(*! scope embedded !*)

case_expr:
  | INT ARROW expr
      { ($1, $3) : (int * t) }

case_expr_list:
  | case_expr PIPE
      { [$1] : (int * t) list }
  | case_expr PIPE case_expr_list
      { $1::$3 : (int * t) list }

(*! endscope !*)

pattern:
  | variant_label l_ident { PVariant { variant_label = $1 ; payload_id = $2 } }
  | variant_label OPEN_PAREN l_ident CLOSE_PAREN { PVariant { variant_label = $1 ; payload_id = $3 } }
  (*! scope bluejay !*)
  | OPEN_BRACKET CLOSE_BRACKET { PEmptyList }
  | l_ident DOUBLE_COLON l_ident { PDestructList { hd_id = $1 ; tl_id = $3 } }
  (*! endscope !*)
  | OPEN_PAREN pattern CLOSE_PAREN { $2 }
;

(* no patterns may follow these in a list of match expressions *)
terminating_pattern:
  | UNDERSCORE { PAny }
  | ident { PVariable $1 } (* not l_ident because we handle underscore above *)
  | OPEN_PAREN terminating_pattern CLOSE_PAREN { $2 }
