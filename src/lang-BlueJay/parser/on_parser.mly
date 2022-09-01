%{
open On_ast;;
module List = BatList;;

(* Functions relating to parsing record entries *)

let sep = Odefa_ast.Ast_tools.label_sep;;
let dup_label_count = ref 0;;

let new_record lbl value =
  let (Label k) = lbl in
  let key = Ident k in
  Ident_map.singleton key value
;;

let add_record_entry lbl value old_record =
  let (Label k) = lbl in
  let key =
    if Ident_map.mem (Ident k) old_record then
      let key' = Ident (k ^ sep ^ (string_of_int !dup_label_count)) in
      dup_label_count := !dup_label_count + 1;
      key'
    else
      Ident k
  in
  Ident_map.add key value old_record
;;

let record_from_list pr_list = 
  pr_list
  |> List.fold_left 
     (fun acc (lbl, v) -> add_record_entry lbl v acc)
     Ident_map.empty

let new_rec_fun_with_type 
  (fun_sig_and_type : (syntactic_only funsig * syntactic_only expr) list) 
  (let_body : syntactic_only expr_desc) = 
  let fun_sig_list = List.map fst fun_sig_and_type in 
  let fun_type_list = 
    fun_sig_and_type 
    |> List.map (fun p -> new_expr_desc (snd p)) 
  in
  LetRecFunWithType (fun_sig_list, let_body, fun_type_list)

let new_let_fun_with_type 
  (fun_sig_and_type : (syntactic_only funsig * syntactic_only expr)) 
  (let_body : syntactic_only expr_desc) =
  let fun_sig, fun_type = fun_sig_and_type in
  LetFunWithType (fun_sig, let_body, (new_expr_desc fun_type))

let new_fun_with_type 
  (fun_name : ident) 
  (typed_param_list : (ident * syntactic_only expr_desc) list) 
  (return_type : syntactic_only expr_desc)
  (fun_body : syntactic_only expr_desc) = 
  let param_list = List.map fst typed_param_list in
  let (type_list : syntactic_only expr_desc list) = List.map snd typed_param_list in
  let function_type_p = 
    match type_list with
    (* Please throw the correct exception here! *)
    | [] -> failwith "undefined"
    | _ -> 
      let reversed_list = List.rev type_list in
      let last_type = List.hd reversed_list in
      let accumulator = TypeArrow (last_type, return_type) in
      List.fold_left
          (fun acc -> fun t -> TypeArrow (t, (new_expr_desc acc))) accumulator 
          (List.tl reversed_list)
  in
  (Funsig (fun_name, param_list, fun_body), function_type_p)

let new_dependent_fun   
  (fun_name : ident) 
  (typed_param : (ident * syntactic_only expr_desc)) 
  (return_type : syntactic_only expr_desc)
  (fun_body : syntactic_only expr_desc) = 
  let (param, _) = typed_param in
  (Funsig (fun_name, [param], fun_body), TypeArrowD (typed_param, return_type))
%}

%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token <bool> BOOL
%token EOF
%token OPEN_BRACE
%token CLOSE_BRACE
%token COMMA
%token BACKTICK
%token APOSTROPHE
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
%token DOLLAR
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

%start <On_ast.syntactic_only On_ast.expr> prog
%start <(On_ast.syntactic_only On_ast.expr) option> delim_expr

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
      { $1 }
  | ASSERT expr
      { Assert(new_expr_desc $2) }
  | ASSUME expr
      { Assume(new_expr_desc $2) }
  | variant_label expr %prec prec_variant
      { VariantExpr($1, new_expr_desc $2) }
  | expr ASTERISK expr
      { Times(new_expr_desc $1, new_expr_desc $3) }
  | expr SLASH expr
      { Divide(new_expr_desc $1, new_expr_desc $3) }
  | expr PERCENT expr
      { Modulus(new_expr_desc $1, new_expr_desc $3) }
  | expr PLUS expr
      { Plus(new_expr_desc $1, new_expr_desc $3) }
  | expr MINUS expr
      { Minus(new_expr_desc $1, new_expr_desc $3) }
  | expr DOUBLE_COLON expr
      { ListCons(new_expr_desc $1, new_expr_desc $3) }
  | expr EQUAL_EQUAL expr
      { Equal(new_expr_desc $1, new_expr_desc $3) }
  | expr NOT_EQUAL expr
      { Neq(new_expr_desc $1, new_expr_desc $3) }
  | expr GREATER expr
      { GreaterThan(new_expr_desc $1, new_expr_desc $3) }
  | expr GREATER_EQUAL expr
      { Geq(new_expr_desc $1, new_expr_desc $3) }
  | expr LESS expr
      { LessThan(new_expr_desc $1, new_expr_desc $3) }
  | expr LESS_EQUAL expr
      { Leq(new_expr_desc $1, new_expr_desc $3) }
  | NOT expr
      { Not(new_expr_desc $2) }
  | expr AND expr
      { And(new_expr_desc $1, new_expr_desc $3) }
  | expr OR expr
      { Or(new_expr_desc $1, new_expr_desc $3) }
  | IF expr THEN expr ELSE expr %prec prec_if
      { If(new_expr_desc $2, new_expr_desc $4, new_expr_desc $6) }
  | FUNCTION param_list ARROW expr %prec prec_fun
      { Function($2, new_expr_desc $4) }
  | LET REC fun_sig_list IN expr %prec prec_fun
      { LetRecFun($3, new_expr_desc $5) }
  | LET REC fun_sig_with_type_list IN expr %prec prec_let 
      { new_rec_fun_with_type $3 (new_expr_desc $5) }
  | LET_D REC fun_sig_dependent_list IN expr %prec prec_let 
      { new_rec_fun_with_type $3 (new_expr_desc $5) }
  | LET ident_decl EQUALS expr IN expr %prec prec_let
      { Let($2, new_expr_desc $4, new_expr_desc $6) }
  | LET OPEN_PAREN ident_decl COLON expr CLOSE_PAREN EQUALS expr IN expr %prec prec_let
      { LetWithType($3, new_expr_desc $8, new_expr_desc $10, new_expr_desc $5) }
  | LET fun_sig IN expr %prec prec_fun
      { LetFun($2, new_expr_desc $4) }
  | LET fun_sig_with_type IN expr %prec prec_fun
      { new_let_fun_with_type $2 (new_expr_desc $4) }
  | LET_D fun_sig_dependent IN expr %prec prec_fun
      { new_let_fun_with_type $2 (new_expr_desc $4) }
  | MATCH expr WITH PIPE? match_expr_list END
      { Match(new_expr_desc $2, $5) }
  // Types expressions
  | basic_types { $1 }
  | type_parameter { $1 }
  | MU ident_decl DOT expr { TypeRecurse ($2, new_expr_desc $4) }
  | expr ARROW expr { TypeArrow (new_expr_desc $1, new_expr_desc $3) }
  | OPEN_PAREN ident_decl COLON expr CLOSE_PAREN ARROW expr { TypeArrowD (($2, new_expr_desc $4), new_expr_desc $7) }
  // TODO: Change this to fancy curly
  | OPEN_BRACE DOT basic_types PIPE expr CLOSE_BRACE { TypeSet (new_expr_desc $3, new_expr_desc $5) } 
  | expr DOUBLE_PIPE expr { TypeUnion (new_expr_desc $1, new_expr_desc $3) }
  | expr DOUBLE_AMPERSAND expr { TypeIntersect (new_expr_desc $1, new_expr_desc $3) }
;

type_parameter:
  | APOSTROPHE IDENTIFIER { TypeUntouched $2 }

type_var:
  | DOLLAR IDENTIFIER { TypeVar $2 }

record_type:
  | OPEN_BRACE_TYPE record_type_body CLOSE_BRACE_TYPE
      { TypeRecord $2 }
  | OPEN_BRACE_TYPE CLOSE_BRACE CLOSE_BRACE_TYPE
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

fun_sig_dependent_list:
  | fun_sig_dependent { [$1] }
  | fun_sig_dependent WITH fun_sig_dependent_list { $1 :: $3 }

fun_sig_dependent:
  | ident_decl param_with_type COLON expr EQUALS expr
      { new_dependent_fun $1 $2 (new_expr_desc $4) (new_expr_desc $6) }

/* let rec foo x y = ... with bar a b = ... in ... */
fun_sig_list:
  | fun_sig { [$1] }
  | fun_sig WITH fun_sig_list { $1 :: $3 }

/* let rec foo (x : int) (y : bool) ... : (bool -> bool) = ... with bar (a : int) (b : int) : ... = ... in ... */
fun_sig_with_type_list:
  | fun_sig_with_type { [$1] }
  | fun_sig_with_type WITH fun_sig_with_type_list { $1 :: $3 }

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
      { Record (Ident_map.empty) }
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
  | ident_decl { [$1] }
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

/* [1, 2, true] (Unlike ocaml, natodefa lists can be heterogenous) */
list_body:
  | expr COMMA list_body { (new_expr_desc $1) :: $3 }
  | expr { [new_expr_desc $1] }
;

/* `Variant 2 */
variant_label:
  | BACKTICK IDENTIFIER { Variant_label $2 }

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
