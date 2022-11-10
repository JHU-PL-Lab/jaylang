%{
open Bluejay_ast;;
module List = BatList;;

(* Functions relating to parsing record entries *)

let sep = Jayil.Ast_tools.label_sep;;
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

let new_fun_with_type 
  (fun_name : ident) 
  (typed_param_list : (ident * expr_desc) list) 
  (return_type : expr_desc)
  (fun_body : expr_desc) = 
  Typed_funsig (fun_name, typed_param_list, (fun_body, return_type))

let new_dependent_fun   
  (fun_name : ident) 
  (typed_param : ident * expr_desc)
  (return_type : expr_desc)
  (fun_body : expr_desc) = 
  DTyped_funsig (fun_name, typed_param, (fun_body, return_type))

let rec build_recursive_type (t_var : ident) (ed : expr_desc) = 
  let e = ed.body in
  let tag = ed.tag in
  let body' = 
    match e with
    | Int _ | Bool _ | TypeError _ | TypeUntouched _ | Input -> e
    | Var x -> if t_var = x then TypeVar t_var else e 
    | Function (ids, f_edesc) ->
      if List.mem t_var ids
      then e
      else 
        let f_edesc' = build_recursive_type t_var f_edesc in
        Function (ids, f_edesc')
    | Appl (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Appl (e_desc1', e_desc2')
    | Let (x, e_desc1, e_desc2) ->
      if x = t_var 
      then e
      else
        let e_desc1' = build_recursive_type t_var e_desc1 in
        let e_desc2' = build_recursive_type t_var e_desc2 in
        Let (x, e_desc1', e_desc2')
    | LetRecFun (funsigs, e_desc) ->
      let locally_bound = 
        List.fold_left 
          (fun acc (Funsig (f, _, _)) -> if f = t_var then true else acc)
          false funsigs
      in
      if locally_bound 
      then
        e
      else
        let funsigs' = 
          List.map 
            (fun (Funsig (f, params, f_body)) -> Funsig (f, params, build_recursive_type t_var f_body))
          funsigs
        in
        let e_desc' = build_recursive_type t_var e_desc in
        LetRecFun (funsigs', e_desc')
    | LetFun (Funsig (f, params, f_body), e_desc) ->
      if f = t_var 
      then
        e
      else
        let f_body' = build_recursive_type t_var f_body in
        let e_desc' = build_recursive_type t_var e_desc in
        LetFun (Funsig(f, params, f_body'), e_desc')
    | LetWithType (x, e_desc1, e_desc2, t) -> 
      if (x = t_var) 
      then e
      else
        let e_desc1' = build_recursive_type t_var e_desc1 in
        let e_desc2' = build_recursive_type t_var e_desc2 in
        let t' = build_recursive_type t_var t in
        LetWithType (x, e_desc1', e_desc2', t')
    | LetRecFunWithType (funsigs, e_desc) -> 
      let locally_bound = 
        List.fold_left 
          (fun acc fun_sig -> 
            match fun_sig with
            | Typed_funsig (f, _, _) | DTyped_funsig (f, _, _) ->
              if f = t_var then true else acc)
          false funsigs
      in
      if locally_bound 
      then
        e
      else
        let funsigs' = 
          List.map 
            (fun fun_sig -> 
              match fun_sig with
              | Typed_funsig (f, typed_params, (f_body, ret_type)) ->
                let typed_params' = 
                  List.map 
                    (fun (arg, t) -> (arg, build_recursive_type t_var t))
                    typed_params
                in
                let f_body' = build_recursive_type t_var f_body in
                let ret_type' = build_recursive_type t_var ret_type in
                Typed_funsig (f, typed_params', (f_body', ret_type'))
              | DTyped_funsig (f, (arg, arg_type), (f_body, ret_type)) ->
                let typed_param' = 
                  (arg, build_recursive_type t_var arg_type) 
                in
                let f_body' = build_recursive_type t_var f_body in
                let ret_type' = build_recursive_type t_var ret_type in
                DTyped_funsig (f, typed_param', (f_body', ret_type'))
              )
          funsigs
        in
        let e_desc' = build_recursive_type t_var e_desc in
        LetRecFunWithType (funsigs', e_desc')
    | LetFunWithType (fun_sig, e_desc) ->
      (
      match fun_sig with 
      | Typed_funsig (f, typed_params, (f_body, ret_type)) ->
        if f = t_var 
        then
          e
        else
          let typed_params' = 
            List.map 
              (fun (arg, t) -> (arg, build_recursive_type t_var t))
              typed_params
          in
          let f_body' = build_recursive_type t_var f_body in
          let ret_type' = build_recursive_type t_var ret_type in
          let e_desc' = build_recursive_type t_var e_desc in
          LetFunWithType (Typed_funsig(f, typed_params', (f_body', ret_type')), e_desc')
      | DTyped_funsig (f, (arg, arg_type), (f_body, ret_type)) ->
        if f = t_var 
        then
          e
        else
          let typed_param' = arg, build_recursive_type t_var arg_type in
          let f_body' = build_recursive_type t_var f_body in
          let ret_type' = build_recursive_type t_var ret_type in
          let e_desc' = build_recursive_type t_var e_desc in
          LetFunWithType (DTyped_funsig(f, typed_param', (f_body', ret_type')), e_desc')
      )
    | Plus (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Plus (e_desc1', e_desc2')
    | Minus (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Minus (e_desc1', e_desc2')
    | Times (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Times (e_desc1', e_desc2')
    | Divide (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Divide (e_desc1', e_desc2')
    | Modulus (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Modulus (e_desc1', e_desc2')
    | Equal (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Equal (e_desc1', e_desc2')
    | Neq (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Neq (e_desc1', e_desc2')
    | LessThan (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      LessThan (e_desc1', e_desc2')
    | Leq (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Leq (e_desc1', e_desc2')
    | GreaterThan (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      GreaterThan (e_desc1', e_desc2')
    | Geq (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Geq (e_desc1', e_desc2')
    | And (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      And (e_desc1', e_desc2')
    | Or (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      Or (e_desc1', e_desc2')
    | Not e_desc ->
      let e_desc' = build_recursive_type t_var e_desc in
      Not e_desc'
    | If (e_desc1, e_desc2, e_desc3) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      let e_desc3' = build_recursive_type t_var e_desc3 in
      If (e_desc1', e_desc2', e_desc3')
    | Record r ->
      let r' =
        Ident_map.map
          (fun e_desc -> build_recursive_type t_var e_desc)
          r
      in
      Record r'
    | RecordProj (e_desc, l) ->
      let e_desc' = build_recursive_type t_var e_desc in
      RecordProj (e_desc', l)
    | Match (match_edesc, pat_edesc_lst) ->
      let match_edesc' = build_recursive_type t_var match_edesc in
      let pat_edesc_lst' = 
        List.map 
          (fun (pat, e_desc) -> 
            let e_desc' = build_recursive_type t_var e_desc in
            (pat, e_desc')
          )
          pat_edesc_lst
      in
      Match (match_edesc', pat_edesc_lst')
    | VariantExpr (l, e_desc) ->
      let e_desc' = build_recursive_type t_var e_desc in
      VariantExpr (l, e_desc')
    | List e_descs ->
      let e_descs' = 
        List.map (build_recursive_type t_var) e_descs 
      in
      List e_descs'
    | ListCons (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      ListCons (e_desc1', e_desc2')
    | Assert e_desc ->
      let e_desc' = build_recursive_type t_var e_desc in
      Assert e_desc'
    | Assume e_desc ->
      let e_desc' = build_recursive_type t_var e_desc in
      Assume e_desc'
    | TypeVar _ | TypeInt | TypeBool -> e
    | TypeRecord r ->
      let r' =
        Ident_map.map
          (fun e_desc -> build_recursive_type t_var e_desc)
          r
      in
      TypeRecord r'
    | TypeList t ->
      let t' = build_recursive_type t_var t in
      TypeList t'
    | TypeArrow (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      TypeArrow (e_desc1', e_desc2')
    | TypeArrowD ((x, e_desc1), e_desc2) ->
      if x = t_var 
      then
        e
      else
        let e_desc1' = build_recursive_type t_var e_desc1 in
        let e_desc2' = build_recursive_type t_var e_desc2 in
        TypeArrowD ((x, e_desc1'), e_desc2')
    | TypeSet (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      TypeSet (e_desc1', e_desc2')
    | TypeUnion (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      TypeUnion (e_desc1', e_desc2')
    | TypeIntersect (e_desc1, e_desc2) ->
      let e_desc1' = build_recursive_type t_var e_desc1 in
      let e_desc2' = build_recursive_type t_var e_desc2 in
      TypeIntersect (e_desc1', e_desc2')
    | TypeRecurse (tv, e_desc) ->
      if t_var = tv 
      then
        e
      else
        let e_desc' = build_recursive_type t_var e_desc in
        TypeRecurse (tv, e_desc')
  in
  {tag = tag; body = body'}
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

%start <Bluejay_ast.expr> prog
%start <Bluejay_ast.expr option> delim_expr

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
      { LetRecFunWithType ($3, new_expr_desc $5) }
  | LET_D REC fun_sig_dependent_list IN expr %prec prec_let 
      { LetRecFunWithType ($3, new_expr_desc $5) }
  | LET ident_decl EQUALS expr IN expr %prec prec_let
      { Let($2, new_expr_desc $4, new_expr_desc $6) }
  | LET OPEN_PAREN ident_decl COLON expr CLOSE_PAREN EQUALS expr IN expr %prec prec_let
      { LetWithType($3, new_expr_desc $8, new_expr_desc $10, new_expr_desc $5) }
  | LET fun_sig IN expr %prec prec_fun
      { LetFun($2, new_expr_desc $4) }
  | LET fun_sig_with_type IN expr %prec prec_fun
      { LetFunWithType ($2, new_expr_desc $4) }
  | LET_D fun_sig_dependent IN expr %prec prec_fun
      { LetFunWithType ($2, new_expr_desc $4) }
  | MATCH expr WITH PIPE? match_expr_list END
      { Match(new_expr_desc $2, $5) }
  // Types expressions
  | basic_types { $1 }
  | type_parameter { $1 }
  | MU ident_decl DOT expr 
    { TypeRecurse ($2, build_recursive_type $2 (new_expr_desc $4)) }
  | expr ARROW expr { TypeArrow (new_expr_desc $1, new_expr_desc $3) }
  | OPEN_PAREN ident_decl COLON expr CLOSE_PAREN ARROW expr { TypeArrowD (($2, new_expr_desc $4), new_expr_desc $7) }
  // TODO: Change this to fancy curly
  | OPEN_BRACE DOT expr PIPE expr CLOSE_BRACE { TypeSet (new_expr_desc $3, new_expr_desc $5) } 
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

/* [1, 2, true] (Unlike ocaml, bluejay lists can be heterogenous) */
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
