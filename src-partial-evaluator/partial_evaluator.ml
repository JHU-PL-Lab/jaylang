open Core
open Graph
open Dj_common
open Log.Export
open Jayil
open Ast

open Batteries
open Jhupllib

let rec constant_folding (Expr clauses) =
  let new_clauses = List.map constant_folding_clause clauses in
  Expr new_clauses

and constant_folding_clause clause =
  let (Clause (Var (x, s), cbody)) = clause in
  match cbody with
  | Binary_operation_body (x1, Binary_operator_minus, x2)
    when Ast.Var.equal x1 x2 ->
      let new_body = Value_body (Value_int 0) in
      Clause (Var (x, s), new_body)
  | _ -> clause

let eval expr =

  Fmt.pr "%a\n" Jayil.Pp.expr expr ;
  let new_expr = constant_folding expr in
  Fmt.pr "%a\n" Jayil.Pp.expr new_expr ;

  new_expr
;;

let eval_file raw_source = raw_source |> Dj_common.File_utils.read_source |> eval;;





(* New types & module definitions *)
type lexadr = int * int [@@deriving eq, ord, show, to_yojson]

module LexAdr = struct
  type t = lexadr

  let equal = equal_lexadr
  let compare = compare_lexadr
  let pp = pp_lexadr
  let show = show_lexadr
  let to_yojson = lexadr_to_yojson
  let hash = Hashtbl.hash
end

module LexAdr_set = struct
  module S = Set.Make (LexAdr)
  include S
  include Pp_utils.Set_pp (S) (LexAdr)
  include Yojson_utils.Set_to_yojson (S) (LexAdr)
end

type identline = Ident of Ident.t | LexAdr of lexadr [@@deriving eq, ord, show, to_yojson]

module IdentLine = struct
  type t = identline

  let equal = equal_identline
  let compare = compare_identline
  (* let pp = pp_identline *)
  let show = show_identline
  let to_yojson = identline_to_yojson
  let hash = Hashtbl.hash


  
  let pp oc = function
  | Ident ident -> Jayil.Ast_pp.pp_ident oc ident
  | LexAdr lexadr -> LexAdr.pp oc lexadr

end

module IdentLine_map = struct
  module M = Map.Make (IdentLine)
  include M
  include Pp_utils.Map_pp (M) (IdentLine)
  include Yojson_utils.Map_to_yojson (M) (IdentLine)

  let key_list map = keys map |> List.of_enum
end

type pvalue =
  | Direct of value
  | FunClosure of Ident.t * function_value * penv
  | RecordClosure of record_value * penv
  | AbortClosure of penv

and presidual = PValue of pvalue | PClause of clause_body (* Note that Value_body of clause_body should not be used... *)
and presidual_with_ident_lexadr_deps = Ident.t * presidual * LexAdr_set.t
and penv = presidual_with_ident_lexadr_deps IdentLine_map.t

let value_of_pvalue = function
  | Direct v -> v
  | FunClosure (_fid, fv, _env) -> Value_function fv
  | RecordClosure (r, _env) -> Value_record r
  | AbortClosure _ -> Value_bool false

let rec pp_pvalue oc = function
  | Direct v -> Jayil.Ast_pp.pp_value oc v
  | FunClosure _ -> Format.fprintf oc "(fc)"
  | RecordClosure (r, env) -> pp_record_c (r, env) oc
  | AbortClosure _ -> Format.fprintf oc "(abort)"

and pp_record_c (Record_value r, env) oc =
  let pp_entry oc (x, v) =
    Fmt.pf oc "%a = %a" Jayil.Ast_pp.pp_ident x Jayil.Ast_pp.pp_var v
  in
  (Fmt.braces (Fmt.iter_bindings ~sep:(Fmt.any ", ") Ident_map.iter pp_entry))
    oc r

and pp_presidual oc = function
  | PValue pvalue -> pp_pvalue oc pvalue
  | PClause clause_body -> Jayil.Ast_pp.pp_clause_body oc clause_body

and pp_pwild oc ((ident, presidual, lexadrset) : presidual_with_ident_lexadr_deps) =
  Format.fprintf oc "%a = %a, %a" Jayil.Ast_pp.pp_ident ident pp_presidual presidual LexAdr_set.pp lexadrset

let pp_penv : penv Pp_utils.pretty_printer = IdentLine_map.pp pp_pwild





(* Helper functions *)
let add_ident_line (ident : Ident.t) (lexadr : int * int) (v : 'a) (map : 'a IdentLine_map.t) =
  let envnum, _ = lexadr
  in IdentLine_map.add (LexAdr (envnum, -1)) v (IdentLine_map.add (LexAdr lexadr) v (IdentLine_map.add (Ident ident) v map))

let add_ident_line_penv (ident : Ident.t) (lexadr : int * int) (lexadrs : LexAdr_set.t) (v : presidual) (map : penv) =
  let mapval = (ident, v, LexAdr_set.add lexadr lexadrs) in
  add_ident_line ident lexadr mapval map
;;

let get_from_ident (Var (ident, _) : var) (env : penv) =
  match IdentLine_map.find_opt (Ident ident) env with
    | Some v -> v
    | None -> failwith ("Expression not closed! " ^ show_ident ident ^ " not in environment!")
  
let get_pvalue_from_ident (ident : var) (env : penv) = let [@warning "-8"] (_, PValue v, _) = get_from_ident ident env in v

let get_pvalue_deps_from_ident (ident : var) (env : penv) = let [@warning "-8"] (_, PValue v, deps) = get_from_ident ident env in deps, v


(* Eval helpers *)
let check_pattern (v : pvalue) (pattern : pattern) : bool =
  match v, pattern with
  | Direct (Value_int _), Int_pattern -> true
  | Direct (Value_bool _), Bool_pattern -> true
  | Direct (Value_function _), _ -> failwith "fun must be a closure (Impossible!)"
  | Direct (Value_record _), _ -> failwith "record must be a closure (Impossible!)"
  | RecordClosure (Record_value record, _), Rec_pattern key_set ->
      Ident_set.for_all (fun id -> Ident_map.mem id record) key_set
  | RecordClosure (Record_value record, _), Strict_rec_pattern key_set ->
      Ident_set.equal key_set (Ident_set.of_enum @@ Ident_map.keys record)
  | FunClosure (_, _, _), Fun_pattern -> true
  | _, Any_pattern -> true
  | _ -> false

and binop = function
  | Binary_operator_plus, Value_int n1, Value_int n2 ->
      Value_int (n1 + n2)
  | Binary_operator_minus, Value_int n1, Value_int n2 ->
      Value_int (n1 - n2)
  | Binary_operator_times, Value_int n1, Value_int n2 ->
      Value_int (n1 * n2)
  | Binary_operator_divide, Value_int n1, Value_int n2 ->
      Value_int (n1 / n2)
  | Binary_operator_modulus, Value_int n1, Value_int n2 ->
      Value_int (n1 mod n2)
  | Binary_operator_less_than, Value_int n1, Value_int n2 ->
      Value_bool (n1 < n2)
  | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 ->
      Value_bool (n1 <= n2)
  | Binary_operator_equal_to, Value_int n1, Value_int n2 ->
      Value_bool (n1 = n2)
  | Binary_operator_equal_to, Value_bool b1, Value_bool b2 ->
      Value_bool (Core.Bool.( = ) b1 b2)
  | Binary_operator_and, Value_bool b1, Value_bool b2 ->
      Value_bool (b1 && b2)
  | Binary_operator_or, Value_bool b1, Value_bool b2 ->
      Value_bool (b1 || b2)
  | _, _, _ -> failwith "incorrect binop"




(* evals *)
let simple_eval (expr : expr) : value * penv = (

  let rec eval_expr (envnum : int) (env : penv) (Expr (clauses) : expr) : penv = 
    let foldable_eval_clause (env : penv) (index : int) (clause : clause) : penv = (
      let env' = eval_clause (envnum, index+1) env clause in env' 
    )
    in List.fold_lefti foldable_eval_clause env clauses

  and eval_clause (lexadr : int * int) (env : penv) (Clause (Var (x, _), body) : clause) : penv = 
    let linedeps, res_value = match body with

    (* Deps list is inaccurate, need to actually go through function and see what variables are captured *)
    | Value_body (Value_function vf) -> LexAdr_set.empty, FunClosure (x, vf, env)
    (* Deps list is inaccurate, need to actually go through record and see what variables are captured *)
    | Value_body (Value_record vr) -> LexAdr_set.empty, RecordClosure (vr, env)
    | Value_body v -> LexAdr_set.empty, Direct v

    | Var_body vx -> get_pvalue_deps_from_ident vx env

    | Input_body -> LexAdr_set.empty, Direct (Value_int (read_int ())) (* This will be interesting to peval...We'll need arguments for whether input is known at peval-time or not *)

    | Match_body (vx, p) -> let lexadrs, v = get_pvalue_deps_from_ident vx env in
      lexadrs, Direct (Value_bool (check_pattern v p))

    | Conditional_body (x2, e1, e2) -> failwith "Evaluation does not yet support conditionals!"

    | Appl_body (vx1, (Var (x2, _) as vx2)) -> failwith "Evaluation does not yet support function application!"

    (* Deps list here actually only needs to be the original captured variable, not the whole list for the record! *)
    | Projection_body (v, key) -> ( match get_pvalue_from_ident v env with
      | RecordClosure (Record_value r, renv) ->
        let proj_x = Ident_map.find key r in
        get_pvalue_deps_from_ident proj_x renv
      | _ -> failwith "Type error! Projection attempted on a non-record!"
    )
    
    | Not_body vx -> ( match get_pvalue_deps_from_ident vx env with
      | lexadrs, Direct (Value_bool b) -> lexadrs, Direct (Value_bool (not b))
      | _ -> failwith "Type error! Not attempted on a non-bool!"
    )
    
    | Binary_operation_body (vx1, op, vx2) ->
      let lexadrs1, v1 = get_pvalue_deps_from_ident vx1 env
      and lexadrs2, v2 = get_pvalue_deps_from_ident vx2 env in
      let v1, v2 = match v1, v2 with
        | Direct v1, Direct v2 -> v1, v2
        | _ -> failwith "Type error! Binary ops attempted on incompatible types!"
      in let v = binop (op, v1, v2)
      in LexAdr_set.union lexadrs1 lexadrs2, Direct v
    
    | Abort_body | Assert_body _ | Assume_body _ -> failwith "Evaluation does not yet support abort, assert, and assume!"
  
    in (add_ident_line_penv x lexadr linedeps (PValue res_value) env)
  
  in let endenv = eval_expr 0 IdentLine_map.empty expr
  in let [@warning "-8"] _, PValue endpvalue, _ = (IdentLine_map.find (LexAdr (0, -1)) endenv)
  in value_of_pvalue endpvalue, endenv
)
;;





(* Printing functions & modules *)
module type Parser = sig
  val parse : string -> expr
end

module type PEvaler = sig
  type t
  val eval : expr -> t * penv
  val unparse : t -> string
end

module PEToploop (P : Parser) (E : PEvaler) = struct
  include P
  include E

  let parse_eval (a : string) = a |> parse |> eval |> fst;;

  let debug_parse_eval (a : string) = a |> parse |> eval |> snd;;
  
  
  let parse_eval_unparse (a : string) = a |> parse_eval |> unparse;;
  let peu = parse_eval_unparse;;
  let parse_eval_print (a : string) = a |> peu |> print_endline;; (* print_endline "";; *)
  let rep = parse_eval_print;;
  let debug_parse_eval_print (a : string) = a |> debug_parse_eval |> Format.printf "%a" pp_penv;;
  let drep = debug_parse_eval_print;;
end

let parse = Jayil_parser.Parse.parse_program_str;;
let pfile s = Dj_common.File_utils.read_source s;;

let unparse_value = Jayil.Ast_pp.show_value;;
let unparse_expr = Jayil.Ast_pp.show_expr;;

module StringParser = struct
  let parse = parse
end

module FileParser = struct
  let parse = pfile
end

module SimpleEval = PEToploop (FileParser) (struct type t = value;; let eval = simple_eval;; let unparse = unparse_value end)

module PartialEval = PEToploop (FileParser) (struct type t = value;; let eval = simple_peval;; let unparse = unparse_expr end)

(*
let sparse_eval (a : string) = a |> parse |> simple_eval |> fst;;

let debug_sparse_eval (a : string) = a |> parse |> simple_eval |> snd;;


let sparse_eval_unparse (a : string) = a |> parse |> simple_eval |> fst |> unparse;;
let speu = sparse_eval_unparse;;
let sparse_eval_print (a : string) = a |> speu |> print_endline;; (* print_endline "";; *)
let srep = sparse_eval_print;;
let debug_sparse_eval_print (a : string) = a |> debug_sparse_eval |> Format.printf "%a" pp_penv;;
let sdrep = debug_sparse_eval_print;;



let spfile_eval (a : string) = a |> pfile |> simple_eval |> fst;;

let debug_spfile_eval (a : string) = a |> pfile |> simple_eval |> snd;;


let spfile_eval_unparse (a : string) = a |> pfile |> simple_eval |> fst |> unparse;;
let spfeu = spfile_eval_unparse;;
let spfile_eval_print (a : string) = a |> spfeu |> print_endline;; (* print_endline "";; *)
let sfrep = spfile_eval_print;;
let debug_spfile_eval_print (a : string) = a |> debug_spfile_eval |> Format.printf "%a" pp_penv;;
let sdfrep = debug_spfile_eval_print;;
*)



exception Found_target of { x : Id.t; stk : Concrete_stack.t; v : pvalue }
exception Found_abort of pvalue
exception Terminate of pvalue
exception Reach_max_step of Id.t * Concrete_stack.t
exception Run_the_same_stack_twice of Id.t * Concrete_stack.t
exception Run_into_wrong_stack of Id.t * Concrete_stack.t

type clause_cb = Id.t -> Concrete_stack.t -> value -> unit
type debug_mode = No_debug | Debug_clause of clause_cb

module G = Imperative.Digraph.ConcreteBidirectional (Id_with_stack)

type session = {
  (* tuning *)
  step : int ref;
  max_step : int option;
  (* book-keeping *)
  alias_graph : G.t;
  (* debug *)
  is_debug : bool; (* TODO: get rid of this *)
  debug_mode : debug_mode;
  val_def_map : (Id_with_stack.t, clause_body * pvalue) Core.Hashtbl.t;
  (* term_detail_map : (Lookup_key.t, Term_detail.t) Hashtbl.t; *)
  (* block_map : Cfg.block Jayil.Ast.Ident_map.t; *)
  (* rstk_picked : (Rstack.t, bool) Hashtbl.t; *)
  (* lookup_alert : Lookup_key.t Hash_set.t; *)
}

let make_default_session () =
  {
    max_step = None;
    is_debug = false;
    debug_mode = No_debug;
    step = ref 0;
    alias_graph = G.create ();
    val_def_map = Core.Hashtbl.create (module Id_with_stack);
    (* term_detail_map = Hashtbl.create (module Lookup_key); *)
    (* block_map = Jayil.Ast.Ident_map.empty; *)
    (* rstk_picked = Hashtbl.create (module Rstack); *)
    (* lookup_alert = Hash_set.create (module Lookup_key); *)
  }

let create_session ?max_step ?(debug_mode = No_debug) (* state : Global_state.t *)
    (config : Global_config.t) mode input_feeder : session =
  (* = With_full_target (config.target, target_stk) *)
  {
    max_step;
    is_debug = config.debug_interpreter;
    debug_mode;
    step = ref 0;
    alias_graph = G.create ();
    val_def_map = Core.Hashtbl.create (module Id_with_stack);
    (* term_detail_map = Hashtbl.create (module Lookup_key); (* state.term_detail_map; *) *)
    (* block_map = Jayil.Ast.Ident_map.empty; (* state.block_map; *) *)
    (* rstk_picked = Hashtbl.create (module Rstack); (* state.rstk_picked; *) *)
    (* lookup_alert = Hash_set.create (module Lookup_key); (* state.lookup_alert; *) *)
  }

let cond_fid b = if b then Ast.Ident "$tt" else Ast.Ident "$ff"

(* This function will add a directed edge x1 -> x2 in the alias graph. Thus
   x1 here needs to be the *later* defined variable. *)
let add_alias x1 x2 session : unit =
  let alias_graph = session.alias_graph in
  G.add_edge alias_graph x1 x2

let add_val_def_mapping x vdef session : unit =
  let val_def_mapping = session.val_def_map in
  Core.Hashtbl.add_exn ~key:x ~data:vdef val_def_mapping

let debug_update_read_node (session : session) (x : ident) (stk : Concrete_stack.t) =  ()

let debug_update_write_node (session : session) (x : ident) (stk : Concrete_stack.t) = ()

let debug_stack (session : session) (x : ident) (stk : Concrete_stack.t) ((v : pvalue), _) = ()

let raise_if_with_stack (session : session) (x : ident) (stk : Concrete_stack.t) (v : pvalue) = ()

let alert_lookup (session : session) (x : ident) (stk : Concrete_stack.t) = ()

let rec same_stack s1 s2 =
  match (s1, s2) with
  | (cs1, fid1) :: ss1, (cs2, fid2) :: ss2 ->
      Ident.equal cs1 cs2 && Ident.equal fid1 fid2 && same_stack ss1 ss2
  | [], [] -> true
  | _, _ -> false

let debug_clause ~session x v stk =
  ILog.app (fun m -> m "@[%a = %a@]" Id.pp x pp_pvalue v) ;

  raise_if_with_stack session x stk v ;
  debug_stack session x stk (v, stk) ;
  ()

(* OB: we cannot enter the same stack twice. *)
let rec fetch_val_with_stk ~session ~stk env (Var (x, _)) :
    pvalue * Concrete_stack.t =
  let res = Ident_map.find x env in
  debug_update_read_node session x stk ;
  res

and fetch_val ~session ~stk env x : pvalue =
  fst (fetch_val_with_stk ~session ~stk env x)

and fetch_val_to_direct ~session ~stk env vx : value =
  match fetch_val ~session ~stk env vx with
  | Direct v -> v
  | _ -> failwith "eval to non direct value"

and fetch_val_to_bool ~session ~stk env vx : bool =
  match fetch_val ~session ~stk env vx with
  | Direct (Value_bool b) -> b
  | _ -> failwith "eval to non bool"

and check_pattern ~session ~stk env vx pattern : bool =
  let is_pass =
    match (fetch_val ~session ~stk env vx, pattern) with
    | Direct (Value_int _), Int_pattern -> true
    | Direct (Value_bool _), Bool_pattern -> true
    | Direct (Value_function _), _ -> failwith "fun must be a closure"
    | Direct (Value_record _), _ -> failwith "record must be a closure"
    | RecordClosure (Record_value record, _), Rec_pattern key_set ->
        Ident_set.for_all (fun id -> Ident_map.mem id record) key_set
    | RecordClosure (Record_value record, _), Strict_rec_pattern key_set ->
        Ident_set.equal key_set (Ident_set.of_enum @@ Ident_map.keys record)
    | FunClosure (_, _, _), Fun_pattern -> true
    | _, Any_pattern -> true
    | _, _ -> false
  in
  is_pass