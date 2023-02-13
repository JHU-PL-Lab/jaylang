open Jayil
open Ast

open Batteries
open Jhupllib

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

(* Source: https://gist.github.com/hcarty/3789149 *)
let pp_enum ?(flush = false) ?(first = "") ?(last = "") ?(sep = " ") ?(indent = String.length first) pp f e =
  let open Format in
  pp_open_box f indent;
  pp_print_cut f ();
  pp_print_string f first;
  pp_print_cut f ();
  match Enum.get e with
  | None ->
      pp_print_string f last;
      pp_close_box f ();
      if flush then pp_print_flush f ()
  | Some x ->
      pp_open_box f indent;
      pp f x;
      let rec aux () =
        match Enum.get e with
        | None ->
            pp_close_box f ();
            pp_print_cut f ();
            pp_print_string f last;
            pp_close_box f ();
            if flush then pp_print_flush f ()
        | Some x ->
            pp_print_string f sep;
            pp_close_box f ();
            pp_print_cut f ();
            pp_open_box f indent;
            pp f x;
            aux ()
      in
      aux ()



module OptionSyntax : sig

  (* Monad *)
  val ( let* ) : 'a option * LexAdr_set.t -> ('a -> ('b option * LexAdr_set.t) ) -> 'b option * LexAdr_set.t

  (* Applicatives *)
  val ( let+ ) : 'a option * LexAdr_set.t -> ('a -> 'b) -> 'b option * LexAdr_set.t
  val ( and+ ) : 'a option * LexAdr_set.t -> 'b option * LexAdr_set.t -> ('a * 'b) option * LexAdr_set.t
  val pure : 'a -> 'a option * LexAdr_set.t

end = struct
  (*
  let (let*\) x f = match x with
  | None -> None
  | Some x -> (f x)
  let (let+) x f = match x with
  | None -> None
  | Some x -> Some (f x)
  *)
  let (let*) x f = match x with
  | None, set -> None, set
  | (Some x), set -> let res, setres = f x in res, (LexAdr_set.union set setres)
  let (let+) (opt, set) f = match opt with
  | None -> None, set
  | Some x -> Some (f x), set
  let (and+) (o1, set1) (o2, set2) = (match o1, o2 with
    | Some x, Some y  -> Some (x,y)
    | _               -> None), (LexAdr_set.union set1 set2)
  let pure x = Some x, LexAdr_set.empty
end

open OptionSyntax


(* Helper functions *)
let add_ident_line (ident : Ident.t) (lexadr : int * int) (v : 'a) (map : 'a IdentLine_map.t) =
  IdentLine_map.add (LexAdr lexadr) v (IdentLine_map.add (Ident ident) v map)

let add_ident_line_last (ident : Ident.t) ((envnum, _ as lexadr) : int * int) (v : 'a) (map : 'a IdentLine_map.t) =
  IdentLine_map.add (LexAdr (envnum, -1)) v (IdentLine_map.add (LexAdr lexadr) v (IdentLine_map.add (Ident ident) v map))

let add_ident_line_penv (ident : Ident.t) (lexadr : int * int) (lexadrs : LexAdr_set.t) (v : presidual) (map : penv) =
  let mapval = (ident, v, LexAdr_set.add lexadr lexadrs) in
  add_ident_line_last ident lexadr mapval map
;;

let add_ident_el_penv (ident : Ident.t) (((_, _, lexadrs) as mapval) : presidual_with_ident_lexadr_deps) ~(map : penv) =
  add_ident_line_last ident (LexAdr_set.max_elt lexadrs) mapval map

let get_from_ident (Var (ident, _) : var) (env : penv) =
  match IdentLine_map.find_opt (Ident ident) env with
    | Some v -> v
    | None -> failwith ("Expression not closed! " ^ show_ident ident ^ " not in environment!")
  
let get_pvalue_from_ident (ident : var) (env : penv) = let [@warning "-8"] (_, PValue v, _) = get_from_ident ident env in v

let get_pvalue_deps_from_ident (ident : var) (env : penv) = let [@warning "-8"] (_, PValue v, deps) = get_from_ident ident env in deps, v

let get_presidual_from_ident = get_from_ident

(* let get_pvalue_deps_line_from_ident (ident : var) (env : penv) = let [@warning "-8"] (_, PValue v, deps) = get_from_ident ident env in deps, v, LexAdr_set.max_elt deps *)

let get_from_ident_opt (Var (ident, _) : var) (env : penv) =
  IdentLine_map.find_opt (Ident ident) env, LexAdr_set.empty
  
let get_pvalue_from_ident_opt (ident : var) (env : penv) =
  let* (_, v, deps) = get_from_ident_opt ident env in
  let+ v = (match v with | PValue v -> Some v | _ -> None), deps (* PValue deps guaranteed trivial for now *)
  in v

let get_wrapped_pvalue_from_ident_opt (ident : var) (env : penv) =
  let* (_, v, deps) = get_from_ident_opt ident env in
  let+ v = (match v with | PValue _ -> Some v | _ -> None), deps (* PValue deps guaranteed trivial for now *)
  in v

let get_presidual_from_ident_opt (ident : var) (env : penv) =
  let* (_, v, deps) = get_from_ident_opt ident env
  in Some v, deps (* deps guaranteed non-trivial for PClauses *)

let get_presidual_from_ident_ref_opt (ident : var) (env : penv) =
  let* (_, v, deps) = get_from_ident_opt ident env
  in match v with
  | PValue _ -> Some v, deps (* deps guaranteed trivial for now *)
  | PClause _ -> Some v, LexAdr_set.pop_max deps |> snd (* This may fail if deps contains higher envnums then itself? *)

let get_presidual_from_ident_semi_ref_opt (ident : var) (env : penv) =
  let* (_, v, deps) = get_from_ident_opt ident env
  in match v with
  | PClause Var_body _ -> Some v, LexAdr_set.pop_max deps |> snd (* This may fail if deps contains higher envnums then itself? *)
  | PClause _ -> Some (PClause (Var_body ident)), deps
  | _ -> Some v, deps (* deps guaranteed trivial for now *)

let get_deps_from_ident_opt (ident : var) (env : penv) =
  let* (_, _, deps) = get_from_ident_opt ident env in
  Some (), deps (* Add deps to state only? *)

let get_many_deps_from_ident_opt (idents : var Enum.t) (env : penv) =
  let deps = Enum.fold (fun prev_deps ident ->
    let (_, next_deps) = get_deps_from_ident_opt ident env
    in LexAdr_set.union prev_deps next_deps) LexAdr_set.empty idents
  in Some (), deps (* deps might also suffice if this doesn't have to be in the monad *)
  
let get_many_lines_from_ident_opt (idents : var Enum.t) (env : penv) =
  let lines, deps = Enum.fold (fun (prev_lines, prev_deps) ident ->
    let (_, next_deps) = get_deps_from_ident_opt ident env
    in (LexAdr_set.max_elt next_deps) :: prev_lines, LexAdr_set.union prev_deps next_deps
  ) ([], LexAdr_set.empty) idents
  in (* Some *) lines, deps



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

let pstring = Jayil_parser.Parse.parse_program_str;;
let pfile s = Dj_common.File_utils.read_source ~check_wellformed:false s;;

let unparse_value = Jayil.Ast_pp.show_value;;
let unparse_expr = Jayil.Ast_pp.show_expr;;