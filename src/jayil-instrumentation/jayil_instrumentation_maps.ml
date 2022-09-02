open Batteries;;

open Jayil;;

module Odefa_clause = struct
  type t = Ast.clause;;
  let pp = Ast_pp.pp_clause;;
end;;

type t = {

  is_natodefa : bool;
  (** A set of odefa variables that were added during instrumentation
      (as opposed to being in the original code or added during pre-
      instrumentation translation).  The instrumentation variable
      is the key; the value is the pre-instrumentation variable
      it aliases.  Note that the value is an Option; it is none if
      the variable has no associated pre-instrumentation alias (namely
      if it was added as a pattern match conditional var). *)
  jayil_instrument_vars_map : (Ast.Ident.t option) Ast.Ident_map.t;

  (** Mapping between variables that were added during instrumentation
      with the variables whose clauses the instrumenting clause is
      constraining.  This is mainly used to obtain the operation that
      an instrumenting conditional is constrianing. *)
  jayil_pre_instrument_clause_mapping : Odefa_clause.t Ast.Ident_map.t;
  
}
[@@ deriving show]
;;

let empty is_natodefa = {
  is_natodefa = is_natodefa;
  jayil_pre_instrument_clause_mapping = Ast.Ident_map.empty;
  jayil_instrument_vars_map = Ast.Ident_map.empty;
}
;;

let add_odefa_instrument_var mappings inst_ident ident_opt =
  let instrument_set = mappings.jayil_instrument_vars_map in
  { mappings with
    jayil_instrument_vars_map =
      Ast.Ident_map.add inst_ident ident_opt instrument_set;
  }
;;

let add_odefa_var_clause_mapping mappings var_ident clause =
  let instrument_map = mappings.jayil_pre_instrument_clause_mapping in
  { mappings with
    jayil_pre_instrument_clause_mapping =
      Ast.Ident_map.add var_ident clause instrument_map;
  }
;;

let get_pre_inst_equivalent_clause mappings odefa_ident =
  let inst_map = mappings.jayil_instrument_vars_map in
  let clause_map = mappings.jayil_pre_instrument_clause_mapping in
  (* Get pre-instrument var from instrument var *)
  let odefa_ident' =
    match Ast.Ident_map.Exceptionless.find odefa_ident inst_map with
    | Some (Some pre_inst_ident) -> pre_inst_ident
    | Some (None) | None -> odefa_ident
  in
  (* Get clause from var *)
  match Ast.Ident_map.Exceptionless.find odefa_ident' clause_map with
  | Some clause -> clause
  | None ->
    raise @@ Invalid_argument
      (Printf.sprintf
        "%s needs to have an associated clause."
        (Ast.show_ident odefa_ident))
;;

let is_natodefa mappings = mappings.is_natodefa;;

let is_var_instrumenting mappings odefa_ident =
  let inst_map = mappings.jayil_instrument_vars_map in
  Ast.Ident_map.mem odefa_ident inst_map
;;

let get_pre_inst_var_opt mappings x = 
  let inst_map = mappings.jayil_instrument_vars_map in
  match (Ast.Ident_map.find_opt x inst_map) with
  | None -> None
  | Some x' -> x'
;;

let inherit_from_jay_to_jayil_maps inst_map_nat = 
  {
  is_natodefa = true;
  jayil_pre_instrument_clause_mapping = Ast.Ident_map.empty;
  jayil_instrument_vars_map = inst_map_nat;
}
;;