open Core
open Graph
open Jhupllib

open Dbmc
open Odefa_ast
open Odefa_ast.Ast
open Odefa_natural.On_to_odefa_maps

module type Error_location = sig
  type t;;
  val show : t -> string;;
  val show_brief : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

let replace_linebreaks (str : string) : string =
  String.tr ~target:'\n' ~replacement:' ' str 
;;

module Odefa_error_location
  : Error_location with type t = Ast.clause = struct
  type t = Ast.clause;;
  let show = Ast_pp.show_clause;;
  let show_brief = Ast_pp_brief.show_clause;;
  let to_yojson clause =
    `String (replace_linebreaks @@ show clause);;
end;;

module Natodefa_error_location
  : Error_location with type t = Odefa_natural.On_ast.expr_desc = struct
  type t = Odefa_natural.On_ast.expr_desc;;
  let show = Pp_utils.pp_to_string Odefa_natural.On_ast_pp.pp_expr_desc_without_tag;;
  let show_brief = Pp_utils.pp_to_string Odefa_natural.On_ast_pp.pp_expr_desc_without_tag;;
  let to_yojson expr = 
    `String (replace_linebreaks @@ show expr);;
end;;

module type Sato_Result = sig
  type t;;
  val description : string;;
  val get_result : 
    Sato_state.t -> Dbmc.Types.State.t -> 
    Dbmc.Interpreter.session -> Dbmc.Interpreter.denv -> 
    int option list -> t;;
  val show : t -> string;;
  val show_compact : t -> string;;
  val count : t -> int;;
  val generation_successful : t -> bool;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

(* **** String showing utilities **** *)

let pp_input_sequence formatter (input_seq : int list) =
  Pp_utils.pp_list Format.pp_print_int formatter input_seq
;;

let show_input_sequence : int list -> string =
  Pp_utils.pp_to_string pp_input_sequence
;;

let get_expected_type_from_operator op = 
  match op with
  | Binary_operator_plus | Binary_operator_minus
  | Binary_operator_times | Binary_operator_divide
  | Binary_operator_modulus | Binary_operator_less_than
  | Binary_operator_less_than_or_equal_to | Binary_operator_equal_to
  | Binary_operator_not_equal_to -> Int_type
  | Binary_operator_and | Binary_operator_or -> Bool_type

let get_value_type v = 
  match v with
  | Value_int _ -> Int_type
  | Value_bool _ -> Bool_type
  | Value_function _ -> Fun_type
  | Value_record (Record_value r) -> 
    let lbls = Ident_set.of_enum @@ Ident_map.keys r in
    Rec_type lbls

let get_expected_type_from_cls cls_body = 
  match cls_body with
  | Binary_operation_body (_, op, _) ->
    get_expected_type_from_operator op
  | Not_body _ -> Bool_type
  | Appl_body _ -> Fun_type
  | Projection_body (_, lbl) -> Rec_type (Ident_set.singleton lbl)
  | Conditional_body _ -> Bool_type
  | _ -> failwith @@
    "Errors should only arise from unary/binary operations, function application, record projection, or a conditional."

let get_abort_cond_clause_id
  (abort_mapping : (Ident_new.t, abort_value) Hashtbl.t) 
  (ab_id : Ident_new.t) 
  : Ident_new.t =
  match (Hashtbl.find abort_mapping ab_id) with 
  | Some ab_val -> ab_val.abort_conditional_ident
  | None -> failwith "Should have a corresponding clause here!"

let get_odefa_errors
  (sato_state : Sato_state.t)
  (symb_interp_state : Dbmc.Types.State.t)
  (interp_session : Dbmc.Interpreter.session) 
  (final_env : Dbmc.Interpreter.denv)
  : Ast.clause * Error.Odefa_error.t list =
  let abort_var = symb_interp_state.target in
  let ab_mapping = sato_state.abort_mapping in
  let on_to_odefa_maps = sato_state.on_to_odefa_maps in
  let abort_cond_var = get_abort_cond_clause_id ab_mapping abort_var in
  let Clause (_, cls) as error_loc = 
    get_pre_inst_equivalent_clause on_to_odefa_maps abort_cond_var 
  in
  let alias_graph = interp_session.alias_graph in
  let rec find_alias acc x_with_stk = 
    if Hash_set.mem acc x_with_stk then acc
    else
      let () = Hash_set.add acc x_with_stk in
      if (Interpreter.G.mem_vertex alias_graph x_with_stk) then
        let (succs : Interpreter.Ident_with_stack.t list) = Interpreter.G.succ alias_graph x_with_stk in
        let (aliases : Interpreter.Ident_with_stack.t Hash_set.t) = 
          List.fold succs ~init:acc ~f:(fun acc next -> Hash_set.union acc (find_alias acc next))
        in
        aliases
      else
        (Hash_set.add acc x_with_stk; acc)
  in
  let rec find_source_cls cls_mapping xs =
    match xs with
    | [] -> failwith "Should have found a value definition clause!"
    | hd :: tl ->
      let () = print_endline @@ Interpreter.show_ident_with_stack hd in
      let found = Hashtbl.find cls_mapping hd in
      match found with
      | Some cls -> cls
      | None -> find_source_cls cls_mapping tl
  in
  let mk_match_err expected_type actual_val x x_stk = 
    match expected_type, actual_val with
    | Int_type, Value_int _| Bool_type, Value_bool _ -> []
    | _ -> 
      let match_aliases_raw =
        find_alias (Hash_set.create(module Interpreter.Ident_with_stack)) (x, x_stk)
        |> Hash_set.to_list
      in
      let match_val_source = 
        find_source_cls interp_session.val_def_map match_aliases_raw 
      in
      let match_aliases = 
        match_aliases_raw
        |> List.map ~f:(fun (x, _) -> x)
      in
      let actual_type = get_value_type actual_val in
      let match_error = Error.Odefa_error.Error_match {
        err_match_aliases = match_aliases;
        err_match_val = match_val_source;
        err_match_expected = expected_type;
        err_match_actual = actual_type;
      }
      in
      [match_error]
  in
  let mk_value_error x x_stk = 
      let value_aliases_raw =
        find_alias (Hash_set.create(module Interpreter.Ident_with_stack)) (x, x_stk)
        |> Hash_set.to_list
      in
      let val_source = 
        find_source_cls interp_session.val_def_map value_aliases_raw 
      in
      let value_aliases = 
        value_aliases_raw
        |> List.map ~f:(fun (x, _) -> x)
      in
      let value_error = Error.Odefa_error.Error_value {
        err_value_aliases = value_aliases;
        err_value_val = val_source;
      }
      in
      [value_error]
  in
  let error_list = 
    match cls with
    (* If the point of error is a binary operation, we know that one of
      the two operands must have taken the wrong type.
    *)
    | Binary_operation_body (Var (x1, _), _, Var (x2, _)) ->
      let expected_type = 
        get_expected_type_from_cls cls
      in
      let (x1_val, x1_stk), (x2_val, x2_stk) = 
        let (dv1, stk1) = Ident_map.find x1 final_env in
        let (dv2, stk2) = Ident_map.find x2 final_env in
        let v1, v2 = 
          Interpreter.value_of_dvalue dv1,
          Interpreter.value_of_dvalue dv2
        in 
        (v1, stk1), (v2, stk2)
      in
      let left_error = mk_match_err expected_type x1_val x1 x1_stk in
      let right_error = mk_match_err expected_type x2_val x2 x2_stk in
      let errors = List.append left_error right_error in
      errors
    (* The following operations are all potential candidates where type errors
        can occur. *)
    | Not_body (Var (x, _)) | Appl_body (Var (x, _), _) 
    | Projection_body (Var (x, _), _) 
    | Conditional_body (Var (x, _), _, _) ->
      let expected_type = get_expected_type_from_cls cls in
      let (x_val, x_stk) = 
        let (dv, stk) = Ident_map.find x final_env in
        let v = 
          Interpreter.value_of_dvalue dv
        in 
        (v, stk)
      in
      let error = mk_match_err expected_type x_val x x_stk in
      (* If the error list is empty, that means it's an error where
        the condition variable leads to an abort in one of the
        branches. *)
      if List.is_empty error then
        let val_error = mk_value_error x x_stk in
        val_error
      else
        error
    | _ -> []
  in
  (error_loc, error_list)
;;

(* **** Type Errors **** *)

module Odefa_type_errors : Sato_Result = struct

  type error_record = {
    err_errors : Error.Odefa_error.t list;
    err_input_seq : int option list;
    err_location : Odefa_error_location.t;
  }
  [@@ deriving to_yojson]
  ;;

  type t = error_record option
  ;;

  let description = "error";;

  let get_result
    (sato_state : Sato_state.t)
    (symb_interp_state : Dbmc.Types.State.t)
    (interp_session : Dbmc.Interpreter.session) 
    (final_env : Dbmc.Interpreter.denv)
    (inputs : int option list) = 
    let (error_loc, odefa_errors) = 
      get_odefa_errors sato_state symb_interp_state interp_session final_env 
    in
    let on_to_odefa_maps = sato_state.on_to_odefa_maps in
    let rm_inst_fn =
      Odefa_natural.On_error.odefa_error_remove_instrument_vars on_to_odefa_maps
    in
    Some {
      err_input_seq = inputs;
      err_location = error_loc;
      err_errors = List.map ~f:rm_inst_fn odefa_errors;
    }
  ;;

  (* TODO: Pretty-print *)

  let show : t -> string = function
    | Some error ->
      "** Odefa Type Errors **\n" ^
      (Printf.sprintf "- Input sequence  : %s\n" (Dbmc.Std.string_of_inputs error.err_input_seq)) ^
      (Printf.sprintf "- Found at clause : %s\n" (Odefa_error_location.show error.err_location)) ^
      (* (Printf.sprintf "- Found in steps  : %s\n" (string_of_int error.err_steps)) ^ *)
      "--------------------\n" ^
      (String.concat ~sep:"\n--------------------\n"
        @@ List.map ~f:Error.Odefa_error.show error.err_errors)
    | None -> ""
  ;;

  let show_compact : t -> string = function
    | Some error ->
      "- err at: " ^ (Odefa_error_location.show_brief error.err_location)
    | None ->
      "- no errs"
  ;;

  let count : t -> int = function
    | Some err -> List.length err.err_errors
    | None -> 0;;

  let generation_successful : t -> bool = Option.is_some;;

  let to_yojson : t -> Yojson.Safe.t = function
    | Some err -> error_record_to_yojson err
    | None -> `Null
  ;;
end;;

module Natodefa_type_errors : Sato_Result = struct

  type error_record = {
    err_errors : Odefa_natural.On_error.On_error.t list;
    err_input_seq : int option list;
    err_location : Natodefa_error_location.t;
  }
  [@@ deriving to_yojson]
  ;;

  type t = error_record option;;

  let description = "natodefa type error";;

  let get_result 
    (sato_state : Sato_state.t)
    (symb_interp_state : Dbmc.Types.State.t)
    (interp_session : Dbmc.Interpreter.session) 
    (final_env : Dbmc.Interpreter.denv)
    (inputs : int option list) = 
    let open Odefa_natural in
    let ((Clause (Var (err_id, _), _) as error_loc), odefa_errors) = 
      get_odefa_errors sato_state symb_interp_state interp_session final_env 
    in
    let on_to_odefa_maps = sato_state.on_to_odefa_maps in
    let on_err_loc_core =
      err_id
      |> On_to_odefa_maps.get_natodefa_equivalent_expr on_to_odefa_maps 
    in
    let on_err_list =
      let mapper = 
        (On_error.odefa_to_natodefa_error on_to_odefa_maps) 
      in 
      List.map ~f:mapper odefa_errors
    in
    Some {
      err_input_seq = inputs;
      err_location = on_err_loc_core;
      err_errors = on_err_list;
    }
  ;;

  (* Reporting Natodefa errors. *)
  (* let answer_from_result steps e x result =
    match (!odefa_on_maps_option_ref, !ton_on_maps_option_ref) with
    | (Some odefa_on_maps, Some ton_on_maps) ->
      begin
        let (input_seq, error_opt) =
          Generator_utils.input_sequence_from_result e x result
        in
        match error_opt with
        | Some (error_loc, error_lst, solution) ->
          (* TODO (Earl): This probably should be the place to trace all the way
              back to the original, user-written Natodefa code.
              The current issue with how mappings are kept is that the abort vars
              are bound to the "assert false" statements directly. 
              Need to find a way to chain up the assert false to the original point of
              error.
          *)
          (* Getting the original syntactic natodefa expression *)
          let on_err_loc_core =
            error_loc
            |> On_to_odefa_maps.get_natodefa_equivalent_expr odefa_on_maps 
          in
          (* let on_err_sem = 
            on_err_loc_core
            |> Ton_to_on_maps.sem_natodefa_from_on_err ton_on_maps 
          in *)
          let on_err_loc_nat = 
            on_err_loc_core
            |> Ton_to_on_maps.get_syn_nat_equivalent_expr ton_on_maps
          in
          (* If type error, we need to find the corresponding point of error.
          *)
          let is_type_error = 
            (* false *)
            match on_err_loc_nat.body with
            | LetWithType _ | LetFunWithType _ | LetRecFunWithType _ -> true
            | _ -> false
          in
          (* This helper function is called in the case of a type error.
             When given an odefa_err, it expects an Error_value. Through
             the aliases, it will retrieve error variable in syn nat. 
           *)
          let find_err_ident odefa_err = 
            match odefa_err with
            | Error.Odefa_error.Error_binop _
            | Error.Odefa_error.Error_match _ ->
              failwith "This shouldn't happen!"
            | Error.Odefa_error.Error_value err ->
              let odefa_symbols = err.err_value_aliases in
              (* let () = print_endline "----------" in
              let () =
                List.iter 
                (fun s -> print_endline @@ Interpreter_types.show_symbol s) 
                odefa_symbols 
              in
              let () = print_endline "----------" in *)
              let odefa_aliases = 
                odefa_symbols
                |> List.map (fun (Interpreter_types.Symbol (x, _)) -> x)
                |> List.unique
              in
              let sem_nat_aliases = 
                odefa_aliases
                |> (On_to_odefa_maps.odefa_to_on_aliases odefa_on_maps)
                |> List.map (Ton_to_on_maps.sem_natodefa_from_core_natodefa ton_on_maps)
              in
              let sem_val_exprs = 
                sem_nat_aliases
                |> List.filter_map @@ Ton_to_on_maps.get_value_expr_from_sem_expr ton_on_maps
              in
              let odefa_vars = 
                sem_val_exprs
                |> List.filter_map @@ Ton_to_on_maps.get_core_expr_from_sem_expr ton_on_maps
                |> List.filter_map @@ On_to_odefa_maps.get_odefa_var_opt_from_natodefa_expr odefa_on_maps
              in
              (* let () =
                List.iter (fun v-> print_endline @@ show_var v) odefa_vars
              in  *)
              (* let () =
                List.iter (fun ed -> print_endline @@ On_to_odefa.show_expr_desc ed) sem_nat_aliases
              in *)
              (* let core_eds = 
                Ton_to_on_maps.get_core_match_expr_from_err_ident ton_on_maps sem_nat_aliases
              in *)
              (* let () = print_endline @@ string_of_bool @@ List.is_empty core_eds in
              let () =
                List.iter (fun ed -> print_endline @@ On_to_odefa.show_expr_desc ed) core_eds
              in *)
              (* let odefa_subj_var = 
                List.map
                (On_to_odefa_maps.get_odefa_subj_var_from_natodefa_expr odefa_on_maps)
                core_eds
              in *)
              (* let () = print_endline @@ "Is odefa_subj_var empty?" in
              let () = print_endline @@ string_of_bool @@ List.is_empty odefa_subj_var in
              let () =
                List.iter (fun v-> print_endline @@ show_var v) odefa_subj_var
              in *)
              let relstacks = 
                odefa_symbols 
                |> List.map (fun (Interpreter_types.Symbol (_, relstack)) -> relstack)
              in
              let res = 
                (* odefa_subj_var *)
                odefa_vars
                |> List.map (fun (Var (x, _)) -> x)
                |> List.map 
                  (fun x -> 
                    List.map (fun relstack -> Interpreter_types.Symbol (x, relstack))
                    relstacks
                  )
                |> List.concat
              in
              (* res *)
              (* |> List.filter_map  *)
                (* (Generator_utils.answer_from_solution solution x result) *)
              (* let () = print_endline @@ "???" in
              let () = print_endline @@ string_of_bool @@ List.is_empty odefa_subj_var in *)
              (* let () = print_endline @@ Ast_pp.show_var @@ List.hd odefa_subj_var in *)
              res
          in
          let err_vals_lst_opt = 
            List.map 
              (fun err -> 
                if is_type_error 
                then
                  let additional_queries = 
                    err
                    |> find_err_ident
                    |> List.filter_map (Generator_utils.answer_from_solution solution)
                  in
                  (err, Some (on_err_loc_nat, additional_queries))
                else
                  (err, None))
            error_lst
          in
          let on_err_list =
            let mapper = 
              (On_error.odefa_to_natodefa_error odefa_on_maps ton_on_maps) 
            in 
            List.map mapper err_vals_lst_opt
          in
          Some {
            err_input_seq = input_seq;
            err_location = on_err_loc_nat;
            err_errors = on_err_list;
            err_steps = steps;
          }
        | None -> None
      end
    | None, _ -> failwith "Odefa/natodefa maps were not set!"
    | _, None -> failwith "typed natodefa/natodefa maps were not set!"
  ;; *)

  let show : t -> string = function
    | Some error ->
      "** NatOdefa Type Errors **\n" ^
      (Printf.sprintf "- Input sequence  : %s\n" (Dbmc.Std.string_of_inputs error.err_input_seq)) ^
      (Printf.sprintf "- Found at clause : %s\n" (Natodefa_error_location.show error.err_location)) ^
      "--------------------\n" ^
      (String.concat ~sep:"\n--------------------\n"
        @@ List.map ~f:Odefa_natural.On_error.On_error.show error.err_errors)
    | None -> ""
  ;;

  let show_compact : t -> string = function
    | Some error ->
      "- err at: " ^ (Natodefa_error_location.show_brief error.err_location) 
    | None ->
      "- no errs"
  ;;

  let count : t -> int = function
    | Some err -> List.length err.err_errors
    | None -> 0
  ;;

  let generation_successful : t -> bool = Option.is_some;;

  let to_yojson : t -> Yojson.Safe.t = function
    | Some err -> error_record_to_yojson err
    | None -> `Null
  ;;
end;;