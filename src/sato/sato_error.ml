open Core
open Jhupllib

open Odefa_ast
open Odefa_natural
open Odefa_instrumentation

exception Parse_failure of string

module type Error_ident = sig
  type t

  val equal : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

module type Error_value = sig
  type t

  val equal : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

module type Error_binop = sig
  type t

  val equal : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

module type Error_type = sig
  type t

  val equal : t -> t -> bool
  val subtype : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

let replace_linebreaks (str : string) : string =
  String.tr ~target:'\n' ~replacement:' ' str 

module type Error = sig
  module Error_ident : Error_ident
  module Error_value : Error_value
  module Error_binop : Error_binop
  module Error_type : Error_type

  type ident
  type value
  type binop
  type type_sig

  type error_binop = {
    err_binop_left_aliases : ident list;
    err_binop_right_aliases : ident list;
    err_binop_left_val : value;
    err_binop_right_val : value;
    err_binop_operation : binop;
  }

  type error_match = {
    err_match_aliases : ident list;
    err_match_val : value;
    err_match_expected : type_sig;
    err_match_actual : type_sig;
  }

  type error_value = { err_value_aliases : ident list; err_value_val : value }

  type t =
    | Error_binop of error_binop
    | Error_match of error_match
    | Error_value of error_value

  val equal : t -> t -> bool
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

module Make
    (Ident : Error_ident)
    (Value : Error_value)
    (Binop : Error_binop)
    (Type : Error_type) :
  Error
    with type ident := Ident.t
     and type value := Value.t
     and type binop := Binop.t
     and type type_sig := Type.t = struct
  module Error_ident = Ident
  module Error_value = Value
  module Error_binop = Binop
  module Error_type = Type

  type error_binop = {
    err_binop_left_aliases : Ident.t list;
    err_binop_right_aliases : Ident.t list;
    err_binop_left_val : Value.t;
    err_binop_right_val : Value.t;
    err_binop_operation : Binop.t;
  }
  [@@deriving equal, to_yojson]

  type error_match = {
    err_match_aliases : Ident.t list;
    err_match_val : Value.t;
    err_match_expected : Type.t;
    err_match_actual : Type.t;
  }
  [@@deriving equal, to_yojson]

  type error_value = {
    err_value_aliases : Ident.t list;
    err_value_val : Value.t;
  }
  [@@deriving equal, to_yojson]

  type t =
    | Error_binop of error_binop
    | Error_match of error_match
    | Error_value of error_value
  [@@deriving equal, to_yojson]

  let equal = equal

  let pp_alias_list formatter aliases =
    Pp_utils.pp_concat_sep " ="
      (fun formatter x -> Ident.pp formatter x)
      formatter (Batteries.List.enum aliases)

  let pp_error_binop formatter err =
    let pp_left_value formatter err =
      let l_aliases = err.err_binop_left_aliases in
      let l_value = err.err_binop_left_val in
      if List.length l_aliases > 0
      then
        Format.fprintf formatter "@[* Left Val   : @[%a@ =@ %a@]@]@,"
          pp_alias_list l_aliases Value.pp l_value
      else Format.pp_print_string formatter ""
    in
    let pp_right_value formatter err =
      let r_aliases = err.err_binop_right_aliases in
      let r_value = err.err_binop_right_val in
      if List.length r_aliases > 0
      then
        Format.fprintf formatter "@[* Right Val  : @[%a@ =@ %a@]@]@,"
          pp_alias_list r_aliases Value.pp r_value
      else Format.pp_print_string formatter ""
    in
    let pp_constraint formatter err =
      Format.fprintf formatter "@[* Constraint : @[%a@]@]" Binop.pp
        err.err_binop_operation
    in
    Format.fprintf formatter "@[<v 0>%a%a%a@]" pp_left_value err pp_right_value
      err pp_constraint err

  let pp_error_match formatter err =
    let pp_value formatter err =
      let aliases = err.err_match_aliases in
      let value = err.err_match_val in
      if not @@ List.is_empty aliases
      then
        Format.fprintf formatter "@[* Value    : @[%a@ =@ %a@]@]@,"
          pp_alias_list aliases Value.pp value
      else Format.fprintf formatter "@[* Value    : @[%a@]@]@," Value.pp value
    in
    let pp_expected formatter err =
      Format.fprintf formatter "@[* Expected : @[%a@]@]@," Type.pp
        err.err_match_expected
    in
    let pp_actual formatter err =
      Format.fprintf formatter "@[* Actual   : @[%a@]@]" Type.pp
        err.err_match_actual
    in
    Format.fprintf formatter "@[<v 0>%a%a%a@]" pp_value err pp_expected err
      pp_actual err

  let pp_error_value formatter err =
    let pp_value formatter err =
      let aliases = err.err_value_aliases in
      let value = err.err_value_val in
      if not @@ List.is_empty aliases
      then
        Format.fprintf formatter "@[* Value : @[%a@ =@ %a@]@]" pp_alias_list
          aliases Value.pp value
      else Format.fprintf formatter "@[* Value : @[%a@]@]" Value.pp value
    in
    Format.fprintf formatter "@[<v 0>%a@]" pp_value err

  let pp formatter error =
    match error with
    | Error_binop err -> pp_error_binop formatter err
    | Error_match err -> pp_error_match formatter err
    | Error_value err -> pp_error_value formatter err

  let show = Pp_utils.pp_to_string pp
end

(* **** Odefa modules **** *)

module Odefa_ident : Error_ident with type t = Ast.ident = struct
  type t = Ast.ident

  let equal = Ast.equal_ident
  let pp = Ast_pp.pp_ident
  let show = Ast_pp.show_ident
  let to_yojson ident = `String (replace_linebreaks @@ show ident)
end

module Odefa_value : Error_value with type t = Ast.clause_body = struct
  type t = Ast.clause_body

  let equal = Ast.equal_clause_body

  (* We use the brief version in order to avoid printing out function
     bodies (which may have clauses added during instrumentation. *)
  let pp = Ast_pp_brief.pp_clause_body
  let show = Ast_pp_brief.show_clause_body
  let to_yojson value = `String (replace_linebreaks @@ show value)
end

module Odefa_binop :
  Error_binop
    with type t = Ast.clause_body * Ast.binary_operator * Ast.clause_body =
struct
  type t = Ast.clause_body * Ast.binary_operator * Ast.clause_body
  [@@deriving equal, to_yojson]

  let equal = equal

  let pp formatter (binop : t) =
    let left, op, right = binop in
    Format.fprintf formatter "%a %a %a" Ast_pp.pp_clause_body left
      Ast_pp.pp_binary_operator op Ast_pp.pp_clause_body right

  let show binop = Pp_utils.pp_to_string pp binop
  let to_yojson binop = `String (replace_linebreaks @@ show binop)
end

module Odefa_type : Error_type with type t = Ast.type_sig = struct
  type t = Ast.type_sig

  let equal = Ast.equal_type_sig
  let subtype = Ast.Type_signature.subtype
  let pp = Ast_pp.pp_type_sig
  let show = Ast_pp.show_type_sig
  let to_yojson typ = `String (Ast_pp.show_type_sig typ)
end

module Odefa_error = Make (Odefa_ident) (Odefa_value) (Odefa_binop) (Odefa_type)

(* **** Natodefa modules **** *)

module Natodefa_ident : (Error_ident with type t = On_ast.ident) = struct
  type t = On_ast.ident;;
  let equal = On_ast.equal_ident;;
  let pp = On_ast_pp.pp_ident;;
  let show = On_ast_pp.show_ident;;
  let to_yojson ident =
    `String (replace_linebreaks @@ show ident);;
end;;

module Natodefa_value : (Error_value with type t = On_ast.expr) = struct
  type t = On_ast.expr;;
  let equal = On_ast.equal_expr;;
  let pp = On_ast_pp.pp_expr;;
  let show = On_ast_pp.show_expr;;
  let to_yojson value =
    `String (replace_linebreaks @@ show value);;
end;;

module Natodefa_binop : (Error_binop with type t = On_ast.expr) = struct
  type t = On_ast.expr;;
  let equal = On_ast.equal_expr;;
  let pp = On_ast_pp.pp_expr;;
  let show = On_ast_pp.show_expr;;
  let to_yojson binop =
    `String (replace_linebreaks @@ show binop);;
end;;

module Natodefa_type : (Error_type with type t = On_ast.type_sig) = struct
  type t = On_ast.type_sig;;
  let equal = On_ast.equal_type_sig;;
  let subtype _ _ = false;;
  let pp = On_ast_pp.pp_on_type;;
  let show = On_ast_pp.show_on_type;;
  let to_yojson typ =
    `String (replace_linebreaks @@ show typ);;
end;;

module On_error = Make(Natodefa_ident)(Natodefa_value)(Natodefa_binop)(Natodefa_type);;

(* **** Odefa error cleanup **** *)

let odefa_error_remove_instrument_vars
    (odefa_on_maps : Odefa_instrumentation_maps.t)
    (error : Odefa_error.t)
  : Odefa_error.t =
  let remove_instrument_aliases aliases =
    List.filter
      ~f:(fun alias ->
        not @@ Odefa_instrumentation_maps.is_var_instrumenting odefa_on_maps alias)
      aliases
  in
  match error with
  | Error_binop err ->
    begin
      let left_aliases' =
        remove_instrument_aliases err.err_binop_left_aliases
      in
      let right_aliases' =
        remove_instrument_aliases err.err_binop_right_aliases
      in
      let (_, op, _) =
        err.err_binop_operation
      in
      let l_operand' =
        match left_aliases' with
        | [] -> err.err_binop_left_val
        | v :: _ -> Ast.Var_body (Var (v, None))
      in
      let r_operand' =
        match right_aliases' with
        | [] -> err.err_binop_right_val
        | v :: _ -> Ast.Var_body (Var (v, None))
      in
      Error_binop {
        err with
        err_binop_left_aliases = left_aliases';
        err_binop_right_aliases = right_aliases';
        err_binop_operation = (l_operand', op, r_operand')
      }
    end
  | Error_match err ->
    begin
      let match_aliases = err.err_match_aliases in
      Error_match {
        err with
        err_match_aliases = remove_instrument_aliases match_aliases;
      }
    end
  | Error_value err ->
    begin
      let aliases = err.err_value_aliases in
      Error_value {
        err with
        err_value_aliases = remove_instrument_aliases aliases;
      }
    end
;;

(* **** Odefa to natodefa error translation **** *)

(* Helper function that returns a natodefa binop, depending on the odefa
   binary operator. *)
let odefa_to_on_binop (odefa_binop : Ast.binary_operator) :
    On_ast.expr_desc -> On_ast.expr_desc -> On_ast.expr =
  match odefa_binop with
  | Ast.Binary_operator_plus -> (fun e1 e2 -> On_ast.Plus (e1, e2))
  | Ast.Binary_operator_minus -> (fun e1 e2 -> On_ast.Minus (e1, e2))
  | Ast.Binary_operator_times -> (fun e1 e2 -> On_ast.Times (e1, e2))
  | Ast.Binary_operator_divide -> (fun e1 e2 -> On_ast.Divide (e1, e2))
  | Ast.Binary_operator_modulus -> (fun e1 e2 -> On_ast.Modulus (e1, e2))
  | Ast.Binary_operator_equal_to -> (fun e1 e2 -> On_ast.Equal (e1, e2))
  | Ast.Binary_operator_not_equal_to -> (fun e1 e2 -> On_ast.Neq (e1, e2))
  | Ast.Binary_operator_less_than -> (fun e1 e2 -> On_ast.LessThan (e1, e2))
  | Ast.Binary_operator_less_than_or_equal_to -> (fun e1 e2 -> On_ast.Leq (e1, e2))
  | Ast.Binary_operator_and -> (fun e1 e2 -> On_ast.And (e1, e2))
  | Ast.Binary_operator_or -> (fun e1 e2 -> On_ast.Or (e1, e2))
;;

let odefa_to_natodefa_error 
    (odefa_inst_maps : Odefa_instrumentation_maps.t)
    (odefa_on_maps : On_to_odefa_maps.t)
    (interp_session : Dbmc.Interpreter.session) 
    (final_env : Dbmc.Interpreter.denv)
    (odefa_err : Odefa_error.t)
    : On_error.t list =
  (* Helper functions *)
  let open On_ast in
  let get_pre_inst_id x =
    Odefa_instrumentation_maps.get_pre_inst_var_opt odefa_inst_maps x
    |> Option.value ~default:x
  in 
  let odefa_to_on_expr =
    On_to_odefa_maps.get_natodefa_equivalent_expr odefa_on_maps
  in
  let odefa_to_on_aliases aliases =
    aliases
    |> List.filter_map
      ~f:(fun alias ->
        let alias' = get_pre_inst_id alias in
        let e_desc = odefa_to_on_expr alias' in
        match (e_desc.body) with
        | (Var _) | Error _ -> Some e_desc
        | _ -> None
      )
    |> Batteries.List.unique
  in
  let get_idents_from_aliases (aliases : expr_desc list) =
    aliases
    |> List.filter_map 
    ~f:(fun ed -> 
      match ed.body with
      | Var x -> Some x
      | _ -> None
    ) 
  in
  let odefa_to_on_value (aliases : Ast.ident list) : expr_desc =
    let last_var =
      try
        List.last_exn aliases
      with Invalid_argument _ ->
        raise @@ Jhupllib.Utils.Invariant_failure "Can't have empty alias list!"
    in
    odefa_to_on_expr @@ get_pre_inst_id last_var
  in
  let odefa_to_on_type (typ : Ast.type_sig) : On_ast.type_sig =
    match typ with
    | Ast.Top_type -> TopType
    | Ast.Int_type -> IntType
    | Ast.Bool_type -> BoolType
    | Ast.Fun_type -> FunType
    | Ast.Rec_type lbls ->
      On_to_odefa_maps.get_type_from_idents odefa_on_maps lbls
    | Ast.Bottom_type ->
      raise @@ Jhupllib.Utils.Invariant_failure
        (Printf.sprintf "Bottom type not in natodefa")
  in
  (* Odefa to natodefa *)
  match odefa_err with
  | Odefa_error.Error_binop err ->
    begin
      let l_aliases = err.err_binop_left_aliases in
      let r_aliases = err.err_binop_right_aliases in
      let l_aliases_on = odefa_to_on_aliases l_aliases in
      let r_aliases_on = odefa_to_on_aliases r_aliases in
      let (_, op, _) = err.err_binop_operation in
      let l_value = odefa_to_on_value l_aliases in
      let r_value = odefa_to_on_value r_aliases in
      let constraint_expr =
        let left_expr =
          if List.is_empty l_aliases_on then l_value else
            List.hd_exn l_aliases_on
        in
        let right_expr =
          if List.is_empty r_aliases_on then r_value else
            List.hd_exn r_aliases_on
        in
        odefa_to_on_binop op left_expr right_expr
      in
      [ Error_binop {
        err_binop_left_aliases = get_idents_from_aliases l_aliases_on;
        err_binop_right_aliases = get_idents_from_aliases r_aliases_on;
        err_binop_left_val = l_value.body;
        err_binop_right_val = r_value.body;
        err_binop_operation = constraint_expr;
      } ]
    end
  | Odefa_error.Error_match err ->
    begin
      let aliases = err.err_match_aliases in
      (* let () = print_endline "Printing aliases" in
      let () = List.iter (fun a -> print_endline @@ Ast.show_ident a) aliases in
      let () = print_endline @@ show_expr ((odefa_to_on_value aliases).body) in *)
      [ Error_match {
        err_match_aliases = get_idents_from_aliases @@ odefa_to_on_aliases aliases;
        err_match_val = (odefa_to_on_value aliases).body;
        err_match_expected = odefa_to_on_type err.err_match_expected;
        err_match_actual = odefa_to_on_type err.err_match_actual;
      } ]
    end
  | Odefa_error.Error_value err ->
    begin
      let aliases = err.err_value_aliases in
      (* let () = print_endline "Printing aliases" in
      let () = List.iter (fun a -> print_endline @@ Ast.show_ident a) aliases in *)
      let err_val_edesc = odefa_to_on_value aliases in
      match err_val_edesc.body with
      | Match (subj, pat_ed_lst) ->
        begin
        let odefa_var_opt = 
          On_to_odefa_maps.get_odefa_var_opt_from_natodefa_expr odefa_on_maps subj
        in
        match odefa_var_opt with
        | Some (Var (x, _)) ->
          let (dv1, stk) = Ast.Ident_map.find x final_env in
          let v = Dbmc.Interpreter.value_of_dvalue dv1 in
          let alias_graph = interp_session.alias_graph in
          let odefa_aliases_raw = 
            Sato_tools.find_alias alias_graph [] (x, stk) 
          in
          let odefa_aliases = 
            odefa_aliases_raw
            |> List.map ~f:(fun (x, _) -> x)
            |> List.rev
          in
          let actual_type = Sato_tools.get_value_type v in
          let errors = 
            let mapper (pat, _) = 
              let expected_type = 
                Sato_tools.get_expected_type_from_pattern pat 
              in
              On_error.Error_match {
                err_match_aliases = get_idents_from_aliases @@ odefa_to_on_aliases odefa_aliases;
                err_match_val = (odefa_to_on_value odefa_aliases).body;
                err_match_expected = expected_type;
                err_match_actual = odefa_to_on_type actual_type;
              }
            in
            List.map ~f:mapper pat_ed_lst
          in
          errors
        | None -> failwith "Should have found an odefa var!"
        end
      | _ ->
        [ Error_value {
          err_value_aliases = get_idents_from_aliases @@ odefa_to_on_aliases aliases;
          err_value_val = err_val_edesc.body;
        } ]
    end
;;