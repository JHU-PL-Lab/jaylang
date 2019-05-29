(**
   This module provides a rudimentary mechanism for typechecking formulae for
   the satisfiability engine.
*)

open Batteries;;
open Odefa_ast;;

open Ast;;
open Interpreter_types;;
open Sat_types;;

type symbol_type =
  | IntSymbol
  | BoolSymbol
[@@deriving eq]
;;

(** Describes the state of a symbol during type inference.  A given variable may
    be assigned a type or be aliased to the type of another symbol.  An
    inferred type of None indicates that the symbol has a type which cannot be
    represented in the solver (e.g. a function). *)
type inference_state =
  | InferredType of symbol_type option
  | InferredAlias of symbol
;;

exception FormulaTypeError of string * symbol * symbol_type option list;;

let _infer_binop_signature (binop : binary_operator)
  : symbol_type * symbol_type * symbol_type =
  match binop with
  | Binary_operator_plus -> (IntSymbol, IntSymbol, IntSymbol)
  | Binary_operator_minus -> (IntSymbol, IntSymbol, IntSymbol)
  | Binary_operator_less_than -> (IntSymbol, IntSymbol, BoolSymbol)
  | Binary_operator_less_than_or_equal_to -> (IntSymbol, IntSymbol, BoolSymbol)
  | Binary_operator_equal_to -> (IntSymbol, IntSymbol, BoolSymbol)
  | Binary_operator_and -> (BoolSymbol, BoolSymbol, BoolSymbol)
  | Binary_operator_or -> (BoolSymbol, BoolSymbol, BoolSymbol)
  | Binary_operator_xor -> (BoolSymbol, BoolSymbol, BoolSymbol)
;;

let infer_types (formulae : Formula_set.t) : symbol_type Symbol_map.t =
  let inference_state : inference_state Symbol_map.t ref =
    ref Symbol_map.empty
  in
  let get_state (symbol : Symbol.t) : inference_state option =
    Symbol_map.Exceptionless.find symbol !inference_state
  in
  let set_state (symbol : Symbol.t) (state : inference_state) : unit =
    inference_state := Symbol_map.add symbol state !inference_state
  in
  (** Asserts that two types, both given to the same symbol, are equal. *)
  let assert_equal_types
      (symbol : Symbol.t)
      (t1o : symbol_type option)
      (t2o : symbol_type option)
    : unit =
    if Option.eq ~eq:equal_symbol_type t1o t2o then () else begin
      raise @@ FormulaTypeError(
        Printf.sprintf "Multiple inferred types for symbol %s"
          (Symbol.show symbol),
        symbol, [t1o;t2o])
    end
  in
  (** Flattens symbols during inference.  This procedure starts at a symbol and
      follows its chain of indirections until reaching either an alias or a
      type.  It then remaps each variable in the chain to the end of that
      chain.  Upon completion, the variable either refers to a concrete type
      (if possible), nothing (if it wasn't in the map to begin with), or an
      alias (but only if the variable to which it is aliased does not exist). *)
  let flatten (symbol : Symbol.t) : unit =
    let rec follow (s : Symbol.t) : inference_state option =
      let my_state = get_state s in
      match my_state with
      | Some (InferredAlias s') ->
        let their_state = follow s' in
        begin
          match their_state with
          | None -> my_state
          | Some state -> set_state s state; Some state
        end
      | None
      | Some (InferredType _) -> my_state
    in
    ignore @@ follow symbol
  in
  (** Unifies two symbols during inference.  Either both symbols will have the
      same inference state or one will point directly to the other.  If an error
      occurs (e.g. int = s1 = s2 = bool), a FormulaTypeError is raised. *)
  let unify (symbol1 : Symbol.t) (symbol2 : Symbol.t) : unit =
    flatten symbol1;
    flatten symbol2;
    let state1o = get_state symbol1 in
    let state2o = get_state symbol2 in
    match state1o, state2o with
    | None, _ ->
      set_state symbol1 @@ InferredAlias symbol2
    | Some _, None ->
      set_state symbol2 @@ InferredAlias symbol1
    | Some (InferredType t1o), Some (InferredType t2o) ->
      (* Make sure both symbols map to the same type. *)
      assert_equal_types symbol1 t1o t2o
    | Some (InferredType _), Some (InferredAlias symbol2') ->
      (* Make symbol2', which can't exist (due to flattening), point to
         symbol1. *)
      set_state symbol2' @@ InferredAlias symbol1
    | Some (InferredAlias symbol1'), Some (InferredType _) ->
      (* Make symbol1', which can't exist (due to flattening), point to
         symbol2. *)
      set_state symbol1' @@ InferredAlias symbol2
    | Some (InferredAlias symbol1'), Some (InferredAlias symbol2') ->
      (* Due to flattening, neither symbol1' nor symbol2' can exist.  Make one
         point to the other. *)
      set_state symbol1' @@ InferredAlias symbol2'
  in
  (** Constrains a symbol to have a particular type.  If that symbol is already
      in an inference state, this routine unifies those results. *)
  let assign (symbol : Symbol.t) (t : symbol_type option) : unit =
    flatten symbol;
    match get_state symbol with
    | None -> set_state symbol (InferredType t)
    | Some (InferredType t') -> assert_equal_types symbol t t'
    | Some (InferredAlias symbol') ->
      set_state symbol' (InferredType t);
      set_state symbol (InferredType t);
  in
  (* Start by establishing aliasing for every formula. *)
  formulae
  |> Formula_set.iter
    (fun (Formula(symbol, expr)) ->
       match expr with
       | Formula_expression_value v ->
         let typ =
           match v with
           | Value_function _ -> None
           | Value_int _ -> Some IntSymbol
           | Value_bool _ -> Some BoolSymbol
         in
         assign symbol typ
       | Formula_expression_alias symbol' ->
         unify symbol symbol'
       | Formula_expression_binop (symbol1, op, symbol2) ->
         let (tin1,tin2,tout) = _infer_binop_signature op in
         assign symbol1 (Some tin1);
         assign symbol2 (Some tin2);
         assign symbol (Some tout);
    );
  (* Flatten the aliases for all variables. *)
  !inference_state
  |> Symbol_map.keys
  |> Enum.iter flatten;
  (* Read out the result. *)
  !inference_state
  |> Symbol_map.enum
  |> Enum.filter_map
    (fun (symbol, state) ->
       match state with
       | InferredAlias _ ->
         (* It's an alias with no constraints.  Don't bother. *)
         None
       | InferredType None ->
         (* It's a non-solver type.  Don't bother. *)
         None
       | InferredType (Some t) ->
         (* It's actually inferred to have a type.  Return this pairing. *)
         Some (symbol, t)
    )
  |> Symbol_map.of_enum
;;
