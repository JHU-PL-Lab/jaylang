open Batteries;;
open Odefa_ast;;

open Ast;;
open Interpreter_types;;
open Sat_types;;

(* NOTE: this implementation makes use of mutation, but the interface it
   provides to the rest of the system should be immutable; the mutation here is
   only for performance. *)

(** The type of symbols in a collection of formulae.  This is used to define
    symbols for the solver and also to detect errors outside of the solver when
    possible.  Note that these types include an intersection operation below. *)
type symbol_type =
  | IntSymbol
  | BoolSymbol
  | TrueSymbol
  | FalseSymbol
  | FunctionSymbol of function_value
[@@deriving eq]
;;

let intersect_symbol_types (t1 : symbol_type) (t2 : symbol_type)
  : symbol_type option =
  match t1,t2 with
  | IntSymbol,IntSymbol -> Some IntSymbol
  | IntSymbol,(BoolSymbol|TrueSymbol|FalseSymbol|FunctionSymbol _) -> None
  | BoolSymbol,BoolSymbol -> Some BoolSymbol
  | BoolSymbol,(TrueSymbol|FalseSymbol) -> Some t2
  | BoolSymbol,(IntSymbol|FunctionSymbol _) -> None
  | TrueSymbol,(TrueSymbol|BoolSymbol) -> Some TrueSymbol
  | TrueSymbol,(IntSymbol|FalseSymbol|FunctionSymbol _) -> None
  | FalseSymbol,(FalseSymbol|BoolSymbol) -> Some FalseSymbol
  | FalseSymbol,(IntSymbol|TrueSymbol|FunctionSymbol _) -> None
  | FunctionSymbol fv1,FunctionSymbol fv2 ->
    if equal_function_value fv1 fv2 then Some t1 else None
  | FunctionSymbol _,(IntSymbol|BoolSymbol|TrueSymbol|FalseSymbol) -> None
;;

exception SymbolTypeContradiction of
    symbol * symbol_type * symbol_type
;;

let force_intersect_symbol_types
    (s : symbol) (t1 : symbol_type) (t2 : symbol_type)
  : symbol_type =
  match intersect_symbol_types t1 t2 with
  | None -> raise @@ SymbolTypeContradiction(s,t1,t2)
  | Some t -> t
;;

(** Describes the type inference state of a symbol in a collection.  The
    InferredType state indicates that all values assigned to this symbol in any
    solution to the associated formulae will have the specified type.  The
    inferred alias state indicates that the type of this symbol's solution
    values will be the same as another symbol's. *)
type inference_state =
  | InferredType of symbol_type
  | InferredAlias of symbol
;;

type t =
  { formulae : Formula_set.t;
    mutable inference_map : inference_state Symbol_map.t
  }
;;

let _get_state (symbol : Symbol.t) (collection : t) : inference_state option =
  Symbol_map.Exceptionless.find symbol collection.inference_map
;;

let _set_state
    (symbol : Symbol.t) (state : inference_state) (collection : t)
  : unit =
  collection.inference_map <-
    Symbol_map.add symbol state collection.inference_map
;;

(** Determines the input types and output type of a binary operator. *)
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

(** Flattens symbols during inference.  This procedure starts at a symbol and
    follows its chain of indirections until reaching either an alias or a
    type.  It then remaps each variable in the chain to the end of that
    chain.  Upon completion, the variable either refers to a concrete type
    (if possible), nothing (if it wasn't in the map to begin with), or an
    alias (but only if the variable to which it is aliased does not exist). *)
let _flatten (symbol : Symbol.t) (collection : t) : unit =
  let rec follow (s : Symbol.t) : inference_state option =
    let my_state = _get_state s collection in
    match my_state with
    | Some (InferredAlias s') ->
      let their_state = follow s' in
      begin
        match their_state with
        | None -> my_state
        | Some state -> _set_state s state collection; Some state
      end
    | None
    | Some (InferredType _) -> my_state
  in
  ignore @@ follow symbol
;;

(** Unifies two symbols during inference.  Either both symbols will have the
    same inference state or one will point directly to the other.  If an error
    occurs (e.g. int = s1 = s2 = bool), a FormulaTypeError is raised. *)
let _unify (symbol1 : Symbol.t) (symbol2 : Symbol.t) (collection : t) : unit =
  _flatten symbol1 collection;
  _flatten symbol2 collection;
  let state1o = _get_state symbol1 collection in
  let state2o = _get_state symbol2 collection in
  match state1o, state2o with
  | None, _ ->
    _set_state symbol1 (InferredAlias symbol2) collection
  | Some _, None ->
    _set_state symbol2 (InferredAlias symbol1) collection
  | Some (InferredType t1), Some (InferredType t2) ->
    (* Refine the symbol types to map to the same type. *)
    if equal_symbol_type t1 t2 then () else begin
      let t' = force_intersect_symbol_types symbol1 t1 t2 in
      _set_state symbol1 (InferredType t') collection;
      _set_state symbol2 (InferredType t') collection;
    end
  | Some (InferredType _), Some (InferredAlias symbol2') ->
    (* Make symbol2', which can't exist (due to flattening), point to
       symbol1. *)
    _set_state symbol2' (InferredAlias symbol1) collection
  | Some (InferredAlias symbol1'), Some (InferredType _) ->
    (* Make symbol1', which can't exist (due to flattening), point to
       symbol2. *)
    _set_state symbol1' (InferredAlias symbol2) collection
  | Some (InferredAlias symbol1'), Some (InferredAlias symbol2') ->
    (* Due to flattening, neither symbol1' nor symbol2' can exist.  Make one
       point to the other. *)
    _set_state symbol1' (InferredAlias symbol2') collection
;;

(** Constrains a symbol to have a particular type.  If that symbol is already
    in an inference state, this routine unifies those results. *)
let _assign (symbol : Symbol.t) (t : symbol_type) (collection : t) : unit =
  _flatten symbol collection;
  match _get_state symbol collection with
  | None -> _set_state symbol (InferredType t) collection
  | Some (InferredType t') ->
    if equal_symbol_type t t' then () else begin
      let t'' = force_intersect_symbol_types symbol t t' in
      _set_state symbol (InferredType t'') collection
    end
  | Some (InferredAlias symbol') ->
    _set_state symbol' (InferredType t) collection;
    _set_state symbol (InferredType t) collection;
;;

(* ************ Interface Functions ************ *)

let empty : t =
  { formulae = Formula_set.empty;
    inference_map = Symbol_map.empty;
  }
;;

let add (formula : formula) (collection : t) : t =
  (* Derive a set containing the new formula. *)
  let formulae' = Formula_set.add formula collection.formulae in
  let collection' = { collection with formulae = formulae' } in
  (* Update that set to infer type information. *)
  let Formula(symbol, expr) = formula in
  begin
    match expr with
    | Formula_expression_value v ->
      let typ =
        match v with
        | Value_function f -> FunctionSymbol f
        | Value_int _ -> IntSymbol
        | Value_bool b -> if b then TrueSymbol else FalseSymbol
      in
      _assign symbol typ collection';
    | Formula_expression_alias symbol' ->
      _unify symbol symbol' collection'
    | Formula_expression_binop (symbol1, op, symbol2) ->
      let (tin1,tin2,tout) = _infer_binop_signature op in
      _assign symbol1 tin1 collection';
      _assign symbol2 tin2 collection';
      _assign symbol tout collection';
  end;
  collection'
;;

let singleton (formula : formula) : t =
  add formula empty
;;

let union (collection1 : t) (collection2 : t) : t =
  let (smaller, larger) =
    if Formula_set.cardinal collection1.formulae <
       Formula_set.cardinal collection2.formulae then
      (collection1, collection2)
    else
      (collection2, collection1)
  in
  smaller.formulae
  |> Formula_set.enum
  |> Enum.fold
    (fun acc formula -> add formula acc)
    larger
;;

let enum (collection : t) : formula Enum.t =
  Formula_set.enum collection.formulae
;;

let of_enum (enum : formula Enum.t) : t =
  enum
  |> Enum.fold
    (fun a e -> add e a)
    empty
;;

let iter (fn : formula -> unit) (collection : t) : unit =
  Enum.iter fn @@ enum collection
;;

let type_of (symbol : symbol) (collection : t) : symbol_type option =
  _flatten symbol collection;
  match _get_state symbol collection with
  | None -> raise Not_found
  | Some(InferredType t) -> Some t
  | Some(InferredAlias _) -> None
;;
