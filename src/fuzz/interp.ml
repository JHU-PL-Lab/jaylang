
open Core
open Jayil.Ast (* opens the value types you see here *)

(*
  The thing is, this fuzzer sucks because it doesn't try small values. We should do a few runs
  with a bunch of zeros and ones as inputs because those are likely good to get through
  recursive functions.

  It is only so very slightly faster than the concolic interpreter on easy programs, and it can't
  find the errors on hard programs.

  We can spawn up a bunch of these with parametrized input feeders. So we try several with small
  inputs like (-2, 2), (-10, 10), and so on. And several with nearly unbounded inputs. However,
  I have implemented such a thing yet.
*)

module Exns =
  struct
    exception Reach_max_step
    exception Found_abort (* TODO: add input list payload *)
    exception Type_mismatch (* .. *)
    exception Failed_assume
  end

module rec Dvalue :
  sig
    type t =
      | Direct of value (* record, function, int, or bool *)
      | FunClosure of function_value * Denv.t
      | RecordClosure of record_value * Denv.t

    val value_of_t : t -> value

    val pp : Format.formatter -> t -> unit
  end
  =
  struct
    type t =
      | Direct of value
      | FunClosure of function_value * Denv.t
      | RecordClosure of record_value * Denv.t

    let value_of_t = function
      | Direct v -> v
      | FunClosure (fv, _) -> Value_function fv
      | RecordClosure (r, _) -> Value_record r

    let rec pp oc = function
      | Direct v -> Jayil.Pp.value oc v
      | FunClosure _ -> Format.fprintf oc "(fc)"
      | RecordClosure (r, _) -> pp_record_c r oc

    and pp_record_c (Record_value r) oc =
      let pp_entry oc (x, v) = Fmt.pf oc "%a = %a" Dj_common.Id.pp x Jayil.Pp.var_ v in
      (Fmt.braces (Fmt.iter_bindings ~sep:(Fmt.any ", ") Ident_map.iter pp_entry))
        oc r
  end
and Denv :
  sig
    type t 

    val empty : t

    val add : t -> Ident_new.t -> Dvalue.t -> t

    val fetch : t -> var -> Dvalue.t

    val fetch_to_val : t -> var -> value

    val check_pattern : t -> var -> pattern -> bool
  end
  =
  struct
    type t = Dvalue.t Ident_map.t 

    let empty : t = Ident_map.empty

    let add (env : t) (x : Ident_new.t) (dv : Dvalue.t) : t =
      Ident_map.add x dv env

    let fetch (env : t) (Var (x, _) : var) : Dvalue.t =
      Ident_map.find x env

    let fetch_to_val (env : t) (vx : var) : value =
      match fetch env vx with
      | Direct v -> v
      | _ -> raise Exns.Type_mismatch

    let check_pattern (env : t) (vx : var) (p : pattern) : bool =
      match (fetch env vx, p) with
      | Direct (Value_int _), Int_pattern -> true
      | Direct (Value_bool _), Bool_pattern -> true
      | Direct (Value_function _), _ -> failwith "fun must be a closure"
      | Direct (Value_record _), _ -> failwith "record must be a closure"
      | RecordClosure (Record_value record, _), Rec_pattern label_set ->
          Ident_set.for_all (fun id -> Ident_map.mem id record) label_set
      | RecordClosure (Record_value record, _), Strict_rec_pattern label_set ->
          Ident_set.equal label_set (Ident_set.of_enum @@ Ident_map.keys record)
      | FunClosure _, Fun_pattern -> true
      | _, Any_pattern -> true
      | _, _ -> false
  end

open Dvalue

module Session =
  struct
    type t =
      { input_feeder    : unit -> int
      ; mutable step    : int
      ; max_step        : int }

    let create_default () =
      { input_feeder    = (fun () -> Random.int_incl Int.min_value Int.max_value)
      ; step            = 0
      ; max_step        = Int.(5 * 10 ** 4) }

    let incr_step (x : t) : unit =
      x.step <- x.step + 1

    let is_max_step (x : t) : bool =
      x.step > x.max_step
  end


(*
  ----------
  BEGIN EVAL
  ----------

  This section is basically an interpreter injected with concolic logic.
  It is an evaluation within a single concolic session.
*)

let rec eval_exp
  ~(session : Session.t) (* Note: is mutable *)
  (env : Denv.t)
  (Expr clauses : expr)
  (continue : Denv.t * Dvalue.t -> Denv.t * Dvalue.t)
  : (Denv.t * Dvalue.t)
  =
  match clauses with
  | [] -> failwith "empty clause list" (* empty clause list is parse error, so this is safe *)
  | clause :: [] ->
    eval_clause ~session env clause continue
  | clause :: nonempty_tl ->
    eval_clause ~session env clause (fun (res_env, _) ->
      eval_exp ~session res_env (Expr nonempty_tl) (fun a -> a)
    )

and eval_clause
  ~(session : Session.t)
  (env : Denv.t)
  (clause : clause)
  (continue : Denv.t * Dvalue.t -> Denv.t * Dvalue.t)
  : Denv.t * Dvalue.t
  =
  let Clause (Var (x, _), cbody) = clause in
  Session.incr_step session;
  if Session.is_max_step session
  then raise Exns.Reach_max_step;

  let return v =
    continue (Denv.add env x v, v)
  in
  
  match cbody with
  | Value_body (Value_function vf) -> return @@ FunClosure (vf, env)
  | Value_body (Value_record r) -> return @@ RecordClosure (r, env)
  | Value_body v -> return @@ Direct v
  | Var_body vx -> return @@ Denv.fetch env vx
  | Conditional_body (cx, e1, e2) ->
    let e =
      if
        match Denv.fetch env cx with
        | Direct (Value_bool b) -> b
        | _ -> raise Exns.Type_mismatch
      then e1
      else e2
    in
    eval_exp ~session env e (fun (_, r) -> return r)
  | Input_body -> return @@ Direct (Value_int (session.input_feeder ()))
  | Appl_body (vf, varg) -> begin
    match Denv.fetch env vf with
    | FunClosure (Function_value (Var (param, _), body), fenv) ->
      (* varg is the argument that fills in param *)
      let arg = Denv.fetch env varg in
      let env' = Denv.add fenv param arg in

      (* returned value of function *)
      eval_exp ~session env' body (fun (_, r) -> return r)
    | _ -> raise Exns.Type_mismatch
    end
  | Match_body (vy, p) -> return @@ Direct (Value_bool (Denv.check_pattern env vy p))
  | Projection_body (v, label) -> begin
    match Denv.fetch env v with
    | RecordClosure (Record_value r, denv) ->
      let proj_var = Ident_map.find label r in
      return @@ Denv.fetch denv proj_var
    | Direct (Value_record (Record_value _record)) ->
      failwith "project should also have a closure"
    | _ -> raise Exns.Type_mismatch
    end
  | Not_body vy -> (* x = not y ; *)
    begin
    match Denv.fetch env vy with
    | Direct (Value_bool b) -> return @@ Direct (Value_bool (not b))
    | _ -> raise Exns.Type_mismatch
    end
  | Binary_operation_body (vy, op, vz) -> (* x = y op z *)
    let v1 = Denv.fetch_to_val env vy
    and v2 = Denv.fetch_to_val env vz in
    let v =
      match op, v1, v2 with
      | Binary_operator_plus, Value_int n1, Value_int n2                  -> Value_int  (n1 + n2)
      | Binary_operator_minus, Value_int n1, Value_int n2                 -> Value_int  (n1 - n2)
      | Binary_operator_times, Value_int n1, Value_int n2                 -> Value_int  (n1 * n2)
      | Binary_operator_divide, Value_int n1, Value_int n2                -> Value_int  (n1 / n2)
      | Binary_operator_modulus, Value_int n1, Value_int n2               -> Value_int  (n1 mod n2)
      | Binary_operator_less_than, Value_int n1, Value_int n2             -> Value_bool (n1 < n2)
      | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 -> Value_bool (n1 <= n2)
      | Binary_operator_equal_to, Value_int n1, Value_int n2              -> Value_bool (n1 = n2)
      | Binary_operator_equal_to, Value_bool b1, Value_bool b2            -> Value_bool (Bool.(b1 = b2))
      | Binary_operator_and, Value_bool b1, Value_bool b2                 -> Value_bool (b1 && b2)
      | Binary_operator_or, Value_bool b1, Value_bool b2                  -> Value_bool (b1 || b2)
      | Binary_operator_not_equal_to, Value_int n1, Value_int n2          -> Value_bool (n1 <> n2)
      | _ -> raise Exns.Type_mismatch
    in
    return @@ Direct v
  | Abort_body -> raise Exns.Found_abort
  | Assert_body cx | Assume_body cx ->
    if
      match Denv.fetch env cx with
      | Direct (Value_bool b) -> b
      | _ -> raise Exns.Type_mismatch
    then return @@ Direct (Value_bool true)
    else raise Exns.Failed_assume

let eval_exp_default
  (e : expr)
  : unit
  =
  let _ =
    eval_exp ~session:(Session.create_default ()) Denv.empty e (fun a -> a)
  in
  ()

let rec test_for_failure (e : expr) (n_runs : int) : bool =
  if n_runs <= 0
  then false
  else
    try
      let _ =
        eval_exp_default e
      in
      false
    with
    | Exns.Found_abort
    | Exns.Type_mismatch -> true
    | Exns.Failed_assume
    | Exns.Reach_max_step -> test_for_failure e (n_runs - 1)