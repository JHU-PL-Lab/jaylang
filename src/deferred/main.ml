
open Core

let unimplemented () = 
  failwith "unimplemented"

(*
  Still haven't...
    - set up an executable
*)

open Effects

let rec eval (expr : Lang.Ast.Embedded.With_program_points.t) : Value.nonerr m =
  let open Value in
  match expr with
  | EUnit -> return VUnit
  | EInt i -> return (VInt i)
  | EBool b -> return (VBool b)
  | EVar id -> begin
    match%bind fetch id with
    | Some v -> return v
    | None -> unbound_variable id
  end
  | EId -> return VId
  (* inputs *)
  | EPick_i { data = () ; point } -> get_input point
  | EPick_b { data = () ; point = _ } -> unimplemented () (* TODO: bool inputs *)
  (* operations *)
  | EBinop { left ; binop ; right } -> begin
    let%bind a = stern_eval left in
    let%bind b = stern_eval right in
    match binop, a, b with
    | BPlus        , VInt n1  , VInt n2              -> return (VInt (n1 + n2))
    | BMinus       , VInt n1  , VInt n2              -> return (VInt (n1 - n2))
    | BTimes       , VInt n1  , VInt n2              -> return (VInt (n1 * n2))
    | BDivide      , VInt n1  , VInt n2 when n2 <> 0 -> return (VInt (n1 / n2))
    | BModulus     , VInt n1  , VInt n2 when n2 <> 0 -> return (VInt (n1 mod n2))
    | BEqual       , VInt n1  , VInt n2              -> return (VBool (n1 = n2))
    | BEqual       , VBool b1 , VBool b2             -> return (VBool Bool.(b1 = b2))
    | BNeq         , VInt n1  , VInt n2              -> return (VBool (n1 <> n2))
    | BLessThan    , VInt n1  , VInt n2              -> return (VBool (n1 < n2))
    | BLeq         , VInt n1  , VInt n2              -> return (VBool (n1 <= n2))
    | BGreaterThan , VInt n1  , VInt n2              -> return (VBool (n1 > n2))
    | BGeq         , VInt n1  , VInt n2              -> return (VBool (n1 >= n2))
    | BAnd         , VBool b1 , VBool b2             -> return (VBool (b1 && b2))
    | BOr          , VBool b1 , VBool b2             -> return (VBool (b1 || b2))
    | _ -> type_mismatch "bad binop; no good error message yet"
  end
  | ENot expr -> begin
    match%bind stern_eval expr with
    | VBool b -> return (VBool (not b))
    | _v -> type_mismatch "bad not"
  end
  | EProject { record ; label } -> begin
    match%bind stern_eval record with
    | (VRecord body | VModule body) -> begin
      match Map.find body label with
      | Some Safe _v -> failwith "would get a type constructor escaping scope" (*return v*)
      | None -> type_mismatch "missing label in projection"
    end
    | _v -> type_mismatch "project from non record/module"
  end
  (* control flow / branches *)
  | EMatch { subject = _ ; patterns = _ } -> unimplemented ()
  | EIf { cond = _ ; true_body = _ ; false_body = _ } -> unimplemented ()
  | ECase { subject = _ ; cases = _ ; default = _ } -> unimplemented ()
  (* closures and applications *)
  | EFunction { param = _ ; body = _ } -> unimplemented ()
  | EFreeze _expr -> unimplemented ()
  | ELet { var = _ ; defn = _ ; body = _ } -> unimplemented ()
  | EIgnore { ignored = _ ; body = _ } -> unimplemented () (* eval ignored, discard it, then eval body *)
  | EAppl { data = { func = _ ; arg = _ } ; point = _ } -> unimplemented ()
  | EThaw { data = _expr_closure ; point = _ } -> unimplemented ()
  (* modules, records, and variants  *)
  | ERecord _label_map -> unimplemented ()
  | EVariant { label = _ ; payload = _ } -> unimplemented ()
  | EModule _stmt_ls -> unimplemented ()
  | EUntouchable _e -> unimplemented ()
  (* termination *)
  | EDiverge { data = () ; point = _ } -> unimplemented ()
  | EAbort { data = _msg ; point = _ } -> unimplemented ()
  (* unhandled and currently ignored *)
  | EDet expr
  | EEscapeDet expr -> eval expr (* it is fine to ignore these and just eval what's inside for now *)
  (* unhandled and currently aborting -- okay to ignore for now because these are uncommon *)
  | EIntensionalEqual _ -> failwith "unhandled intensional equality in deferred evaluation"
  | ETable
  | ETblAppl _ -> failwith "unhandled table operations in deferred evaluation"

and stern_eval (expr : Lang.Ast.Embedded.With_program_points.t) : Value.nonsymb m = 
  let open Value in
  match expr with
  | EUnit -> return VUnit
  | EInt i -> return (VInt i)
  | EBool _b -> unimplemented ()
  | EVar _id -> unimplemented ()
  | EId -> unimplemented () (* baked in identify function *)
  (* inputs *)
  | EPick_i { data = () ; point = _ } -> unimplemented ()
  | EPick_b { data = () ; point = _ } -> unimplemented ()
  (* operations *)
  | EBinop { left = _ ; binop = _ ; right = _ } -> unimplemented ()
  | ENot _expr -> unimplemented ()
  | EProject { record = _ ; label = _ } -> unimplemented ()
  (* control flow / branches *)
  | EMatch { subject = _ ; patterns = _ } -> unimplemented ()
  | EIf { cond = _ ; true_body = _ ; false_body = _ } -> unimplemented ()
  | ECase { subject = _ ; cases = _ ; default = _ } -> unimplemented ()
  (* closures and applications *)
  | EFunction { param = _ ; body = _ } -> unimplemented ()
  | EFreeze _expr -> unimplemented ()
  | ELet { var = _ ; defn = _ ; body = _ } -> unimplemented ()
  | EIgnore { ignored = _ ; body = _ } -> unimplemented () (* eval ignored, discard it, then eval body *)
  | EAppl { data = { func = _ ; arg = _ } ; point = _ } -> unimplemented ()
  | EThaw { data = _expr_closure ; point = _ } -> unimplemented ()
  (* modules, records, and variants  *)
  | ERecord _label_map -> unimplemented ()
  | EVariant { label = _ ; payload = _ } -> unimplemented ()
  | EModule _stmt_ls -> unimplemented ()
  | EUntouchable _e -> unimplemented ()
  (* termination *)
  | EDiverge { data = () ; point = _ } -> unimplemented ()
  | EAbort { data = _msg ; point = _ } -> unimplemented ()
  (* unhandled and currently ignored *)
  | EDet expr
  | EEscapeDet expr -> stern_eval expr (* it is fine to ignore these and just eval what's inside for now *)
  (* unhandled and currently aborting -- okay to ignore for now because these are uncommon *)
  | EIntensionalEqual _ -> failwith "unhandled intensional equality in deferred evaluation"
  | ETable
  | ETblAppl _ -> failwith "unhandled table operations in deferred evaluation"
