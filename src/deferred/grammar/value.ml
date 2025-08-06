
open Core
open Lang.Ast

(* deferred values *)

module C = struct
  type 'a symbol = 'a constraint 'a = [> `Symbol ]
  type 'a ok = 'a constraint 'a = [> `Ok ]
end

open C

(*
  I opt for this representation instead of an Either on normal
  value vs symbol because we can avoid the extra boxing that comes with that.

  The only downside is we have to "cast" sometimes or we get type
  parameters escaping their scopes. The `cast_up` we have later is
  safer than magic and just as fast, but it simply looks word. It also
  skips allocation like an Either would have to do
*)

(*
  V holds the int and bool payloads.
  It is intended to be identity for normal, concrete interpretation,
  or it is some concolic representation.
*)
module Make (V : Utils.Equatable.P1) = struct
  type _ v =
    | VUnit : 'a ok v
    | VInt : int V.t -> 'a ok v
    | VBool : bool V.t -> 'a ok v
    | VFunClosure : { param : Ident.t ; closure : closure } -> 'a ok v
    | VVariant : { label : VariantLabel.t ; payload : t } -> 'a ok v
    | VRecord : t RecordLabel.Map.t -> 'a ok v
    | VModule : t RecordLabel.Map.t -> 'a ok v
    | VId : 'a ok v
    | VFrozen : closure -> 'a ok v
    | VUntouchable : t -> 'a ok v
    | VSymbol : Interp_common.Timestamp.t -> 'a symbol v

  and env = t Lang.Value.List_store.t

  and closure = { body : Embedded.t ; env : env }

  and t = [ `Ok | `Symbol ] v

  (* Values in weak head normal form *)
  type whnf = [ `Ok ] v

  (* Value symbol *)
  type symb = [ `Symbol ] v

  type any = [ `Ok | `Symbol ]
  type ok = [ `Ok ]

  (* Compiles to identity function and helps cast to the general value type *)
  let[@inline always] cast_up (type a) (v : a v) : t =
    match v with
    | (VUnit
      | VInt _
      | VBool _
      | VFunClosure _
      | VVariant _
      | VRecord _
      | VModule _
      | VId
      | VFrozen _ 
      | VUntouchable _
      | VSymbol _) as x -> x

  let[@inline always] split (type a b) (v : a v) ~(whnf : whnf -> b) ~(symb : symb -> b) : b =
    match v with
    | (VUnit
      | VInt _
      | VBool _
      | VFunClosure _
      | VVariant _
      | VRecord _
      | VModule _
      | VId
      | VFrozen _ 
      | VUntouchable _) as x -> whnf x
    | VSymbol _ as x -> symb x


  let timestamp_of_symbol (VSymbol t : symb) : Interp_common.Timestamp.t =
    t

  (* returns binding if there is a matching pattern *)
  let matches (v : whnf) (p : Embedded.pattern) : [ `Matches | `Matches_with of t * Ident.t | `No_match ] =
    match p, v with
    | Pattern.PUntouchable id, VUntouchable v -> `Matches_with (v, id)
    | _, VUntouchable _ -> `No_match
    | PAny, _
    | PInt, VInt _ 
    | PBool, VBool _
    | PUnit, VUnit
    | PRecord, VRecord _ (* Currently, types match this *)
    | PModule, VModule _
    | PFun, VFunClosure _ -> `Matches
    | PType, VRecord m ->
      if List.for_all Lang.Ast_tools.Reserved.[ gen ; check ; wrap ] ~f:(Map.mem m)
      then `Matches
      else `No_match
    | PVariable id, v -> `Matches_with (cast_up v, id)
    | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
        when VariantLabel.equal variant_label label ->
      `Matches_with (payload, payload_id)
    | _ -> `No_match

  module Env = struct
    include Lang.Value.List_store
    type t = env
  end

  let rec to_string : type a. a v -> string = function
    | VUnit -> "()"
    | VInt vi -> V.to_string Int.to_string vi
    | VBool vb -> V.to_string Bool.to_string vb
    | VFunClosure { param = Ident s ; _ } -> Format.sprintf "(fun %s -> <expr>)" s
    | VVariant { label ; payload } -> Format.sprintf "(`%s (%s))" (VariantLabel.to_string label) (to_string payload)
    | VRecord record_body -> RecordLabel.record_body_to_string ~sep:"=" record_body to_string
    | VModule module_body -> 
      Format.sprintf "struct %s end" 
      (String.concat ~sep:" " @@ List.map (Map.to_alist module_body) ~f:(fun (key, data) -> Format.sprintf "let %s = %s" (RecordLabel.to_string key) (to_string data)))
    | VId -> "(fun x -> x)"
    | VFrozen _ -> "(Freeze <expr>)"
    (* | VTable { alist } -> 
      Format.sprintf "Table (%s)\n"
        (String.concat ~sep:" ; " @@ List.map ~f:(fun (k, v) -> Format.sprintf "(%s, %s)" (to_string k) (to_string v)) alist) *)
    | VUntouchable v -> Format.sprintf "Untouchable (%s)" (to_string v)
    | VSymbol t -> Format.sprintf "T%s" (Interp_common.Timestamp.to_string t)

  module Error_msg = struct
    let project_non_record label v =
      Format.sprintf "Label %s not found in non-record/module `%s`" (RecordLabel.to_string label) (to_string v)

    let project_missing_label label record =
      Format.sprintf "Label %s not found in record/module %s" (RecordLabel.to_string label) (to_string record)

    let thaw_non_frozen v =
      Format.sprintf "Thaw non-frozen value `%s`" (to_string v)

    let pattern_not_found patterns v =
      Format.sprintf "Value `%s` not in pattern list [ %s ]"
        (to_string v)
        (String.concat ~sep:", " @@ List.map patterns ~f:(fun (p, _) -> Pattern.to_string p))

    let bad_appl vfunc =
      Format.sprintf "Apply to non-function %s" (to_string vfunc)

    let bad_binop vleft binop vright =
      Format.sprintf "Bad binop %s %s %s"
        (to_string vleft)
        (Binop.to_string binop)
        (to_string vright)

    let bad_not v =
      Format.sprintf "Bad unary operation `not %s`" (to_string v)

    let cond_non_bool v = 
      Format.sprintf "Condition on non-bool `%s`" (to_string v)

    let case_non_int v = 
      Format.sprintf "Case on non-int `%s`" (to_string v)

    let appl_non_table v =
      Format.sprintf "Use non-table `%s` as a table" (to_string v)
  end

  module Pending_proofs = struct
    type t = closure Time_map.t

    let empty : t = Time_map.empty

    (* May want to raise an exception, just to check invariants, if the symbol is duplicate *)
    let push (VSymbol t : symb) (work : closure) (m : t) : t =
      Time_map.add t work m

    let pop (VSymbol t : symb) (m : t) : (closure * t) option =
      Option.map (Time_map.find_opt t m) ~f:(fun closure ->
        closure, Time_map.remove t m
      )

    (*
      Cuts off all symbols at least as big as [t].
    *)
    let cut (VSymbol t : symb) (m : t) : t =
      Tuple3.get1
      @@ Time_map.split t m
  end

  module Symbol_map = struct
    type t = whnf Time_map.t

    let empty : t = Time_map.empty

    (*
      Cuts off all symbols at least as big as [t].
    *)
    let cut (VSymbol t : symb) (m : t) : t =
      Tuple3.get1
      @@ Time_map.split t m
  end

end

(* Standard implemention is with identity *)
include Make (Utils.Identity)

module Without_symbols = Lang.Value.Embedded (Utils.Identity)

(* Values cannot be recursive, so this will terminate *)
(* Don't worry about performance here. Just do as many substs as needed. *)
let rec subst (v : t) ~(f : Interp_common.Timestamp.t -> whnf) : Without_symbols.t =
  match v with
  | VSymbol t -> subst (cast_up (f t)) ~f
  (* Nothing to do *)
  | VId -> VId
  | VUnit -> VUnit
  | VInt i -> VInt i
  | VBool b -> VBool b
  (* Homomorphic *)
  | VVariant { label ; payload } -> VVariant { label ; payload = subst payload ~f }
  | VRecord record_body -> VRecord (Map.map record_body ~f:(subst ~f))
  | VModule module_body -> VModule (Map.map module_body ~f:(subst ~f))
  | VUntouchable v -> VUntouchable (subst v ~f)
  (* Expressions FIXME : subst into env *)
  | VFunClosure { param ; closure = { env = _ ; body } } -> VFunClosure { param ; closure = { env = Without_symbols.Env.empty ; body } }
  | VFrozen { env = _ ; body } -> VFrozen { env = Without_symbols.Env.empty ; body }
  (* Unhandled *)
  (* | VTable _ -> failwith "unhandled" *)