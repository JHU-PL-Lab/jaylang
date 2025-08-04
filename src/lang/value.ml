(**
   Module [Value].

   This module defines the values for each programming language
   in this system. It also provides an environment, which is
   recursive with the values because of closures.

   Individual uses of the programming languages have different
   requirements, so we parametrize the values stored in the
   [VInt] and [VBool] constructors, as well as the cell used to
   wrap the environment in the closures (e.g. Bluejay has recursion
   and benefits from a lazy environment in its closures).

   We use type constraints (from [Ast]) and GADTs to allow
   subtyping and reuse of constructors.
*)

open Core
open Ast
open Constraints

module type STORE = sig
  type 'a t
  val empty : 'a t
  val add : Ident.t -> 'a -> 'a t -> 'a t
  val fetch : Ident.t -> 'a t -> 'a option

  val mappings : 'a t -> (Ident.t * 'a) list
end

module type CELL = sig
  type 'a t
  val put : 'a -> 'a t
  val get : 'a t -> 'a

  val to_string :('a -> string) -> 'a t -> string
end

let default_to_string_closure_depth =
  ref @@
  match Sys.getenv "TO_STRING_CLOSURE_DEPTH" with
  | Some s ->
    begin
      try
        int_of_string s
      with
      | Failure _ ->
        prerr_endline
          (Printf.sprintf
             "Unparseable TO_STRING_CLOSURE_DEPTH env var: %s" s);
        0
    end
  | None -> 0

(*
  V is the payload of int and bool. We do this so that we can
  inject Z3 expressions into the values of the concolic evaluator.
*)
module Make (Store : STORE) (Env_cell : CELL) (V : Utils.Equatable.P1) = struct
  module T = struct
    type _ t =
      (* all languages *)
      | VUnit : 'a t
      | VInt : int V.t -> 'a t
      | VBool : bool V.t -> 'a t
      | VFunClosure : { param : Ident.t ; closure : 'a closure } -> 'a t
      | VVariant : { label : VariantLabel.t ; payload : 'a t } -> 'a t
      | VRecord : 'a t RecordLabel.Map.t -> 'a t
      | VModule : 'a t RecordLabel.Map.t -> 'a t
      | VTypeMismatch : 'a t
      | VAbort : 'a t (* this results from `EAbort` or `EAssert e` where e => false *)
      | VVanish : 'a t (* this results from `EVanish` or `EAssume e` where e => false *)
      | VUnboundVariable : Ident.t -> 'a t
      (* embedded only *)
      | VId : 'a embedded_only t
      | VFrozen : 'a closure -> 'a embedded_only t
      | VTable : { mutable alist : ('a t * 'a t) list } -> 'a embedded_only t
      | VUntouchable : 'a t -> 'a embedded_only t
      (* bluejay or type erased *)
      | VList : 'a t list -> 'a bluejay_or_type_erased t
      | VMultiArgFunClosure : { params : Ident.t list ; closure : 'a closure } -> 'a bluejay_or_type_erased t
      (* types in desugared and embedded *)
      | VType : 'a bluejay_or_desugared t
      | VTypeInt : 'a bluejay_or_desugared t
      | VTypeBool : 'a bluejay_or_desugared t
      | VTypeTop : 'a bluejay_or_desugared t
      | VTypeBottom : 'a bluejay_or_desugared t
      | VTypeUnit : 'a bluejay_or_desugared t
      | VTypeRecord : 'a t RecordLabel.Map.t -> 'a bluejay_or_desugared t
      | VTypeModule : (RecordLabel.t * 'a closure) list -> 'a bluejay_or_desugared t
      | VTypeFun : { domain : 'a t ; codomain : 'a t ; det : bool } -> 'a bluejay_or_desugared t
      | VTypeDepFun : { binding : Ident.t ; domain : 'a t ; codomain : 'a closure ; det : bool } -> 'a bluejay_or_desugared t
      | VTypeRefinement : { tau : 'a t ; predicate : 'a t } -> 'a bluejay_or_desugared t
      | VTypeMu : { var : Ident.t ; params : Ident.t list ; closure : 'a closure } -> 'a bluejay_or_desugared t
      | VTypeVariant : (VariantTypeLabel.t * 'a t) list -> 'a bluejay_or_desugared t
      | VTypeSingleFun : 'a bluejay_or_desugared t
      | VTypeSingle : 'a t -> 'a bluejay_or_desugared t
      (* types in bluejay only *)
      | VTypeListFun : 'a bluejay_only t
      | VTypeList : 'a t -> 'a bluejay_only t
      | VTypeIntersect : (VariantTypeLabel.t * 'a t * 'a t) list -> 'a bluejay_only t

    and 'a env = 'a t Store.t

    (* 
      An expression to be evaluated in an environment. The environment is in a cell in case
      laziness is used to implement recursion.    
    *)
    and 'a closure = { body : 'a Expr.t ; env : 'a env Env_cell.t }

    let is_error : type a. a t -> bool = function
      | VAbort | VUnboundVariable _ | VTypeMismatch -> true
      | _ -> false

    let matches : type a. a t -> a Pattern.t -> (a t * Ident.t) list option =
      fun v pattern ->
      match pattern, v with
      | PUntouchable id, VUntouchable v -> Some [ v, id ]
      | _, VUntouchable _ -> None (* untouchable cannot match any *)
      | PAny, _
      | PInt, VInt _ 
      | PBool, VBool _
      | PUnit, VUnit
      | PRecord, VRecord _ (* Currently, types match this *)
      | PModule, VModule _
      | PFun, VFunClosure _ -> Some []
      | PType, VRecord m ->
        if List.for_all Ast_tools.Reserved.[ gen ; check ; wrap ] ~f:(Map.mem m)
        then Some []
        else None
      | PVariable id, _ -> Some [ v, id ]
      | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
        when VariantLabel.equal variant_label label ->
        Some [ payload, payload_id ]
      | PEmptyList, VList [] -> Some []
      | PDestructList { hd_id ; tl_id }, VList (v_hd :: v_tl) ->
        Some [ v_hd, hd_id ; VList v_tl, tl_id ]
      | _ -> None

    let rec equal : type a. a t -> a t -> bool =
      fun a b ->
      if phys_equal a b then true else
        match a, b with
        | VInt i1, VInt i2 -> V.equal Int.equal i1 i2
        | VBool b1, VBool b2 -> V.equal Bool.equal b1 b2
        | VFunClosure r1, VFunClosure r2 ->
          equal_closure [ r1.param, r2.param ] r1.closure r2.closure
        | VVariant r1, VVariant r2 ->
          VariantLabel.equal r1.label r2.label
          && equal r1.payload r2.payload
        | VRecord m1, VRecord m2 -> RecordLabel.Map.equal equal m1 m2
        | VModule m1, VModule m2 -> RecordLabel.Map.equal equal m1 m2
        | VFrozen c1, VFrozen c2 -> equal_closure [] c1 c2
        | VTable r1, VTable r2 ->
          List.equal (Tuple2.equal ~eq1:equal ~eq2:equal) r1.alist r2.alist
        | VUntouchable v1, VUntouchable v2 -> equal v1 v2
        | VList l1, VList l2 -> List.equal equal l1 l2
        | VMultiArgFunClosure r1, VMultiArgFunClosure r2 -> begin
            match Expr.Alist.cons_assocs r1.params r2.params Expr.Alist.empty with
            | `Bindings bindings -> equal_closure bindings r1.closure r2.closure
            | `Unequal_lengths _ -> false
          end
        | VTypeRecord m1, VTypeRecord m2 -> RecordLabel.Map.equal equal m1 m2
        | VTypeModule l1, VTypeModule l2 -> begin
            match
              List.fold2 l1 l2 ~init:(true, []) ~f:(fun (acc_b, acc_l) (RecordLabel label1, c1) (RecordLabel label2, c2) ->
                  if acc_b then
                    equal_closure acc_l c1 c2
                  , Expr.Alist.cons_assoc label1 label2 acc_l
                  else
                    acc_b, acc_l
                )
            with
            | Ok (b, _) -> b
            | Unequal_lengths -> false
          end
        | VTypeFun r1, VTypeFun r2 ->
          Bool.(=) r1.det r2.det
          && equal r1.domain r2.domain
          && equal r1.codomain r2.codomain
        | VTypeDepFun r1, VTypeDepFun r2 ->
          Bool.(=) r1.det r2.det
          && equal r1.domain r2.domain
          && equal_closure [ r1.binding, r2.binding ] r1.codomain r2.codomain
        | VTypeRefinement r1, VTypeRefinement r2 ->
          equal r1.tau r2.tau && equal r1.predicate r2.predicate
        | VTypeMu r1, VTypeMu r2 -> begin
            match Expr.Alist.cons_assocs (r1.var :: r1.params) (r2.var :: r2.params) Expr.Alist.empty with
            | `Bindings bindings -> equal_closure bindings r1.closure r2.closure
            | `Unequal_lengths _ -> false
          end
        | VTypeVariant l1, VTypeVariant l2 ->
          List.equal (Tuple2.equal ~eq1:VariantTypeLabel.equal ~eq2:equal) l1 l2
        | VTypeSingle v1, VTypeSingle v2 -> equal v1 v2
        | VTypeList v1, VTypeList v2 -> equal v1 v2
        | VTypeIntersect l1, VTypeIntersect l2 ->
          List.equal (Tuple3.equal ~eq1:VariantTypeLabel.equal ~eq2:equal ~eq3:equal) l1 l2
        (* intensionally equal *)
        | VUnboundVariable id1, VUnboundVariable id2 -> Ident.equal id1 id2
        | VTypeMismatch, VTypeMismatch
        | VAbort, VAbort
        | VVanish, VVanish
        | VId, VId 
        | VType, VType
        | VTypeInt, VTypeInt
        | VTypeBool, VTypeBool
        | VTypeTop, VTypeTop
        | VTypeBottom, VTypeBottom
        | VTypeSingleFun, VTypeSingleFun
        | VTypeListFun, VTypeListFun
        | VUnit, VUnit
        | VTypeUnit, VTypeUnit -> true
        | _ -> false (* these are structurally different and cannot be equal *)

    and equal_closure : type a. Expr.Alist.t -> a closure -> a closure -> bool =
      fun bindings a b ->
      if phys_equal a b then true else
        Expr.compare (fun id1 id2 ->
            match Expr.Alist.compare_in_t id1 id2 bindings with
            | `Found x -> x
            | `Not_found -> begin
                match Store.fetch id1 (Env_cell.get a.env), Store.fetch id2 (Env_cell.get a.env) with
                | Some v1, Some v2 -> if equal v1 v2 then 0 else 1 (* doesn't need to be real comparison *)
                | _, _ -> 1 (* not equal. Just claim 1 *)
              end
          ) a.body b.body = 0
  end

  module Env = struct
    type 'a t = 'a T.t Store.t
    let empty : 'a t = Store.empty
    let add : Ident.t -> 'a T.t -> 'a t -> 'a t = Store.add
    let fetch : Ident.t -> 'a t -> 'a T.t option = Store.fetch
  end

  include T

  let _closure_depth_counter = ref 0

  let rec _to_string : type a. a t -> string = function
    | VUnit -> "()"
    | VInt i -> V.to_string Int.to_string i
    | VBool b -> V.to_string Bool.to_string b
    | VFunClosure { param = Ident s ; closure } -> Format.sprintf "(fun %s -> %s)" s (_closure_to_string closure)
    | VVariant { label ; payload } -> Format.sprintf "(`%s (%s))" (VariantLabel.to_string label) (_to_string payload)
    | VRecord record_body -> RecordLabel.record_body_to_string ~sep:"=" record_body _to_string
    | VModule module_body -> 
      Format.sprintf "struct %s end" 
        (String.concat ~sep:" " @@ List.map (Map.to_alist module_body) ~f:(fun (key, data) -> Format.sprintf "let %s = %s" (RecordLabel.to_string key) (_to_string data)))
    | VTypeMismatch -> "Type_mismatch"
    | VUnboundVariable Ident v -> Format.sprintf "Unbound_variable %s" v
    | VAbort -> "Abort"
    | VVanish -> "Vanish"
    | VId -> "(fun x -> x)"
    | VFrozen e -> Format.sprintf "(Freeze %s)" (_closure_to_string e)
    | VTable { alist } -> 
      Format.sprintf "Table (%s)\n"
        (String.concat ~sep:" ; " @@ List.map ~f:(fun (k, v) -> Format.sprintf "(%s, %s)" (_to_string k) (_to_string v)) alist)
    | VUntouchable v -> Format.sprintf "Untouchable (%s)" (_to_string v)
    | VList ls -> Format.sprintf "[ %s ]" (String.concat ~sep:" ; " @@ List.map ~f:_to_string ls)
    | VMultiArgFunClosure { params ; closure } -> Format.sprintf "(fun %s -> %s)" (String.concat ~sep:" ; " @@ List.map ~f:(fun (Ident s) -> s) params) (_closure_to_string closure)
    | VType -> "type"
    | VTypeInt -> "int"
    | VTypeBool -> "bool"
    | VTypeTop -> "top"
    | VTypeBottom -> "bottom"
    | VTypeUnit -> "unit"
    | VTypeRecord record_body -> RecordLabel.record_body_to_string ~sep:":" record_body _to_string
    | VTypeModule ls -> Format.sprintf "sig %s end" (String.concat ~sep:" " @@ List.map ls ~f:(fun (label, body) -> Format.sprintf "val %s : %s" (RecordLabel.to_string label) (_closure_to_string body)))
    | VTypeFun { domain ; codomain ; det } -> Format.sprintf "(%s %s %s)" (_to_string domain) (if det then "-->" else "->") (_to_string codomain)
    | VTypeDepFun { binding = Ident s ; domain ;  det ; codomain } -> Format.sprintf "((%s : %s) %s %s)" s (if det then "-->" else "->") (_to_string domain) (_closure_to_string codomain)
    | VTypeRefinement { tau ; predicate } -> Format.sprintf "{ %s | %s }" (_to_string tau) (_to_string predicate)
    | VTypeSingleFun -> Format.sprintf "singlet"
    | VTypeSingle v -> Format.sprintf "(singlet (%s))" (_to_string v)
    | VTypeListFun -> Format.sprintf "list"
    | VTypeList v -> Format.sprintf "(list (%s))" (_to_string v)
    | VTypeIntersect ls ->
      Format.sprintf "(%s)"
        (String.concat ~sep:" && " @@ List.map ls ~f:(fun (VariantTypeLabel Ident s, tau1, tau2) -> Format.sprintf "((``%s (%s)) -> %s)" s (_to_string tau1) (_to_string tau2)))
    | VTypeMu { var = Ident s ; params ; closure } -> 
      Format.sprintf "(mu %s. %s)"
        (s ^ String.concat ~sep:" " @@ List.map params ~f:Ident.to_string) (_closure_to_string closure)
    | VTypeVariant ls ->
      Format.sprintf "(%s)"
        (String.concat ~sep: "| " @@ List.map ls ~f:(fun (VariantTypeLabel Ident s, tau) -> Format.sprintf "(`%s of %s)" s (_to_string tau)))

  and _closure_to_string : type a. a closure -> string = fun c ->
    if !_closure_depth_counter <= 0 then "«closure»" else
      let () = _closure_depth_counter := !_closure_depth_counter - 1 in
      let answer =
        let {body; env} = c in 
        let env_to_string e =
          String.concat ~sep:", " (List.map (Store.mappings e) ~f:(fun (Ident x, v) -> 
              let v_str = _to_string v in
              Format.sprintf "(%s, %s)" x v_str)) in
        let body_eval = Expr.to_string body in
        let env_eval = Env_cell.to_string (env_to_string) env in
        Format.sprintf "{%s ; %s}" body_eval env_eval
      in
      let () = _closure_depth_counter := !_closure_depth_counter + 1 in
      answer

  let to_string_by_depth : type a. max_closure_depth:int -> a t -> string =
    fun ~(max_closure_depth:int) v ->
    let () = _closure_depth_counter := max_closure_depth in
    _to_string v

  let closure_to_string_by_depth :
    type a. max_closure_depth:int -> a closure -> string =
    fun ~(max_closure_depth:int) closure ->
    let () = _closure_depth_counter := max_closure_depth in
    _closure_to_string closure

  let to_string : type a. a t -> string = fun v ->
    to_string_by_depth ~max_closure_depth:!default_to_string_closure_depth v

  let closure_to_string : type a. a closure -> string = fun closure ->
    closure_to_string_by_depth
      ~max_closure_depth:!default_to_string_closure_depth closure
end

module Constrain (C : sig type constrain end) (Store : STORE) (Cell : CELL) (V : Utils.Equatable.P1) = struct
  module M = Make (Store) (Cell) (V)

  module T = struct
    type t = C.constrain M.T.t
  end

  include T

  let matches = M.matches

  let to_string = M.to_string

  module Env = struct
    type value = T.t
    type t = C.constrain M.Env.t
    let empty : t = M.Env.empty
    let add : Ident.t -> value -> t -> t = M.Env.add
    let fetch : Ident.t -> t -> value option = M.Env.fetch
  end
end

module List_store = struct
  type 'a t = (Ident.t * 'a) list

  let empty : 'a t = []

  let[@inline always] add (id : Ident.t) (v : 'a) (env : 'a t) : 'a t =
    if Ident.equal Ast_tools.Reserved.catchall id
    then env
    else (id, v) :: env

  (*
    Inlining is pretty important here. We fetch a lot, and the compiler can 
    make some optimizations if we inline.
    This is experimentally (but informally) confirmed.
  *)
  let[@inline always] fetch (id : Ident.t) (env : 'a t) : 'a option =
    let rec loop = function
      | [] -> None
      | (id', v) :: _ when Ident.equal id id' -> Some v
      | _ :: tl -> loop tl
    in
    loop env

  let[@inline always] mappings (env : 'a t) : (Ident.t * 'a) list =
    env
end

module Map_store = struct
  type 'a t = 'a Ident.Map.t 

  let empty : 'a t = Ident.Map.empty

  let[@inline always] add (id : Ident.t) (v : 'a) (env : 'a t) : 'a t =
    if Ident.equal Ast_tools.Reserved.catchall id
    then env
    else Map.set env ~key:id ~data:v

  let[@inline always] fetch (id : Ident.t) (env : 'a t) : 'a option =
    Map.find env id

  let[@inline always] mappings (env : 'a t) : (Ident.t * 'a) list =
    Map.to_alist env
end

module Lazy_cell = struct
  include Lazy
  let put a = lazy a
  let get x = force x

  let to_string f x = if Lazy.is_val x then f (get x) else "lazy»"
end

(*
  Values in embedded language. There is no explicit recursion, so the closures
  do not use lazy environments.
  Variables are often used immediately after being created, so use a list as an environment.
*)
module Embedded = Constrain (struct type constrain = Ast.Constraints.embedded end) (List_store) (Utils.Identity)

(*
  Values in the desugared language. Uses laziness to implement recursion.
  Usage is unknown, so use a Map as the environment to be general.
*)
module Desugared = Constrain (struct type constrain = Ast.Constraints.desugared end) (Map_store) (Lazy_cell)

(*
  Values in Bluejay. Uses laziness to implement recursion.
  Usage is unknown, so use a Map as the environment to be general.
*)
module Bluejay = Constrain (struct type constrain = Ast.Constraints.bluejay end) (Map_store) (Lazy_cell)

module Error_msg (Value : sig type t val to_string : t -> string end) = struct
  let project_non_record label v =
    Format.sprintf "Label %s not found in non-record/module `%s`" (RecordLabel.to_string label) (Value.to_string v)

  let project_missing_label label record =
    Format.sprintf "Label %s not found in record/module %s" (RecordLabel.to_string label) (Value.to_string record)

  let thaw_non_frozen v =
    Format.sprintf "Thaw non-frozen value `%s`" (Value.to_string v)

  let pattern_not_found patterns v =
    Format.sprintf "Value `%s` not in pattern list [ %s ]"
      (Value.to_string v)
      (String.concat ~sep:", " @@ List.map patterns ~f:(fun (p, _) -> Pattern.to_string p))

  let bad_appl vfunc =
    Format.sprintf "Apply to non-function %s" (Value.to_string vfunc)

  let bad_binop vleft binop vright =
    Format.sprintf "Bad binop %s %s %s"
      (Value.to_string vleft)
      (Binop.to_string binop)
      (Value.to_string vright)

  let bad_not v =
    Format.sprintf "Bad unary operation `not %s`" (Value.to_string v)

  let cond_non_bool v = 
    Format.sprintf "Condition on non-bool `%s`" (Value.to_string v)

  let case_non_int v = 
    Format.sprintf "Case on non-int `%s`" (Value.to_string v)

  let appl_non_table v =
    Format.sprintf "Use non-table `%s` as a table" (Value.to_string v)
end
