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

module type V = sig
  type 'a t
  val to_string : ('a -> string) -> 'a t -> string
end

module type STORE = sig
  type 'a t
  val empty : 'a t
  val add : Ident.t -> 'a -> 'a t -> 'a t
  val fetch : Ident.t -> 'a t -> 'a option
end

(*
  V is the payload of int and bool. We do this so that we can
  inject Z3 expressions into the values of the concolic evaluator.
*)
module Make (Store : STORE) (Env_cell : T1) (V : V) = struct
  module T = struct
    type _ t =
      (* all languages *)
      | VInt : int V.t -> 'a t
      | VBool : bool V.t -> 'a t
      | VFunClosure : { param : Ident.t ; body : 'a closure } -> 'a t
      | VVariant : { label : VariantLabel.t ; payload : 'a t } -> 'a t
      | VRecord : 'a t RecordLabel.Map.t -> 'a t
      | VTypeMismatch : 'a t
      | VAbort : 'a t (* this results from `EAbort` or `EAssert e` where e => false *)
      | VDiverge : 'a t (* this results from `EDiverge` or `EAssume e` where e => false *)
      | VUnboundVariable : Ident.t -> 'a t
      (* embedded only *)
      | VId : 'a embedded_only t
      | VFrozen : 'a closure -> 'a embedded_only t
      (* bluejay only *)
      | VList : 'a t list -> 'a bluejay_only t
      | VMultiArgFunClosure : { params : Ident.t list ; body : 'a closure } -> 'a bluejay_only t
      (* types in desugared and embedded *)
      | VType : 'a bluejay_or_desugared t
      | VTypeInt : 'a bluejay_or_desugared t
      | VTypeBool : 'a bluejay_or_desugared t
      | VTypeTop : 'a bluejay_or_desugared t
      | VTypeBottom : 'a bluejay_or_desugared t
      | VTypeRecord : 'a t RecordLabel.Map.t -> 'a bluejay_or_desugared t
      | VTypeRecordD : (RecordLabel.t * 'a closure) list -> 'a bluejay_or_desugared t
      | VTypeArrow : { domain : 'a t ; codomain : 'a t } -> 'a bluejay_or_desugared t
      | VTypeArrowD : { binding : Ident.t ; domain : 'a t ; codomain : 'a closure } -> 'a bluejay_or_desugared t
      | VTypeRefinement : { tau : 'a t ; predicate : 'a t } -> 'a bluejay_or_desugared t
      | VTypeMu : { var : Ident.t ; body : 'a closure } -> 'a bluejay_or_desugared t
      | VTypeVariant : (VariantTypeLabel.t * 'a t) list -> 'a bluejay_or_desugared t
      | VTypeSingle : 'a t -> 'a bluejay_or_desugared t
      (* types in bluejay only *)
      | VTypeList : 'a t -> 'a bluejay_only t
      | VTypeIntersect : (VariantTypeLabel.t * 'a t * 'a t) list -> 'a bluejay_only t

    and 'a env = 'a t Store.t

    (* 
      An expression to be evaluated in an environment. The environment is in a cell in case
      laziness is used to implement recursion.    
    *)
    and 'a closure = { expr : 'a Expr.t ; env : 'a env Env_cell.t }

    let matches : type a. a t -> a Pattern.t -> (a t * Ident.t) list option =
      fun v pattern ->
        match pattern, v with
        | PAny, _ -> Some []
        | PVariable id, _ -> Some [ v, id ]
        | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
            when VariantLabel.equal variant_label label ->
          Some [ payload, payload_id ]
        | PEmptyList, VList [] -> Some []
        | PDestructList { hd_id ; tl_id }, VList (v_hd :: v_tl) ->
          Some [ v_hd, hd_id ; VList v_tl, tl_id ]
        | _ -> None
  end

  module Env = struct
    type 'a t = 'a T.t Store.t
    let empty : 'a t = Store.empty
    let add : Ident.t -> 'a T.t -> 'a t -> 'a t = Store.add
    let fetch : Ident.t -> 'a t -> 'a T.t option = Store.fetch
  end

  include T

  let rec to_string : type a. a t -> string = function
    | VInt i -> V.to_string Int.to_string i
    | VBool b -> V.to_string Bool.to_string b
    | VFunClosure { param = Ident s ; _ } -> Format.sprintf "(fun %s -> <expr>)" s
    | VVariant { label ; payload } -> Format.sprintf "(`%s (%s))" (VariantLabel.to_string label) (to_string payload)
    | VRecord record_body -> RecordLabel.record_body_to_string ~sep:"=" record_body to_string
    | VTypeMismatch -> "Type_mismatch"
    | VUnboundVariable Ident v -> Format.sprintf "Unbound_variable %s" v
    | VAbort -> "Abort"
    | VDiverge -> "Diverge"
    | VId -> "(fun x -> x)"
    | VFrozen _ -> "(Freeze <expr>)"
    | VList ls -> Format.sprintf "[ %s ]" (String.concat ~sep:" ; " @@ List.map ~f:to_string ls)
    | VMultiArgFunClosure { params ; _ } -> Format.sprintf "(fun %s -> <expr>)" (String.concat ~sep:" ; " @@ List.map ~f:(fun (Ident s) -> s) params)
    | VType -> "type"
    | VTypeInt -> "int"
    | VTypeBool -> "bool"
    | VTypeTop -> "top"
    | VTypeBottom -> "bottom"
    | VTypeRecord record_body -> RecordLabel.record_body_to_string ~sep:":" record_body to_string
    | VTypeRecordD ls -> Format.sprintf "{: %s :}" (String.concat ~sep:" ; " @@ List.map ls ~f:(fun (label, _) -> Format.sprintf "%s : <expr>" (RecordLabel.to_string label)))
    | VTypeArrow { domain ; codomain } -> Format.sprintf "(%s -> %s)" (to_string domain) (to_string codomain)
    | VTypeArrowD { binding = Ident s ; domain ; _ } -> Format.sprintf "((%s : %s) -> <expr>)" s (to_string domain)
    | VTypeRefinement { tau ; predicate } -> Format.sprintf "{ %s | %s }" (to_string tau) (to_string predicate)
    | VTypeSingle v -> Format.sprintf "(singlet (%s))" (to_string v)
    | VTypeList v -> Format.sprintf "(list (%s))" (to_string v)
    | VTypeIntersect ls ->
      Format.sprintf "(%s)"
        (String.concat ~sep:" && " @@ List.map ls ~f:(fun (VariantTypeLabel Ident s, tau1, tau2) -> Format.sprintf "((``%s (%s)) -> %s)" s (to_string tau1) (to_string tau2)))
    | VTypeMu { var = Ident s ; _ } -> Format.sprintf "(Mu %s. <expr>)" s
    | VTypeVariant ls ->
      Format.sprintf "(%s)"
        (String.concat ~sep: "| " @@ List.map ls ~f:(fun (VariantTypeLabel Ident s, tau) -> Format.sprintf "(`%s of %s)" s (to_string tau)))
end

module Constrain (C : sig type constrain end) (Store : STORE) (Cell : T1) (V : V) = struct
  module M = Make (Store) (Cell) (V)

  module T = struct
    type t = C.constrain M.T.t
  end
  
  include T

  let matches = M.matches

  let to_string = M.to_string

  module Env = struct
    type t = C.constrain M.Env.t
    let empty : t = M.Env.empty
    let add : Ident.t -> T.t -> t -> t = M.Env.add
    let fetch : Ident.t -> t -> T.t option = M.Env.fetch
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
module Desugared = Constrain (struct type constrain = Ast.Constraints.desugared end) (Map_store) (Lazy)

(*
  Values in Bluejay. Uses laziness to implement recursion.
  Usage is unknown, so use a Map as the environment to be general.
*)
module Bluejay = Constrain (struct type constrain = Ast.Constraints.bluejay end) (Map_store) (Lazy)
