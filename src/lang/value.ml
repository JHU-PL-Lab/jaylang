
open Core
open Ast
open Constraints

exception UnboundVariable of Ident.t

module type CELL = sig
  type 'a t
  val make : 'a -> 'a t
  val get : 'a t -> 'a
end

module Make (Cell : CELL) (V : T1) = struct
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
      | VTypeRecord : 'a t RecordLabel.Map.t -> 'a bluejay_or_desugared t
      | VTypeArrow : { domain : 'a t ; codomain : 'a t } -> 'a bluejay_or_desugared t
      | VTypeArrowD : { binding : Ident.t ; domain : 'a t ; codomain : 'a closure } -> 'a bluejay_or_desugared t
      | VTypeRefinement : { tau : 'a t ; predicate : 'a t } -> 'a bluejay_or_desugared t
      | VTypeIntersect : (VariantTypeLabel.t * 'a t * 'a t) list -> 'a bluejay_or_desugared t
      | VTypeMu : { var : Ident.t ; body : 'a closure } -> 'a bluejay_or_desugared t
      | VTypeVariant : (VariantTypeLabel.t * 'a t) list -> 'a bluejay_or_desugared t
      (* types in bluejay only *)
      | VTypeList : 'a t -> 'a bluejay_only t
      | VTypeForall : { type_variables : Ident.t list ; tau : 'a closure } -> 'a bluejay_only t
      (* recursive function stub for bluejay and mu type for desugared *)
      | VRecStub : 'a bluejay_or_desugared t

    and 'a env = 'a t Cell.t Ident.Map.t (* cell is to handle recursion *)

    and 'a closure = { expr : 'a Expr.t ; env : 'a env } (* an expression to be evaluated in an environment *)
  end

  include T

  module Env = struct
    type 'a t = 'a T.env

    let empty : 'a env = Ident.Map.empty

    let add (env : 'a t) (id : Ident.t) (v : 'a T.t) : 'a t =
      Map.set env ~key:id ~data:(Cell.make v)

    let fetch (env : 'a t) (id : Ident.t) : 'a T.t =
      match Map.find env id with
      | None -> raise @@ UnboundVariable id
      | Some r -> Cell.get r

    let add_stub (env : 'a Constraints.bluejay_or_desugared t) (id : Ident.t) : 'a T.t Cell.t * 'a t =
      let v_cell = Cell.make VRecStub in
      v_cell, Map.set env ~key:id ~data:v_cell
  end
end

module Constrain (C : sig type constrain end) (Cell : CELL) (V : T1) = struct
  module M = Make (Cell) (V)

  module T = struct
    type t = C.constrain M.T.t
  end
  
  include T

  module Env = struct
    type t = C.constrain M.Env.t
    let empty : t = M.Env.empty
    let add : t -> Ident.t -> T.t -> t = M.Env.add
    let fetch : t -> Ident.t -> T.t = M.Env.fetch
  end
end

module Id_cell = struct
  type 'a t = 'a
  let make x = x
  let get x = x
end

module Ref_cell = struct
  type 'a t = 'a Ref.t 
  let make x = ref x
  let get x = !x
end

module Embedded = Constrain (struct type constrain = Ast.Constraints.embedded end) (Id_cell)

module Desugared (V : T1) = struct
  include Constrain (struct type constrain = Ast.Constraints.desugared end) (Ref_cell) (V)
  module Env = struct
    include Env
    let add_stub : t -> Ident.t -> T.t Ref_cell.t * t = M.Env.add_stub
  end
end 

module Bluejay (V : T1) = struct
  include Constrain (struct type constrain = Ast.Constraints.bluejay end) (Ref_cell) (V)
  module Env = struct
    include Env
    let add_stub : t -> Ident.t -> T.t Ref_cell.t * t = M.Env.add_stub
  end
end 