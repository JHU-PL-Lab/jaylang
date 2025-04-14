
open Core

(* Items earlier in the cons are those that are deeper in the path tree *)
type t =
  | Root
  | Beginning_from of Target.t
  | Bool_branch of { claim : bool Claim.t ; tail : t } (* it's easy to derive the other direction *)
  | Int_branch of { claim : int Claim.t ; other_cases : int Claim.t list ; tail : t }

let empty : t = Root

let of_target (target : Target.t) : t =
  Beginning_from target

let push_branch (stem : t) (dir : bool Direction.t) (e : bool Expression.t) : t =
  Bool_branch { claim = Equality (e, dir) ; tail = stem }

let push_case (stem : t) (dir : int Direction.t) (e : int Expression.t) (other_ints : int list) : t =
  let other_claims = List.map other_ints ~f:(fun i -> Claim.Equality (e, Case_int i)) in
  match dir with
  | Case_int i -> (* did not take the default, so the default needs to be derived *)
    let default = Direction.Case_default { not_in = i :: other_ints } in
    Int_branch
      { claim = Equality (e, dir)
      ; other_cases = Equality (e, default) :: other_claims
      ; tail = stem }
  | Case_default _ -> (* default is this direction, so no need to derive it *)
    Int_branch
      { claim = Equality (e, dir)
      ; other_cases = other_claims
      ; tail = stem }

(*
  Only builds the targets that are necessarily new: they
  come after the target that this stem comes from.
*)
let to_new_targets (stem : t) : Target.t list =
  let rec build_targets = function
    | Root -> Target.empty, []
    | Beginning_from target -> target, []
    | Bool_branch { claim ; tail } ->
      let prev_target, targets = build_targets tail in
      Target.cons claim prev_target
      , Target.cons (Claim.flip claim) prev_target :: targets
    | Int_branch { claim ; other_cases ; tail } ->
      let prev_target, targets = build_targets tail in
      let new_targets = List.map other_cases ~f:(fun claim -> Target.cons claim prev_target) in
      Target.cons claim prev_target
      , new_targets @ targets
  in
  Tuple2.get2 @@ build_targets stem
