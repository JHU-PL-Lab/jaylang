
open Core

include Interp_common.Input_feeder.Make (Interp_common.Step)

let of_model (model : Interp_common.Step.t Overlays.Typed_smt.model) : t =
  let get : type a. a Interp_common.Key.Stepkey.t -> a = function
    | I k as key -> begin
      let s = Formula.Symbol.make_int k in
      match model.value s with
      | Some i -> i
      | None -> default.get key
    end
    | B k as key -> begin
      let s = Formula.Symbol.make_bool k in
      match model.value s with
      | Some b -> b
      | None -> default.get key
    end
  in
  { get }

module Make (Z : Z3_api.S) = struct
  let from_model_and_subs (model : Z.model) (subs : Expression.Subst.t list) : t =
    let model_vars =
      Z.constrained_vars model
      |> List.map ~f:(fun n -> Interp_common.Step.Step n)
      |> Interp_common.Step.Set.of_list
    in
    let sub_vars =
      Interp_common.Step.Set.of_list
      @@ List.map subs ~f:(function
        | I (k, _) -> Key.extract k
        | B (k, _) -> Key.extract k
      )
    in
    { get = 
      let f (type a) (key : a Key.t) : a =
        if Set.mem model_vars @@ Key.extract key then
          match Z.value_of_key model key with
          | Some i -> i
          | None -> default.get key
        else if Set.mem sub_vars @@ Key.extract key then
          List.find_map_exn subs ~f:(fun sub ->
            match sub, key with
            | I (I k, i), I k' when Interp_common.Step.equal k k' -> Some (i : a)
            | B (B k, b), B k' when Interp_common.Step.equal k k' -> Some (b : a)
            | _ -> None
          )
        else
          default.get key (* input is completely unconstrained, so use the default *)
      in
      f
  }
end
