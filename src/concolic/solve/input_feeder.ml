
open Core

type t = { get : 'a. 'a Stepkey.t -> 'a } [@@unboxed]

let zero : t =
  { get = 
    let f (type a) (key : a Stepkey.t) : a =
      match key with
      | I _ -> 0
      | B _ -> false
    in
    f
  }

let default : t =
  { get = 
    let f (type a) (key : a Stepkey.t) : a =
      match key with
      | I _ -> C_random.int_incl (-10) 10
      | B _ -> C_random.bool ()
    in
    f
  }

module Make (Z : Z3_api.S) = struct
  let from_model_and_subs (model : Z.model) (subs : Expression.Subst.t list) : t =
    let model_vars =
      Int.Set.of_list
      @@ Z.constrained_vars model
    in
    let sub_vars =
      Int.Set.of_list
      @@ List.map subs ~f:(function
        | I (k, _) -> Stepkey.extract k
        | B (k, _) -> Stepkey.extract k
      )
    in
    { get = 
      let f (type a) (key : a Stepkey.t) : a =
        if Set.mem model_vars @@ Stepkey.extract key then
          match Z.value_of_key model key with
          | Some i -> i
          | None -> default.get key
        else if Set.mem sub_vars @@ Stepkey.extract key then
          List.find_map_exn subs ~f:(fun sub ->
            match sub, key with
            | I (I k, i), I k' when k = k' -> Some (i : a)
            | B (B k, b), B k' when k = k' -> Some (b : a)
            | _ -> None
          )
        else
          default.get key (* input is completely unconstrained, so use the default *)
      in
      f
  }
end
