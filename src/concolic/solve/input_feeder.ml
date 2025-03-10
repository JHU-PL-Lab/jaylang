
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
  let from_model (model : Z.model) : t =
    let vars =
      Z.constrained_vars model
      |> Int.Set.of_list
    in
    { get = 
      let f (type a) (key : a Stepkey.t) : a =
        if Set.mem vars @@ Stepkey.extract key
        then
          match Z.value_of_key model key with
          | Some i -> i
          | None -> default.get key
        else
          default.get key 
      in
      f
  }
end
