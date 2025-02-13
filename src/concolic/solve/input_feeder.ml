
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

module Make (Expr : Z3_intf.S) = struct
  let from_model (model : Z3.Model.model) : t =
    { get = 
      let f (type a) (key : a Stepkey.t) : a =
        match Expr.value_of_key model key with
        | Some i -> i
        | None -> default.get key
      in
      f
  }
end
