
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

let from_model (model : Z3.Model.model) : t =
  { get = 
    let f (type a) (key : a Stepkey.t) : a =
      match C_sudu.value_of_key model key with
      | Some i -> i
      | None -> default.get key
    in
    f
  }
