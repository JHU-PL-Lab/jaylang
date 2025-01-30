
open Core

module Input = struct
  type t =
    | Int of int
    | Bool of bool
    [@@deriving compare, sexp]
end

type t = { get : 'a. 'a Concolic_key.t -> 'a } [@@unboxed]

let zero : t =
  { get = 
    let f (type a) (key : a Concolic_key.t) : a =
      match key with
      | Int_key _ -> 0
      | Bool_key _ -> false
    in
    f
  }

let default : t =
  { get = 
    let f (type a) (key : a Concolic_key.t) : a =
      match key with
      | Int_key _ -> C_random.int_incl (-10) 10
      | Bool_key _ -> C_random.bool ()
    in
    f
  }

let from_model (model : Z3.Model.model) : t =
  { get = 
    let f (type a) (key : a Concolic_key.t) : a =
      match C_sudu.get_expr model key with
      | Some i -> i
      | None -> default.get key
    in
    f
  }
