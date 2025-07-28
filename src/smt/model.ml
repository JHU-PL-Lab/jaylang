
type 'k t = { value : 'a. ('a, 'k) Symbol.t -> 'a option }

let empty : 'k t = { value = fun _ -> None }
