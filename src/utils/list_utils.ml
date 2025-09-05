open Core

let map_in_context
    (type a b) (xs : a list)
    ~(f:(before:a list -> after:a list -> idx:int -> item:a -> b))
  : b list =
  List.mapi xs ~f:(fun n x ->
      let before = List.take xs n in
      let after = List.drop xs (n+1) in
      f ~before ~after ~idx:n ~item:x
    )

let[@inline always] rec take_first_mapped
    (xs : 'a list) ~(f:('a -> 'b option)) : 'b option =
  match xs with
  | [] -> None
  | x::xs' ->
    match f x with
    | Some y -> Some y
    | None -> take_first_mapped xs' ~f