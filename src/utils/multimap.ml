open Batteries;;

module type Multimap_sig =
sig
  type t
  type key
  type value

  module M : Map.S with type key = key
  module S : Set.S with type elt = value

  val empty : t

  val is_empty : t -> bool

  val num_keys : t -> int

  val num_values : t -> int

  val add : key -> value -> t -> t

  val find : key -> t -> value Enum.t

  val remove : key -> value -> t -> t

  val remove_all : key -> t -> t

  val mem : key -> value -> t -> bool

  val mem_any : key -> t -> bool

  val singleton : key -> value -> t

  val keys : t -> key Enum.t

  val enum : t -> (key * value) Enum.t

  val of_enum : (key * value) Enum.t -> t
end;;

module Make(Key_ord : BatInterfaces.OrderedType)
           (Value_ord : BatInterfaces.OrderedType)
  : Multimap_sig with type key = Key_ord.t and type value = Value_ord.t =
struct
  module M = Map.Make(Key_ord);;
  module S = Set.Make(Value_ord);;

  type t = Multimap of S.t M.t;;
  type key = Key_ord.t;;
  type value = Value_ord.t;;

  let empty = Multimap(M.empty);;

  let is_empty (Multimap m) = M.is_empty m;;

  let num_keys (Multimap m) = M.cardinal m;;

  let num_values (Multimap m) =
    M.enum m
    |> Enum.fold (fun a (_,v) -> a + S.cardinal v) 0
  ;;

  let find_internal k m =
    match M.Exceptionless.find k m with
      | None -> S.empty
      | Some v -> v
  ;;

  let add k v (Multimap m) =
    let old_set = find_internal k m in
    let new_set = S.add v old_set in
    Multimap(M.add k new_set m)
  ;;

  let find k (Multimap m) =
    match M.Exceptionless.find k m with
      | None -> Enum.empty ()
      | Some v -> S.enum v
  ;;

  let remove k v (Multimap m) =
    let old_set = find_internal k m in
    let new_set = S.remove v old_set in
    if S.is_empty new_set
    then Multimap(M.remove k m)
    else Multimap(M.add k new_set m)
  ;;

  let remove_all k (Multimap m) =
    Multimap(M.remove k m)
  ;;

  let mem k v (Multimap m) =
    find_internal k m |> S.mem v
  ;;

  let mem_any k (Multimap m) =
    M.mem k m
  ;;

  let singleton k v = Multimap(M.add k (S.singleton v) M.empty);;

  let keys (Multimap m) = M.keys m;;

  let enum (Multimap m) =
    M.enum m
    |> Enum.map
        (fun (k,vs) ->
          S.enum vs |> Enum.map (fun v -> (k,v))
        )
    |> Enum.concat
  ;;

  let of_enum =
    Enum.fold (fun a (k,v) -> add k v a) empty
  ;;
end;;
