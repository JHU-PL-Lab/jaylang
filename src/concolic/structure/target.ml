
let uid = ref 0 (* we can safely use this when making because it's promised that we never make a target twice *)

type t =
  { dir    : Direction.Packed.t
  ; path_n : int
  ; path   : Path.Reverse.t (* maybe should hashcons this instead of the uniq id hack to be more safe (but hashcons would be slower) *)
  ; uniq_id : int
  }

let make (rev_path : Path.Reverse.t) : t =
  match rev_path.backward_path with
  | [] -> failwith "bad empty target"
  | dir :: backward_path ->
    { dir
    ; path = { backward_path }
    ; path_n = List.length backward_path + 1
    ; uniq_id = (incr uid; !uid) }

(*
  SUPER IMPORTANT NOTE:
  * The concolic evaluator is currently built to only make each
    target once. Therefore, we can create a unique id upon target
    creation, and that is safe to use when comparing.
  * This will break if the concolic evaluator does not have this
    property, and it won't break loudly, so the developer must be
    very careful that this assumption continues to hold.
  * Use the commented comparison if that assumption no longer holds.
*)
let compare (a : t) (b : t) : int =
  Int.compare a.uniq_id b.uniq_id
  (* match Int.compare a.path_n b.path_n with
  | 0 -> begin
    match Direction.Packed.compare a.dir b.dir with
    | 0 -> Path.Reverse.compare a.path b.path
    | x -> x
  end 
  | x -> x *)

let to_rev_path (target : t) : Path.Reverse.t =
  Path.Reverse.cons target.dir target.path

let to_path (target : t) : Path.t =
  Path.Reverse.to_forward_path
  @@ to_rev_path target

let append_path (path_to : Path.t) (target : t) : t =
  { dir = target.dir
  ; path_n = target.path_n + List.length path_to.forward_path
  ; path = Path.Reverse.concat target.path @@ Path.Reverse.of_forward_path path_to
  ; uniq_id = (incr uid; !uid) }

let path_n ({ path_n ; _ } : t) : int =
  path_n

let dir ({ dir ; _ } : t) : Direction.Packed.t =
  dir
