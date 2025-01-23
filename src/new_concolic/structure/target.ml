
type t =
  { dir    : Direction.Packed.t
  ; path_n : int
  ; path   : Path.Reverse.t }

let make (rev_path : Path.Reverse.t) : t =
  match rev_path.backward_path with
  | [] -> failwith "bad empty target"
  | dir :: backward_path ->
    { dir
    ; path = { backward_path }
    ; path_n = List.length backward_path + 1 }

let compare (a : t) (b : t) : int =
  match Int.compare a.path_n b.path_n with
  | 0 -> begin
    match Direction.Packed.compare a.dir b.dir with
    | 0 -> Path.Reverse.compare a.path b.path
    | x -> x
  end 
  | x -> x

let to_path (target : t) : Path.t =
  Path.Reverse.to_forward_path
  @@ Path.Reverse.cons target.dir target.path

let append_path (path_to : Path.t) (target : t) : t =
  { dir = target.dir
  ; path_n = target.path_n + List.length path_to.forward_path
  ; path = Path.Reverse.concat target.path @@ Path.Reverse.of_forward_path path_to }
