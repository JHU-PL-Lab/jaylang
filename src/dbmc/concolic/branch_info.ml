open Core

module Status =
  struct
    type t = 
      | Found_abort
      | Hit
      | Unknown
      | Unhit
      [@@deriving variants, compare, sexp]

    let red_string = Format.sprintf "\027[31m%s\027[0m"

    let to_string (x : t) : string =
      match x with
      | Found_abort -> Variants.to_name x |> String.capitalize |> red_string
      | _ -> 
        String.capitalize
        @@ Variants.to_name x

    let update (new_status : t) (old_status : t) : t =
      match old_status with
      | Found_abort -> old_status
      | Hit -> begin
        match new_status with
        | Found_abort -> new_status
        | _ -> old_status
      end
      | Unknown -> begin
        match new_status with
        | Hit | Found_abort -> new_status
        | _ -> old_status
      end
      | Unhit -> new_status
  end

module M = Map.Make (Branch)

type t = Status.t M.t [@@deriving compare]

let empty = M.empty

let add_branch_id (map : t) (id : Jayil.Ast.ident) : t =
  let set_unhit = function
    | Some _ -> failwith "adding duplicate branch ident"
    | None -> Status.Unhit
  in
  map
  |> Fn.flip Map.update Branch.{ branch_ident = id ; direction = Branch.Direction.True_direction } ~f:set_unhit
  |> Fn.flip Map.update Branch.{ branch_ident = id ; direction = Branch.Direction.False_direction } ~f:set_unhit

let rec find_branches (e : Jayil.Ast.expr) (m : t) : t =
  let open Jayil.Ast in
  let (Expr clauses) = e in
  List.fold clauses ~init:m ~f:(fun m clause -> find_branches_in_clause clause m)

and find_branches_in_clause (clause : Jayil.Ast.clause) (m : t) : t =
  let open Jayil.Ast in
  let (Clause (Var (x, _), cbody)) = clause in
  match cbody with
  | Conditional_body (_, e1, e2) ->
    add_branch_id m x
    |> find_branches e1
    |> find_branches e2
  | Value_body (Value_function (Function_value (_, e))) ->
    find_branches e m
  | _ -> m (* no branches in non-conditional or value *)

let of_expr (e : Jayil.Ast.expr) : t =
  find_branches e empty

let to_list (map : t) : (string * Status.t * Status.t) list =
  Map.to_alist map ~key_order:`Increasing
  |> List.chunks_of ~length:2
  |> List.map ~f:(function
    | [ ({ branch_ident = Ident a; direction = True_direction }, true_status)
      ; ({ branch_ident = Ident b; direction = False_direction }, false_status) ]
      when String.(a = b) ->
      (a, true_status, false_status)
    | _ -> failwith "malformed status store during to_list"
  )

(* depends on well-formedness of the map from loading in branches *)
let to_string (map : t) : string = 
  "Branch Information:\n" ^ begin
  let pad =
    map
    |> to_list
    |> List.fold ~init:0 ~f:(fun m (e, _, _) -> let l = String.length e in if l > m then l else m)
    |> fun l -> (fun s -> s ^ String.make (l - String.length s) ' ')
  in
  map
  |> to_list
  |> List.map ~f:(fun (s, true_status, false_status) ->
      Format.sprintf "%s : True=%s; False=%s\n"
        (pad s)
        (Status.to_string true_status)
        (Status.to_string false_status)
    )
  |> String.concat
  end

(* throws exception on unbound branch *)
(* let update_branch_status ~(new_status : Status.t) (map : t) (branch : Branch.t) : t =
  Map.update map branch ~f:(function
    | Some old_status -> Status.update new_status old_status
    | None -> failwith "unbound branch" 
  ) *)

(* overwrites unbound branch *)
let set_branch_status ~(new_status : Status.t) (map : t) (branch : Branch.t) : t =
  Map.set map ~key:branch ~data:new_status

(* let is_hit (map : t) (branch : Branch.t) : bool =
  match Map.find map branch with
  | Some Hit -> true
  | _ -> false *)

let contains (map : t) (status : Status.t) : bool =
  Map.exists map ~f:(fun v -> Status.compare v status = 0)

(* merges [m2] into [m1] with [Status.update] *)
let merge (m1 : t) (m2 : t) : t =
  Map.merge m1 m2 ~f:(fun ~key:_ -> function
    | `Both (old_status, new_status) -> Some (Status.update new_status old_status)
    | `Left old_status -> Some old_status
    | `Right new_status -> failwith "only new status for branch when old status is expected"
    )

let find (map : t) ~(f : Branch.t -> Status.t -> bool) : (Branch.t * Status.t) option =
  Map.fold_until
    map
    ~init:()
    ~f:(fun ~key ~data () ->
      if f key data
      then Stop (Some (key, data))
      else Continue ()
    )
    ~finish:(fun () -> None)

(* Custom sexp conversions so that it's easier to hand-write the sexp files *)
module Sexp_conversions =
  struct
    module My_tuple =
      struct
        (* branch name, true status, false status *)
        type t = string * Status.t * Status.t [@@deriving sexp]
      end

    let sexp_of_t (map : t) : Sexp.t =
      map
      |> to_list
      |> List.sexp_of_t My_tuple.sexp_of_t
    
    let t_of_sexp (sexp : Sexp.t) : t =
      sexp
      |> List.t_of_sexp My_tuple.t_of_sexp
      |> List.fold ~init:empty ~f:(fun acc (id, true_status, false_status) ->
        acc
        |> Map.set ~key:({ branch_ident = Jayil.Ast.Ident id ; direction = Branch.Direction.True_direction}) ~data:true_status
        |> Map.set ~key:({ branch_ident = Jayil.Ast.Ident id ; direction = Branch.Direction.False_direction}) ~data:false_status
      )
  end

include Sexp_conversions
