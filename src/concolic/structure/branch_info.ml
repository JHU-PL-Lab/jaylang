open Core

module Status =
  struct
    type t = 
      | Found_abort of (Jil_input.t list [@compare.ignore])
      | Type_mismatch of (Jayil.Ast.Ident_new.t * Jil_input.t list [@compare.ignore])
      | Hit
      | Unknown
      | Unhit
      [@@deriving variants, compare, sexp]

    let red_string = Format.sprintf "\027[31m%s\027[0m"

    let to_string (x : t) : string =
      match x with
      | Found_abort _ -> Variants.to_name x |> String.capitalize |> red_string
      | _ -> 
        String.capitalize
        @@ Variants.to_name x

    let update (new_status : t) (old_status : t) : t =
      match old_status with
      | Found_abort _ -> old_status
      | Type_mismatch _ -> old_status
      | Hit -> begin
        match new_status with
        | Found_abort _ | Type_mismatch _ -> new_status
        | _ -> old_status
      end
      | Unknown -> begin
        match new_status with
        | Hit | Found_abort _ | Type_mismatch _ -> new_status
        | _ -> old_status
      end
      | Unhit -> new_status
  end

module M = Map.Make (Branch)

module M2 =
  struct
    type t = Status.t M.t [@@deriving compare]
  end

open M2

type t =
  { global : Status.t
  ; map    : M2.t
  ; branch_hit_counter : Branch_hit_counter.t }
  [@@deriving compare]

let return (m : M2.t) : t =
  { global = Status.Unhit
  ; map    = m
  ; branch_hit_counter = Branch_hit_counter.empty }

let (>==) x f = { x with map = f x.map }
let (>=|) x f = { x with branch_hit_counter = f x.branch_hit_counter }

let empty = return M.empty

let add_branch_id (map : t) (id : Jayil.Ast.ident) : t =
  let set_unhit = function
    | Some _ -> failwith "adding duplicate branch ident"
    | None -> Status.Unhit
  in
  map
  >== Fn.flip Map.update Branch.{ branch_ident = id ; direction = Branch.Direction.True_direction } ~f:set_unhit
  >== Fn.flip Map.update Branch.{ branch_ident = id ; direction = Branch.Direction.False_direction } ~f:set_unhit

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

(* TODO: fix how this doesn't have global branch *)
let to_list (map : t) : (string * Status.t * Status.t) list =
  Map.to_alist map.map ~key_order:`Increasing
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

(* overwrites unbound branch *)
let set_branch_status ~(new_status : Status.t) (map : t) (branch : Branch.Or_global.t) : t =
  match branch with
  | Global -> { map with global = new_status }
  | Branch key ->
    map >== Map.set ~key ~data:new_status >=| fun x ->
      match new_status with
      | Hit -> Branch_hit_counter.hit_branch x key
      | _ -> x

let contains ({ global ; map ; _ } : t) (status : Status.t) : bool =
  Status.compare global status = 0
  || Map.exists map ~f:(fun v -> Status.compare v status = 0)

(* merges [m2] into [m1] with [Status.update] *)
let merge (m1 : t) (m2 : t) : t =
  { global = Status.update m1.global m2.global
  ; map =
    Map.merge m1.map m2.map ~f:(fun ~key:_ -> function
      | `Both (old_status, new_status) -> Some (Status.update new_status old_status)
      | `Left old_status -> Some old_status
      | `Right new_status -> failwith "only new status for branch when old status is expected"
      )
  ; branch_hit_counter = Branch_hit_counter.merge m1.branch_hit_counter m2.branch_hit_counter
  }

let find ({ global ; map ; _ } : t) ~(f : Branch.Or_global.t -> Status.t -> bool) : (Branch.Or_global.t * Status.t) option =
  let open Branch.Or_global in
  if f Global global
  then Some (Global, global)
  else
    Map.fold_until
      map
      ~init:()
      ~f:(fun ~key ~data () ->
        if f (Branch key) data
        then Stop (Some (Branch key, data))
        else Continue ()
      )
      ~finish:(fun () -> None)

let get_hit_count ({ branch_hit_counter ; _ } : t) (branch : Branch.t) : int =
  Branch_hit_counter.get_count branch_hit_counter branch

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
        >== Map.set ~key:(Branch.{ branch_ident = Jayil.Ast.Ident id ; direction = Branch.Direction.True_direction}) ~data:true_status
        >== Map.set ~key:(Branch.{ branch_ident = Jayil.Ast.Ident id ; direction = Branch.Direction.False_direction}) ~data:false_status
      )
  end

include Sexp_conversions
