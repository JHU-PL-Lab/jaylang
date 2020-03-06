open Batteries
open Odefa_ast
open Ast

module HybridMonad :
sig
  include Monad.Monad
  (* val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m *)
  val sequence : 'a m list -> 'a list m
  val make_table_for_clause : clause -> unit m
  val make_table : expr -> unit m
  val run : 'a m -> value Ident_map.t -> 'a * value Ident_map.t
end =
struct
  include Monad.Make(
    struct
      type 'a m = value Ident_map.t -> 'a * value Ident_map.t
      let return x : 'a m = 
        fun map -> x, map
      let bind (m : 'a m) (f : 'a -> 'b m) : 'b m = 
        fun map1 ->
          let x, map2 = m map1 in
          f x map2
    end
  )

  let run m env0 = m env0
  
  (* let ( let* ) = bind *)
  let rec sequence ms =
    match ms with
    | [] -> return []
    | h::t ->
      let%bind h' = h in
      let%bind t' = sequence t in
      return @@ h' :: t'

  (* Though flatten returns a list of the traversing sequence,
  the id is unique so it's still used for lexical scoping
(env : expr Ident_map.t) *)
  let make_table_for_clause clause : unit m =
  (* let open HybridMonad in
  *)
  fun (map : value Ident_map.t) -> 
    begin
      let Clause(Var(id, _fstack), cbody) = clause in
        match cbody with
        | Value_body (Value_int i) -> (
          let map2 = Ident_map.add id (Value_int i) map in
          (), map2)
        | Value_body (Value_bool b) -> (
          let map2 = Ident_map.add id (Value_bool b) map in
          (), map2)

        | Var_body (Var (id2, _fstack2)) -> (
          match Ident_map.Exceptionless.find id2 map with
          | Some v -> (
            let map2 = Ident_map.add id v map in
            (), map2
          )
          | None -> (), map
        )
        (* these three are eliminated by `flatten` *)
        | Conditional_body (_, _, _) 
        | Value_body (Value_record _)
        | Value_body (Value_function _)

        | Input_body 
        | Appl_body (_, _)
        | Match_body (_, _)
        | Projection_body (_, _)
        | Binary_operation_body (_, _, _)
         -> (), map
    end

  let make_table (e : expr) : unit m =
    let clauses = Ast_tools.flatten e in
    let%bind _ = sequence @@ List.map make_table_for_clause clauses in
    return ()
end

(* a lookup table for immediate values int or bool
  for any var, regardless of freshing stack, if it's bound to an int or bool
  in its lexical scope, it must be that int or bool. *)

let env_table (e : expr) : value Ident_map.t =
  let open HybridMonad in
  let _, map = run (make_table e) Ident_map.empty in
  map