open Core
open Jayil.Ast
open Cfg

let block_map_of_expr e : t Ident_map.t =
  let map = ref Ident_map.empty in

  let main_block =
    let clauses = clauses_of_expr e in
    { id = Id.main_block; clauses; kind = Main }
  in
  map := Ident_map.add Id.main_block main_block !map ;

  let rec loop outer_id e =
    let (Expr clauses) = e in
    let handle_clause = function
      | Clause
          ( Var (cid, _),
            Value_body (Value_function (Function_value (Var (para, _), fbody)))
          ) ->
          let clauses = clauses_of_expr fbody in
          let block =
            { id = cid; clauses; kind = Fun { outer_id; para; callsites = [] } }
          in
          map := Ident_map.add cid block !map ;
          loop cid fbody
      | Clause (Var (cid, _), Conditional_body (Var (cond, _), e1, e2)) ->
          let make_block e beta =
            let clauses = clauses_of_expr e in
            {
              id = Id.cond_block_id cid beta;
              clauses;
              kind =
                Cond
                  {
                    outer_id;
                    condsite = cid;
                    cond;
                    choice = beta;
                    reachable = true;
                  };
            }
          in
          let block_then = make_block e1 true in
          let block_else = make_block e2 false in
          map := Ident_map.add block_then.id block_then !map ;
          map := Ident_map.add block_else.id block_else !map ;
          loop block_then.id e1 ;
          loop block_else.id e2
      | _ -> ()
    in
    List.iter clauses ~f:handle_clause
  in

  loop Id.main_block e ;
  !map
