open Batteries;;

open Odefa_ast;;

open Ast;;
open Constraint;;
open Interpreter_types;;
open Symbol_cache;;

type contradiction =
  | StackContradiction of
      Relative_stack.concrete_stack * Relative_stack.concrete_stack
  | TypeContradiction of
      symbol * Constraint.symbol_type * Constraint.symbol_type
  | ValueContradiction of symbol * value * value
  | ProjectionContradiction of symbol * symbol * ident
;;
exception Contradiction of contradiction;;

module Symbol_to_symbol_multimap = Jhupllib.Multimap.Make(Symbol)(Symbol);;

module Symbol_and_ident =
struct;;
  type t = symbol * ident [@@deriving ord];;
end;;

module Symbol_to_symbol_and_ident_multimap =
  Jhupllib.Multimap.Make(Symbol)(Symbol_and_ident)
;;

type t =
  { (** The set of all constraints in the solver. *)
    constraints : Constraint.Set.t;

    (** An index of all alias constraints for a particular symbol.  As a given
        symbol may be aliased to many other symbols, this is a multimap. *)
    alias_constraints_by_symbol : Symbol_to_symbol_multimap.t;

    (** An index of all value constraints by symbol.  As values are unique and
        no symbol may be constrained to multiple different values, this is just
        a normal dictionary. *)
    value_constraints_by_symbol : value Symbol_map.t;

    (** An index of all record projection constraints by the record symbol.
        As a given record symbol may be projected many times (and the results
        assigned to many symbols), this is a multimap. *)
    projection_constraints_by_record_symbol : Symbol_to_symbol_and_ident_multimap.t;

    (** An index of all symbol type constraints.  Because each symbol must have
        exactly one type, this is a normal dictionary. *)
    type_constraints_by_symbol : symbol_type Symbol_map.t;

    (** The unique stack constraint which may appear in this solver.  Only one
        stack constraint may appear in any particular solver because all stack
        constraints contradict with one another. *)
    stack_constraint : Relative_stack.concrete_stack option;

    choice_total : int;
    choices : Ident_set.t list;
    choice_map : symbol_type Ident_map.t;
  }
;;

type solution =
  (symbol -> Ast.value option) * Relative_stack.concrete_stack option
;;

let empty =
  { constraints = Constraint.Set.empty;
    alias_constraints_by_symbol = Symbol_to_symbol_multimap.empty;
    value_constraints_by_symbol = Symbol_map.empty;
    projection_constraints_by_record_symbol =
      Symbol_to_symbol_and_ident_multimap.empty;
    type_constraints_by_symbol = Symbol_map.empty;
    stack_constraint = None;

    choice_total = 0;
    choices = [];
    choice_map = Ident_map.empty;
  }
;;

let _binop_types (op : binary_operator)
  : symbol_type * symbol_type * symbol_type =
  match op with
  | Binary_operator_plus
  | Binary_operator_minus
  | Binary_operator_times
  | Binary_operator_divide
  | Binary_operator_modulus -> (IntSymbol, IntSymbol, IntSymbol)
  | Binary_operator_less_than
  | Binary_operator_less_than_or_equal_to
  | Binary_operator_equal_to -> (IntSymbol, IntSymbol, BoolSymbol)
  | Binary_operator_and
  | Binary_operator_or
  | Binary_operator_xor -> (BoolSymbol, BoolSymbol, BoolSymbol)
;;

let _get_type_of_symbol (symbol : symbol) (solver : t) : symbol_type option =
  Symbol_map.Exceptionless.find symbol solver.type_constraints_by_symbol
;;

let rec _add_constraints_and_close
    (constraints : Constraint.Set.t) (solver : t)
  : t =
  if Constraint.Set.is_empty constraints then solver else
    let (c, constraints') = Constraint.Set.pop constraints in
    if Constraint.Set.mem c solver.constraints then
      _add_constraints_and_close constraints' solver
    else
      let new_solver : t =
        match c with
        | Constraint_value(x,v) ->
          { solver with
            constraints = Constraint.Set.add c solver.constraints;
            value_constraints_by_symbol =
              begin
                begin
                  match Symbol_map.Exceptionless.find x
                          solver.value_constraints_by_symbol with
                  | None -> ();
                  | Some v' ->
                    if not (equal_value v v') then
                      raise @@ Contradiction(ValueContradiction(x,v,v'))
                end;
                Symbol_map.add x v solver.value_constraints_by_symbol
              end;
          }
        | Constraint_alias(x1,x2) ->
          { solver with
            constraints = Constraint.Set.add c solver.constraints;
            alias_constraints_by_symbol =
              Symbol_to_symbol_multimap.add x1 x2
                solver.alias_constraints_by_symbol
          }
        | Constraint_binop _ ->
          { solver with
            constraints = Constraint.Set.add c solver.constraints;
          }
        | Constraint_projection(x1,x2,lbl) ->
          { solver with
            constraints = Constraint.Set.add c solver.constraints;
            projection_constraints_by_record_symbol =
              Symbol_to_symbol_and_ident_multimap.add x2 (x1,lbl)
                solver.projection_constraints_by_record_symbol
          }
        | Constraint_type(x,t) ->
          { solver with
            constraints = Constraint.Set.add c solver.constraints;
            type_constraints_by_symbol =
              begin
                begin
                  match Symbol_map.Exceptionless.find x
                          solver.type_constraints_by_symbol with
                  | None -> ();
                  | Some t' ->
                    if not (equal_symbol_type t t') then
                      raise @@ Contradiction(TypeContradiction(x,t,t'))
                end;
                Symbol_map.add x t solver.type_constraints_by_symbol;
              end;
          }
        | Constraint_stack(s) ->
          (* begin
             match solver.stack_constraint with
             | Some s' ->
              begin
                if Relative_stack.equal_concrete_stack s s' then
                  ()
                else (
                  print_endline "I know it";
                  raise @@ Contradiction(StackContradiction(s,s'))
                )
              end;
             | None -> ()
             end; *)
          { solver with
            constraints = Constraint.Set.add c solver.constraints;
            stack_constraint = Some s;
          }
        | Constraint_ids _ ->
          { solver with
            constraints = Constraint.Set.add c solver.constraints;
          }
        | Constraint_and _ -> 
          { solver with
            constraints = Constraint.Set.add c solver.constraints;
          }
        | Constraint_exclusive _ ->
          { solver with
            constraints = Constraint.Set.add c solver.constraints;
          }
      in
      let new_constraints : Constraint.Set.t =
        match c with
        | Constraint_value(x,v) ->
          let transitivity_constraints =
            Symbol_to_symbol_multimap.find x solver.alias_constraints_by_symbol
            |> Enum.map (fun x' -> Constraint_value(x',v))
          in
          let projection_constraints =
            match v with
            | Record m ->
              solver.projection_constraints_by_record_symbol
              |> Symbol_to_symbol_and_ident_multimap.find x
              |> Enum.map
                (fun (x',lbl) ->
                   match Ident_map.Exceptionless.find lbl m with
                   | None ->
                     (* This means that we have two constraints.  One is a
                        record value assignment and the other is a projection
                        from that record.  But the projection is for a label
                        that the record doesn't have.  Contradiction! *)
                     raise @@ Contradiction(ProjectionContradiction(x',x,lbl))
                   | Some x'' ->
                     Constraint_alias(x',x'')
                )
            | Int _ | Bool _ | Function _ ->
              Enum.empty ()
          in
          let type_constraints =
            let t =
              match v with
              | Int _ -> IntSymbol
              | Bool _ -> BoolSymbol
              | Record _ -> RecordSymbol
              | Function f -> FunctionSymbol f
            in
            Enum.singleton (Constraint_type(x,t))
          in
          Constraint.Set.of_enum @@
          Enum.append transitivity_constraints @@
          Enum.append projection_constraints type_constraints
        | Constraint_alias(x,x') ->
          let symmetry_constraint =
            Enum.singleton(Constraint_alias(x',x))
          in
          let value_constraints =
            match Symbol_map.Exceptionless.find x
                    solver.value_constraints_by_symbol with
            | None -> Enum.empty ()
            | Some v -> Enum.singleton(Constraint_value(x',v))
          in
          let type_constraints =
            match Symbol_map.Exceptionless.find x
                    solver.type_constraints_by_symbol with
            | None -> Enum.empty ()
            | Some t -> Enum.singleton(Constraint_type(x',t))
          in
          Constraint.Set.of_enum @@
          Enum.append symmetry_constraint @@
          Enum.append value_constraints type_constraints
        | Constraint_binop(x,x',op,x'') ->
          let (tLeft,tRight,tOut) = _binop_types op in
          Constraint.Set.of_enum @@ List.enum @@
          [Constraint_type(x,tOut);
           Constraint_type(x',tLeft);
           Constraint_type(x'',tRight);
          ]
        | Constraint_projection(x,x',lbl) ->
          begin
            match Symbol_map.Exceptionless.find x'
                    solver.value_constraints_by_symbol with
            | None -> Constraint.Set.empty
            | Some(Int _ | Bool _ | Function _) -> Constraint.Set.empty
            | Some(Record record_body) ->
              match Ident_map.Exceptionless.find lbl record_body with
              | None ->
                (* This means that we have two constraints.  One is a
                   record value assignment and the other is a projection
                   from that record.  But the projection is for a label
                   that the record doesn't have.  Contradiction! *)
                raise @@ Contradiction(ProjectionContradiction(x,x',lbl))
              | Some x'' ->
                Constraint.Set.singleton @@ Constraint_alias(x,x'')
          end
        | Constraint_type(x,t) ->
          Symbol_to_symbol_multimap.find x solver.alias_constraints_by_symbol
          |> Enum.map (fun x' -> Constraint_type(x',t))
          |> Constraint.Set.of_enum
        | Constraint_stack _ ->
          Constraint.Set.empty
        | Constraint_ids _ ->
          Constraint.Set.empty
        | Constraint_and _ ->
          Constraint.Set.empty
        | Constraint_exclusive _ ->
          Constraint.Set.empty
      in
      _add_constraints_and_close
        (Constraint.Set.union new_constraints constraints)
        new_solver
;;

let add c solver =
  _add_constraints_and_close (Constraint.Set.singleton c) solver
;;

let singleton c = add c empty;;

let union s1 s2 =
  let (smaller, larger) =
    if Constraint.Set.cardinal s1.constraints <
       Constraint.Set.cardinal s2.constraints then
      (s1,s2)
    else
      (s2,s1)
  in
  _add_constraints_and_close smaller.constraints larger
;;

let z3_expr_of_symbol
    (ctx : Z3.context)
    (symbol_cache : symbol_cache)
    (solver : t)
    (symbol : symbol)
  : Z3.Expr.expr option =
  let z3symbol = define_symbol symbol_cache symbol in
  match _get_type_of_symbol symbol solver with
  | Some IntSymbol -> Some(Z3.Arithmetic.Integer.mk_const ctx z3symbol)
  | Some BoolSymbol -> Some(Z3.Boolean.mk_const ctx z3symbol)
  | Some (FunctionSymbol _) -> Some(Z3.Boolean.mk_const ctx z3symbol)
  | Some RecordSymbol -> None
  | None -> Some(Z3.Boolean.mk_const ctx z3symbol)
;;

let z3_symbol_of_counter ctx i =
  Z3.Symbol.mk_string ctx @@ "$$$cv" ^ (string_of_int i)

let z3_expr_of_stk_ids
    (ctx : Z3.context)
    (symbol_cache : symbol_cache)
    (solver : t)
    stk
    ids
  : Z3.Expr.expr option =
  (* print_endline "g1"; *)
  let symbol0 = Symbol (List.hd ids, stk) in
  let name = 
    (String.concat ","
     @@ List.map (fun (Ident name) -> name) ids
    ) ^ symbol_suffix_of_relative_stack stk in
  let (_ctx, r) = symbol_cache in
  (* print_endline "g2"; *)
  let z3symbol = match Symbol_map.Exceptionless.find symbol0 !r with
    | None ->
      let z3sym = Z3.Symbol.mk_string ctx @@ name in
      r := Symbol_map.add symbol0 z3sym !r;
      z3sym
    | Some z3sym -> z3sym
  in
  (* print_endline "g3"; *)
  match _get_type_of_symbol symbol0 solver with
  | Some IntSymbol -> Some(Z3.Arithmetic.Integer.mk_const ctx z3symbol)
  | Some BoolSymbol -> Some(Z3.Boolean.mk_const ctx z3symbol)
  | Some (FunctionSymbol _) -> Some(Z3.Boolean.mk_const ctx z3symbol)
  | Some RecordSymbol -> None
  | None -> Some(Z3.Arithmetic.Integer.mk_const ctx z3symbol)
;;

let z3_expr_of_value
    (ctx : Z3.context)
    (value : Constraint.value)
  : Z3.Expr.expr option =
  (match value with
   | Constraint.Int n -> Some(Z3.Arithmetic.Integer.mk_numeral_i ctx n)
   | Constraint.Bool b -> Some(Z3.Boolean.mk_val ctx b)
   | Constraint.Function _ -> None
   | Constraint.Record _ -> None)
;;

let z3_fn_of_operator
    (ctx : Z3.context)
    (operator : binary_operator)
  : (Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr) option =
  let z3_listop_to_binop f =
    fun arg1 arg2 -> f ctx [arg1;arg2]
  in
  match operator with
  | Binary_operator_plus -> Some(z3_listop_to_binop Z3.Arithmetic.mk_add)
  | Binary_operator_minus -> Some(z3_listop_to_binop Z3.Arithmetic.mk_sub)
  | Binary_operator_times -> Some(z3_listop_to_binop Z3.Arithmetic.mk_mul)
  | Binary_operator_divide -> Some(Z3.Arithmetic.mk_div ctx)
  | Binary_operator_modulus -> Some(Z3.Arithmetic.Integer.mk_mod ctx)
  | Binary_operator_less_than -> Some(Z3.Arithmetic.mk_lt ctx)
  | Binary_operator_less_than_or_equal_to -> Some(Z3.Arithmetic.mk_le ctx)
  | Binary_operator_equal_to -> Some(Z3.Boolean.mk_eq ctx)
  | Binary_operator_and -> Some(z3_listop_to_binop Z3.Boolean.mk_and)
  | Binary_operator_or -> Some(z3_listop_to_binop Z3.Boolean.mk_or)
  | Binary_operator_xor -> Some(Z3.Boolean.mk_xor ctx)
;;

let rec z3_constraint_of_constraint
    (ctx : Z3.context)
    (symbol_cache : symbol_cache)
    (solver : t)
    (c : Constraint.t)
  : Z3.Expr.expr option * t =
  print_endline @@ Printf.sprintf "%s"
    (Jhupllib.Pp_utils.pp_to_string Constraint.pp c);
  let open Option.Monad in
  let translate_symbol symbol =
    z3_expr_of_symbol ctx symbol_cache solver symbol
  in
  let translate_value value =
    z3_expr_of_value ctx value
  in
  match c with
  | Constraint_value(x,v) ->
    (let%bind z3x = translate_symbol x in
     let%bind z3v = translate_value v in
     Some(Z3.Boolean.mk_eq ctx z3x z3v)), solver
  | Constraint_alias(x1,x2) ->
    (* print_endline "a1"; *)
    (let%bind z3x1 = translate_symbol x1 in
     (* print_endline "a2"; *)
     let%bind z3x2 = translate_symbol x2 in
     (* print_endline "a3"; *)
     Some(Z3.Boolean.mk_eq ctx z3x1 z3x2)), solver
  | Constraint_binop(x1,x2,op,x3) ->
    (let%bind fn = z3_fn_of_operator ctx op in
     let%bind z3x1 = translate_symbol x1 in
     let%bind z3x2 = translate_symbol x2 in
     let%bind z3x3 = translate_symbol x3 in
     let binary_c = Z3.Boolean.mk_eq ctx z3x1 (fn z3x2 z3x3) in
     ( match op with
       | Binary_operator_divide
       | Binary_operator_modulus -> (
           let%bind z3zero = translate_value (Int(0)) in
           let is_zero = Z3.Boolean.mk_eq ctx z3x3 z3zero in
           let not_zero = Z3.Boolean.mk_not ctx is_zero in
           Some(Z3.Boolean.mk_and ctx [binary_c; not_zero]))
       | _ -> Some binary_c)), solver
  | Constraint_ids(s1, xs1, s2, xs2) -> (
      print_endline "c1";
      (let%bind z3x1 = z3_expr_of_stk_ids ctx symbol_cache solver s1 xs1 in
       print_endline "c2";
       let%bind z3x2 = z3_expr_of_stk_ids ctx symbol_cache solver s2 xs2 in
       print_endline "c3";
       let t = Z3.Boolean.mk_eq ctx z3x1 z3x2 in
       print_endline "c4";
       Some t), solver
    )
  | Constraint_projection _ ->
    None, solver
  | Constraint_type _ ->
    None, solver
  | Constraint_stack _ ->
    None, solver
  | Constraint_and (c1, c2) -> (
      print_endline "d1";
      let z3c1, s1 = z3_constraint_of_constraint ctx symbol_cache solver c1 in
      print_endline "d2";
      let z3c2, s2 =  z3_constraint_of_constraint ctx symbol_cache s1 c2 in
      print_endline "d3";
      match z3c1, z3c2 with 
      | Some p1, Some p2 -> (Some(Z3.Boolean.mk_and ctx [p1; p2]), s2)
      | _ -> None, s2
    )
  | Constraint_exclusive phis -> 
    print_endline "->";
    let payloads = List.map (fun phi -> 
        let phi', _ = z3_constraint_of_constraint ctx symbol_cache solver phi in
        match phi' with
        | Some p -> p
        | None -> failwith "ahaha"
      ) phis in
    print_endline "1";
    let choice_symbols = List.mapi (fun i _phi -> 
        let ci = i + solver.choice_total in
        z3_symbol_of_counter ctx ci
      ) phis in
    print_endline "2";
    let choice_consts = List.map (fun sym ->
        Z3.Boolean.mk_const ctx sym
      ) choice_symbols in
    print_endline "3";
    let choice_with_payload_list =
      List.map2i (fun i payload cv ->
          let exclusion = 
            List.remove_at i choice_consts 
            |> Z3.Boolean.mk_or ctx
            |> Z3.Boolean.mk_not ctx
          in
          let payload' = Z3.Boolean.mk_and ctx [payload; exclusion] in
          Z3.Boolean.mk_implies ctx cv payload'
        ) payloads choice_consts in
    print_endline "4";
    let must_one_true = Z3.Boolean.mk_or ctx choice_consts in
    print_endline "5";
    let z3all = Z3.Boolean.mk_and ctx @@ must_one_true::choice_with_payload_list in
    print_endline "<-";
    Some z3all, 
    { solver with
      choice_total = solver.choice_total + (List.length phis) }


;;
(* 
(declare-const a Bool)
(declare-const b Bool)
(declare-const c Bool)
(assert (=> a (and (not b) (not c))))
(assert (=> b (and (not a) (not c))))
(assert (=> c (and (not a) (not b))))
(assert (or a (or b c)))
      choice_total : int;
    choices : Ident_set.t list;
    choice_map : symbol_type Ident_map.t;
     *)

let solve (solver : t) : solution option =
  let ctx = Z3.mk_context [] in
  let z3 = Z3.Solver.mk_solver ctx None in
  let symbol_cache = new_symbol_cache ctx in

  (* print_endline "aaaa"; *)

  let z3constraints, _ =
    Set.fold (fun phi (z3phis, solver) -> 
        match z3_constraint_of_constraint ctx symbol_cache solver phi with
        | (Some phi), solver -> (phi :: z3phis), solver
        | None, solver -> z3phis, solver
      ) solver.constraints ([], solver)
  in
  Z3.Solver.add z3 z3constraints;

  (* print_endline "cccc"; *)
  print_endline @@ Z3.Solver.to_string z3;
  (* print_endline @@ String.concat "\n" @@ List.map Z3.Expr.to_string phis; *)

  match Z3.Solver.check z3 [] with
  | Z3.Solver.SATISFIABLE ->
    begin
      match Z3.Solver.get_model z3 with
      | None ->
        raise @@ Jhupllib.Utils.Invariant_failure
          "Z3 reports no model for a checked formula set"
      | Some model ->
        let get_value symbol =
          match z3_expr_of_symbol ctx symbol_cache solver symbol with
          | None -> None
          | Some expr ->
            begin
              match _get_type_of_symbol symbol solver with
              | Some IntSymbol ->
                begin
                  match Z3.Model.eval model expr true with
                  | None -> None
                  | Some expr' ->
                    (* Z3 documents a get_int function, but the latest on OPAM
                       doesn't seem to have it defined. *)
                    let n = Z3.Arithmetic.Integer.get_big_int expr' in
                    Some(Value_int(Big_int_Z.int_of_big_int n))
                end
              | Some BoolSymbol ->
                begin
                  match Z3.Model.eval model expr true with
                  | None -> None
                  | Some expr' ->
                    begin
                      match Z3.Boolean.get_bool_value expr' with
                      | Z3enums.L_TRUE -> Some(Value_bool true)
                      | Z3enums.L_FALSE -> Some(Value_bool false)
                      | Z3enums.L_UNDEF ->
                        raise @@ Jhupllib.Utils.Not_yet_implemented "L_UNDEF"
                    end
                end
              | Some (FunctionSymbol _) -> None
              | Some RecordSymbol ->
                (* TODO: look up the corresponding record *)
                raise @@ Jhupllib_utils.Not_yet_implemented "solution for record"
              | None -> None
            end
        in
        Some(get_value, solver.stack_constraint)
    end
  | Z3.Solver.UNSATISFIABLE ->
    (* Return no dictionary. *)
    None
  | Z3.Solver.UNKNOWN ->
    failwith @@ Printf.sprintf "Unknown result in solve: %s"
      (Z3.Solver.get_reason_unknown z3)
;;

let solvable solver =
  Option.is_some @@ solve solver
;;

let enum solver = Constraint.Set.enum solver.constraints;;

let of_enum constraints = Enum.fold (flip add) empty constraints;;

let iter fn solver = Constraint.Set.iter fn solver.constraints;;

let pp formatter solver =
  Constraint.Set.pp formatter solver.constraints
;;

let show solver = Jhupllib.Pp_utils.pp_to_string pp solver;;
