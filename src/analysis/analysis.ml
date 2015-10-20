(**
  This module gives an implementation of the CBA analysis.  It is parametric
  in the choice of context stack.
*)

open Batteries;;

open Analysis_context_stack;;
open Ast;;
open Ast_pretty;;
open Cba_graph;;
open Nondeterminism;;
open Pds_reachability_types_stack;;

let logger = Logger_utils.make_logger "Analysis";;

module Pattern_ord =
struct
  type t = pattern
  let compare = compare_pattern
end;;

module Pattern_set = Set.Make(Pattern_ord);;

let pp_pattern_set pats =
  String_utils.concat_sep_delim "{" "}" ", " @@ Enum.map pretty_pattern @@
    Pattern_set.enum pats
;;

module type Analysis_sig =
sig
  type cba_analysis
  
  val create_analysis : expr -> cba_analysis
  
  val values_of_variable : var -> cba_analysis -> Abs_value_set.t
end;;

(**
  A functor which constructs a CBA analysis module.
*)
module Make(C : Context_stack) =
struct
  let negative_pattern_set_selection record_type pattern_set =
    let (Record_value m) = record_type in
    let record_labels = Ident_set.of_enum @@ Ident_map.keys m in
    let relevant_patterns = pattern_set
      |> Pattern_set.enum
      |> Enum.filter
          (fun (Record_pattern m') ->
            Ident_set.subset (Ident_set.of_enum @@ Ident_map.keys m')
              record_labels)
    in
    (* This function selects a single label from a given pattern and constructs
       a pattern from it. *)
    let pick_pattern (Record_pattern m') =
      let open Nondeterminism_monad in
      let%bind (k,v) = pick_enum @@ Ident_map.enum m' in
      return @@ Record_pattern(Ident_map.singleton k v)
    in
    let open Nondeterminism_monad in
    let%bind selected_patterns = 
      Nondeterminism_monad.mapM pick_pattern relevant_patterns
    in
    return @@ Pattern_set.of_enum selected_patterns
  ;;

  let pattern_projection (Record_pattern m) label =
    try
      Some (Ident_map.find label m)
    with
    | Not_found -> None
  ;;

  let pattern_set_projection set label =
    set
    |> Pattern_set.enum
    |> Enum.map (flip pattern_projection label)
    |> Enum.filter_map identity
    |> Pattern_set.of_enum
  ;;

  let labels_in_record (Record_value m) =
    Ident_set.of_enum @@ Ident_map.keys m
  ;;

  let labels_in_pattern (Record_pattern m) =
    Ident_set.of_enum @@ Ident_map.keys m
  ;;

  let labels_in_pattern_set set =
    set
    |> Pattern_set.enum
    |> Enum.map labels_in_pattern
    |> Enum.fold Ident_set.union Ident_set.empty
  ;;
  
  type pds_continuation =
    | Lookup_var of var * Pattern_set.t * Pattern_set.t
    | Project of ident * Pattern_set.t * Pattern_set.t
    | Jump of annotated_clause * C.t
    | Deref of Pattern_set.t * Pattern_set.t
    | Capture
    | Continuation_value of abstract_value
    | Alias_huh
    [@@deriving ord]
  ;;

  module Pds_continuation_ord =
  struct
    type t = pds_continuation
    let compare = compare_pds_continuation
  end;;

  let pp_pds_continuation = function
    | Lookup_var(x,patsp,patsn) ->
      Printf.sprintf "%s(%s/%s)"
        (pretty_var x) (pp_pattern_set patsp) (pp_pattern_set patsn)
    | Project(i,patsp,patsn) ->
      Printf.sprintf ".%s(%s/%s)"
        (pretty_ident i) (pp_pattern_set patsp) (pp_pattern_set patsn)
    | Jump(acl,ctx) ->
      Printf.sprintf "Jump(%s,%s)" (pp_annotated_clause acl) (C.pretty ctx)
    | Deref(patsp,patsn) ->
      Printf.sprintf "!(%s,%s)" (pp_pattern_set patsp) (pp_pattern_set patsn)
    | Capture -> "Capture"
    | Continuation_value v -> pp_abstract_value v
    | Alias_huh -> "Alias?"
  ;;

  type pds_state =
    Pds_state of annotated_clause * C.t
    [@@deriving ord]
  ;;

  let pp_pds_state (Pds_state(acl,ctx)) =
    Printf.sprintf "(%s @ %s)"
      (pp_annotated_clause acl) (C.pretty ctx)
  ;;

  module Pds_state_ord =
  struct
    type t = pds_state
    let compare = compare_pds_state
  end;;

  module Cba_pds_reachability_basis = 
  struct
    type state = pds_state
    type stack_element = pds_continuation
    module State_ord = Pds_state_ord
    module Stack_element_ord = Pds_continuation_ord
    let pp_state = pp_pds_state
    let pp_stack_element = pp_pds_continuation
  end
  
  type pds_targeted_dynamic_pop_action =
    | Variable_discovery of var
      (** Represents the rule that, upon discovering the variable for which we
          are looking, we pop that variable from the stack and move on.  The
          payload is the variable which we could discover here. *)
    | Variable_aliasing of var * var
      (** Represents variable aliasing, as in "x = y".  The first variable
          is the one being declared; the second variable is the one being
          used.  This pop action is used for every rule which performs basic
          x-to-y aliasing regardless of whether the clause in question is
          annotated. *)
    | Stateless_nonmatching_clause_skip_1_of_2 of var
      (** Represents the first step of skipping a non-matching clause while
          stateless.  If we are searching for x, we may skip over any clause of
          the form x'' = b (even if b is non-immediate) as long as we are not
          trying to dereference x''.  The first step of this process captures
          the variable which we should *not* match.  The second step carries
          the variable continuation and confirms the absence of a dereference
          continuation. *)
    | Stateless_nonmatching_clause_skip_2_of_2 of pds_continuation
      (** The second step of skipping a non-matching clause while stateless. *)
    | Function_closure_lookup of var * var
      (** Represents a function closure lookup.  The first variable is the
          parameter of the function; the second variable is the function itself.
          If the lookup variable does not match the parameter, then this lookup
          is for a non-local and we must search for the function's definition
          first. *)
    | Conditional_closure_lookup of var * pattern * bool
      (** Represents a conditional closure lookup.  If the variable matches our
          lookup target, then we've learned something about it: that it matches
          the pattern (if the boolean is true) or that it does not (if the
          boolean is false).  If the variable does not match our lookup target,
          we have learned nothing. *)
    | Record_projection_lookup of var * var * ident
      (** Represents a record projection.  If the first variable matches our
          lookup target, then we've discovered that we are looking up the
          projection of the ident label from a record stored in the second
          variable. *)
    | Record_construction_lookup_1_of_2 of
        pds_state * pds_state * var * record_value
      (** Represents lookup as applied to a record construction node.  If the
          variable matches our lookup variable, then we've found our value and
          should take action.  The particular action to take depends upon
          whether the next stack element is a projection (in which case we
          should do the projection) or if it is not (in which case we should
          just validate the record filters).  The PDS states are the source and
          target of the operation (respectively) and are carried here for
          convenience. *)
      (* FIXME: Shouldn't rule 1b specifically exclude variables from which we
                are projecting values? *)
      (* FIXME: There's no good way for the caller to get at the filters which
                applied to the record under this model, right?  That seems a
                shame. *)
      (* FIXME: This doesn't work if the record is the eventual lookup target!
                (That is, this is a problem if the record lookup is on the
                bottom of the stack.)  Perhaps we need a distinguished
                start-of-stack element?  We could perhaps use such a thing to
                fix the above, too, by moving to a dedicated answer state when
                we pop the last stack element. *)
    | Record_construction_lookup_2_of_2 of
        pds_state * pds_state * var * record_value *
          Pattern_set.t * Pattern_set.t
      (** The second step of record construction lookup. *)
    | Function_filter_validation of var
      (** Represents the validation of filters for a function under lookup.  If
          the variable matches our lookup variable, any negative filters are
          admissible and can be erased (although positive filters cannot). *)
    (* TODO *)
    [@@deriving ord]
  ;;
  
  let pp_pds_targeted_dynamic_pop_action action =
    match action with
    | Variable_discovery x ->
      Printf.sprintf "Variable_discovery(%s)" (pretty_var x)
    | Variable_aliasing(x,x') ->
      Printf.sprintf "Variable_aliasing(%s,%s)" (pretty_var x) (pretty_var x')
    | Stateless_nonmatching_clause_skip_1_of_2(x) ->
      Printf.sprintf "Stateless_nonmatching_clause_skip_1_of_2(%s)"
        (pretty_var x)
    | Stateless_nonmatching_clause_skip_2_of_2(k) ->
      Printf.sprintf "Stateless_nonmatching_clause_skip_2_of_2(%s)"
        (pp_pds_continuation k)
    | Function_closure_lookup(x'',xf) ->
      Printf.sprintf "Function_closure_lookup(%s,%s)"
        (pretty_var x'') (pretty_var xf)
    | Conditional_closure_lookup(x,p,b) ->
      Printf.sprintf "Function_closure_lookup(%s,%s,%b)"
        (pretty_var x) (pretty_pattern p) b
    | Record_projection_lookup(x,x',l) ->
      Printf.sprintf "Record_projection_lookup(%s,%s,%s)"
        (pretty_var x) (pretty_var x') (pretty_ident l)
    | Record_construction_lookup_1_of_2(source_state,target_state,x,r) ->
      Printf.sprintf "Record_construction_lookup_1_of_2(%s,%s,%s,%s)"
        (pp_pds_state source_state) (pp_pds_state target_state) (pretty_var x)
          (pretty_record_value r)
    | Record_construction_lookup_2_of_2(
        source_state,target_state,x,r,patsp,patsn) ->
      Printf.sprintf "Record_construction_lookup_2_of_2(%s,%s,%s,%s,%s,%s)"
        (pp_pds_state source_state) (pp_pds_state target_state) (pretty_var x)
          (pretty_record_value r) (pp_pattern_set patsp) (pp_pattern_set patsn)
    | Function_filter_validation(x) ->
      Printf.sprintf "Function_filter_validation(%s)" (pretty_var x)
  ;;

  type pds_untargeted_dynamic_pop_action =
    | Do_jump
      (** The action for performing basic jump operations. *)
    [@@deriving ord]
  ;; (* TODO *)
  
  let pp_pds_untargeted_dynamic_pop_action action =
    match action with
    | Do_jump -> "Do_jump"
  ;;

  module Dph =
  struct
    type stack_element = pds_continuation;;
    type state = pds_state;;
    type targeted_dynamic_pop_action = pds_targeted_dynamic_pop_action;;
    type untargeted_dynamic_pop_action = pds_untargeted_dynamic_pop_action;;
    type stack_action =
      ( stack_element
      , targeted_dynamic_pop_action
      ) pds_stack_action
    ;;
    let compare_targeted_dynamic_pop_action =
      compare_pds_targeted_dynamic_pop_action;;
    let pp_targeted_dynamic_pop_action = pp_pds_targeted_dynamic_pop_action;;
    let compare_untargeted_dynamic_pop_action =
      compare_pds_untargeted_dynamic_pop_action;;
    let pp_untargeted_dynamic_pop_action =
      pp_pds_untargeted_dynamic_pop_action;;
    let perform_targeted_dynamic_pop element action =
      Nondeterminism_monad.enum @@
      let open Nondeterminism_monad in
      match action with
      | Variable_discovery x ->
        let%orzero (Lookup_var(x',patsp,patsn)) = element in
        [%guard equal_var x x'];
        [%guard Pattern_set.is_empty patsp];
        [%guard Pattern_set.is_empty patsn];
        (* If we've gotten this far, then we just need to pop the element in
           question; no other actions are required.  Strictly speaking, this
           could be accomplished with a static pop rather than a dynamic one.
           It's here for now because its presence homogenizes the rule
           implementations. *)
        return []
      | Variable_aliasing(x2,x1) ->
        let%orzero (Lookup_var(x',patsp,patsn)) = element in
        [%guard equal_var x' x2];
        (* We're looking for x2 and we've discovered here that it's aliased to
           x1. *)
        return [Push(Lookup_var(x1,patsp,patsn))]
      | Stateless_nonmatching_clause_skip_1_of_2 x'' ->
        let%orzero (Lookup_var(x,_,_)) = element in
        [%guard (not @@ equal_var x x'')];
        (* We're looking for a variable which does not match the one in this
           clause.  If we're stateless, that'll be fine. *)
        return [Pop_dynamic_targeted(
                  Stateless_nonmatching_clause_skip_2_of_2 element)]
      | Stateless_nonmatching_clause_skip_2_of_2 element' ->
        begin
          match element with
          | Deref(_,_) ->
            (* This means we're in a stateful mode.  Stateless non-matching
               clause skip is inappropriate here. *)
            zero ()
          | _ ->
            (* We're not in a stateful mode, so we can skip the clause.  We
               still have to put these elements back on the stack, though. *)
            return [Push(element);Push(element')]
        end
      | Function_closure_lookup(x'',xf) ->
        let%orzero (Lookup_var(x,_,_)) = element in
        [%guard (not @@ equal_var x x'')];
        (* We're looking for a non-local variable.  Push a lookup for the
           function. *)
        return [ Push(element)
               ; Push(Lookup_var(xf,Pattern_set.empty,Pattern_set.empty))
               ]
      | Conditional_closure_lookup(xc,pat,positive_side) ->
        let%orzero (Lookup_var(x,patsp,patsn)) = element in
        if not @@ equal_var x xc
        then return [Push(element)]
        else
          let (patsp',patsn') =
            if positive_side
            then (Pattern_set.add pat patsp,patsn)
            else (patsp,Pattern_set.add pat patsn)
          in
          return [Push(Lookup_var(x,patsp',patsn'))]
      | Record_projection_lookup(x,x',l) ->
        let%orzero (Lookup_var(x0,patsp,patsn)) = element in
        [%guard (equal_var x0 x)];
        return [ Push(Project(l,patsp,patsn))
               ; Push(Lookup_var(x',Pattern_set.empty,Pattern_set.empty))
               ]
      | Record_construction_lookup_1_of_2(source_state,taret_state,x,r) ->
        let%orzero (Lookup_var(x0,patsp,patsn)) = element in
        [% guard (equal_var x x0) ];
        return @@ List.of_enum @@ Nondeterminism_monad.enum @@
        begin
          let open Nondeterminism_monad in
          let%bind patsn' = negative_pattern_set_selection r patsn in
          return @@ Pop_dynamic_targeted(
                      Record_construction_lookup_2_of_2(
                        source_state,taret_state,x,r,patsp,patsn'))
        end
      | Record_construction_lookup_2_of_2(
          source_state,target_state,x,r,patsp0,patsn2) ->
        let Record_value m = r in
        let Pds_state(acl1,ctx1) = source_state in
        let Pds_state(acl0,ctx0) = target_state in
        begin
          match element with
          | Project(l,patsp1,patsn1) ->
            [% guard (Ident_map.mem l m) ];
            let x' = Ident_map.find l m in
            (* This case is record projection. *)
            return [ Push(Lookup_var( x'
                                    , (Pattern_set.union patsp1
                                        (pattern_set_projection patsp0 l))
                                    , (Pattern_set.union patsn1
                                        (pattern_set_projection patsn2 l))))
                   ; Push(Jump(acl0,ctx0))
                   ; Push(Lookup_var(x, patsp0, patsn2))
                   ]
          | _ ->
            (* This case is record validation. *)
            let record_labels = labels_in_record r in
            let pattern_set_labels = labels_in_pattern_set patsp0 in
            [%guard (Ident_set.subset pattern_set_labels record_labels) ];
            let make_k'' l =
              let x'' = Ident_map.find l m in
              List.enum [ Push(Lookup_var( x''
                                         , pattern_set_projection patsp0 l
                                         , pattern_set_projection patsn2 l))
                        ; Push(Jump(acl1,ctx1))
                        ]
            in
            let first_pushes =
              List.enum [ Push(element)
                        ; Push(Lookup_var( x
                                         , Pattern_set.empty
                                         , Pattern_set.empty))
                        ; Push(Jump(acl0,ctx0))
                        ]
            in
            let all_pushes =
              pattern_set_labels
              |> Ident_set.enum
              |> Enum.map make_k''
              |> Enum.concat
              |> flip Enum.append first_pushes
            in
            return @@ List.of_enum all_pushes
        end
      | Function_filter_validation(x) ->
        let%orzero (Lookup_var(x0,patsp,patsn)) = element in
        [% guard (equal_var x x0) ];
        [% guard (Pattern_set.is_empty patsp) ];
        (* The following isn't strictly necessary, but it'd be redundant to do
           this for an empty negative pattern set. *)
        [% guard (not @@ Pattern_set.is_empty patsn) ];
        return [ Push(Lookup_var(x0,Pattern_set.empty,Pattern_set.empty)) ]
    ;;
    let perform_untargeted_dynamic_pop element action =
      Nondeterminism_monad.enum @@
      let open Nondeterminism_monad in
      match action with
      | Do_jump ->
        begin
          let%orzero (Jump(acl1,ctx)) = element in
          return ([], Pds_state(acl1,ctx))
        end
    ;;
  end;;
  
  module Cba_pds_reachability = Pds_reachability.Make(Cba_pds_reachability_basis)(Dph);;

  (*
    Here, we're creating the analysis data type in an internal module with a
    signature that specifically abstracts its internals.  This helps us
    guarantee that the analysis is not directly manipulated outside of this
    module; that encapsulation helps us keep the CBA graph and the reachability
    graph in sync.
  *)
  module type Analysis_struct_sig =
  sig
    type cba_analysis
    val empty_analysis : cba_analysis
    val add_edge : cba_edge -> cba_analysis -> cba_analysis
  end;;

  module Analysis_struct : Analysis_struct_sig =
  struct
    type cba_analysis =
      Cba_analysis of cba_graph * Cba_pds_reachability.analysis
    ;;
    let empty_analysis =
      Cba_analysis(Cba_graph.empty, Cba_pds_reachability.empty)
    ;;
    let add_edge edge (Cba_analysis(cba_graph,pds_reachability) as analysis) =
      if Cba_graph.has_edge edge cba_graph then analysis else
        let (Cba_edge(acl1,acl0)) = edge in
        let edge_function (Pds_state(acl0',ctx)) =
          (* TODO: There should be a way to associate each edge function with
                   its corresponding acl0. *)
          if compare_annotated_clause acl0 acl0' <> 0 then Enum.empty () else
            let open Option.Monad in
            let zero () = None in
            let targeted_dynamic_pops = Enum.filter_map identity @@ List.enum
              [
                begin
                  let%orzero
                    (Unannotated_clause(Abs_clause(x, Abs_value_body _))) = acl0
                  in
                  (* x = v *)
                  return (Variable_discovery x,Pds_state(acl1,ctx))
                end
              ;
                begin
                  let%orzero
                    (Unannotated_clause(Abs_clause(x, Abs_var_body x'))) = acl0
                  in
                  (* x = x' *)
                  return (Variable_aliasing(x,x'),Pds_state(acl1,ctx))
                end
              ;
                begin
                  let%orzero (Enter_clause(x,x',c)) = acl0 in
                  (* x =(down)c x' *)
                  [%guard C.is_top c ctx];
                  let ctx' = C.pop ctx in
                  return (Variable_aliasing(x,x'),Pds_state(acl1,ctx'))
                end
              ;
                begin
                  let%orzero (Exit_clause(x,x',c)) = acl0 in
                  (* x =(up)c x' *)
                  let ctx' = C.push c ctx in
                  return (Variable_aliasing(x,x'),Pds_state(acl1,ctx'))
                end
              ;
                begin
                  let%orzero (Unannotated_clause(Abs_clause(x,_))) = acl0 in
                  (* x'' = b *)
                  return ( Stateless_nonmatching_clause_skip_1_of_2 x
                         , Pds_state(acl0,ctx)
                         )
                end
              ;
                begin
                  let%orzero (Enter_clause(x'',_,c)) = acl0 in
                  let%orzero (Abs_clause(_,Abs_appl_body(xf,_))) = c in
                  (* x'' =(down)c x' for functions *)
                  [%guard C.is_top c ctx];
                  let ctx' = C.pop ctx in
                  return ( Function_closure_lookup(x'',xf)
                         , Pds_state(acl1,ctx')
                         )
                end
              ;
                begin
                  (* This block represents *all* conditional closure
                     handling. *)
                  let%orzero (Enter_clause(x'',_,c)) = acl0 in
                  let%orzero (Abs_clause(_,Abs_conditional_body(x1,p,f1,_))) = c in
                  let Abs_function_value(f1x,_) = f1 in
                  (* x'' =(down)c x' for conditionals *)
                  [%guard C.is_top c ctx];
                  [%guard (not @@ equal_var x'' x1)];
                  let closure_for_positive_path = equal_var f1x x'' in
                  (* FIXME: should be popping ctx, yes? *)
                  return ( Conditional_closure_lookup
                            (x1,p,closure_for_positive_path)
                         , Pds_state(acl1,ctx)
                         )
                end
              ;
                begin
                  let%orzero
                    (Unannotated_clause(
                      Abs_clause(x,Abs_projection_body(x',l)))) = acl0
                  in
                  (* x = x'.l *)
                  return (Record_projection_lookup(x,x',l),Pds_state(acl1,ctx))
                end
              ;
                begin
                  let%orzero
                    (Unannotated_clause(
                      Abs_clause(x,Abs_value_body(Abs_value_record(r))))) = acl0
                  in
                  (* x = r *)
                  let source_state = Pds_state(acl1,ctx) in
                  let target_state = Pds_state(acl0,ctx) in
                  return ( Record_construction_lookup_1_of_2(
                             source_state,target_state,x,r)
                         , target_state
                         )
                end
              ;
                begin
                  let%orzero
                    (Unannotated_clause(Abs_clause(
                        x,Abs_value_body(Abs_value_function(_))))) = acl0
                  in
                  (* x = f *)
                  return (Function_filter_validation(x), Pds_state(acl0,ctx))
                end
              ]
            in
            targeted_dynamic_pops
            |> Enum.map
                (fun (action,state) -> ([Pop_dynamic_targeted(action)], state))
        in
        let untargeted_dynamic_pop_action_function (Pds_state(acl0',_)) =
          (* TODO: There should be a way to associate each action function with
                   its corresponding acl0. *)
          if compare_annotated_clause acl0 acl0' <> 0 then Enum.empty () else
            let open Option.Monad in
            (* let zero () = None in *)
            let untargeted_dynamic_pops = Enum.filter_map identity @@ List.enum
              [
                begin
                  return @@ Do_jump
                end
              ]
            in
            untargeted_dynamic_pops
        in
        let pds_reachability' =
          pds_reachability
          |> Cba_pds_reachability.add_edge_function edge_function
          |> Cba_pds_reachability.add_untargeted_dynamic_pop_action_function
              untargeted_dynamic_pop_action_function
        in
        let cba_graph' = Cba_graph.add_edge edge cba_graph in
        (Cba_analysis(cba_graph',pds_reachability'))
    ;;
  end;;

  include Analysis_struct;;

  let create_analysis e =
    let Abs_expr(cls) = lift_expr e in
    (* Put the annotated clauses together. *)
    let acls =
      List.enum cls
      |> Enum.map (fun x -> Unannotated_clause x)
      |> Enum.append (Enum.singleton Start_clause)
      |> flip Enum.append (Enum.singleton End_clause)
    in
    (* For each pair, produce a CBA edge. *)
    let rec mk_edges acls' =
      match Enum.get acls' with
      | None -> []
      | Some acl1 ->
        match Enum.peek acls' with
        | None -> []
        | Some acl2 ->
          Cba_edge(acl1,acl2) :: mk_edges acls'
    in
    let edges = mk_edges acls in
    List.fold_left (flip add_edge) empty_analysis edges
  ;;

  let values_of_variable =
    raise @@ Utils.Not_yet_implemented "values_of_variable"
  ;;


end;;
