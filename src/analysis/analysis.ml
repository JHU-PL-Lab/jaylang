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
  type pds_continuation =
    | Lookup_var of var * Pattern_set.t * Pattern_set.t
    | Project of ident * Pattern_set.t * Pattern_set.t
    | Jump of var * annotated_clause * C.t * Pattern_set.t * Pattern_set.t
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
    | Jump(x,acl,ctx,patsp,patsn) ->
      Printf.sprintf "Jump(%s,%s,%s,%s,%s)"
        (pretty_var x) (pp_annotated_clause acl) (C.pretty ctx)
          (pp_pattern_set patsp) (pp_pattern_set patsn)
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
  
  type pds_dynamic_pop_action =
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
    (* TODO *)
    [@@deriving ord]
  ;;
  
  let pp_pds_dynamic_pop_action action =
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
  ;;

  module Dph =
  struct
    type stack_element = pds_continuation;;
    type dynamic_pop_action = pds_dynamic_pop_action;;
    let compare_dynamic_pop_action = compare_pds_dynamic_pop_action;;
    let pp_dynamic_pop_action = pp_pds_dynamic_pop_action;;
    let perform_dynamic_pop element action =
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
        return [Pop_dynamic(Stateless_nonmatching_clause_skip_2_of_2 element)]
      | Stateless_nonmatching_clause_skip_2_of_2 element' ->
        match element with
        | Deref(_,_) ->
          (* This means we're in a stateful mode.  Stateless non-matching clause
             skip is inappropriate here. *)
          zero ()
        | _ ->
          (* We're not in a stateful mode, so we can skip the clause.  We still
             have to put these elements back on the stack, though. *)
          return [Push(element');Push(element)]
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
            let edge_actions = Enum.filter_map identity @@ List.enum
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
              ]
            in
            edge_actions
            |> Enum.map
                (* FIXME: we can't always have this be ctx *)
                (fun (action,state) -> ([Pop_dynamic(action)], state))
        in
        let pds_reachability' =
          Cba_pds_reachability.add_edge_function edge_function pds_reachability
        in
        let cba_graph' = Cba_graph.add_edge edge cba_graph in
        (Cba_analysis(cba_graph',pds_reachability'))
        
          
          (* TODO
            begin
              (* Variable aliasing for annotated entrance clauses. *)
              let%orzero Enter_clause(x,x',c) = acl1 in
              return @@
              (fun (Pds_state(acl0',ctx)) ->
                Enum.concat @@ Nondeterminism_monad.enum @@
                let open Nondeterminism_monad in
                [%guard equal_annotated_clause acl0 acl0'];
                [%guard C.is_top c ctx];
                let dynamic_pop element =
                  Nondeterminism_monad.enum @@
                  let%orzero (Lookup_var(x0,patsp,patsn)) = element in
                  [%guard equal_var x x0];
                  return @@ [Push (Lookup_var(x',patsp,patsn))]
                in
                return @@ Enum.singleton
                  ( [Pop_dynamic dynamic_pop]
                  , Pds_state(acl1, C.pop ctx) )
              )
            end
          ;
            begin
              (* Variable aliasing for annotated exit clauses. *)
              let%orzero Exit_clause(x,x',c) = acl1 in
              return @@
              (fun (Pds_state(acl0',ctx)) ->
                Enum.concat @@ Nondeterminism_monad.enum @@
                let open Nondeterminism_monad in
                [%guard equal_annotated_clause acl0 acl0'];
                let dynamic_pop element =
                  Nondeterminism_monad.enum @@
                  let%orzero (Lookup_var(x0,patsp,patsn)) = element in
                  [%guard equal_var x x0];
                  return @@ [Push (Lookup_var(x',patsp,patsn))]
                in
                return @@ Enum.singleton
                  ( [Pop_dynamic dynamic_pop]
                  , Pds_state(acl1, C.push c ctx) )
              )
            end
          ]
          *)
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
