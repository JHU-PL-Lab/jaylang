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
  
  module Cba_pds_reachability = Pds_reachability.Make(
    struct
      type state = pds_state
      type stack_element = pds_continuation
      module State_ord = Pds_state_ord
      module Stack_element_ord = Pds_continuation_ord
      let pp_state = pp_pds_state
      let pp_stack_element = pp_pds_continuation
    end
  );;

  open Cba_pds_reachability;;

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
    let add_edge edge (Cba_analysis(cba_graph,pda_reachability) as analysis) =
      if Cba_graph.has_edge edge cba_graph then analysis else
        let (Cba_edge(acl1,acl0)) = edge in
        let edge_functions =
          let open Monad.Option in
          Enum.filter_map identity @@ List.enum
          [
            (* Variable discovery.  Base cases do not lead to edges in the
               PDS, but eliminations of variables from the stack do. *)
            let%orzero
              Unannotated_clause(Abs_clause(x, Abs_value_body _)) = acl1
            in
            return @@
            (fun (Pds_state(acl0',ctx)) ->
              Enum.concat @@ Nondeterminism_monad.enum @@
              let open Nondeterminism_monad in
              [%guard equal_annotated_clause acl0 acl0'];
              return @@ Enum.singleton
                ( [Pop (Lookup_var(x,Pattern_set.empty,Pattern_set.empty))]
                , Pds_state(acl1,ctx) )
            )
          ]
        in
        let pda_reachability' =
          edge_functions |>
          Enum.fold (flip Cba_pds_reachability.add_edge_function)
            pda_reachability
        in
        let cba_graph' = Cba_graph.add_edge edge cba_graph in
        (Cba_analysis(cba_graph',pda_reachability'))
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
