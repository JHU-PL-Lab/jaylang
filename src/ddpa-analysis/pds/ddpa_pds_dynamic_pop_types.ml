open Batteries;;

open Core_ast;;
open Core_ast_pp;;
open Ddpa_abstract_ast;;
open Ddpa_context_stack;;

module Make
    (C : Context_stack)
    (S : (module type of Ddpa_pds_structure_types.Make(C)) with module C = C) =
struct
  module C = C;;
  module S = S;;
  open S;;
  type pds_targeted_dynamic_pop_action =
    | Value_drop
    (** An action to drop values from the top of the stack.  This is necessary
        for actions such as non-local lookup which perform a subordinate
        lookup but do not act upon the resulting value. *)
    | Value_discovery_2_of_2
    (** The second step of value discovery, which confirms that the special
        "bottom" stack element is next on the stack.  This step pops that
        element so that the targeted value state can be accepted (by empty
        stack).  Note that the first step of value discovery is untargeted. *)
    | Variable_aliasing of abstract_var * abstract_var
    (** Represents variable aliasing, as in "x = y".  The first variable
        is the one being declared; the second variable is the one being
        used.  This pop action is used for every rule which performs basic
        x-to-y aliasing regardless of whether the clause in question is
        annotated. *)
    | Stateless_nonmatching_clause_skip_1_of_2 of abstract_var
    (** Represents the first step of skipping a non-matching clause while
        stateless.  If we are searching for x, we may skip over any clause of
        the form x'' = b (even if b is non-immediate) as long as we are not
        trying to dereference x''.  The first step of this process captures
        the variable which we should *not* match.  The second step carries
        the variable continuation and confirms the absence of a dereference
        continuation. *)
    | Stateless_nonmatching_clause_skip_2_of_2 of Pds_continuation.t
    (** The second step of skipping a non-matching clause while stateless. *)
    | Value_capture_1_of_3
    (** Represents the first step of the value capture action.  This step
        pops and stores the value being captured. *)
    | Value_capture_2_of_3 of abs_filtered_value
    (** Represents the second step of the value capture action.  This step
        extracts the number of other stack elements to gather up before
        capturing the value. *)
    | Value_capture_3_of_3 of
        abs_filtered_value * Pds_continuation.t list * Bounded_capture_size.t
    (** Represents the third step of the value capture action.  This action
        collects other stack elements into a list until it has consumed as
        many as the original Capture stack element dictated.  It then pushes
        a value to the stack followed by all of the elements it collected. *)
    | Rewind_step of annotated_clause * C.t
    (** Represents the rewind step to the end of the current scope, to support
        _natural recursion_. *)
    | Function_call_flow_validation of
        abstract_var * abstract_var * annotated_clause * C.t *
        annotated_clause * C.t * abstract_var
    (** Represents the validation of a function at a call site to ensure that
        we only explore exit nodes which apply in this context.  The first
        variable is the called function. The second variable is the argument
        passed to the function.  The given states are used to build the targets
        to which to jump once the function argument has been validated and
        once a function value has been discovered.  The third variable is the
        call site for which this validation is occurring. *)
    | Function_call_flow_validation_resolution_1_of_2 of
        abstract_var * abstract_var
    (** The first step of resolving function call flow validation.  The first
        variable is the call site; the second variable is the return variable
        for the called function's wiring node. *)
    | Function_call_flow_validation_resolution_2_of_2 of
        abstract_var * abstract_var
    (** We've captured the "RealFlow?" and now expect to see a value. *)
    | Function_closure_lookup of
        abstract_var * abstract_var
    (** Represents a function closure lookup.  The first variable is the
        parameter of the function; the second variable is the function itself.
        If the lookup variable does not match the parameter, then this lookup
        is for a non-local and we must search for the function's definition
        first. *)
    | Conditional_closure_lookup of abstract_var * abstract_var * pattern * bool
    (** Represents a conditional closure lookup.  The first variable is the
        formal parameter of the case branch that we are leaving from the top.
        The second variable is the subject of the pattern match.  The provided
        pattern is the pattern against which the subject was matched; the
        boolean is true if the match succeeded and false otherwise. *)
    | Conditional_subject_validation of
        abstract_var * abstract_var * abstract_var * pattern * bool *
        annotated_clause * C.t
    (** Represents the handling of an exit wiring node for a conditional.  The
        first variable is the conditional site; the second variable is the
        return variable of the conditional branch.  The third variable is the
        subject of the conditional; it is followed by the conditional's
        pattern.  The boolean indicates whether this is the "then" branch
        (true) or the "else" branch (false).  The state and context refer to
        this wiring clause so a jump can be issued after the subject is
        validated in this branch. *)
    | Record_projection_lookup of abstract_var * abstract_var * ident
    (** Represents the start of a record projection.  If the first variable
        matches our lookup target, then we've discovered that we are looking
        up the projection of the ident label from a record stored in the
        second variable. *)
    | Record_projection_1_of_2
    (** Represents the processing of a record projection on the stack.  This
        action requires two steps: one to grab the record and one to grab the
        value. *)
    | Record_projection_2_of_2 of
        abstract_record_value * Pattern_set.t * Pattern_set.t
    (** The second step of handling record projection. *)
    | Function_filter_validation of abstract_var * abstract_function_value
    (** Represents the validation of filters for a function under lookup.  If
        the variable matches our lookup variable, only the `fun' filter on the
        positive set is admissible and anything but `fun' in the negative
        filters is admissible and can be erased. *)
    | Record_filter_validation of
        abstract_var * abstract_record_value * annotated_clause * C.t
    (** Represents the validation of filters for a record under lookup.  If
        the variable matches our lookup variable, we will perform a series
        of lookups on each field of the record to validate that this record
        does, in fact, exist.  Once those lookups succeed, the resulting
        record value will replace the lookup variable on the stack.  The
        PDS state given here is the source node (which follows the record
        assignment node in control flow); it is used to construct the
        subordinate lookups used in filter validation. *)
    | Int_filter_validation of abstract_var
    (** Represents the validation of filters for an integer under lookup.  If
        the variable matches our lookup variable, only the `int' filter on the
        positive set is admissible and anything but `int' in the negative
        filters is admissible and can be erased. *)
    | Bool_filter_validation of abstract_var * bool
    (** Represents the validation of filters for Boolean values under lookup.
        If the variable matches our lookup variable, only the `true'/`false'
        filter on the positive set is admissible and anything but `true'/`false'
        in the negative filters is admissible and can be erased. *)
    | String_filter_validation of abstract_var
    (** Represents the validation of filters for String values under lookup.  If
        the variable matches our lookup variable, only the `string' filter on
        the positive set is admissible and anything but `string' in the negative
        filters is admissible and can be erased. *)
    | Empty_record_value_discovery of abstract_var
    (** Represents the discovery of an empty record value assuming that the
        current lookup variable matches the one provided here. *)
    | Dereference_lookup of abstract_var * abstract_var
    (** Represents a dereferencing action.  The first variable is the lookup
        variable; the second variable is the variable it dereferences. *)
    | Cell_filter_validation of abstract_var * abstract_ref_value
    (** Represents the validation of filters for a cell under lookup.  If
        the variable matches our lookup variable, the cell (provided as the
        given abstract value) is pushed in its place. *)
    | Cell_dereference_1_of_2
    (** Represents the first step of dereferencing a located cell.  This is
        a stack reduction operation: the dereferenced cell must be on the
        value stack. *)
    | Cell_dereference_2_of_2 of abstract_ref_value
    (** Represents the second step of dereferencing a located cell.  The
        provided value is the cell in question. *)
    | Cell_update_alias_analysis_init_1_of_2 of
        abstract_var * Pds_state.t * Pds_state.t
    (** Represents the initialization of alias analysis for a given cell
        update.  This is used to determine if the update in question is
        modifying a cell for which we are looking via a different name.  The
        variable here is the cell being updated; the states are the source and
        target state of the original transition, respectively. *)
    | Cell_update_alias_analysis_init_2_of_2 of
        abstract_var * Pds_state.t * Pds_state.t *
        abstract_var * Pattern_set.t * Pattern_set.t
    (** Represents the second step of alias analysis initialization for a cell
        update.  The additional parameters are the contents of the
        continuation found during the first step. *)
    | Alias_analysis_resolution_1_of_5 of abstract_var
    (** Represents the final step of alias analysis which determines whether
        a cell may be an alias of the one currently under lookup.  The
        variable here is the name of the contents of the cell under
        consideration. *)
    | Alias_analysis_resolution_2_of_5 of abstract_var
    (** Alias analysis resolution after consuming the alias question from
        the stack. *)
    | Alias_analysis_resolution_3_of_5 of abstract_var * abstract_value
    (** Alias analysis resolution after consuming the first abstract value
        from the stack. *)
    | Alias_analysis_resolution_4_of_5 of abstract_var * bool
    (** Alias analysis resolution after consuming the second abstract value
        from the stack.  The boolean indicates whether the two abstract values
        were equal. *)
    | Alias_analysis_resolution_5_of_5 of
        abstract_var * bool * abstract_var * Pattern_set.t * Pattern_set.t
    (** Alias analysis resolution after consuming the lookup variable from the
        stack.  The additional elements here are the components of the lookup
        continuation. *)
    | Nonsideeffecting_nonmatching_clause_skip of abstract_var
    (** Represents the skip of a non-matching, non-stateful clause.  Although
        the rules indicate that this step should occur only when a deref
        exists on the stack, the rule overlaps with the stateless nonmatching
        clause skip in each occasion that a deref is not present.  So we
        simply drop that requirement here, as the overlap does not present
        a problem and it reduces the number of steps involved in performing
        this task.  The associated variable is the variable that a lookup must
        *not* match to be able to skip the clause. *)
    | Side_effect_search_init_1_of_2 of
        abstract_var * annotated_clause * C.t
    (** Represents the initialization of a search for side effects.  The
        provided variable must *not* be the lookup variable (since we'd be
        looking for an immediate definition in that case).  The clause and
        context represent the starting point of the side-effect search. *)
    | Side_effect_search_init_2_of_2 of
        abstract_var * Pattern_set.t * Pattern_set.t * annotated_clause * C.t
    (** Represents the initialization of a search for side effects.  At this
        point, all work has been performed except (1) validating the presence
        of a deref and (2) pushing the appropriate lookup continuations onto
        the stack. *)
    | Side_effect_search_nonmatching_clause_skip
    (** Represents the action of skipping any immediate, unannotated,
        non-update clause while searching for side effects. *)
    | Side_effect_search_exit_wiring
    (** Represents the action of moving into an exit wiring node during a
        side-effect search. *)
    | Side_effect_search_enter_wiring
    (** Represents the action of moving into an entrance wiring node during a
        side-effect search. *)
    | Side_effect_search_without_discovery
    (** Represents the end of a side-effect search which did not discover any
        relevant side effects. *)
    | Side_effect_search_alias_analysis_init of
        abstract_var * annotated_clause * C.t
    (** Represents the initialization of alias analysis for a cell update
        while in a side-effect search.  The variable is the cell being
        updated; the clause and context designate the state at which the
        alias analysis began. *)
    | Side_effect_search_alias_analysis_resolution_1_of_4 of abstract_var
    (** Represents the resolution of an alias analysis within a side-effect
        lookup.  The variable is the one being assigned to the cell. *)
    | Side_effect_search_alias_analysis_resolution_2_of_4 of abstract_var
    (** The second step of alias analysis resolution in a side-effect search.
        This step has consumed the "Alias?" question. *)
    | Side_effect_search_alias_analysis_resolution_3_of_4 of
        abstract_var * abstract_value
    (** The third step of alias analysis resolution in a side-effect search.
        The first abstract value has been consumed. *)
    | Side_effect_search_alias_analysis_resolution_4_of_4 of abstract_var * bool
    (** The last step of alias analysis resolution in a side-effect search.
        The variable is the one being assigned to a cell; the boolean
        indicates whether the particular value pair discovered here indicates
        aliasing or not. *)
    | Side_effect_search_escape_1_of_2
    (** The first step of processing a side-effect search escape.  This is
        used when the alias analysis of a cell update during a side-effect
        search successfully identifies a possible aliasing of the cell we are
        attempting to dereference.  This process is used to eliminate the
        stack frames which represent the side-effect search portion of the
        overall analysis so that lookup can proceed from the point at which
        the alias update is found. *)
    | Side_effect_search_escape_2_of_2 of abstract_var
    (** The second step of processing a side-effect search escape.  The
        variable is the one which was found to be the new value of the cell
        being dereferenced. *)
    | Side_effect_search_escape_completion_1_of_4
    (** Represents the completion of a side-effect search escape. *)
    | Side_effect_search_escape_completion_2_of_4 of abstract_var
    (** Represents the completion of a side-effect search escape.  The given
        variable is the one to which the aliased cell is being assigned. *)
    | Side_effect_search_escape_completion_3_of_4 of abstract_var
    (** Represents the completion of a side-effect search escape.  The given
        variable is the one to which the aliased cell is being assigned. *)
    | Side_effect_search_escape_completion_4_of_4 of abstract_var
    (** Represents the completion of a side-effect search escape.  The given
        variable is the one to which the aliased cell is being assigned. *)
    | Binary_operator_lookup_init of
        abstract_var * abstract_var * abstract_var *
        annotated_clause * C.t * annotated_clause * C.t
    (** Represents the kickstart of a process which looks up values for a
        binary operation.  The first variable above must be the
        current target of lookup.  The next two variables are the operands
        of the operation.  The remaining two pairs of values represent the
        source and target states of the DDPA edge. *)
    | Unary_operator_lookup_init of
        abstract_var * abstract_var * annotated_clause * C.t
    (** Represents the kickstart of a process which looks up values for a
        unary operation.  The first variable above must be the
        current target of lookup.  The second variable is the operand
        of the operation.  The state is the source states of the DDPA edge. *)
    | Indexing_lookup_init of
        abstract_var * abstract_var * abstract_var *
        annotated_clause * C.t * annotated_clause * C.t
    (** Represents the kickstart of a process which looks up values for
        indexing.  The first variable above must be the
        current target of lookup.  The next two variables are the subject
        and the index, respectively.  The remaining two pairs of values
        represent the source and target states of the DDPA edge. *)
    | Binary_operator_resolution_1_of_4 of abstract_var * binary_operator
    (** Represents the start of the resolution of a binary operator after its
        operands have been found.  The variable here is the one under
        lookup. *)
    | Binary_operator_resolution_2_of_4 of abstract_var * binary_operator
    (** The second step of binary operator resolution.  This step collects the
        first operand if it is a valid operand for the given kind of
        operation. *)
    | Binary_operator_resolution_3_of_4 of
        abstract_var * binary_operator * abstract_value
    (** The third step of binary operator resolution.  This step
        collects the second operand.  The `abstract_value' is the first operand
        accumulated on the previous step. *)
    | Binary_operator_resolution_4_of_4 of
        abstract_var * binary_operator * abstract_value
    (** The forth step of binary operator resolution.  This step
        collects and checks the lookup variable. The `abstract_value' is
        the result of the operation. A check guarantees that the given result
        is valid for the given operation. *)
    | Unary_operator_resolution_1_of_3 of abstract_var * unary_operator
    (** Represents the start of the resolution of a unary operator after its
        operands have been found.  The variable here is the one under
        lookup. *)
    | Unary_operator_resolution_2_of_3 of abstract_var * unary_operator
    (** The second step of unary operator resolution.  This step collects the
        operand if it is a valid operand for the given kind of
        operation. *)
    | Unary_operator_resolution_3_of_3 of
        abstract_var * unary_operator * abstract_value
    (** The third step of binary operator resolution.  This step
        collects and checks the lookup variable. The `abstract_value' is
        the result of the operation. A check guarantees that the given result
        is valid for the given operation. *)
    | Indexing_resolution_1_of_4 of abstract_var
    (** Represents the start of the resolution of indexing after its
        operands have been found.  The variable here is the one under
        lookup. *)
    | Indexing_resolution_2_of_4 of abstract_var
    (** The second step of binary operator resolution.  This step
        collects the index. *)
    | Indexing_resolution_3_of_4 of abstract_var
    (** The third step of binary operator resolution.  This step
        collects the subject. *)
    | Indexing_resolution_4_of_4 of abstract_var * abstract_value
    (** The forth step of binary operator resolution.  This step
        collects and checks the lookup variable. The `abstract_value' is
        the result of the operation. A check guarantees that the given result
        is valid for the given operation. *)
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Pds_targeted_dynamic_pop_action =
  struct
    type t = pds_targeted_dynamic_pop_action
    let equal = equal_pds_targeted_dynamic_pop_action
    let compare = compare_pds_targeted_dynamic_pop_action
    let pp = pp_pds_targeted_dynamic_pop_action
    let show = show_pds_targeted_dynamic_pop_action
    let to_yojson = pds_targeted_dynamic_pop_action_to_yojson
  end;;

  type pds_untargeted_dynamic_pop_action =
    | Do_jump
    (** The action for performing basic jump operations. *)
    | Value_discovery_1_of_2
    (** Represents the rule that, if a value is the only element on the stack,
        we transition to one of the result states.  To determine that the
        value is the only element, we must be able to pop the special "bottom"
        stack element, so this is the first step of the process (which pops
        the value).  Because the value dictates the target of the second
        step, this is an untargeted action.  The second step is targeted. *)
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Pds_untargeted_dynamic_pop_action =
  struct
    type t = pds_untargeted_dynamic_pop_action
    let equal = equal_pds_untargeted_dynamic_pop_action
    let compare = compare_pds_untargeted_dynamic_pop_action
    let pp = pp_pds_untargeted_dynamic_pop_action
    let show = show_pds_untargeted_dynamic_pop_action
    let to_yojson = pds_untargeted_dynamic_pop_action_to_yojson
  end;;
end;;
