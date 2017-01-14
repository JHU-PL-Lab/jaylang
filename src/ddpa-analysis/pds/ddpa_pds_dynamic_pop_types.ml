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
    | Immediate_filter_validation of
        abstract_var * Pattern_set.t * abstract_value
    (** The second step of handling immediate filter validation.  If the
        variable matches our lookup variable, only the provided set of filters
        is permitted on the positive side.  On the negative side, none of this
        set of filters may be present.  If this is successful, the result will
        be the specified abstract value with no filters. *)
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
    | Empty_record_value_discovery of abstract_var
    (** Represents the discovery of an empty record value assuming that the
        current lookup variable matches the one provided here. *)
    | Dereference_lookup of abstract_var * abstract_var
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
    | Side_effect_search_start_function_flow_check_1_of_2 of
        C.t * annotated_clause * abstract_clause * abstract_var
    (** Represents the start of a function flow check for a side effect search.
        The provided variable must *not* be the lookup variable (as that would
        indicate that we're looking for the return of the function and other
        rules would apply).  The given annotated clause is the return point
        (acl0) while the abstract clause is the call site. *)
    | Side_effect_search_start_function_flow_check_2_of_2 of
        C.t * annotated_clause * abstract_clause * Pds_continuation.t
    (** Represents the second part of a function flow check for a side effect
        search.  This step pops the deref instruction from the stack to ensure
        that it is there.  As opposed to the first step, the abstract variable
        for which we must *not* be looking has been replaced by the stack
        element representing the variable for which we *are* looking, as we will
        need to re-push it. *)
    | Side_effect_search_start_function_flow_validated_1_of_4 of
        annotated_clause * C.t * abstract_var * abstract_var
    (** The first step of starting a side-effect search after the called
        function has been validated.  This step merely pops the RealFlow?
        marker.  The arguments are the return clause (acl0) and context, the
        variable which we must *not* be seeking, and the required return
        variable of the function. *)
    | Side_effect_search_start_function_flow_validated_2_of_4 of
        annotated_clause * C.t * abstract_var * abstract_var
    (** The second step of starting a side-effect search after the called
        function has been validated.  This step pops the function value and
        confirms that it has the correct return variable. *)
    | Side_effect_search_start_function_flow_validated_3_of_4 of
        annotated_clause * C.t * abstract_var
    (** The third step of starting a side-effect search after the called
        function has been validated.  This step ensures that we're not looking
        for the return flow of the function. *)
    | Side_effect_search_start_function_flow_validated_4_of_4 of
        annotated_clause * C.t * Pds_continuation.t
    (** The fourth step of starting a side-effect search after the called
        function has been validated.  This step confirms the existence of a
        deref element.  The third argument has been replaced with the variable
        lookup element which we must repush. *)
    | Side_effect_search_start_conditional_1_of_2 of
        annotated_clause * annotated_clause * C.t
    (** The first step of starting a side-effect search with a conditional.
        The arguments are the exit wiring node, the return node (acl0), and the
        context in which the search is occurring. *)
    | Side_effect_search_start_conditional_2_of_2 of
        annotated_clause * annotated_clause * C.t * Pds_continuation.t
    (** The second step of starting a side-effect search with a conditional.
        The provided argument is the additional stack element which represents
        the variable lookup which we must repush. *)
    | Side_effect_search_nonmatching_clause_skip
    (** Represents the action of skipping any immediate, unannotated,
        non-update clause while searching for side effects. *)
    | Side_effect_search_function_bottom_flow_check of
        annotated_clause * annotated_clause * C.t
    (** Handles a function wiring exit during a side-effect search.  The
        arguments are the exit wiring node, the return node (acl0), and the
        context in which the search is occurring. *)
    | Side_effect_search_function_bottom_flow_validated_1_of_3 of abstract_var
    (** Handles a function wiring exit during a side-effect search.  This step
        simply pops the RealFlow? symbol.  The argument is the function's return
        variable from the wiring node (used to validate the flow). *)
    | Side_effect_search_function_bottom_flow_validated_2_of_3 of abstract_var
    (** Handles a function wiring exit during a side-effect search.  This step
        verifies the function flow.  The argument is the function's return
        variable from the wiring node (used to validate the flow). *)
    | Side_effect_search_function_bottom_flow_validated_3_of_3
    (** Handles a function wiring exit during a side-effect search.  This step
        verifies that we are performing a side-effect search. *)
    | Side_effect_search_conditional of annotated_clause * C.t
    (** Handles conditionals during a side-effect search.  The arguments are the
        wiring node and search context. *)
    | Side_effect_search_top
    (** Handles a wiring entrance node for side-effect search. *)
    | Side_effect_search_complete_none_found
    (** Handles the termination of an unsuccessful side-effect search. *)
    | Side_effect_search_alias_analysis_start of
        annotated_clause * C.t * abstract_var
    (** Starts alias analysis within a side-effect search.  The provided
        arguments are the return clause (acl0) and context as well as the
        variable describing the cell being updated. *)
    | Side_effect_search_may_not_alias_1_of_4
    (** Handles the case in which an alias analysis during a side-effect search
        concludes that the alias may not hold. *)
    | Side_effect_search_may_not_alias_2_of_4
    (** Handles the case in which an alias analysis during a side-effect search
        concludes that the alias may not hold. *)
    | Side_effect_search_may_not_alias_3_of_4
    (** Handles the case in which an alias analysis during a side-effect search
        concludes that the alias may not hold. *)
    | Side_effect_search_may_not_alias_4_of_4
    (** Handles the case in which an alias analysis during a side-effect search
        concludes that the alias may not hold. *)
    | Side_effect_search_may_alias_1_of_4 of abstract_var
    (** Handles the case in which an alias analysis during a side-effect search
        concludes that the alias may hold.  This step pops the Alias? symbol.
        The argument is the variable which replaces the lookup value. *)
    | Side_effect_search_may_alias_2_of_4 of abstract_var
    (** Handles the case in which an alias analysis during a side-effect search
        concludes that the alias may hold.  This step pops the first value.
        The argument is the variable which replaces the lookup value. *)
    | Side_effect_search_may_alias_3_of_4 of abstract_var * abstract_value
    (** Handles the case in which an alias analysis during a side-effect search
        concludes that the alias may hold.  This step pops the second value and
        confirms that it is equal to the first.  The arguments are the variable
        which replaces the lookup value and the first cell value. *)
    | Side_effect_search_may_alias_4_of_4 of abstract_var
    (** Handles the case in which an alias analysis during a side-effect search
        concludes that the alias may hold.  This step pops the side-effect
        search symbol and replaces it with an escape symbol. *)
    | Side_effect_search_escape_incremental_1_of_2
    (** Handles the incremental case of escaping from side-effect search. *)
    | Side_effect_search_escape_incremental_2_of_2 of Pds_continuation.t
    (** Handles the incremental case of escaping from side-effect search.  The
        argument is the escape symbol. *)
    | Side_effect_search_escape_base_1_of_4
    (* Handles the base case of escaping from side-effect search. *)
    | Side_effect_search_escape_base_2_of_4 of abstract_var
    (* Handles the base case of escaping from side-effect search.  The argument
       is the escape variable. *)
    | Side_effect_search_escape_base_3_of_4 of abstract_var
    (* Handles the base case of escaping from side-effect search.  The argument
       is the escape variable. *)
    | Side_effect_search_escape_base_4_of_4 of abstract_var
    (* Handles the base case of escaping from side-effect search.  The argument
       is the escape variable. *)

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
    | Binary_operator_resolution_1_of_4 of abstract_var * binary_operator
    (** Represents the start of the resolution of a binary operator after its
        operands have been found.  The variable is the one defined by the
        operation; the operands are on the stack.  This action simply pops the
        Binary_operation header from the stack. *)
    | Binary_operator_resolution_2_of_4 of abstract_var * binary_operator
    (** The second step of binary operator resolution.  This step collects the
        SECOND operand if it is a valid operand for the given kind of
        operation.  (The operands are produced in opposite order on the
        stack.) *)
    | Binary_operator_resolution_3_of_4 of
        abstract_var * binary_operator * abstract_value
    (** The third step of binary operator resolution.  The variable is the one
        assigned by the operation; the value is the operation's second operand.
        This step collects the FIRST operand and transforms the operands into
        a result value.*)
    | Binary_operator_resolution_4_of_4 of
        abstract_var * abstract_value
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
        abstract_var * abstract_value
    (** The third step of binary operator resolution.  This step
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
    | Rewind_step of annotated_clause * C.t
    (** Represents the rewind step to the end of the current scope, to support
        _natural recursion_. *)
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
