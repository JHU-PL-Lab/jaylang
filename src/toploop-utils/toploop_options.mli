(** This logging option sets the global logging state of the application.  It
    "produces" a unit but, as each argument is parsed, has the side effect of
    configuring the logger. *)
val logging_option : unit BatOptParse.Opt.t

(** This logging option selects a particular DDPA analysis to perform. *)
val select_context_stack_option :
  (module Analysis_context_stack.Context_stack) option BatOptParse.Opt.t

(** This logging option configures how DDPA DOT graphs are logged. *)
val ddpa_logging_option :
  Ddpa_graph_logger.ddpa_graph_logger_level BatOptParse.Opt.t

(** This logging option configures how PDS reachability DOT graphs are
    logged. *)
val pdr_logging_option :
  Pds_reachability_logger_utils.pds_reachability_logger_level BatOptParse.Opt.t

(** This option sets an end-of-program analysis variable filter.  If an analysis
    is configured, the toploop uses it to display the potential values appearing
    in certain variables.  This option permits the user to configure which
    variables will be analyzed. *)
val analyze_variables_option :
  (Ast.ident -> bool) BatOptParse.Opt.t
