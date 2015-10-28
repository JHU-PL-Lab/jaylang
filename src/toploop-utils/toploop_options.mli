(** This logging option sets the global logging state of the application.  It
    "produces" a unit but, as each argument is parsed, has the side effect of
    configuring the logger. *)
val logging_option : unit BatOptParse.Opt.t

(** This logging option selects a particular CBA analysis to perform. *)
val select_analysis_option :
  (module Analysis.Analysis_sig) option BatOptParse.Opt.t

(** This logging option configures how CBA DOT graphs are logged. *)
val cba_logging_option : Cba_graph_logger.level BatOptParse.Opt.t
