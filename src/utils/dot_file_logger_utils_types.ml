(**
  A type-only declaration module.  This module is intended to be re-exported
  from the Dot_file_logger_utils module interface and implementation; it should
  not be used directly.
*)

open Batteries;;

(** A type used to describe the properties of a node in a DOT graph. *)
type 'node_id dot_node =
  { dot_node_id : 'node_id
  ; dot_node_color : string option
  ; dot_node_text : string option
  }
;;

(** A type used to describe the properties of edges in a DOT graph. *)
type 'node_id dot_edge =
  { dot_edge_source : 'node_id
  ; dot_edge_target : 'node_id
  ; dot_edge_text : string option
  }
;;

module type Dot_file_logger_base =
sig
  (** A type describing the logging levels of the module. *)
  type level
  
  (** A function to compare logging levels. *)
  val compare_level : level -> level -> int
  
  (** A function to pretty print logging levels. *)
  val pp_level : level -> string
  
  (** A default logging level.  Used when no logging level is set. *)
  val default_level : level
  
  (** A type describing the different logging actions which may be taken by the
      module. *)
  type action
  
  (** A function which determines the logging level of an action. *)
  val level_of : action -> level
  
  (** A type used to identify nodes in a DOT graph. *)
  type dot_node_id
  
  (** Translates a node identity into its string representation for the DOT
      graph. *)
  val string_of_dot_node_id : dot_node_id -> string
  
  (** A function to generate the DOT graph for an action. *)
  val graph_of :
    action -> dot_node_id dot_node Enum.t * dot_node_id dot_edge Enum.t
    
  (** A function to select a name for the DOT graph for an action.  This is used
      as both the filename in which to save the graph as well as the title of
      the graph within the file. *)
  val name_of : action -> string
end;;

module type Dot_file_logger_sig =
sig
  include Dot_file_logger_base;;

  val get_level : unit -> level

  val set_level : level -> unit
  
  val log : action -> unit
end;;
