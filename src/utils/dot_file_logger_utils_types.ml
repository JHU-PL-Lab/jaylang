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
  
  (** A type describing the namespace of the logging actions in the module. *)
  type name
  
  (** A type describing the data which is logged by this module. *)
  type data
  
  (** A type used to identify nodes in a DOT graph. *)
  type dot_node_id
  
  (** Translates a node identity into its string representation for the DOT
      graph. *)
  val string_of_dot_node_id : dot_node_id -> string
  
  (** A function to generate the DOT graph for a log datum. *)
  val graph_of :
    data -> dot_node_id dot_node Enum.t * dot_node_id dot_edge Enum.t
    
  (** A function which translates a namespace value into a string.  This is used
      as both the filename in which to save the graph as well as the title of
      the graph within the file. *)
  val string_of_name : name -> string
end;;

module type Dot_file_logger_sig =
sig
  include Dot_file_logger_base;;

  val get_level : unit -> level

  val set_level : level -> unit
  
  val log : level -> name -> data -> unit
end;;
