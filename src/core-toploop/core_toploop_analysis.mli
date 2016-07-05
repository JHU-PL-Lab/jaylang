(* This module provides an error checker for Odefa programs which relies upon
   DDPA to find problems such as non-function application, non-record
   projection, etc. *)

open Core_toploop_analysis_types;;
open Core_toploop_ddpa_wrapper_types;;

module Make :
  functor (DDPA_wrapper : DDPA_wrapper) ->
    Analysis_sig with module DDPA_wrapper = DDPA_wrapper
;;
