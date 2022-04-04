(* This module provides an error checker for Odefa programs which relies upon
   DDPA to find problems such as non-function application, non-record
   projection, etc. *)

open Toploop_analysis_types;;
open Toploop_ddpa_wrapper_types;;

module Make :
  functor (DDPA_wrapper : DDPA_wrapper) ->
    Analysis_sig with module DDPA_wrapper = DDPA_wrapper
;;
