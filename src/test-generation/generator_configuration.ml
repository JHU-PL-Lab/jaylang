(**
   This module defines the form of a configuration for test generation.
*)

open Odefa_ddpa;;

type configuration = {
  conf_context_model : (module Ddpa_context_stack.Context_stack);
};;
