module Cba_0stack = Analysis.Make(Analysis_unit_stack.Stack);;
module Cba_1stack = Analysis.Make(Analysis_single_element_stack.Stack);;
module Cba_2stack = Analysis.Make(Analysis_two_element_stack.Stack);;
module Cba_nonrepeating_stack =
  Analysis.Make(Analysis_nonrepeating_stack.Stack)
;;
