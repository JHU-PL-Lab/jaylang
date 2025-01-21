type translation_context = {
  tc_fresh_suffix_separator : string;
  tc_contextual_recursion : bool;
  mutable tc_fresh_name_counter : int;
  mutable tc_jayil_jay_mappings : Jay_to_jayil_maps.t;
}

(* ?(is_jay = false)   *)
let new_translation_context suffix is_jay () =
  {
    tc_fresh_name_counter = 0;
    tc_fresh_suffix_separator = suffix;
    tc_contextual_recursion = true;
    tc_jayil_jay_mappings = Jay_to_jayil_maps.empty is_jay;
  }

let new_translation_context_for_jay suffix is_jay () =
  {
    tc_fresh_name_counter = 0;
    tc_fresh_suffix_separator = suffix;
    tc_contextual_recursion = true;
    tc_jayil_jay_mappings = Jay_to_jayil_maps.empty is_jay;
  }
