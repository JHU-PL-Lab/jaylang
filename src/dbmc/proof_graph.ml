open Core

type t = node

and node = Search_node of search_node | Control_node of control_node

and search_node = {
  key : Lookup_key.t;
  block_id : Id.t;
  rule : rule;
  mutable preds : t ref list;
  mutable is_search_complete : bool;
}

and rule =
  | Done of Concrete_stack.t
  | Alias of t ref
  | Binop of search_node ref * search_node ref
  | Cond_choice of search_node ref * search_node ref
  | Callsite of search_node ref * control_node ref * Helper.cf
  | Para_local of search_node ref * control_node ref * Helper.fc

and control_node = {
  cvar : Cvar.t;
  ctrl : control_group;
  mutable pred_sns : search_node ref list;
  mutable is_control_complete : bool;
}

and control_group =
  | Exclusive_one of search_node ref
  (* And applies on two search nodes *)
  | And_group of search_node ref * search_node ref
  (* Or applied on a list of search nodes *)
  | Or_group of control_node ref list

let mk_alias_node ~block_id ~key ~parent ~rule =
  { key; block_id; rule; preds = [ parent ]; is_search_complete = false }
