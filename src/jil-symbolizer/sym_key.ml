open Core
open Dj_common

module Make_V1 (Sym_val : Sym_val_sig.S) = struct
  open Sym_val

  let counter = ref 0
  let reset () = counter := 0

  (* let eq_bool key b = Sym_val.eq (key_to_var key) (Sym_val.bool_ b) *)
  let key_to_var key = key |> Lookup_key.to_string |> Sym_val.var_s
end

module type S = module type of struct
  include Make_V1 (Sym_val)
end

module Make_V2 (Sym_val : Sym_val_sig.S) = struct
  open Sym_val

  let keys = Key_map.create ()
  let counter = ref 0

  let reset () =
    counter := 0 ;
    Key_map.clear keys ;
    Record_logic_bv.clear_labels ()
  (* let key_to_i key =
     Key_map.get_i keys key *)

  (* let key_to_var key = key |> Lookup_key.to_string |> Sym_val.var_s *)
  let key_to_var key = Key_map.get_expr keys key Sym_val.var_i

  let phi_of_value_opt (key : Lookup_key.t) = function
    | Some v -> phi_of_value key v
    | None -> key_to_var key
end
