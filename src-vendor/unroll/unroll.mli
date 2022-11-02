include module type of Unroll_intf

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig with type key = Key.t) :
  S
    with type message = M.message
     and type result = M.result
     and type key = M.key