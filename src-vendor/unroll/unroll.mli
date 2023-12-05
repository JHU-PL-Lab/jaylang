include module type of Unroll_intf

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) :
  S
    with type message = M.message
     and type payload = M.payload
     and type key = Key.t

module Make_just_payload (Key : Base.Hashtbl.Key.S) (M : P_sig) :
  S
    with type message = M.payload
     and type payload = M.payload
     and type key = Key.t
