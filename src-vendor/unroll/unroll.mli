include module type of Unroll_intf

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) :
  S
    with type message = M.message
     and type key = Key.t
     and type 'a act = unit Lwt.t

module Make_just_payload (Key : Base.Hashtbl.Key.S) (M : P_sig) :
  S
    with type message = M.payload
     and type key = Key.t
     and type 'a act = unit Lwt.t

module Make_just_payload_no_wait (Key : Base.Hashtbl.Key.S) (M : P_sig) :
  S with type message = M.payload and type key = Key.t and type 'a act = unit

module Make_pipe (Key : Base.Hashtbl.Key.S) (M : P_sig) :
  SP with type payload = M.payload and type key = Key.t

module Make_pipe_no_wait (Key : Base.Hashtbl.Key.S) (M : P_sig) :
  SP with type payload = M.payload and type key = Key.t
