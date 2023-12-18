include module type of Unroll_intf

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) :
  S
    with type key = Key.t
     and type message = M.message
     and type 'a act = unit Lwt.t

module Make_just_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) :
  S
    with type key = Key.t
     and type message = P.payload
     and type payload = P.payload
     and type pipe = Key.t
     and type 'a act = unit Lwt.t

module Make_just_payload_no_wait (Key : Base.Hashtbl.Key.S) (P : P_sig) :
  S
    with type key = Key.t
     and type message = P.payload
     and type payload = P.payload
     and type pipe = Key.t
     and type 'a act = unit

module Make_pipe (Key : Base.Hashtbl.Key.S) (P : P_sig) :
  S with type key = Key.t and type payload = P.payload

module Make_pipe_no_wait (Key : Base.Hashtbl.Key.S) (P : P_sig) :
  S with type key = Key.t and type payload = P.payload
