include module type of Unroll_intf

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) :
  S
    with type key = Key.t
     and type message = M.message
     and type 'a act = unit Lwt.t

module Make_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) :
  S
    with type key = Key.t
     and type message = P.payload
     and type payload = P.payload
     and type pipe = Key.t
     and type 'a act = unit Lwt.t

module Make_payload_bg (Key : Base.Hashtbl.Key.S) (P : P_sig) :
  S_bg
    with type key = Key.t
     and type message = P.payload
     and type payload = P.payload
     and type pipe = Key.t

module Make_pipe (Key : Base.Hashtbl.Key.S) (P : P_sig) :
  S
    with type key = Key.t
     and type payload = P.payload
     and type 'a act = 'a Lwt.t

module Make_pipe_bg (Key : Base.Hashtbl.Key.S) (P : P_sig) :
  S_bg with type key = Key.t and type payload = P.payload
