
module Make (T : sig type 'a t end) = struct
  type t = Pack : 'a T.t -> t
end