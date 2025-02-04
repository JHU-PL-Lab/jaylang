
open Core

module Make (Err : sig type t end) = struct
  module C = Preface.Continuation.Monad
  type 'a m = ('a, Err.t) result C.t

  let[@inline_always] bind (x : 'a m) (f : 'a -> 'b m) : 'b m = 
    C.bind (function
      | Ok r -> f r
      | Error e -> C.return (Error e)
    ) x

  let[@inline_always] return (a : 'a) : 'a m =
    C.return
    @@ Result.return a

  let list_map (f : 'a -> 'b m) (ls : 'a list) : 'b list m =
    List.fold_right ls ~init:(return []) ~f:(fun a acc_m ->
      let%bind acc = acc_m in
      let%bind b = f a in
      return (b :: acc)
    )

  let fail (e : Err.t) : 'a m =
    C.return
    @@ Result.fail e
end