(**
  Module [Cps_result].

  This module makes a monad for a result with a fixed
  error type in continuation passing style.

  Sometimes interpretation of long programs can be slow
  (see lang/interp.ml) when lots of stack space is used.
  Continuation passing style speeds that up, but it adds
  overhead to short programs (which becomes nearly
  negligible when we inline the definitions)>
*)

open Core

module Make (Err : sig type t end) = struct

  type 'a m = {
    cont : 'r. error:(Err.t -> 'r) -> ok:('a -> 'r) -> 'r
  }

  let[@inline always] bind (x : 'a m) (f : 'a -> 'b m) : 'b m = 
    { cont = fun ~error ~ok ->
      x.cont ~error ~ok:(fun a -> (f a).cont ~error ~ok)
    }

  let[@inline always] return (a : 'a) : 'a m =
    { cont = fun ~error:_ ~ok -> ok a}

  let list_map (f : 'a -> 'b m) (ls : 'a list) : 'b list m =
    List.fold_right ls ~init:(return []) ~f:(fun a acc_m ->
      let%bind acc = acc_m in
      let%bind b = f a in
      return (b :: acc)
    )

  let[@inline always] fail (e : Err.t) : 'a m =
    { cont = fun ~error ~ok:_ -> error e }

  let run (x : 'a m) : ('a, Err.t) result =
    x.cont ~error:(fun e -> Error e) ~ok:(fun a -> Ok a)
end