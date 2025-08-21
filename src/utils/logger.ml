
module type S = sig
  type tape
  type a
  include Types.MONAD
  val log : a -> unit m
  val tell : tape -> unit m
  val listen : 'a m -> ('a * tape) m
end

module type FULL = sig
  module M : Types.MONAD
  module B : Builder.S
  type 'a m = ('a * B.t) M.m
  include Types.TRANSFORMED with type 'a m := 'a m and type 'a lower = 'a M.m
  include S with type tape = B.t and type a = B.a and type 'a m := 'a m
  val run : 'a m -> ('a * tape) M.m
end

module type TRANSFORMER = sig
  module B : Builder.S
  type tape = B.t

  module Transform : functor (M : Types.MONAD) -> FULL with module M = M and module B = B
end

module Over_monad_with_builder (B : Builder.S) (M : Types.MONAD) : FULL with module M = M with module B = B = struct
  module M = M
  module B = B
  type a = B.a
  type tape = B.t 
  type 'a lower = 'a M.m
  type 'a m = ('a * B.t) M.m
  let return a = M.return (a, B.empty)
  let bind m f =
    M.bind m (fun (a, t) ->
      M.bind (f a) (fun (b, t') ->
        M.return (b, B.combine t t')
      )
    )
  let log x = M.return ((), B.cons x B.empty)
  let tell t = M.return ((), t)
  let run m = m
  let listen m = 
    M.bind m (fun (a, t) ->
      M.return ((a, t), t)
    )
  let upper x =
    M.bind x (fun a ->
      M.return (a, B.empty)
    )
  let map_t (f : 'a lower -> 'b lower) : 'a m -> 'b m =
    fun a_m ->
      M.bind a_m (fun (a, t) ->
        M.bind (f (M.return a)) (fun b -> 
          M.return (b, t)
        )
      )
end

module Transformer_of_builder (B : Builder.S) : TRANSFORMER with module B = B = struct
  module B = B
  type tape = B.t
  module Transform = Over_monad_with_builder (B)
end

module From_builder (B : Builder.S) = Over_monad_with_builder (B) (Identity.Monad)
