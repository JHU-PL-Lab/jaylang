(**
  Module [Translation_tools].

  Provided are some common utilities used during translation.
  We have support for fresh names and define some constant functions.
*)

module Fresh_names : sig
  module type S = sig
    val fresh_id : ?suffix:string -> unit -> Lang.Ast.Ident.t
    val fresh_poly_value : unit -> int
  end

  module Make () : S
end

module Desugared_functions : sig
  (*
    let filter_list x =
      match x with 
      | `Nil _ -> x
      | `Cons _ -> x
      end
  *)
  val filter_list : Lang.Ast.Desugared.t
end

module Embedded_functions : sig
  (*
    Y-combinator for Mu types: 

      fun f ->
        (fun x -> freeze (thaw (f (x x))))
        (fun x -> freeze (thaw (f (x x))))
    
    Notes:
    * f is a function, so it has be captured with a closure, so there is nothing
      wrong about using any names here. However, I use tildes to be safe and make
      sure they're fresh.
    * This y-combinator is unconventional in that it uses freeze and thaw instead of
      passing an argument. This is because we know the use case is for mu types.
  *)
  val y_comb : Lang.Ast.Embedded.t
end