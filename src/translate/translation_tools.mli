
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
        (fun x -> fun dummy -> f (x x) dummy)
        (fun x -> fun dummy -> f (x x) dummy)
    
    Notes:
    * f is a function, so it has be captured with a closure, so there is nothing
      wrong about using any names here. However, I use tildes to be safe and make
      sure they're fresh.
  *)
  val y_comb : Lang.Ast.Embedded.t
end