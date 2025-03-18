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

(**
  [Let_builder] is a monad that allows us to write let-bindings
  to a tape and then build that tape into an expression. It simply
  gives nice notation that more closely follows the specification
  of the translation.
*)
module Let_builder (L : sig
  type a
  type t
  val t_to_expr : t -> cont:(a Lang.Ast.Expr.t) -> a Lang.Ast.Expr.t
  (** [t_to_expr t ~cont] makes an expression using [t] that continues to [cont].
      e.g. implementation [fun (var, body) ~cont -> ELet { var ; body ; cont }].
      i.e. take one tape item and turn it into a let binding. *)
end) : sig
  type 'a m

  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m

  val tell : L.t -> unit m
  (** [tell t] writes [t] to the tape. *)

  val iter : 'a list -> f:('a -> unit m) -> unit m

  val build : L.a Lang.Ast.Expr.t m -> L.a Lang.Ast.Expr.t
  (** [build x] uses the tape in [x] to build an expression. *)
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

  (*
    Generic Y-combinator for one function.

      fun f ->
        (fun s -> fun x -> f (s s) x)
        (fun s -> fun x -> f (s s) x)
  *)
  val y_1 : Lang.Ast.Desugared.t

  (*
    Y-combinator for n functions identified by the given names.

      fun f1 ... fn ->
        Y (fun self f1 ... fn ->
          { l1 = fun x ->
            let r = self f1 ... fn in
            f1 r.f1 ... r.fn x
          ; ...
          ; ln = fun x ->
            let r = self f1 ... fn in
            fn r.f1 ... r.fn x
          }
        ) f1 ... fn
  *)
  val y_n : Lang.Ast.Ident.t list -> Lang.Ast.Desugared.t
  (** [y_n ids] is the fixed point combinator on the functions with names [ids].
      The record after the application will have the labels the same as [ids].
      Note: [ids] must be non-empty. *)
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
  val y_freeze_thaw : Lang.Ast.Embedded.t
end
