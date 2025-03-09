
open Core
open Lang
open Ast

module Fresh_names = struct
  module type S = sig
    val fresh_id : ?suffix:string -> unit -> Ident.t
    val fresh_poly_value : unit -> int
  end

  module Make () : S = struct
    (* suffixes are strictly for readability of target code *)
    let fresh_id : ?suffix : string -> unit -> Ident.t = 
      let count = Utils.Counter.create () in
      fun ?(suffix : string = "") () ->
        let c = Utils.Counter.next count in
        Ident (Format.sprintf "~%d%s" c suffix)

    let fresh_poly_value : unit -> int =
      let count = Utils.Counter.create () in
      fun () -> Utils.Counter.next count
  end
end

module Let_builder (L : sig
  type a
  type t 
  val t_to_expr : t -> cont:a Expr.t -> a Expr.t
end) = struct
  module T = struct
    (* 
      This transforms the identity monad to a writer, where the
      operation to write is list concatenation.
    *)
    module M = Preface.Writer.Over (Preface.List.Monoid (L))
    include M
    type 'a m = 'a M.t
    let bind x f = M.bind f x (* our ppx uses the other argument order *)
    let tell a = tell [ a ] (* in our use case, we only write one at a time, so alias for that *)
  end

  include T

  let iter (ls : 'a list) ~(f : 'a -> unit m) : unit m =
    List.fold ls ~init:(return ()) ~f:(fun acc_m a ->
      let%bind () = acc_m in
      f a
    )

  let[@landmark] build (m : L.a Expr.t m) : L.a Expr.t =
    let cont, resulting_bindings = run_identity m in
    (* we must fold right because of the ordering of Preface.List.Monoid.combine and how it is added to the tape *)
    List.fold_right resulting_bindings ~init:cont ~f:(fun tape cont -> L.t_to_expr tape ~cont)
end

open Ast_tools

module Desugared_functions = struct
  (*
    let filter_list x =
      match x with 
      | `Nil _ -> x
      | `Cons _ -> x
      end
  *)
  let filter_list : Desugared.t =
    let x = Ident.Ident "x" in
    EFunction { param = x ; body =
      EMatch { subject = EVar x ; patterns =
        [ (PVariant
            { variant_label = Reserved.nil
            ; payload_id = Reserved.catchall }
          , EVar x)
        ; (PVariant
            { variant_label = Reserved.cons
            ; payload_id = Reserved.catchall }
          , EVar x)
        ]
      }
    }
end

module Embedded_functions = struct
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
  let y_comb =
    let open Ident in
    let open Expr in
    let f = Ident "~f_y_comb" in
    let x = Ident "~x_y_comb" in
    let body =
      EFunction { param = x ; body =
        EFreeze (EThaw (
          EAppl
            { func = EVar f
            ; arg = EAppl { func = EVar x ; arg = EVar x }
            }
        ))
      }
    in
    EFunction { param = f ; body =
      EAppl
        { func = body
        ; arg  = body
        }
    }
end 
