
open Core
open Lang
open Ast
open Ast_tools

module Fresh_names = struct
  module type S = sig
    val fresh_id : ?suffix:string -> unit -> Ident.t
    val fresh_poly_value : unit -> int
  end

  module Make () : S = struct
    (* suffixes are strictly for readability of target code *)
    let fresh_id : ?suffix : string -> unit -> Ident.t = 
      let count = ref 0 in
      fun ?(suffix : string = "") () ->
        incr count;
        Ident (Format.sprintf "~%d%s" !count suffix)

    let fresh_poly_value : unit -> int =
      let count = ref 0 in
      fun () ->
        incr count;
        !count
  end
end

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
            { variant_label = Reserved_labels.Variants.nil
            ; payload_id = Reserved_labels.Idents.catchall }
          , EVar x)
        ; (PVariant
            { variant_label = Reserved_labels.Variants.cons
            ; payload_id = Reserved_labels.Idents.catchall }
          , EVar x)
        ]
      }
    }
end

module Embedded_functions = struct
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
  let y_comb =
    let open Ident in
    let open Expr in
    let f = Ident "~f_y_comb" in
    let body =
      let x = Ident "~x_y_comb" in
      let dummy = Ident "~dummy_y_comb" in
      EFunction { param = x ; body =
        EFunction { param = dummy ; body =
          EAppl
            { func =
              EAppl
                { func = EVar f
                ; arg = EAppl { func = EVar x ; arg = EVar x }
                }
            ; arg = EVar dummy
            }
        }
      }
    in
    EFunction { param = f ; body =
      EAppl
        { func = body
        ; arg  = body
        }
    }
end 
