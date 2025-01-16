
open Core
open Ast

module Reserved_labels = struct
  module Records = struct
    let gen : RecordLabel.t = RecordLabel (Ident "~gen")
    let check : RecordLabel.t = RecordLabel (Ident "~check")
    let wrap : RecordLabel.t = RecordLabel (Ident "~wrap")
    let hd : RecordLabel.t = RecordLabel (Ident "~hd")
    let tl : RecordLabel.t = RecordLabel (Ident "~tl")
  end

  module Variants = struct
    let cons : VariantLabel.t = VariantLabel (Ident "~Cons") 
    let nil : VariantLabel.t = VariantLabel (Ident "~Nil") 
    let untouched : VariantLabel.t = VariantLabel (Ident "~Untouched")
  end

  module VariantTypes = struct
    let cons : VariantTypeLabel.t = VariantTypeLabel (Ident "~Cons") 
    let nil : VariantTypeLabel.t = VariantTypeLabel (Ident "~Nil") 
  end

  module Idents = struct
    let catchall : Ident.t = Ident "_"
  end
end

module Values = struct
  let dummy : type a. a Expr.t = EInt 0
end

module Fresh_names () = struct
  (* prefixes are strictly for readability of target code *)
  let fresh_id : ?prefix : string -> unit -> Ident.t = 
    let count = ref 0 in
    fun ?(prefix : string = "") () ->
      incr count;
      Ident (Format.sprintf "~%s%d" prefix !count)

  let fresh_poly_value : unit -> int =
    let count = ref 0 in
    fun () ->
      incr count;
      !count
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
    let f = Ident "~f" in
    let body =
      let x = Ident "~x" in
      let dummy = Ident "~dummy" in
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

module Function_components = struct
  type 'a t =
    { func_id : Ident.t
    ; tau_opt : 'a Constraints.bluejay_or_desugared Expr.t option
    ; params  : Ident.t list
    ; body    : 'a Constraints.bluejay_or_desugared Expr.t
    } 

  let map (x : 'a t) ~(f : 'a Expr.t -> 'b Expr.t) : 'b t =
    { func_id = x.func_id
    ; tau_opt = Option.map x.tau_opt ~f
    ; params  = x.params
    ; body    = f x.body
    }
end
