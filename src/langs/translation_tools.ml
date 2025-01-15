
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
    let assert_list x =
      match x with 
      | `Nil _ -> true
      | `Cons _ -> true
      end
  *)
  let assert_list : Desugared.t =
    let x = Ident.Ident "x" in
    EFunction { param = x ; body =
      EMatch { subject = EVar x ; patterns =
        [ (PVariant
            { variant_label = Reserved_labels.Variants.nil
            ; payload_id = Reserved_labels.Idents.catchall }
          , EBool true)
        ; (PVariant
            { variant_label = Reserved_labels.Variants.cons
            ; payload_id = Reserved_labels.Idents.catchall }
          , EBool true)
        ]
      }
    }
    

end