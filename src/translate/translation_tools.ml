
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
  val t_to_expr : t -> body:a Expr.t -> a Expr.t
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

  let build (m : L.a Expr.t m) : L.a Expr.t =
    let body, resulting_bindings = run_identity m in
    (* we must fold right because of the ordering of Preface.List.Monoid.combine and how it is added to the tape *)
    List.fold_right resulting_bindings ~init:body ~f:(fun tape body -> L.t_to_expr tape ~body)
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

  (*
    Generic Y-combinator for one function.

      fun f ->
        (fun s -> fun x -> f (s s) x)
        (fun s -> fun x -> f (s s) x)
  *)
  let y_1 = 
    let open Ident in
    let open Expr in
    let open Ast_tools.Utils in
    let f = Ident "~f_y1" in
    let s = Ident "~s_y1" in
    let x = Ident "~x_y1" in
    let body =
      abstract_over_ids [ s ; x ] @@
        appl_list (EVar f) ([ apply (EVar s) (EVar s) ; EVar x ])
    in
    abstract_over_ids [ f ] @@
      apply body body

  (*
    Y-combinator for n functions identified by `names`.

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
  let y_n = function
    | [] -> failwith "Invalid Y-combinator on 0 functions"
    | [ f ] ->
      (* only one function so can use the simple y_1 combinator *)
      let open Ast_tools.Utils in
      abstract_over_ids [ f ] @@
        let appl_y1 = apply y_1 (EVar f) in
        Ast.Expr.ERecord (RecordLabel.Map.singleton (RecordLabel.RecordLabel f) appl_y1)
    | ids ->
      let open Ident in
      let open Expr in
      let open Ast_tools.Utils in
      let self = Ident "~self_yn" in
      let x = Ident "~x_yn" in
      let r = Ident "~r_yn" in
      let e_ids = List.map ids ~f:(fun id -> EVar id) in
      let labels = List.map ids ~f:(fun id -> RecordLabel.RecordLabel id) in
      let projections = List.map labels ~f:(fun label -> proj (EVar r) label) in
      abstract_over_ids ids (
        appl_list (
          apply y_1 @@
            abstract_over_ids (self :: ids) @@
              ERecord (Ast.RecordLabel.Map.of_alist_exn @@
                let bodies =
                  List.map ids ~f:(fun f ->
                    abstract_over_ids [ x ] @@
                      ELet { var = r ; defn = appl_list (EVar self) e_ids ; body =
                        apply (appl_list (EVar f) projections) (EVar x)
                      }
                  )
                in
                List.zip_exn labels bodies
              )
        ) e_ids
      )

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
  let y_freeze_thaw =
    let open Ident in
    let open Expr in
    let open Ast_tools.Utils in
    let f = Ident "~f_y_freeze_thaw" in
    let x = Ident "~x_y_freeze_thaw" in
    let body =
      abstract_over_ids [x] @@
        EFreeze (EThaw (
          apply (EVar f) (apply (EVar x) (EVar x))
        ))
    in
    abstract_over_ids [ f ] @@
      apply body body

  (*
    Generic Y-combinator for one function.

      fun f ->
        (fun s -> fun x -> f (s s) x)
        (fun s -> fun x -> f (s s) x)
  *)
  let y_1 = 
    let open Ident in
    let open Expr in
    let open Ast_tools.Utils in
    let f = Ident "~f_y1" in
    let s = Ident "~s_y1" in
    let x = Ident "~x_y1" in
    let body =
      abstract_over_ids [ s ; x ] @@
        appl_list (EVar f) ([ apply (EVar s) (EVar s) ; EVar x ])
    in
    abstract_over_ids [ f ] @@
      apply body body
end 
