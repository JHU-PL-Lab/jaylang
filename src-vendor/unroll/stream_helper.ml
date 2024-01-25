(* let product_stream_bg s1 s2 =
   Lwt_stream.map_list_s
     (fun v2 -> Lwt_stream.(clone s1 |> map (fun v1 -> (v1, v2)) |> to_list))
     s2 *)

(* TODO: this logic is obviously incorrect *)
(* let rec loop () =
     let r1 = Lwt_stream.get stream_src1 in
     let r2 = Lwt_stream.get stream_src2 in
     let%lwt v1, v2 = Lwt.both r1 r2 in
     match (v1, v2) with
     | Some v1, Some v2 ->
         cb (f (v1, v2)) ;
         loop ()
     | _, _ -> Lwt.return_unit
   in
   loop () *)

(*
   val map_list : ('a -> 'b list) -> 'a t -> 'b t
   val map_list_s : ('a -> 'b list Lwt.t) -> 'a t -> 'b t
*)
(*
   let product_stream_bg s1 s2 =
     let s, f = Lwt_stream.create () in
     Lwt.async (fun () ->
         Lwt_stream.iter_p
           (fun v2 ->
             Lwt_stream.iter (fun v1 -> f (Some (v1, v2))) (Lwt_stream.clone s1))
           (Lwt_stream.clone s2)) ;
     s

   let _mk_product_stream s1 s2 =
     Lwt_stream.map_list_s
       (fun v2 -> Lwt_stream.(clone s1 |> map (fun v1 -> (v1, v2)) |> to_list))
       (Lwt_stream.clone s2)

   let _mk_product_stream s1 s2 =
     Lwt_stream.map
       (* _list_s *)
         (fun v2 -> Lwt_stream.(clone s1 |> map (fun v1 -> (v1, v2))))
       (Lwt_stream.clone s2)
     |> Lwt_stream.concat *)

(* let product_stream_bg s1 s2 =
   let s, f = Lwt_stream.create () in
   let rec loop () =
     let r1 = Lwt_stream.get s1 in
     let r2 = Lwt_stream.get s2 in
     let%lwt v1, v2 = Lwt.both r1 r2 in
     match (v1, v2) with
     | Some v1, Some v2 ->
         f (Some (v1, v2)) ;
         loop ()
     | _, _ -> Lwt.return_unit
   in
   Lwt.async loop ;
   s *)

open Lwt.Infix

let product_stream_bg s1 s2 =
  let s, f = Lwt_stream.create () in
  Lwt.async (fun () ->
      Lwt_stream.iter_p
        (fun v2 ->
          Lwt_stream.iter_p
            (fun v1 ->
              f (Some (v1, v2)) ;
              Lwt.return_unit)
            (Lwt_stream.clone s1))
        (Lwt_stream.clone s2)
      >|= fun () -> f None) ;
  s

let product_stream = product_stream_bg
let bg x = Lwt.async (fun () -> x >>= fun _ -> Lwt.return_unit)

let bind0 ?sp src1 f_opt =
  let stream, push =
    match sp with
    | Some (stream, push) -> (stream, push)
    | None -> Lwt_stream.create ()
  in
  Lwt_stream.iter_p
    (fun v1 ->
      match f_opt v1 with
      | Some src2 -> Lwt_stream.iter (fun v2 -> push (Some v2)) src2
      | None -> Lwt.return_unit)
    src1
  >|= (fun () -> push None)
  |> bg ;
  stream
