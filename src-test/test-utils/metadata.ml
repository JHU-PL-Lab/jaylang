(**
  Module [Metadata].

  This is one module to contain the full test medatadata for 
  a Bluejay test program.

  The metadata is stored in a comment at the top of the Bluejay
  file as follows
  
    (***
      (
        (features (<test feature list>))
        (reasons (<test reason list>))
        (speed <Fast or Slow>)
        (typing <Well_typed or Ill_typed>)
      )
    *)

  The triple asterisk in the comment opener is the key that the
  comment contains the tests metadata.

  All of these are optional. If absent, the test is assumed to be
  well-typed and run slow. The fields can be in any order.

  Note the reasons must be empty if the test is well-typed because
  they are the reasons for the error, which does not exist.
*)

open Core

module Test_speed = struct
  type t = Fast | Slow [@@deriving sexp] 
end

module Typing = struct
  type t = Well_typed | Ill_typed [@@deriving sexp]
end

type t =
  { features : Ttag.t list  [@default []]
  ; reasons  : Ttag.t list  [@default []]
  ; speed    : Test_speed.t [@default Slow]
  ; typing   : Typing.t     [@default Well_typed]
  } [@@deriving sexp]

let tags_of_t (r : t) : [ `Sorted_list of [ `Feature of Ttag.t | `Reason of Ttag.t | `Absent ] list ] =
  `Sorted_list (
    Ttag.all
    |> List.map ~f:(fun tag ->
      let mem ls = List.mem ls tag ~equal:Ttag.equal in
      if mem r.reasons
      then begin
        (* first need to assert that features are a subset of reasons *)
        if not @@ mem r.features
        then failwith @@ Format.sprintf "Tag %s found in reasons but not features" (Ttag.to_string tag)
        else `Reason tag
      end
      else 
        if mem r.features
        then `Feature tag
        else `Absent
    )
  )

let of_bjy_file (bjy_filename : Filename.t) : t =
  let file_content = In_channel.read_all bjy_filename in (* may consider reading only a short portion to make this faster *)
  let s_opt =
    let open Option.Let_syntax in
    let%bind i0 = String.substr_index file_content ~pattern:"(***" in
    let%bind i1 = String.substr_index file_content ~pos:i0 ~pattern:"*)" in
    return (String.slice file_content (i0 + 4) i1)
  in
  Option.value s_opt ~default:"()"
  |> Sexp.of_string
  |> t_of_sexp
