
(*
  GOAL:
    * annotate all tests in the test folder with some tags
    * allow the user to go through those tests and mark the tags
    * allow easy edits
    * 
*)

open Core

[@@@ocaml.warning "-32"]

(* module Bjy_comments =
  struct
    let read_file (filename : Filename.t) : string list option =
      if Sys_unix.file_exists_exn filename
      then
        In_channel.read_lines filename
        |> List.filter ~f:(String.is_prefix ~prefix:"#")
        |> Option.return
      else None
  end *)

module Tag =
  struct
    type t =
      | Polymorphic_types
      | Variants
      | Intersection_types
      | Recursive_functions
      | Mu_types
      | Higher_order_functions (* includes first class functions *)
      | Subtyping
      | Type_casing
      | OOP_style
      | Refinement_types
      | Dependent_types
      | Parametric_types
      | Records
      | Trees
      | Wrap_required
      | Assertions
      | Operator_misuse (* e.g. 1 + true *)
      | Return_type (* e.g. f (x : int) : int = true *)
      | Match
      [@@deriving variants, sexp, compare, enumerate]

    let sexp_all : Sexp.t =
      all
      |> List.sexp_of_t sexp_of_t

  end

let get_all_bjy_files (dir : Filename.t) : Filename.t list =
  let is_bjy_file fname =
    Filename.check_suffix fname ".bjy"
  in
  let rec loop outlist = function
    | [] -> outlist
    | f :: fs -> begin
      match Sys_unix.is_directory f with
      | `Yes ->
          f
          |> Sys_unix.ls_dir
          |> List.map ~f:(( ^ ) (f ^ "/"))
          |> List.append fs
          |> loop outlist
      | _ when is_bjy_file f -> loop (f :: outlist) fs
      | _ -> loop outlist fs
    end
  in
  loop [] [dir]

(* let prepend_to_file (new_line : string) (filename : Filename.t) : unit =
  let lines = In_channel.read_lines filename in
  Out_channel.write_lines filename (new_line :: lines) *)

let feature_filename (bjy_file : Filename.t) : Filename.t =
  let fname, _ = Filename.split_extension bjy_file 
  in fname ^ ".features.s"

let write_tags (bjy_file : Filename.t) (tags : Tag.t list) : unit =
  Out_channel.write_lines
    (feature_filename bjy_file)
    (["("]
      @ (List.map tags ~f:(fun t -> Tag.sexp_of_t t |> Sexp.to_string))
      @ [")"]
    )

let add_tags (bjy_file : Filename.t) (tags : Tag.t list) : unit = 
  let existing_tags =
    bjy_file
    |> feature_filename
    |> In_channel.create
    |> Sexp.input_sexp
    |> List.t_of_sexp Tag.t_of_sexp
  in
  write_tags bjy_file (existing_tags @ tags)

let () =
  let open List.Let_syntax in
  let _ = 
    get_all_bjy_files "./test/concolic/bjy"
    >>| Fn.flip add_tags [ ]
  in
  ()
  


