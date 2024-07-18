
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
      | Parametric_types (* not including List *)
      | Records
      | Wrap_required
      | Assertions
      | Operator_misuse (* e.g. 1 + true *)
      | Return_type (* where return type is just fundamentally wrong, e.g. f (x : int) : int = true *)
      | Match (* uses match anywhere, not necessarily as the source of the error *)
      [@@deriving variants, sexp, compare, enumerate, equal]

    let sexp_all : Sexp.t =
      all
      |> List.sexp_of_t sexp_of_t

    let to_string x =
      Sexp.to_string
      @@ sexp_of_t x

  end

let get_all_files (dirs : Filename.t list) (filter : Filename.t -> bool) : Filename.t list =
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
      | _ when filter f -> loop (f :: outlist) fs
      | _ -> loop outlist fs
    end
  in
  loop [] dirs

(* let prepend_to_file (new_line : string) (filename : Filename.t) : unit =
  let lines = In_channel.read_lines filename in
  Out_channel.write_lines filename (new_line :: lines) *)

let get_feature_filename (bjy_file : Filename.t) : Filename.t =
  let fname, _ = Filename.split_extension bjy_file 
  in fname ^ ".features.s"

let write_tags (bjy_file : Filename.t) (tags : Tag.t list) : unit =
  Out_channel.write_lines
    (get_feature_filename bjy_file)
    (["("]
      @ (List.map tags ~f:Tag.to_string)
      @ [")"]
    )

let read_tags (feature_filename : Filename.t) : Tag.t list =
  let sexp =
    let ic = In_channel.create feature_filename in
    let s = Sexp.input_sexp ic in
    In_channel.close ic;
    s
  in
  List.t_of_sexp Tag.t_of_sexp sexp

let add_tags (bjy_file : Filename.t) (tags : Tag.t list) : unit = 
  let existing_tags =
    bjy_file
    |> get_feature_filename
    |> read_tags
  in
  write_tags bjy_file (existing_tags @ tags)

let base_filename (fname : Filename.t) : Filename.t =
  Filename.basename fname
  |> String.take_while ~f:(Char.(<>) '.')

let texttt (s : string) : string =
  "\\texttt{" ^ String.substr_replace_all ~pattern:"_" ~with_:"\\_" s ^ "}"

let make_full_table (dirs : Filename.t list) =
  let open List.Let_syntax in
  let create_row fname =
    let tags = read_tags fname in
    Tag.all
    >>| List.mem tags ~equal:Tag.equal
    >>| (function true -> "x" | false -> " ")
    |> String.concat ~sep:" & "
    |> fun row ->
      (texttt @@ base_filename fname)
      ^ " & "
      ^ row
  in
  let header =
    Tag.all
    >>| Tag.to_string
    >>| texttt
    |> List.cons "Filename"
    |> String.concat ~sep:" & "
  in
  get_all_files dirs (Fn.flip Filename.check_suffix ".features.s")
  >>| create_row
  |> List.cons header (* TODO: add the latex env stuff to make this actually a table *)
  |> String.concat ~sep:"\\\\ \n"
  |> Format.printf "%s\n"

let make_count_table (dirs : Filename.t list) =
  let open List.Let_syntax in
  let files = get_all_files dirs (Fn.flip Filename.check_suffix ".features.s") in
  let count tag =
    files
    >>= read_tags
    |> List.count ~f:(Tag.equal tag)
  in
  Tag.all
  >>| (fun x ->
      (texttt @@Tag.to_string x)
      ^ " & " 
      ^ (Int.to_string @@ count x)
  )
  |> List.cons "Feature & Count"
  |> String.concat ~sep:"\\\\ \n"
  |> Format.printf "%s\n"
  

let () =
  let open List.Let_syntax in
  let path = "./test/concolic/bjy/" in
  let dirs =
    [ "oopsla-24a-additional-tests-ill-typed"
    ; "sato-bjy-ill-typed"
    ; "scheme-pldi-2015-ill-typed" ]
  in
    
  let _ = 
    (* get_all_files "./test/concolic/bjy" (Fn.flip Filename.check_suffix ".bjy")
    >>| Fn.flip add_tags [ ] *)
    dirs
    >>| String.append path
    |> make_count_table
  in
  ()
  


