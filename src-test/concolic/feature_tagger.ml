
open Core

open List.Let_syntax

[@@@ocaml.warning "-32"]

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

    let to_texttt x =
      Latex_table.texttt
      @@ to_string x

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

(* Show every feature with an "x" that is used in the test. This table is WAY too big *)
module Full_table = 
  struct
    module Row (* : Latex_table.ROW *) =
      struct
        type t =
          { filename : Filename.t (* as full path, without any "texttt" or similar *)
          ; tags : Tag.t list }

        let names =
          Tag.all
          >>| Tag.to_string
          >>| Latex_table.texttt
          |> List.cons "Filename"

        let to_strings (x : t) : string list =
          Tag.all (* for every tag ... *)
          >>| List.mem x.tags ~equal:Tag.equal (* check if exists in x's tags *)
          >>| (function true -> "x" | false -> " ") (* mark as "x" or blank based on existence *)
          |> List.cons (Latex_table.texttt (base_filename x.filename)) (* put filename at front of row *)
      end

    let make_of_dirs (dirs : Filename.t list) : Row.t Latex_table.t =
      { row_module = (module Row)
      ; rows = 
        get_all_files dirs (Fn.flip Filename.check_suffix ".features.s")
        >>| (fun filename -> Latex_table.Row_or_hline.return Row.{ filename ; tags = read_tags filename })
        |> List.cons Latex_table.Row_or_hline.Hline
      ; col_options = [ Some { right_align = true ; vertical_line_to_right = true }]
      }
  end

(* Show the number of tests that use each feature. *)
module Counts_table =
  struct
    module Row (* : Latex_table.ROW *) =
      struct
        type t =
          { feature : Tag.t
          ; count   : int }

        let names =
          [ "Feature"
          ; "Count" ]

        let to_strings (x : t) : string list =
          [ Tag.to_texttt x.feature
          ; Int.to_string x.count ]
      end

    let make_of_dirs (dirs : Filename.t list) : Row.t Latex_table.t =
      { row_module = (module Row)
      ; rows =
        begin
        let files = get_all_files dirs (Fn.flip Filename.check_suffix ".features.s") in
        let count tag =
          files
          >>= read_tags
          |> List.count ~f:(Tag.equal tag)
        in
        Tag.all
        >>| (fun tag -> Latex_table.Row_or_hline.return Row.{ feature = tag ; count = count tag })
        |> List.cons Latex_table.Row_or_hline.Hline
        end
      ; col_options = [ Some { right_align = true ; vertical_line_to_right = true }]
      }
  end

let () =
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
    |> Counts_table.make_of_dirs
    |> Latex_table.show
    |> print_endline
  in
  ()
  


