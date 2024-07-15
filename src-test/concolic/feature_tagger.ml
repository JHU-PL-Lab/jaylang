
(*
  GOAL:
    * annotate all tests in the test folder with some tags
    * allow the user to go through those tests and mark the tags
    * allow easy edits
    * 
*)

open Core

[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-37"]
[@@@ocaml.warning "-34"]

module Bjy_comments =
  struct
    let read_file (filename : Filename.t) : string list option =
      if Sys_unix.file_exists_exn filename
      then
        In_channel.read_lines filename
        |> List.filter ~f:(String.is_prefix ~prefix:"#")
        |> Option.return
      else None
  end

module Tags =
  struct

    module Status =
      struct
        type t = Checked | Unchecked | Missing [@@deriving compare]

        let t_of_sexp sexp =
          match String.t_of_sexp sexp with
          | "" -> Unchecked
          | "x" -> Checked
          | _ -> Missing
        
        let sexp_of_t x =
          begin
          match x with
          | Checked -> "x"
          | Unchecked -> ""
          | Missing -> "TODO"
          end
          |> String.sexp_of_t
      end

    type t =
      { polymorphic_types      : Status.t [@sexplib.default ""]
      ; variants               : Status.t
      ; intersection_types     : Status.t
      ; recursive_functions    : Status.t
      ; mu_types               : Status.t
      ; higher_order_functions : Status.t
      ; subtyping              : Status.t
      ; type_casing            : Status.t
      ; oop_style              : Status.t
      ; refinement_types       : Status.t
      ; dependent_types        : Status.t
      ; parametric_types       : Status.t
      ; records                : Status.t
      ; trees                  : Status.t
      ; wrap_required          : Status.t }
      [@@deriving sexp, compare]

    let default =
      { polymorphic_types      = Checked
      ; variants               = Checked
      ; intersection_types     = Checked
      ; recursive_functions    = Missing
      ; mu_types               = Checked
      ; higher_order_functions = Checked
      ; subtyping              = Unchecked
      ; type_casing            = Checked
      ; oop_style              = Checked
      ; refinement_types       = Checked
      ; dependent_types        = Checked
      ; parametric_types       = Checked
      ; records                = Checked
      ; trees                  = Checked
      ; wrap_required          = Checked }
  end

module Tag =
  struct
    type t =
      | Polymorphic_types
      | Variants
      | Intersection_types
      | Recursive_functions
      | Mu_types
      | Higher_order_functions
      | Subtyping
      | Type_casing
      | OOP_style
      | Refinement_types
      | Dependent_types
      | Parametric_types
      | Records
      | Trees
      | Wrap_required
      [@@deriving variants, sexp, compare, enumerate]

    let show (x : t) : string = 
      x
      |> Variants.to_name
      |> String.substr_replace_all ~pattern:"_" ~with_:" "

    let show_as_comment (x : t) : string =
      "# - " ^ show x ^ ":    "

    let show_all : string =
      let open List.Let_syntax in
      all
      >>| show_as_comment
      |> String.concat ~sep:"\n"

    let str_contains_tag (s : string) (tag : t) : bool =
      String.substr_index s ~pattern:(show tag)
      |> Option.is_some

    let read (s : string) : t option = 
      List.find all ~f:(str_contains_tag s)

    let read_checked (s : string) : t option * bool =
      match read s with
      | None -> None, false
      | Some tag ->
        let remainder =
          String.substr_replace_all
            s
            ~pattern:(String.strip (show_as_comment tag))
            ~with_:""
        in
        let is_checked =
          String.find remainder ~f:(
            fun c ->
              not (Char.is_whitespace c)
          )
          |> Option.is_some
        in
        (Some tag), is_checked

    module Status =
      struct
        type t =
          | Checked        
          | Unchecked
          | Missing

      end

    (*
      I want to call read_checked on every line, and then I have a map from tag to checked status.

      Then do n^2 iteration over tags and those found, and for missing tags, I need to add the tag to
        the file, and tell the user to update it. This can be by adding "TODO" to the tag, so I should
        discount that from the read_checked function.

      The missing tags are added to the top of the file. It would be nice if they're added after the 
        last seen tag.

      I could also have a "description" box at the top of the file, and a "tags" box. Then it's easier
        to know where to add the tags.
    *)

  end

let () =
  Format.printf "%s\n" (Tags.sexp_of_t Tags.default |> Sexp.to_string_hum ~indent:2)
  (* Format.printf "%s\n" Tag.show_all;
  match Sys.get_argv () |> List.of_array with
  | _ :: filename :: [] -> begin
    match Bjy_comments.read_file filename with
    | None -> Format.eprintf "Filename %s does not exist\n" filename
    | Some lines ->
      List.iter lines ~f:(fun line ->
        let tag_opt_string, is_checked =
          Tag.read_checked line
          |> Tuple2.map_fst ~f:(function None -> "none" | Some tag -> Tag.show tag)
        in
        Format.printf
          "Line is '%s', tag is '%s', checked is %b\n"
          line
          tag_opt_string
          is_checked
      )
  end
  | _ -> Format.eprintf "Bad arguments\n" *)
  


