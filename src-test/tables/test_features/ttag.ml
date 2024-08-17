
(* I have no clue why this can't be found when named `Tag`, but other names are fine, so use `Ttag` for "test tag" *)

open Core

module T =
  struct
    type t =
      | Polymorphic_types (* as reason, includes errors in instantiations of polymorphic types, and any code where error directly involves a polymorphic type *)
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
      | Records (* as reason, includes errors related to projection *)
      | Wrap_required
      | Assertions
      | Operator_misuse (* e.g. 1 + true, 0 0, etc. *)
      | Return_type (* where return type is just fundamentally wrong, e.g. f (x : int) : int = true *)
      | Match (* uses match anywhere, not necessarily as the source of the error *)
      [@@deriving variants, sexp, compare, enumerate, equal]

    let sexp_all : Sexp.t =
      List.sexp_of_t sexp_of_t all

    let to_string = function
    | Polymorphic_types      -> "Polymorphic types"
    | Variants               -> "Variants"
    | Intersection_types     -> "Intersection types"
    | Recursive_functions    -> "Recursive functions"
    | Mu_types               -> "Mu types"
    | Higher_order_functions -> "Higher order functions"
    | Subtyping              -> "Subtyping"
    | Type_casing            -> "Type casing"
    | OOP_style              -> "OOP-style"
    | Refinement_types       -> "Refinement types"
    | Dependent_types        -> "Dependent types"
    | Parametric_types       -> "Parametric types"
    | Records                -> "Records"
    | Wrap_required          -> "Wrap required"
    | Assertions             -> "Assertions"
    | Operator_misuse        -> "Operator misuse"
    | Return_type            -> "Return type"
    | Match                  -> "Match"

    let to_string_with_underline = function
    | Polymorphic_types      -> "\\underline{P}olymorphic types"  
    | Variants               -> "\\underline{V}ariants"           
    | Intersection_types     -> "\\underline{I}ntersection types" 
    | Recursive_functions    -> "\\underline{R}ecursive functions"
    | Mu_types               -> "\\underline{M}u types"           
    | Higher_order_functions -> "\\underline{H}igher order functions"
    | Subtyping              -> "\\underline{S}ubtyping"             
    | Type_casing            -> "\\underline{T}ype casing"           
    | OOP_style              -> "\\underline{O}OP-style"            
    | Refinement_types       -> "Re\\underline{f}inement types"     
    | Dependent_types        -> "\\underline{D}ependent types"      
    | Parametric_types       -> "P\\underline{a}rametric types"     
    | Records                -> "Re\\underline{c}ords"              
    | Wrap_required          -> "\\underline{W}rap required"        
    | Assertions             -> "Assertio\\underline{n}s"           
    | Operator_misuse        -> "Operator mis\\underline{u}se"      
    | Return_type            -> "Return t\\underline{y}pe"          
    | Match                  -> "Match (X)"                   

    let to_string_short = function
    | Polymorphic_types      -> "Po"
    | Variants               -> "V"
    | Intersection_types     -> "I"
    | Recursive_functions    -> "Rf"
    | Mu_types               -> "Mu"
    | Higher_order_functions -> "H"
    | Subtyping              -> "S"
    | Type_casing            -> "T"
    | OOP_style              -> "O"
    | Refinement_types       -> "Rt"
    | Dependent_types        -> "D"
    | Parametric_types       -> "Pa"
    | Records                -> "Rc"
    | Wrap_required          -> "W"
    | Assertions             -> "A"
    | Operator_misuse        -> "Om"
    | Return_type            -> "Ry"
    | Match                  -> "Ma"

    let to_string_super_short = function
    | Polymorphic_types      -> "P"
    | Variants               -> "V"
    | Intersection_types     -> "I"
    | Recursive_functions    -> "R"
    | Mu_types               -> "M"
    | Higher_order_functions -> "H"
    | Subtyping              -> "S"
    | Type_casing            -> "T"
    | OOP_style              -> "O"
    | Refinement_types       -> "F"
    | Dependent_types        -> "D"
    | Parametric_types       -> "A"
    | Records                -> "C"
    | Wrap_required          -> "W"
    | Assertions             -> "N"
    | Operator_misuse        -> "U"
    | Return_type            -> "Y"
    | Match                  -> "X"

    let sort_list =
      List.sort ~compare:(fun a b -> Int.compare (Variants.to_rank a) (Variants.to_rank b))
  end

include T

let get_all_files ?(filter : Filename.t -> bool = fun _ -> true) (dirs : Filename.t list) : Filename.t list =
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

module Make (F : sig val fname_of_bjy : Filename.t -> Filename.t val is_valid_fname : Filename.t -> bool end) =
  struct

    include F

    let get_all_files = get_all_files ~filter:F.is_valid_fname

    let write_tags (bjy_file : Filename.t) (tags : T.t list) : unit =
      Out_channel.write_lines
        (F.fname_of_bjy bjy_file)
        (["("]
          @ (List.map tags ~f:T.to_string)
          @ [")"]
        )

    let read_tags (tag_filename : Filename.t) : T.t list =
      let sexp =
        let ic = In_channel.create tag_filename in
        let s = Sexp.input_sexp ic in
        In_channel.close ic;
        s
      in
      List.t_of_sexp T.t_of_sexp sexp
      |> sort_list

    let add_tags (bjy_file : Filename.t) (tags : T.t list) : unit = 
      let existing_tags =
        bjy_file
        |> F.fname_of_bjy
        |> read_tags
      in
      write_tags bjy_file (existing_tags @ tags)

    let base_filename (fname : Filename.t) : Filename.t =
      Filename.basename fname
      |> String.take_while ~f:(Char.(<>) '.')
    
  end

(* For getting features of a test, from .features.s file *)
module Features = Make (struct
    let fname_of_bjy (bjy_file : Filename.t) : Filename.t =
      let fname, _ = Filename.split_extension bjy_file 
      in fname ^ ".features.s"

    let is_valid_fname (fname : Filename.t) : bool =
      Filename.check_suffix fname ".features.s"
end)

(* For getting reasons for type error in test, from .reasons.s file *)
module Reasons = Make (struct
    let fname_of_bjy (bjy_file : Filename.t) : Filename.t =
      let fname, _ = Filename.split_extension bjy_file 
      in fname ^ ".reasons.s"

    let is_valid_fname (fname : Filename.t) : bool =
      Filename.check_suffix fname ".reasons.s"
end)

let reasons bjy_file =
  Reasons.read_tags
  @@ Reasons.fname_of_bjy bjy_file

let features bjy_file =
  Features.read_tags
  @@ Features.fname_of_bjy bjy_file

let rec list_to_string = function
| [] -> ""
| hd :: tl -> T.to_string_super_short hd ^ list_to_string tl



(* let prepend_to_file (new_line : string) (filename : Filename.t) : unit =
  let lines = In_channel.read_lines filename in
  Out_channel.write_lines filename (new_line :: lines) *)

