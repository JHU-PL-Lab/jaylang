(**
  This module contains support components for the Menhir parser.
*)

type file_pos = { file_pos_lineno : int
                ; file_pos_colno : int
                };;

type file_region = { file_region_filename : string
                   ; file_region_start : file_pos
                   ; file_region_end : file_pos
                   };;

type src_origin =
  | File_origin of file_region
  | Gen_origin of src_origin list
  ;;