
open Core

open List.Let_syntax

(* Show every feature with an "x" that is used in the test. This table is WAY too big *)
module Full_table = 
  struct
    module Row (* : Latex_table.ROW *) =
      struct
        type t =
          { filename : Filename.t (* as full path, without any "texttt" or similar *)
          ; tags : Ttag.t list }

        let names =
          Ttag.all
          (* >>| Ttag.to_string
          >>| Latex_table.texttt *)
          >>| Ttag.to_string_super_short
          |> List.cons "Filename"

        let to_strings (x : t) : string list =
          Ttag.all (* for every tag ... *)
          >>| List.mem x.tags ~equal:Ttag.equal (* check if exists in x's tags *)
          >>| (function true -> "x" | false -> " ") (* mark as "x" or blank based on existence *)
          |> List.cons (Latex_format.texttt (Ttag.Features.base_filename x.filename)) (* put filename at front of row *)
      end

    let make_of_dirs (dirs : Filename.t list) : Row.t Latex_tbl.t =
      { row_module = (module Row)
      ; rows = 
        Ttag.Features.get_all_files dirs
        |> List.sort ~compare:String.compare
        >>| (fun filename -> Latex_tbl.Row_or_hline.return Row.{ filename ; tags = Ttag.Features.read_tags filename })
        |> List.cons Latex_tbl.Row_or_hline.Hline
      ; columns = [ [ Right_align ; Vertical_line_to_right ]]
      }
  end

(* Show the number of tests that use each feature. *)
module Counts_table =
  struct
    module Row (* : Latex_table.ROW *) =
      struct
        type t =
          { feature : Ttag.t
          ; count   : int
          ; err_count : int }

        let names =
          [ "Feature"
          ; "uses"
          ; "errors" ]

        let to_strings (x : t) : string list =
          [ Ttag.to_string x.feature ^ " (" ^ Ttag.to_string_super_short x.feature ^ ")"
          ; Int.to_string x.count
          ; Int.to_string x.err_count ]
      end

    let make_of_dirs (dirs : Filename.t list) : Row.t Latex_tbl.t =
      { row_module = (module Row)
      ; rows =
        begin
        let feature_files = Ttag.Features.get_all_files dirs in 
        let reason_files = Ttag.Reasons.get_all_files dirs in 
        let count tag files read_tags =
          files
          >>= read_tags
          |> List.count ~f:(Ttag.equal tag)
        in
        Ttag.all
        >>| (fun tag ->
            Latex_tbl.Row_or_hline.return
            Row.{ feature = tag
                ; count = count tag feature_files Ttag.Features.read_tags
                ; err_count = count tag reason_files Ttag.Reasons.read_tags })
        |> List.cons Latex_tbl.Row_or_hline.Hline
        end
      ; columns = [ [ Right_align ; Vertical_line_to_right ] ]
      }
  end