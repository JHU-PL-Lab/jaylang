(**
  Executable [feat_count.exe].

  This executable considers each language feature and counts
  up how many ill-typed tests use that feature and how many
  ill-typed tests have an error that is fundamentally related
  to the feature.

  This is quite subjective, and it depends on the feature tagging.
  Not all tests are tagged yet, so they contributed nothing to the
  count.

  Tests use many features, so the sum of a column (in the outputted
  table) is NOT the number of tests.

  See the bottom of this file for the test directories that are
  included in the count (and note how it is only the ill-typed tests).
*)

open Core
open List.Let_syntax

module Tbl = struct
  module Row = struct
    type t =
      { tag  : Ttag.V2.t
      ; uses : int (* number of tests in which the tag is used at all *)
      ; errs : int (* number of ill-typed tests in which the tag is a fundamental involved in the error *)
      (* description not needed because can be derived from the tag *)
      }

    let names = 
      [ "Features"
      ; "Uses"
      ; "Errors"
      ; "Description" ]

    let to_strings (x : t) : string list =
      [ Format.sprintf "%s (%c)" (Ttag.V2.to_name x.tag) (Ttag.V2.to_char x.tag)
      ; Int.to_string x.uses
      ; Int.to_string x.errs
      ; Ttag.V2.to_description x.tag ]
  end

  let make_of_dirs (dirs : Filename.t list) : Row.t Latex_tbl.t =
    { row_module = (module Row)
    ; rows = begin
      dirs
      |> Utils.File_utils.get_all_bjy_files
      >>| Metadata.of_bjy_file
      >>| Metadata.tags_of_t 
      >>| (function `Sorted_list ls -> ls)
      |> List.transpose_exn
      |> List.zip_exn Ttag.V2.all
      >>| (fun (tag, ls) ->
        List.fold ls ~init:Row.{ tag ; uses = 0 ; errs = 0 } ~f:(fun acc -> function
          | `Absent -> acc
          | `Feature t -> assert (Ttag.V2.equal t tag); { acc with uses = acc.uses + 1 }
          | `Reason t -> assert (Ttag.V2.equal t tag); { acc with errs = acc.errs + 1 ; uses = acc.uses + 1 }
          )
      )
      >>| Latex_tbl.Row_or_hline.return
      |> List.cons Latex_tbl.Row_or_hline.Hline
    end
    ; columns = [ [ Right_align ; Vertical_line_to_right ] ]
    }
end

(* Note we only include ill-typed tests *)
let () =
  [ "deep-type-error"
  ; "edge-cases-ill-typed"
  ; "interp-ill-typed"
  ; "oopsla-24-benchmarks-ill-typed"
  (* ; "oopsla-24-tests-ill-typed" *) (* these are mostly trivial, so I don't include them *)
  ; "post-oopsla-ill-typed"
  (* ; "sato-bjy-ill-typed" *) (* these are so trivial, it doesn't feel fair to include them *)
  ; "scheme-pldi-2015-ill-typed"
  ]
  >>| String.append "./test/bjy/"
  |> Tbl.make_of_dirs
  |> Latex_tbl.show 
  |> Format.printf "%s\n"
