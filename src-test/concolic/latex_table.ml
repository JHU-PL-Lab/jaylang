
open Core

module type ROW =
  sig
    type t  (* expected to be a record *)

    val names : string list

    val to_strings : t -> string list
  end

(* This is probably just better as a functor, but this is kind of fun, so I'll leave it *)
type 'row t =
  { row_module : (module ROW with type t = 'row)
  ; rows : 'row list }

let show (type row) (x : row t) : string =
  let open List.Let_syntax in
  let module R = (val x.row_module) in
  let show_row row =
    row
    |> R.to_strings
    |> String.concat ~sep:" & " (* column delimiter in latex *)
    |> fun s -> "    " ^ s ^ " \\\\" (* line delimiter in latex *)
  in
  let tabular_cols = String.make (List.length R.names) 'c' in (* repeat c's, once for each column *)
  let table_begin =
    [ "\\begin{center}"
    ; "  \\begin{tabular}{" ^ tabular_cols ^ "}" ]
  in
  let table_end =
    [ "  \\end{tabular}"
    ; "\\end{center}" ]
  in
  table_begin
  @ [ "    " ^ String.concat R.names ~sep:" & " ^ "\\\\"]
  @ (x.rows >>| show_row)
  @ table_end
  |> String.concat ~sep:"\n"
