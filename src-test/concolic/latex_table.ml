
open Core

module type ROW =
  sig
    type t  (* expected to be a record *)

    val names : string list

    val to_strings : t -> string list
  end

module Row_or_hline =
  struct
    type 'a t =
      | Row of 'a
      | Hline
  end

module Col_option =
  struct
    type t =
      { right_align : bool
      ; vertical_line_to_right : bool }
    
    let to_string x =
      (if x.right_align then "r" else "c")
      ^ (if x.vertical_line_to_right then "|" else "")

    let opt_to_string x =
      match x with
      | None -> to_string { right_align = false ; vertical_line_to_right = false }
      | Some r -> to_string r

  end

(* This is probably just better as a functor, but this is kind of fun, so I'll leave it *)
type 'row t =
  { row_module : (module ROW with type t = 'row)
  ; rows : 'row Row_or_hline.t list
  ; col_options : Col_option.t option list } (* of same or lesser length than (val row_module).to_strings *)

let show (type row) (x : row t) : string =
  let open List.Let_syntax in
  let module R = (val x.row_module) in
  let show_row row =
    match row with
    | Row_or_hline.Hline -> "    \\hline"
    | Row row ->
      row
      |> R.to_strings
      |> String.concat ~sep:" & " (* column delimiter in latex *)
      |> fun s -> "    " ^ s ^ " \\\\" (* line delimiter in latex *)
  in
  let tabular_cols =
    x.col_options
    >>| Col_option.opt_to_string
    |> (fun l ->
      if List.length l < List.length R.names
      then l @ (List.init (List.length R.names - List.length l) ~f:(fun _ -> "c"))
      else l)
    |> String.concat
  in
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
