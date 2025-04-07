
open Core

module type ROW = sig
  type t

  val names : string list
  (** [names] are the column names *)

  val to_strings : t -> string list
  (** [to_strings t] are the strings to be put in the table for the row [t], one string for each column. *)
end

module Row_or_hline = struct
  type 'a t =
    | Row of 'a
    | Hline

  let return x = Row x
end

module Col_option = struct
  type t =
    | Right_align
    | Left_align
    | Center
    | No_space
    | Little_space of { point_size : int } (* incompatible with No_space *)
    | Vertical_line_to_right

  let to_string = function
  | Right_align -> "r"
  | Left_align -> "l"
  | Center -> "c"
  | No_space -> "@{}"
  | Little_space { point_size } -> "@{\\hspace{" ^ Int.to_string point_size ^ "pt}}" (* strangly, Format.sprintf can't show "@{" *)
  | Vertical_line_to_right -> "|"
end

module Column = struct
  type t = Col_option.t list
  (* align style must occur before space and vertical line *)

  let to_string ls =
    let rec loop = function
    | [] -> ""
    | hd :: tl -> Col_option.to_string hd ^ loop tl
    in
    match ls with
    | (Col_option.Center as hd) :: tl
    | (Right_align as hd) :: tl
    | (Left_align as hd) :: tl -> Col_option.to_string hd ^ loop tl
    | _ -> "c" ^ loop ls

  let default = []

  (* n is number of columns in whole table. ls may be shorter *)
  let tabular_cols (n : int) (ls : t list) : string =
    let ss = List.map ls ~f:to_string in
    String.concat
    begin
    if List.length ss < n
    then ss @ List.init (n - List.length ss) ~f:(fun _ -> to_string default)
    else List.take ss n
    end
end

(* This is probably just better as a functor, but this is kind of fun, so I'll leave it *)
type 'row t =
  { row_module : (module ROW with type t = 'row)
  ; rows : 'row Row_or_hline.t list
  ; columns : Column.t list } (* of same or lesser length than (val row_module).to_strings *)

let align_ampersands (ls : string list) : string list =
  let open List.Let_syntax in
  ls
  >>| String.split ~on:'&'
  |> List.transpose_exn
  >>| begin fun row -> 
    let m =
      List.max_elt row ~compare:(fun a b -> Int.compare (String.length a) (String.length b))
      |> Option.value_exn
      |> String.length
    in
    row
    >>| fun s -> s ^ String.make (m - String.length s) ' '
  end
  |> List.transpose_exn
  >>| String.concat ~sep: " & "

let show_rows (type row) (row_to_strings : row -> string list) (x : row Row_or_hline.t list) : string list =
  let show_single_row = function
    | Row_or_hline.Hline -> "      \\hline"
    | Row row ->
      row
      |> row_to_strings
      |> String.concat ~sep:" & " (* column delimiter in latex *)
      |> fun s -> "      " ^ s ^ " \\\\" (* line delimiter in latex *)
  in
  let non_hline_rows = 
    x
    |> List.filter ~f:(function Row_or_hline.Hline -> false | _ -> true)
    |> List.map ~f:show_single_row
    |> align_ampersands
  in
  List.fold x ~init:(0, []) ~f:(fun (i, acc) row ->
    match row with
    | Row_or_hline.Hline -> (i, show_single_row row :: acc)
    | _ -> (i + 1, List.nth_exn non_hline_rows i :: acc)
    )
  |> Tuple2.get2
  |> List.rev

let show (type row) (x : row t) : string =
  let module R = (val x.row_module) in
  let tabular_cols = Column.tabular_cols (List.length R.names) x.columns
  in
  let table_begin =
    [ "\\begin{table}"
    ; "  \\begin{center}"
    ; "    \\begin{tabular}{" ^ tabular_cols ^ "}" ]
  in
  let table_end =
    [ "    \\end{tabular}"
    ; "  \\end{center}"
    ; "\\end{table}" ]
  in
  table_begin
  @ [ "    " ^ String.concat R.names ~sep:" & " ^ "\\\\"]
  @ (show_rows R.to_strings x.rows)
  @ table_end
  |> String.concat ~sep:"\n"
