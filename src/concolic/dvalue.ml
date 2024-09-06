open Jayil.Ast (* opens the value types you see here *)

type t =
  | Direct of value (* record, function, int, or bool *)
  | FunClosure of Ident.t * function_value * denv
  | RecordClosure of record_value * denv
  | AbortClosure of denv

and t_with_key = t * Concolic_key.t
and denv = t_with_key Ident_map.t (* environment *)

let value_of_t = function
  | Direct v -> v
  | FunClosure (_fid, fv, _env) -> Value_function fv
  | RecordClosure (r, _env) -> Value_record r
  | AbortClosure _ -> Value_bool false

let rec pp = function
  | Direct v -> Jayil.Pp.show_value v
  | FunClosure _ -> Format.sprintf "(fc)"
  | RecordClosure (r, _) -> pp_record_c r
  | AbortClosure _ -> Format.sprintf "(abort)"

and pp_record_c (Record_value r) =
  let pp_entry (x, v) = Format.sprintf "%s = %s" (Dj_common.Id.show x) (Jayil.Pp.show_var v) in
  let s = Ident_map.to_list r |> List.map pp_entry |> String.concat ", " in
  Format.sprintf "@[{%s}@]" s
