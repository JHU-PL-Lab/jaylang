open Jayil.Ast (* opens the value types you see here *)

type t =
  | Direct of value (* record, function, int, or bool *)
  | FunClosure of Ident.t * function_value * denv
  | RecordClosure of record_value * denv
  | AbortClosure of denv

and t_with_depth = t * Fun_depth.t
and denv = t_with_depth Ident_map.t (* environment *)

let value_of_t = function
  | Direct v -> v
  | FunClosure (_fid, fv, _env) -> Value_function fv
  | RecordClosure (r, _env) -> Value_record r
  | AbortClosure _ -> Value_bool false

let rec pp oc = function
  | Direct v -> Jayil.Pp.value oc v
  | FunClosure _ -> Format.fprintf oc "(fc)"
  | RecordClosure (r, _) -> pp_record_c r oc
  | AbortClosure _ -> Format.fprintf oc "(abort)"

and pp_record_c (Record_value r) oc =
  let pp_entry oc (x, v) = Fmt.pf oc "%a = %a" Dj_common.Id.pp x Jayil.Pp.var_ v in
  (Fmt.braces (Fmt.iter_bindings ~sep:(Fmt.any ", ") Ident_map.iter pp_entry))
    oc r
