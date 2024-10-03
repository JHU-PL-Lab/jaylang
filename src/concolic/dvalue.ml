open Jayil.Ast (* opens the value types you see here *)

module rec T :
  sig
    type t =
      | Direct of value (* record, function, int, or bool *)
      | FunClosure of Ident.t * function_value * Denv.t
      | RecordClosure of record_value * Denv.t
      | AbortClosure of Denv.t

    val value_of_t : t -> value

    val pp : t -> string

    val is_int_or_bool : t -> bool

    val to_sort_exn : t -> Sudu.Simpler_z3_api.Sort.t
  end
  =
  struct
    type t =
      | Direct of value (* record, function, int, or bool *)
      | FunClosure of Ident.t * function_value * Denv.t
      | RecordClosure of record_value * Denv.t
      | AbortClosure of Denv.t

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

    let is_int_or_bool = function
      | Direct (Value_int _) | Direct (Value_bool _) -> true
      | _ -> false

    let to_sort_exn = function
      | Direct (Value_int _) -> Sudu.Simpler_z3_api.Sort.Int_sort
      | Direct (Value_bool _) -> Bool_sort
      | _ -> failwith "no sort for dvalue that is not direct int or bool"
  end
and Denv :
  sig
    type t

    val empty : t

    val add : t -> Ident_new.t -> T.t -> Concolic_key.t -> t

    val fetch : t -> var -> T.t * Concolic_key.t

    val fetch_val : t -> var -> T.t

    val fetch_key : t -> var -> Concolic_key.t
  end
  =
  struct
    type t = (T.t * Concolic_key.t) Ident_map.t

    let empty : t = Ident_map.empty

    let add (env : t) (id : Ident_new.t) (dvalue : T.t) (key : Concolic_key.t) : t =
      Ident_map.add id (dvalue, key) env

    let fetch (env : t) (Var (x, _) : var) : T.t * Concolic_key.t =
      Ident_map.find x env (* find the variable and key in the environment *)

    let fetch_val (env : t) (x : var) : T.t =
      fst (fetch env x) (* find variable and key, then discard key *)

    let fetch_key (env : t) (x : var) : Concolic_key.t =
      snd (fetch env x) (* find variable and key, then discard variable *)
  end

include T