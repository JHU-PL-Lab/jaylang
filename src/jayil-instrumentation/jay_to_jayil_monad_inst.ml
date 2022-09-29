open Batteries
open Jayil

type translation_context = {
  tc_fresh_suffix_separator : string;
  tc_contextual_recursion : bool;
  mutable tc_fresh_name_counter : int;
  mutable tc_jayil_instrumentation_mappings : Jayil_instrumentation_maps.t;
}
(* [@@deriving eq, ord] *)

let new_translation_context ?(is_jay = false) ?(suffix = "_i_")
    ?(contextual_recursion = true) () : translation_context =
  {
    tc_fresh_name_counter = 0;
    tc_fresh_suffix_separator = suffix;
    tc_contextual_recursion = contextual_recursion;
    tc_jayil_instrumentation_mappings = Jayil_instrumentation_maps.empty is_jay;
  }

let new_translation_context_from_jay ?(suffix = "_i_")
    ?(contextual_recursion = true) natodefa_inst_map : translation_context =
  {
    tc_fresh_name_counter = 0;
    tc_fresh_suffix_separator = suffix;
    tc_contextual_recursion = contextual_recursion;
    tc_jayil_instrumentation_mappings =
      Jayil_instrumentation_maps.inherit_from_jay_to_jayil_maps
        natodefa_inst_map;
  }

module TranslationMonad : sig
  include Monad.Monad

  val run : translation_context -> 'a m -> 'a
  (** Run the monad to completion *)

  val fresh_name : string -> string m
  (** Create a fresh (ie. alphatized) name *)

  val fresh_var : string -> Ast.var m
  (** Create a fresh var *)

  val add_var_clause_pair : Ast.var -> Ast.clause -> unit m
  (** Map an odefa var to its clause *)

  val add_instrument_var : Ast.var -> unit m
  (** Add an odefa var to note that it was added during instrumentation (and
      that it does not have an associated pre-instrumentation clause) *)

  val add_instrument_var_pair : Ast.var -> Ast.var -> unit m
  (** Map an odefa clause to its associated pre-instrumentation clause *)

  val is_instrument_var : Ast.var -> bool m
  (** Returns true if the odefa var was added during instrumentation, false
      otherwise. Used to avoid unnecessary instrumentation. *)

  val get_jayil_inst_maps : Jayil_instrumentation_maps.t m
  (** Retrieve the odefa-to-natodefa maps from the monad *)

  val freshness_string : string m
  (** Retrieve the freshness string from the monad *)

  val acontextual_recursion : bool m
  (** Retrieve the contextual recursion boolean value *)

  val sequence : 'a m list -> 'a list m
  (** Convert a list of monadic values into a singular monadic value *)

  val list_fold_left_m : ('acc -> 'el -> 'acc m) -> 'acc -> 'el list -> 'acc m
  (** Left fold in the monad *)

  val list_fold_right_m : ('el -> 'acc -> 'acc m) -> 'el list -> 'acc -> 'acc m
  (** Right fold in the monad *)

  val ( @@@ ) : ('a -> 'b m) -> 'a m -> 'b m
  (** @@ in the monad *)
end = struct
  include Monad.Make (struct
    type 'a m = translation_context -> 'a

    let return (x : 'a) : 'a m = fun _ -> x
    let bind (x : 'a m) (f : 'a -> 'b m) : 'b m = fun ctx -> f (x ctx) ctx
  end)

  let run ctx m = m ctx

  let fresh_name name ctx =
    let n = ctx.tc_fresh_name_counter in
    ctx.tc_fresh_name_counter <- n + 1 ;
    name ^ ctx.tc_fresh_suffix_separator ^ string_of_int n

  let fresh_var name ctx =
    let name' = fresh_name name ctx in
    Ast.Var (Ast.Ident name', None)

  let add_var_clause_pair v_key cls_val ctx =
    let (Ast.Var (i_key, _)) = v_key in
    let odefa_inst_maps = ctx.tc_jayil_instrumentation_mappings in
    ctx.tc_jayil_instrumentation_mappings <-
      Jayil_instrumentation_maps.add_odefa_var_clause_mapping odefa_inst_maps
        i_key cls_val

  let add_instrument_var v ctx =
    let (Ast.Var (i, _)) = v in
    let odefa_inst_maps = ctx.tc_jayil_instrumentation_mappings in
    ctx.tc_jayil_instrumentation_mappings <-
      Jayil_instrumentation_maps.add_odefa_instrument_var odefa_inst_maps i None

  let add_instrument_var_pair v_key v_val ctx =
    let (Ast.Var (i_key, _)) = v_key in
    let (Ast.Var (i_val, _)) = v_val in
    let odefa_inst_maps = ctx.tc_jayil_instrumentation_mappings in
    ctx.tc_jayil_instrumentation_mappings <-
      Jayil_instrumentation_maps.add_odefa_instrument_var odefa_inst_maps i_key
        (Some i_val)

  let is_instrument_var v ctx =
    let (Ast.Var (i, _)) = v in
    let odefa_inst_maps = ctx.tc_jayil_instrumentation_mappings in
    Jayil_instrumentation_maps.is_var_instrumenting odefa_inst_maps i

  let get_jayil_inst_maps ctx = ctx.tc_jayil_instrumentation_mappings
  let freshness_string ctx = ctx.tc_fresh_suffix_separator
  let acontextual_recursion ctx = not ctx.tc_contextual_recursion

  let rec sequence ms =
    match ms with
    | [] -> return []
    | h :: t ->
        let%bind h' = h in
        let%bind t' = sequence t in
        return @@ (h' :: t')

  let rec list_fold_left_m fn acc els =
    match els with
    | [] -> return acc
    | h :: t ->
        let%bind acc' = fn acc h in
        list_fold_left_m fn acc' t

  let rec list_fold_right_m fn els acc =
    match els with
    | [] -> return acc
    | h :: t ->
        let%bind acc' = list_fold_right_m fn t acc in
        fn h acc'

  let ( @@@ ) f x = bind x f
end
