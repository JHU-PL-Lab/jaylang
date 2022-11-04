open Batteries
open Bluejay_ast_internal

type translation_context = {
  tc_fresh_suffix_separator : string;
  mutable tc_fresh_name_counter : int;
  mutable tc_bluejay_to_jay_mappings : Bluejay_to_jay_maps.t;
}
(* [@@deriving eq, ord] *)

let new_translation_context ?(suffix = "_b_") () : translation_context =
  {
    tc_fresh_suffix_separator = suffix;
    tc_fresh_name_counter = 0;
    tc_bluejay_to_jay_mappings = Bluejay_to_jay_maps.empty;
  }

module BluejayTranslationMonad : sig
  include Monad.Monad

  val run : translation_context -> 'a m -> 'a
  (** Run the monad to completion *)

  val fresh_name : string -> string m
  (** Create a fresh (ie. alphatized) name *)

  val fresh_ident : string -> Bluejay_ast_internal.ident m
  (** Create a fresh var *)

  val add_error_to_bluejay_mapping : ident -> sem_bluejay_edesc -> unit m
  (** Map an error ident to the semantic bluejay expression **)

  val add_sem_to_syn_mapping : sem_bluejay_edesc -> syn_bluejay_edesc -> unit m
  (** Map a semantic bluejay expression to the syntactic bluejay type it has **)

  val add_wrapped_to_unwrapped_mapping :
    sem_bluejay_edesc -> sem_bluejay_edesc -> unit m

  val add_core_to_sem_mapping :
    core_bluejay_edesc -> sem_bluejay_edesc -> unit m
  (** Map a core bluejay expression to the semantic bluejay origin **)

  val add_error_to_tag_mapping : sem_bluejay_edesc -> int -> unit m
  (** Map a semantic bluejay expression to the syntactic bluejay type it has **)

  val add_error_to_rec_fun_mapping : ident -> ident -> unit m
  (** Map to specifically keep track of which type in the let recs that it was
      connected with **)

  val add_error_to_value_expr_mapping :
    sem_bluejay_edesc -> sem_bluejay_edesc -> unit m
  (** Map an error to the specific value expression it was checking **)

  val add_instrumented_tag : int -> unit m
  (** Map an error to the specific value expression it was checking **)

  val bluejay_to_jay_maps : Bluejay_to_jay_maps.t m
  (** Retrieve the typed bluejay to bluejay maps from the monad *)

  val is_instrumented : int -> bool m

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

  let fresh_ident name ctx =
    let name' = fresh_name name ctx in
    Ident name'

  let add_error_to_bluejay_mapping error_id n_expr ctx =
    let bluejay_jay_maps = ctx.tc_bluejay_to_jay_mappings in
    ctx.tc_bluejay_to_jay_mappings <-
      Bluejay_to_jay_maps.add_error_bluejay_expr_mapping bluejay_jay_maps
        error_id n_expr

  let add_sem_to_syn_mapping sem_expr syn_expr ctx =
    let bluejay_jay_maps = ctx.tc_bluejay_to_jay_mappings in
    ctx.tc_bluejay_to_jay_mappings <-
      Bluejay_to_jay_maps.add_sem_syn_expr_mapping bluejay_jay_maps sem_expr
        syn_expr

  let add_wrapped_to_unwrapped_mapping wrapped unwrapped ctx =
    let bluejay_jay_maps = ctx.tc_bluejay_to_jay_mappings in
    ctx.tc_bluejay_to_jay_mappings <-
      Bluejay_to_jay_maps.add_wrapped_unwrapped_mapping bluejay_jay_maps wrapped
        unwrapped

  let add_core_to_sem_mapping core_expr sem_expr ctx =
    let bluejay_jay_maps = ctx.tc_bluejay_to_jay_mappings in
    ctx.tc_bluejay_to_jay_mappings <-
      Bluejay_to_jay_maps.add_core_sem_expr_mapping bluejay_jay_maps core_expr
        sem_expr

  let add_error_to_tag_mapping err_expr expr_tag ctx =
    let bluejay_jay_maps = ctx.tc_bluejay_to_jay_mappings in
    ctx.tc_bluejay_to_jay_mappings <-
      Bluejay_to_jay_maps.add_error_expr_tag_mapping bluejay_jay_maps err_expr
        expr_tag

  let add_error_to_rec_fun_mapping error_id fun_name ctx =
    let bluejay_jay_maps = ctx.tc_bluejay_to_jay_mappings in
    ctx.tc_bluejay_to_jay_mappings <-
      Bluejay_to_jay_maps.add_error_rec_fun_type_mapping bluejay_jay_maps
        error_id fun_name

  let add_error_to_value_expr_mapping error_expr e_expr ctx =
    let bluejay_jay_maps = ctx.tc_bluejay_to_jay_mappings in
    ctx.tc_bluejay_to_jay_mappings <-
      Bluejay_to_jay_maps.add_error_value_expr_mapping bluejay_jay_maps
        error_expr e_expr

  let add_instrumented_tag tag ctx =
    let bluejay_jay_maps = ctx.tc_bluejay_to_jay_mappings in
    ctx.tc_bluejay_to_jay_mappings <-
      Bluejay_to_jay_maps.add_instrumented_tag bluejay_jay_maps tag

  let bluejay_to_jay_maps ctx = ctx.tc_bluejay_to_jay_mappings

  let is_instrumented (tag : int) ctx =
    let instrumented_tags = ctx.tc_bluejay_to_jay_mappings.instrumented_tags in
    List.mem tag instrumented_tags

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

let ident_map_map_m (fn : 'a -> 'b BluejayTranslationMonad.m)
    (m : 'a Ident_map.t) : 'b Ident_map.t BluejayTranslationMonad.m =
  let open BluejayTranslationMonad in
  m |> Ident_map.enum
  |> Enum.map (fun (k, v) ->
         let%bind v' = fn v in
         return (k, v'))
  |> List.of_enum |> sequence |> lift1 List.enum |> lift1 Ident_map.of_enum
