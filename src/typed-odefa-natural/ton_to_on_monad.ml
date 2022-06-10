open Batteries;;

open On_ast;;


type translation_context =
  { tc_fresh_suffix_separator : string;
    mutable tc_fresh_name_counter : int;
    mutable tc_ton_to_on_mappings : Ton_to_on_maps.t;
  }
(* [@@deriving eq, ord] *)
;;

let new_translation_context
    ?suffix:(suffix=("~"))
    ()
  : translation_context =
  { tc_fresh_suffix_separator = suffix;
    tc_fresh_name_counter = 0;
    tc_ton_to_on_mappings = Ton_to_on_maps.empty;
  }
;;

module TonTranslationMonad : sig
  include Monad.Monad;;

  (** Run the monad to completion *)
  val run : translation_context -> 'a m -> 'a

  (** Create a fresh (ie. alphatized) name *)
  val fresh_name : string -> string m

  (** Create a fresh var *)
  val fresh_ident : string -> On_ast.ident m

  (** Map an error ident to the semantic natodefa expression **)
  val add_error_to_natodefa_mapping : ident -> sem_natodefa_edesc -> unit m

  (** Map a semantic natodefa expression to the syntactic natodefa type it has **)
  val add_sem_to_syn_mapping : sem_natodefa_edesc -> syn_natodefa_edesc -> unit m

  (** Map a semantic natodefa expression to the syntactic natodefa type it has **)
  val add_error_to_tag_mapping : ident -> int -> unit m

  (** Map a semantic natodefa expression to the syntactic natodefa type it has **)
  val add_match_to_error_mapping : int -> ident -> unit m

  (** Retrieve the typed natodefa to natodefa maps from the monad *)
  val ton_to_on_maps : Ton_to_on_maps.t m

  (** Convert a list of monadic values into a singular monadic value *)
  val sequence : 'a m list -> 'a list m

  (** Left fold in the monad *)
  val list_fold_left_m : ('acc -> 'el -> 'acc m) -> 'acc -> 'el list -> 'acc m

  (** Right fold in the monad *)
  val list_fold_right_m : ('el -> 'acc -> 'acc m) -> 'el list -> 'acc -> 'acc m

  (** @@ in the monad *)
  val (@@@) : ('a -> 'b m) -> 'a m -> 'b m
end = struct
  include Monad.Make(
    struct
      type 'a m = translation_context -> 'a;;
      let return (x : 'a) : 'a m = (fun _ -> x);;
      let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
        fun ctx -> f (x ctx) ctx
      ;;
    end
    )
  ;;

  let run ctx m = m ctx
  ;;

  let fresh_name name ctx =
    let n = ctx.tc_fresh_name_counter in
    ctx.tc_fresh_name_counter <- n + 1;
    name ^ ctx.tc_fresh_suffix_separator ^ string_of_int n
  ;;

  let fresh_ident name ctx =
    let name' = fresh_name name ctx in
    Ident name'
  ;;

  let add_error_to_natodefa_mapping error_id n_expr ctx =
    let ton_on_maps = ctx.tc_ton_to_on_mappings in
    ctx.tc_ton_to_on_mappings
      <- Ton_to_on_maps.add_error_natodefa_expr_mapping ton_on_maps error_id n_expr 
  ;;

  let add_sem_to_syn_mapping sem_expr syn_expr ctx =
    let ton_on_maps = ctx.tc_ton_to_on_mappings in
    ctx.tc_ton_to_on_mappings
      <- Ton_to_on_maps.add_sem_syn_expr_mapping ton_on_maps sem_expr syn_expr 
  ;;

  let add_error_to_tag_mapping err_id expr_tag ctx = 
    let ton_on_maps = ctx.tc_ton_to_on_mappings in
    ctx.tc_ton_to_on_mappings
      <- Ton_to_on_maps.add_error_expr_tag_mapping ton_on_maps err_id expr_tag

  let add_match_to_error_mapping match_tag err_id ctx = 
    let ton_on_maps = ctx.tc_ton_to_on_mappings in
    ctx.tc_ton_to_on_mappings
      <- Ton_to_on_maps.add_match_tag_error_mapping ton_on_maps match_tag err_id
    
  let ton_to_on_maps ctx = 
    ctx.tc_ton_to_on_mappings
  ;;

  let rec sequence ms =
    match ms with
    | [] -> return []
    | h::t ->
      let%bind h' = h in
      let%bind t' = sequence t in
      return @@ h' :: t'
  ;;

  let rec list_fold_left_m fn acc els =
    match els with
    | [] -> return acc
    | h :: t ->
      let%bind acc' = fn acc h in
      list_fold_left_m fn acc' t
  ;;

  let rec list_fold_right_m fn els acc =
    match els with
    | [] -> return acc
    | h :: t ->
      let%bind acc' = list_fold_right_m fn t acc in
      fn h acc'
  ;;

  let (@@@) f x = bind x f;;
end
;;

let ident_map_map_m
    (fn : 'a -> 'b TonTranslationMonad.m)
    (m : 'a Ident_map.t)
  : 'b Ident_map.t TonTranslationMonad.m =
  let open TonTranslationMonad in
  m
  |> Ident_map.enum
  |> Enum.map (fun (k,v) -> let%bind v' = fn v in return (k,v'))
  |> List.of_enum
  |> sequence
  |> lift1 List.enum
  |> lift1 Ident_map.of_enum
;;