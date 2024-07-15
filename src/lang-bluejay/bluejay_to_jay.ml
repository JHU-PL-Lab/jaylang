open Batteries
open Jhupllib
open Bluejay_ast_internal
open Bluejay_to_jay_monad
open BluejayTranslationMonad

let rec mem (l : 'a list) (elm : 'a) (eq : 'a -> 'a -> bool) : bool =
  match l with
  | [] -> false
  | hd :: tl -> if eq elm hd then true else mem tl elm eq

let lazy_logger = Logger_utils.make_lazy_logger "Bluejay_to_jay"

let transform_funsig (f : 'a expr_desc -> 'b expr_desc m)
    (Funsig (fun_name, params, e) : 'a funsig) : 'b funsig m =
  let%bind e' = f e in
  return @@ Funsig (fun_name, params, e')

let transform_typed_funsig (f : 'a expr_desc -> 'b expr_desc m)
    (fun_sig : 'a typed_funsig) : 'b typed_funsig m =
  match fun_sig with
  | Typed_funsig (fun_name, typed_params, (e, ret_type)) ->
      let typed_params_m =
        List.map
          (fun (param, t) ->
            let%bind t' = f t in
            return (param, t'))
          typed_params
      in
      let%bind typed_params' = sequence typed_params_m in
      let%bind e' = f e in
      let%bind ret_type' = f ret_type in
      return @@ Typed_funsig (fun_name, typed_params', (e', ret_type'))
  | DTyped_funsig (fun_name, (param, t), (e, ret_type)) ->
      let%bind t' = f t in
      let%bind e' = f e in
      let%bind ret_type' = f ret_type in
      return @@ DTyped_funsig (fun_name, (param, t'), (e', ret_type'))

let remove_type_from_funsig (f : 'a expr_desc -> 'b expr_desc m)
    (fun_sig : 'a typed_funsig) : 'b funsig m =
  match fun_sig with
  | Typed_funsig (fun_name, typed_params, (e, _)) ->
      let params = List.map (fun (param, _) -> param) typed_params in
      let%bind e' = f e in
      return @@ Funsig (fun_name, params, e')
  | DTyped_funsig (fun_name, (param, _), (e, _)) ->
      let%bind e' = f e in
      return @@ Funsig (fun_name, [ param ], e')

let new_instrumented_ed (e : 'a expr) : 'a expr_desc m =
  let ed = new_expr_desc e in
  let%bind () = add_instrumented_tag ed.tag in
  (* let () =
       Fmt.pr "New_instrumented_ed: %a; tag: %n \n"
         Bluejay_ast_internal_pp.pp_expr_desc_with_tag ed ed.tag
     in *)
  return ed

let mk_gc_pair_cod arg_id cod arg =
  Appl (new_expr_desc @@ Function ([ arg_id ], cod), new_expr_desc @@ Var arg)

let wrap_flag = ref false

let rec check_type_presence (present_types : 'a expr_desc list)
    (t : 'a expr_desc) : bool =
  match present_types with
  | [] -> false
  | hd :: tl ->
      if tagless_equal_expr_desc hd t then true else check_type_presence tl t

let rec wrap (e_desc : syntactic_only expr_desc) : syntactic_only expr_desc m =
  let mk_check_from_fun_sig fun_sig =
    match fun_sig with
    | Typed_funsig (Ident f, typed_params, (f_body, ret_type)) ->
        let%bind ret_type' = wrap ret_type in
        let%bind f_body' = wrap f_body in
        let typed_params' =
          typed_params |> List.map (fun (p, t) -> (p, wrap t))
        in
        let%bind processed_params, _ =
          list_fold_left_m
            (fun (t_tvs_lst, bound_types) (Ident p, tm) ->
              let%bind t = tm in
              let tvs = get_poly_vars t [] in
              let%bind tvs_tids =
                let%bind tvs_tids_m =
                  tvs
                  |> List.map (fun tv ->
                         if check_type_presence (List.map fst bound_types) tv
                         then return @@ None
                         else
                           let%bind tid = fresh_ident "t" in
                           return @@ Some (tv, tid))
                  |> sequence
                in
                return @@ List.filter_map (fun x -> x) tvs_tids_m
              in
              (* let tvs_unique, _tids =
                   List.fold_right
                     (fun (tvs, tid) (lacc, racc) -> (tvs :: lacc, tid :: racc))
                     tvs_tids ([], [])
                 in *)
              let bound_types' = tvs_tids @ bound_types in
              let t' =
                List.fold_left
                  (fun acc (poly_var, tid) ->
                    replace_tagless_expr_desc acc poly_var
                      (new_expr_desc @@ Var tid))
                  t bound_types'
              in
              let%bind t_wrap_id = fresh_ident "wrap_t" in
              let%bind eta_arg = fresh_ident p in
              return
              @@ ( ((p, eta_arg), (t', tvs_tids, t_wrap_id)) :: t_tvs_lst,
                   bound_types' ))
            ([], []) typed_params'
        in
        let processed_params = List.rev processed_params in
        let folder2 ((og_arg, arg), (t, tvs_tids, wrap_t)) acc =
          let t' =
            List.fold_left
              (fun acc (poly_var, tid) ->
                replace_tagless_expr_desc acc poly_var (new_expr_desc @@ Var tid))
              t tvs_tids
          in
          let%bind proj_ed_1 =
            (* match t_id_opt with
               | None -> new_instrumented_ed @@ RecordProj (t, Label "checker")
               | Some tid ->
                   new_instrumented_ed
                   @@ RecordProj (new_expr_desc @@ Var tid, Label "checker") *)
            new_instrumented_ed @@ RecordProj (t', Label "checker")
          in
          let%bind check_arg =
            new_instrumented_ed
            @@ If
                 ( new_expr_desc
                   @@ GreaterThan
                        (new_expr_desc @@ Input, new_expr_desc @@ Int 0),
                   new_expr_desc @@ Appl (proj_ed_1, new_expr_desc @@ Var arg),
                   new_expr_desc @@ Bool true )
          in
          let wrapped_arg =
            new_expr_desc
            @@ Appl (new_expr_desc @@ Var wrap_t, new_expr_desc @@ Var arg)
          in
          let rebind_arg =
            new_expr_desc @@ Let (Ident og_arg, wrapped_arg, acc)
          in
          let%bind layered_fn =
            let%bind fn_body =
              new_instrumented_ed
              @@ If
                   ( check_arg,
                     rebind_arg,
                     new_expr_desc @@ Assert (new_expr_desc @@ Bool false) )
            in
            return @@ new_expr_desc @@ Function ([ arg ], fn_body)
          in
          let acc' =
            if List.is_empty tvs_tids
            then
              let%bind wrap_fn =
                new_instrumented_ed @@ RecordProj (t', Label "wrapper")
              in
              let wrap_def =
                new_expr_desc @@ Let (wrap_t, wrap_fn, layered_fn)
              in
              return wrap_def
            else
              let%bind wrap_fn =
                new_instrumented_ed @@ RecordProj (t', Label "wrapper")
              in
              let wrap_def =
                new_expr_desc @@ Let (wrap_t, wrap_fn, layered_fn)
              in
              let tids = List.map snd tvs_tids in
              let new_fn = new_expr_desc @@ Function (tids, wrap_def) in
              return new_fn
          in
          acc'
        in
        let%bind proj_ed_ret =
          let ret_type'' =
            List.fold_left
              (fun acc (_, (_, tvs_unique, _)) ->
                List.fold_left
                  (fun acc (poly_var, tid) ->
                    replace_tagless_expr_desc acc poly_var
                      (new_expr_desc @@ Var tid))
                  acc tvs_unique)
              ret_type' processed_params
          in
          new_instrumented_ed @@ RecordProj (ret_type'', Label "wrapper")
        in
        let final_appl = new_expr_desc @@ Appl (proj_ed_ret, f_body') in
        let%bind wrapped_appl =
          list_fold_right_m folder2 (List.tl processed_params) final_appl
        in
        let (fst_og_arg, fst_arg), (fst_t, fst_tvs_tids, fst_wrap_t) =
          List.hd processed_params
        in
        let fst_t' =
          List.fold_left
            (fun acc (poly_var, tid) ->
              replace_tagless_expr_desc acc poly_var (new_expr_desc @@ Var tid))
            fst_t fst_tvs_tids
        in
        let%bind handle_fst =
          let%bind proj_ed_1 =
            (* if List.is_empty fst_tvs_tids
               then new_instrumented_ed @@ RecordProj (fst_t, Label "checker")
               else *)
            new_instrumented_ed @@ RecordProj (fst_t', Label "checker")
          in
          let%bind check_arg =
            new_instrumented_ed
            @@ If
                 ( new_expr_desc
                   @@ GreaterThan
                        (new_expr_desc @@ Input, new_expr_desc @@ Int 0),
                   new_expr_desc
                   @@ Appl (proj_ed_1, new_expr_desc @@ Var fst_arg),
                   new_expr_desc @@ Bool true )
          in
          let wrapped_arg =
            new_expr_desc
            @@ Appl
                 (new_expr_desc @@ Var fst_wrap_t, new_expr_desc @@ Var fst_arg)
          in
          let rebind_arg =
            new_expr_desc @@ Let (Ident fst_og_arg, wrapped_arg, wrapped_appl)
          in
          let%bind layered_fn =
            let%bind fn_body =
              new_instrumented_ed
              @@ If
                   ( check_arg,
                     rebind_arg,
                     new_expr_desc @@ Assert (new_expr_desc @@ Bool false) )
            in
            return @@ new_expr_desc @@ Function ([ fst_arg ], fn_body)
          in
          (* match fst_t_id_opt with *)
          if List.is_empty fst_tvs_tids
          then
            let%bind wrap_fn =
              new_instrumented_ed @@ RecordProj (fst_t, Label "wrapper")
            in
            let wrap_def =
              new_expr_desc @@ Let (fst_wrap_t, wrap_fn, layered_fn)
            in
            let%bind eta_arg = fresh_ident "eta" in
            let%bind eta_appl =
              new_instrumented_ed
              @@ Appl (wrap_def, new_expr_desc @@ Var eta_arg)
            in
            return @@ Funsig (Ident f, [ eta_arg ], eta_appl)
          else
            let%bind wrap_fn =
              new_instrumented_ed @@ RecordProj (fst_t', Label "wrapper")
            in
            let wrap_def =
              new_expr_desc @@ Let (fst_wrap_t, wrap_fn, layered_fn)
            in
            (* let new_fn = new_expr_desc @@ Function ([ t_id ], wrap_def) in *)
            let fst_tids = List.map snd fst_tvs_tids in
            let new_fn = Funsig (Ident f, fst_tids, wrap_def) in
            return new_fn
        in
        (* let ret_ed = new_expr_desc @@ Let (Ident f, wrapped_appl, let_expr) in *)
        return
        @@ ( Typed_funsig (Ident f, typed_params, (f_body', ret_type')),
             handle_fst )
    | DTyped_funsig (Ident f, ((Ident p as og_arg), t), (f_body, ret_type)) ->
        let%bind ret_type' = wrap ret_type in
        let%bind f_body' = wrap f_body in
        let%bind t' = wrap t in
        let tvs = get_poly_vars t' [] in
        let%bind tvs_tids =
          tvs
          |> List.map (fun tv ->
                 let%bind tid = fresh_ident "t" in
                 return @@ (tv, tid))
          |> sequence
        in
        let t'' =
          List.fold_left
            (fun acc (poly_var, tid) ->
              replace_tagless_expr_desc acc poly_var (new_expr_desc @@ Var tid))
            t tvs_tids
        in
        let%bind arg = fresh_ident p in
        let%bind wrap_t = fresh_ident "wrap_t" in
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (t'', Label "checker")
        in
        let%bind check_arg =
          new_instrumented_ed
          @@ If
               ( new_expr_desc
                 @@ GreaterThan (new_expr_desc @@ Input, new_expr_desc @@ Int 0),
                 new_expr_desc @@ Appl (proj_ed_1, new_expr_desc @@ Var arg),
                 new_expr_desc @@ Bool true )
        in
        let wrapped_arg =
          new_expr_desc
          @@ Appl (new_expr_desc @@ Var wrap_t, new_expr_desc @@ Var arg)
        in
        let%bind proj_ed_ret =
          let ret_type'' =
            List.fold_left
              (fun acc (poly_var, tid) ->
                replace_tagless_expr_desc acc poly_var (new_expr_desc @@ Var tid))
              ret_type' tvs_tids
          in
          let%bind arg' = fresh_ident p in
          let ret_type_renamed =
            replace_var_of_expr_desc ret_type'' og_arg arg
          in
          let final_ret_type =
            new_expr_desc @@ mk_gc_pair_cod arg' ret_type_renamed arg
          in
          new_instrumented_ed @@ RecordProj (final_ret_type, Label "wrapper")
        in
        let final_appl = new_expr_desc @@ Appl (proj_ed_ret, f_body') in
        let rebind_arg =
          new_expr_desc @@ Let (og_arg, wrapped_arg, final_appl)
        in
        let%bind layered_fn =
          let%bind fn_body =
            new_instrumented_ed
            @@ If
                 ( check_arg,
                   rebind_arg,
                   new_expr_desc @@ Assert (new_expr_desc @@ Bool false) )
          in
          return @@ new_expr_desc @@ Function ([ arg ], fn_body)
        in
        let%bind wrap_ret =
          if List.is_empty tvs_tids
          then
            let%bind wrap_fn =
              new_instrumented_ed @@ RecordProj (t'', Label "wrapper")
            in
            let wrap_def = new_expr_desc @@ Let (wrap_t, wrap_fn, layered_fn) in
            let%bind eta_arg = fresh_ident "eta" in
            let%bind eta_appl =
              new_instrumented_ed
              @@ Appl (wrap_def, new_expr_desc @@ Var eta_arg)
            in
            return @@ Funsig (Ident f, [ eta_arg ], eta_appl)
            (* return wrap_def *)
          else
            let%bind wrap_fn =
              new_instrumented_ed @@ RecordProj (t'', Label "wrapper")
            in
            let wrap_def = new_expr_desc @@ Let (wrap_t, wrap_fn, layered_fn) in
            (* let new_fn = new_expr_desc @@ Function ([ t_id ], wrap_def) in
               return new_fn *)
            let tids = List.map snd tvs_tids in
            let new_fn = Funsig (Ident f, tids, wrap_def) in
            return new_fn
        in
        (* let ret_ed = new_expr_desc @@ Let (Ident f, wrap_ret, let_expr) in *)
        return
        @@ ( DTyped_funsig (Ident f, (og_arg, t'), (f_body', ret_type')),
             wrap_ret )
  in
  let og_tag = e_desc.tag in
  let og_e = e_desc.body in
  let%bind ret =
    match og_e with
    | TypeVar _ | TypeInt | TypeBool | TypeUntouched _ | TypeError _ | Int _
    | Bool _ | Var _ | Input ->
        return e_desc
    | TypeRecord r ->
        let%bind r' = ident_map_map_m wrap r in
        let e_desc' = new_expr_desc @@ TypeRecord r' in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | TypeList l ->
        let%bind l' = wrap l in
        let e_desc' = new_expr_desc @@ TypeList l' in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | TypeArrow (dom, cod) ->
        let%bind dom' = wrap dom in
        let%bind cod' = wrap cod in
        let e_desc' = new_expr_desc @@ TypeArrow (dom', cod') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | TypeArrowD ((x, dom), cod) ->
        let%bind dom' = wrap dom in
        let%bind cod' = wrap cod in
        let e_desc' = new_expr_desc @@ TypeArrowD ((x, dom'), cod') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | TypeSet (s, p) ->
        let%bind s' = wrap s in
        let%bind p' = wrap p in
        let e_desc' = new_expr_desc @@ TypeSet (s', p') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | TypeUnion (t1, t2) ->
        let%bind t1' = wrap t1 in
        let%bind t2' = wrap t2 in
        let e_desc' = new_expr_desc @@ TypeUnion (t1', t2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | TypeIntersect (t1, t2) ->
        let%bind t1' = wrap t1 in
        let%bind t2' = wrap t2 in
        let e_desc' = new_expr_desc @@ TypeIntersect (t1', t2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | TypeRecurse (tvar, t) ->
        let%bind t' = wrap t in
        let e_desc' = new_expr_desc @@ TypeRecurse (tvar, t') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | TypeVariant vs ->
        let%bind vs' =
          sequence
          @@ List.map
               (fun (v_lbl, t) ->
                 let%bind t' = wrap t in
                 return (v_lbl, t'))
               vs
        in
        let e_desc' = new_expr_desc @@ TypeVariant vs' in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Function (xs, f_body) ->
        let%bind f_body' = wrap f_body in
        let e_desc' = new_expr_desc @@ Function (xs, f_body') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Appl (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Appl (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Let (x, e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Let (x, e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | LetRecFun (funsigs, e) ->
        let%bind funsigs' =
          sequence @@ List.map (transform_funsig wrap) funsigs
        in
        let%bind e' = wrap e in
        let e_desc' = new_expr_desc @@ LetRecFun (funsigs', e') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | LetFun (funsig, e) ->
        let%bind funsig' = transform_funsig wrap funsig in
        let%bind e' = wrap e in
        let e_desc' = new_expr_desc @@ LetFun (funsig', e') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | LetWithType (x, e1, e2, t) ->
        let%bind t' = wrap t in
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (t', Label "wrapper")
        in
        let e1'' = new_expr_desc @@ Appl (proj_ed_1, e1') in
        let rebind = new_expr_desc @@ Let (x, e1'', e2') in
        let res = new_expr_desc @@ LetWithType (x, e1', rebind, t') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | LetRecFunWithType (sig_lst, e) ->
        let%bind a = sequence @@ List.map mk_check_from_fun_sig sig_lst in
        let sig_lst', sig_lst_wrapped =
          List.fold_right
            (fun (l, r) (lacc, racc) -> (l :: lacc, r :: racc))
            a ([], [])
        in
        let%bind og_e' = wrap e in
        let overrides = new_expr_desc @@ LetRecFun (sig_lst_wrapped, og_e') in
        let res = new_expr_desc @@ LetRecFunWithType (sig_lst', overrides) in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | LetFunWithType (fun_sig, e) ->
        let%bind og_e' = wrap e in
        let%bind fun_sig', fun_sig_wrapped = mk_check_from_fun_sig fun_sig in
        let override = new_expr_desc @@ LetFun (fun_sig_wrapped, og_e') in
        let res = new_expr_desc @@ LetFunWithType (fun_sig', override) in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Plus (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Plus (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Minus (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Minus (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Times (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Times (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Divide (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Divide (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Modulus (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Modulus (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Equal (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Equal (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Neq (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Neq (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | LessThan (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ LessThan (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Leq (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Leq (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | GreaterThan (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ GreaterThan (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Geq (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Geq (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | And (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ And (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Or (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ Or (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Not e ->
        let%bind e' = wrap e in
        let e_desc' = new_expr_desc @@ Not e' in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | If (e1, e2, e3) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let%bind e3' = wrap e3 in
        let e_desc' = new_expr_desc @@ If (e1', e2', e3') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Record r ->
        let%bind r' = ident_map_map_m wrap r in
        let e_desc' = new_expr_desc @@ Record r' in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | RecordProj (e, lbl) ->
        let%bind e' = wrap e in
        let e_desc' = new_expr_desc @@ RecordProj (e', lbl) in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Match (e, pat_expr_lst) ->
        let%bind e' = wrap e in
        let%bind pat_expr_lst' =
          sequence
          @@ List.map
               (fun (p, e) ->
                 let%bind e' = wrap e in
                 return (p, e'))
               pat_expr_lst
        in
        let e_desc' = new_expr_desc @@ Match (e', pat_expr_lst') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | VariantExpr (v_lbl, e) ->
        let%bind e' = wrap e in
        let e_desc' = new_expr_desc @@ VariantExpr (v_lbl, e') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | List l ->
        let%bind l' = sequence @@ List.map wrap l in
        let e_desc' = new_expr_desc @@ List l' in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | ListCons (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let e_desc' = new_expr_desc @@ ListCons (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Assert e ->
        let%bind e' = wrap e in
        let e_desc' = new_expr_desc @@ Assert e' in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
    | Assume e ->
        let%bind e' = wrap e in
        let e_desc' = new_expr_desc @@ Assume e' in
        let%bind () = add_wrapped_to_unwrapped_mapping e_desc' e_desc in
        return e_desc'
  in
  let%bind instrumented_bool = is_instrumented og_tag in
  let%bind () =
    if instrumented_bool then add_instrumented_tag ret.tag else return ()
  in
  return ret

(* Phase one of transformation: turning all syntactic types into its
   semantic correspondence.
   i.e. int -> { generator = fun _ -> input,
               , checker = fun e -> isInt e
               , wrap = fun e -> e
               }
   - The transformation should provide the guarantee that only the type
     expression subtrees are modified, and we should be able to recover the
     original tree by walking over the transformed tree and replace everywhere
     where there is a mapping.
   - Note that the backward transformation (sem -> syn) should only need to
     look up the "outmost" layer, since this function maps fully transformed
     expression to its previous form (i.e. all subexpressions of a transformed
     expression is also transformed).
   - Also in this function, because we need to ensure that all subexpressions
     contain unique tags, we cannot reuse a fabricated AST from a recursive
     call, which is a source of redundancy. *)
let rec semantic_type_of (e_desc : syntactic_only expr_desc) :
    semantic_only expr_desc m =
  let t = e_desc.body in
  let tag = e_desc.tag in
  let%bind t' =
    match t with
    | TypeVar tvar ->
        (* When it's a single type variable, we have the invariance that it comes
           from a recursive type. Therefore, we want to roll in the self application
           here.

           tv -> (tv tv) *)
        let res = new_expr_desc @@ Var tvar in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeInt ->
        let generator = Function ([ Ident "~null" ], new_expr_desc Input) in
        let%bind checker =
          let%bind expr_id = fresh_ident "expr" in
          let%bind fail_id = fresh_ident "fail" in
          let matched_expr = new_expr_desc @@ Var expr_id in
          let fail_pat_cls = new_expr_desc @@ Var fail_id in
          let%bind match_edesc =
            new_instrumented_ed
            @@ Match
                 ( matched_expr,
                   [
                     (IntPat, new_expr_desc @@ Bool true); (AnyPat, fail_pat_cls);
                   ] )
          in
          let check_cls = Function ([ expr_id ], match_edesc) in
          let fail_cls =
            Let
              (fail_id, new_expr_desc @@ Bool false, new_expr_desc @@ check_cls)
          in
          (* Here we have a potential point of error, and we need to remember its
             position in the AST, so that later we can use it to replace the node
             with the actual type in error reporting. *)
          let%bind () = add_error_to_tag_mapping fail_pat_cls tag in
          (* Here, we're keeping track of the mapping between the point of error
             and the actual expression that might cause this error. With this
             information, we can find its jayil equivlanet and use the solver
             solution to get a concrete solution. *)
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls matched_expr
          in
          return @@ fail_cls
        in
        let%bind wrapper =
          let%bind expr_id = fresh_ident "expr" in
          return @@ Function ([ expr_id ], new_expr_desc @@ Var expr_id)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        (* Keeping track of which original expression this transformed expr
           corresponds to. Useful when we need to reconstruct the original expr
           later in error reporting. *)
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeBool ->
        let generator =
          Function
            ( [ Ident "~null" ],
              new_expr_desc @@ Geq (new_expr_desc Input, new_expr_desc @@ Int 0)
            )
        in
        let%bind checker =
          let%bind expr_id = fresh_ident "expr" in
          let%bind fail_id = fresh_ident "fail" in
          let matched_expr = new_expr_desc @@ Var expr_id in
          let fail_pat_cls = new_expr_desc @@ Var fail_id in
          let%bind match_edesc =
            new_instrumented_ed
            @@ Match
                 ( matched_expr,
                   [
                     (BoolPat, new_expr_desc @@ Bool true);
                     (AnyPat, fail_pat_cls);
                   ] )
          in
          let check_cls = Function ([ expr_id ], match_edesc) in
          let fail_cls =
            Let
              (fail_id, new_expr_desc @@ Bool false, new_expr_desc @@ check_cls)
          in
          (* We have another point of error here, thus the mapping. *)
          let%bind () = add_error_to_tag_mapping fail_pat_cls tag in
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls matched_expr
          in
          return @@ fail_cls
        in
        let%bind wrapper =
          let%bind expr_id = fresh_ident "expr" in
          return @@ Function ([ expr_id ], new_expr_desc @@ Var expr_id)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeRecord r ->
        (* TODO: Potential source of bug here; I'm assuming that the record values
           we generate will have to go through the wrap/unwarp process as well. *)
        let%bind generator =
          (* For the generator, we need to generate the corresponding value for
             each field, from their declared type. *)
          let all_bindings = Ident_map.bindings r in
          let empty_record = Ident_map.empty in
          let mapper (lbl, lbl_type) =
            let (Ident lbl_str) = lbl in
            let%bind lbl_var = fresh_ident lbl_str in
            return @@ (lbl, lbl_var, lbl_type)
          in
          let%bind lbl_to_var = all_bindings |> List.map mapper |> sequence in
          let folder acc (lbl, lbl_var, _) =
            return @@ Ident_map.add lbl (new_expr_desc @@ Var lbl_var) acc
          in
          let%bind res_record =
            list_fold_left_m folder empty_record lbl_to_var
          in
          let folder' acc (_, lbl_var, cur_t) =
            let%bind gc_pair = semantic_type_of cur_t in
            (* let%bind proj_ed_inner =
                 new_instrumented_ed @@ RecordProj (gc_pair, Label "~actual_rec")
               in *)
            let%bind proj_ed =
              new_instrumented_ed @@ RecordProj (gc_pair, Label "generator")
            in
            let%bind appl_ed =
              new_instrumented_ed @@ Appl (proj_ed, new_expr_desc @@ Int 0)
            in
            let res = new_expr_desc @@ Let (lbl_var, appl_ed, acc) in
            return res
          in
          let%bind base_acc = new_instrumented_ed @@ Record res_record in
          let%bind gen_expr = list_fold_left_m folder' base_acc lbl_to_var in
          let%bind actual_rec =
            let%bind decl_lbls =
              Ident_map.key_list r
              |> list_fold_left_m
                   (fun acc k ->
                     let%bind empty_rec =
                       new_instrumented_ed @@ Record Ident_map.empty
                     in
                     return @@ Ident_map.add k empty_rec acc)
                   Ident_map.empty
            in
            let%bind new_lbls_rec = new_instrumented_ed @@ Record decl_lbls in
            let ret =
              Ident_map.empty
              |> Ident_map.add (Ident "~actual_rec") gen_expr
              |> Ident_map.add (Ident "~decl_lbls") new_lbls_rec
            in
            return ret
          in
          let%bind ret_rec = new_instrumented_ed @@ Record actual_rec in
          return @@ Function ([ Ident "~null" ], ret_rec)
        in
        let%bind checker =
          (* Building the intial check for whether it's a record value *)
          let%bind rec_fail_id_1 = fresh_ident "rec_fail" in
          let%bind rec_fail_id_2 = fresh_ident "rec_fail" in
          let%bind rec_fail_id_3 = fresh_ident "rec_fail" in
          let%bind expr_id = fresh_ident "expr" in
          let fail_pat_cls_1 = new_expr_desc @@ Var rec_fail_id_1 in
          let fail_pat_cls_2 = new_expr_desc @@ Var rec_fail_id_2 in
          let fail_pat_cls_3 = new_expr_desc @@ Var rec_fail_id_3 in
          let matched_expr_1 = new_expr_desc @@ Var expr_id in
          let matched_expr_2 = new_expr_desc @@ Var expr_id in
          let matched_expr_3 = new_expr_desc @@ Var expr_id in
          (* For the checker, we need to first check whether the value in
             question is a record. If not, returns false. Otherwise. we need
             to go through all the fields in this record to check whether it
             has the correct type for each field. *)
          let all_bindings = List.rev @@ Ident_map.bindings r in
          let rec_pat = Ident_map.singleton (Ident "~actual_rec") None in
          let type_dict =
            Ident_map.of_enum
            @@ Enum.map (fun k -> (k, Some k)) (Ident_map.keys r)
          in
          let fold_fun expr_a (lbl, t) =
            let%bind lbl_check_id = fresh_ident "lbl_check" in
            let%bind cur_gc_pair = semantic_type_of t in
            (* let%bind proj_ed_1_inner =
                 new_instrumented_ed @@ RecordProj (cur_gc_pair, Label "~actual_rec")
               in *)
            let%bind proj_ed_1 =
              new_instrumented_ed @@ RecordProj (cur_gc_pair, Label "checker")
            in
            (* let%bind proj_ed_2 =
                 new_instrumented_ed
                 @@ RecordProj (new_expr_desc @@ Var expr_id, Label lbl)
               in *)
            let%bind appl_ed =
              new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var lbl)
            in
            let%bind if_ed =
              new_instrumented_ed
              @@ If
                   ( new_expr_desc @@ Var lbl_check_id,
                     expr_a,
                     new_expr_desc @@ Var lbl_check_id )
            in
            return @@ new_expr_desc @@ Let (lbl_check_id, appl_ed, if_ed)
          in
          let first_lbl, first_type = List.hd all_bindings in
          let%bind gc_pair_fst = semantic_type_of first_type in
          (* let%bind proj_ed_3_inner =
               new_instrumented_ed @@ RecordProj (gc_pair_fst, Label "~actual_rec")
             in *)
          let%bind proj_ed_3 =
            new_instrumented_ed @@ RecordProj (gc_pair_fst, Label "checker")
          in
          let%bind init_acc =
            new_instrumented_ed
            @@ Appl (proj_ed_3, new_expr_desc @@ Var first_lbl)
          in
          let%bind fun_body =
            list_fold_left_m fold_fun init_acc (List.tl all_bindings)
          in
          let%bind actual_rec =
            new_instrumented_ed
            @@ RecordProj (matched_expr_1, Label "~actual_rec")
          in
          let%bind decl_lbls =
            new_instrumented_ed
            @@ RecordProj (matched_expr_2, Label "~decl_lbls")
          in
          let%bind lbls_check =
            new_instrumented_ed
            @@ Match
                 ( actual_rec,
                   [ (RecPat type_dict, fun_body); (AnyPat, fail_pat_cls_1) ] )
          in
          let%bind decl_lbls_check =
            new_instrumented_ed
            @@ Match
                 ( decl_lbls,
                   [ (RecPat type_dict, lbls_check); (AnyPat, fail_pat_cls_2) ]
                 )
          in
          (* Also, the record type we have here is like OCaml; it must have the
             labels with the corresponding types, and nothing more. That's why
             we require a strict pattern match here. *)
          let%bind match_body =
            new_instrumented_ed
            @@ Match
                 ( matched_expr_3,
                   [
                     (RecPat rec_pat, decl_lbls_check); (AnyPat, fail_pat_cls_3);
                   ] )
          in
          let check_cls_1 =
            Let (rec_fail_id_1, new_expr_desc @@ Bool false, match_body)
          in
          let check_cls_2 =
            Let
              ( rec_fail_id_2,
                new_expr_desc @@ Bool false,
                new_expr_desc check_cls_1 )
          in
          let check_cls_3 =
            Let
              ( rec_fail_id_3,
                new_expr_desc @@ Bool false,
                new_expr_desc check_cls_2 )
          in
          (* Since the initial record check could be a point of faliure, we need to
             record it as well. *)
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls_1 actual_rec
          in
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls_2 decl_lbls
          in
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls_3 matched_expr_3
          in
          let%bind () = add_error_to_tag_mapping fail_pat_cls_1 tag in
          let%bind () = add_error_to_tag_mapping fail_pat_cls_2 tag in
          let%bind () = add_error_to_tag_mapping fail_pat_cls_3 tag in
          return @@ Function ([ expr_id ], new_expr_desc check_cls_3)
        in
        let%bind wrapper =
          let%bind expr_id = fresh_ident "expr" in
          let%bind new_lbls =
            r |> Ident_map.key_list
            |> list_fold_left_m
                 (fun acc k ->
                   let%bind empty_rec =
                     new_instrumented_ed @@ Record Ident_map.empty
                   in
                   return @@ Ident_map.add k empty_rec acc)
                 Ident_map.empty
          in
          let%bind actual_rec =
            new_instrumented_ed
            @@ RecordProj (new_expr_desc @@ Var expr_id, Label "~actual_rec")
          in
          let%bind new_lbls_rec = new_instrumented_ed @@ Record new_lbls in
          let new_rec =
            Ident_map.empty
            |> Ident_map.add (Ident "~actual_rec") actual_rec
            |> Ident_map.add (Ident "~decl_lbls") new_lbls_rec
          in
          let%bind new_rec_ed = new_instrumented_ed @@ Record new_rec in
          return @@ Function ([ expr_id ], new_rec_ed)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeList l ->
        let%bind generator =
          (* For the generator, we will generate a list of random length containing
             well-typed elements. *)
          let%bind gc_pair_g = semantic_type_of l in
          let%bind len_id = fresh_ident "len" in
          let%bind maker_id = fresh_ident "list_maker" in
          let%bind elm_id = fresh_ident "elm" in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed @@ RecordProj (gc_pair_g, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair_g, Label "generator")
          in
          let%bind appl_ed_1 =
            new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Int 0)
          in
          let%bind minus_ed =
            new_instrumented_ed
            @@ Minus (new_expr_desc @@ Var len_id, new_expr_desc @@ Int 1)
          in
          let%bind appl_ed_2 =
            new_instrumented_ed @@ Appl (new_expr_desc @@ Var maker_id, minus_ed)
          in
          let recur_call =
            Let
              ( elm_id,
                appl_ed_1,
                new_expr_desc
                @@ ListCons (new_expr_desc @@ Var elm_id, appl_ed_2) )
          in
          let%bind eq_ed =
            new_instrumented_ed
            @@ Equal (new_expr_desc @@ Var len_id, new_expr_desc @@ Int 0)
          in
          let%bind list_maker =
            new_instrumented_ed
            @@ If (eq_ed, new_expr_desc @@ List [], new_expr_desc @@ recur_call)
          in
          let list_maker_fun = Funsig (maker_id, [ len_id ], list_maker) in
          let%bind mk_lst =
            new_instrumented_ed
            @@ Appl (new_expr_desc @@ Var maker_id, new_expr_desc @@ Var len_id)
          in
          let%bind check_len =
            new_instrumented_ed
            @@ Geq (new_expr_desc @@ Var len_id, new_expr_desc @@ Int 0)
          in
          let%bind zero_out =
            new_instrumented_ed @@ Assume (new_expr_desc @@ Bool false)
          in
          let%bind mk_lst_wrapped =
            new_instrumented_ed @@ If (check_len, mk_lst, zero_out)
          in
          let list_len = Let (len_id, new_expr_desc Input, mk_lst_wrapped) in
          let gen_expr =
            LetRecFun ([ list_maker_fun ], new_expr_desc list_len)
          in
          return @@ Function ([ Ident "~null" ], new_expr_desc gen_expr)
        in
        let%bind checker =
          (* For the checker, we need to first check whether the value in question
             is a list. If not, returns false. Otherwise. we need to go through all
             the values in the list to check whether they have the correct type. *)
          let%bind gc_pair_c = semantic_type_of l in
          let%bind test_fun_id = fresh_ident "test_fun" in
          let%bind test_list_id = fresh_ident "test_list" in
          let%bind elm_check_id = fresh_ident "elm_check" in
          let%bind expr_id = fresh_ident "expr" in
          let%bind lst_check_fail = fresh_ident "lst_fail" in
          let%bind test_fun =
            (* let%bind proj_ed_1_inner =
                 new_instrumented_ed @@ RecordProj (gc_pair_c, Label "~actual_rec")
               in *)
            let%bind proj_ed_1 =
              new_instrumented_ed @@ RecordProj (gc_pair_c, Label "checker")
            in
            let%bind appl_ed_1 =
              new_instrumented_ed
              @@ Appl (proj_ed_1, new_expr_desc @@ Var (Ident "hd"))
            in
            let%bind appl_ed_2 =
              new_instrumented_ed
              @@ Appl
                   ( new_expr_desc @@ Var test_fun_id,
                     new_expr_desc @@ Var (Ident "tl") )
            in
            let%bind if_ed =
              new_instrumented_ed
              @@ If
                   ( new_expr_desc @@ Var elm_check_id,
                     appl_ed_2,
                     new_expr_desc @@ Var elm_check_id )
            in
            new_instrumented_ed
            @@ Match
                 ( new_expr_desc @@ Var test_list_id,
                   [
                     (EmptyLstPat, new_expr_desc @@ Bool true);
                     ( LstDestructPat (Ident "hd", Ident "tl"),
                       new_expr_desc @@ Let (elm_check_id, appl_ed_1, if_ed) );
                   ] )
          in
          let check_fun = Funsig (test_fun_id, [ test_list_id ], test_fun) in
          let%bind check_cls =
            new_instrumented_ed
            @@ Appl
                 (new_expr_desc @@ Var test_fun_id, new_expr_desc @@ Var expr_id)
          in
          let fun_body = LetRecFun ([ check_fun ], check_cls) in
          (* Building the intial check for whether it's a list value *)
          let fail_pat_cls = new_expr_desc @@ Var lst_check_fail in
          let matched_expr = new_expr_desc @@ Var expr_id in
          let%bind match_body =
            new_instrumented_ed
            @@ Match
                 ( matched_expr,
                   [
                     (EmptyLstPat, new_expr_desc @@ Bool true);
                     ( LstDestructPat (Ident "~underscore", Ident "~underscore2"),
                       new_expr_desc @@ fun_body );
                     (AnyPat, fail_pat_cls);
                   ] )
          in
          let lst_fail =
            Let (lst_check_fail, new_expr_desc @@ Bool false, match_body)
          in
          (* Since the initial list check could be a point of faliure, we need to
             record it as well. *)
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls matched_expr
          in
          let%bind () = add_error_to_tag_mapping fail_pat_cls tag in
          return @@ Function ([ expr_id ], new_expr_desc lst_fail)
        in
        let%bind wrapper =
          let%bind expr_id = fresh_ident "expr" in
          return @@ Function ([ expr_id ], new_expr_desc @@ Var expr_id)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeArrow (t1, t2) ->
        let%bind generator =
          (* For the generator, we need to produce a function that will accept any
             arbitrary values of the input type, and guarantees the output to be
             of the declared type. *)
          let%bind gc_pair_dom_gen = semantic_type_of t1 in
          let%bind gc_pair_cod_gen = semantic_type_of t2 in
          let%bind arg_assume = fresh_ident "arg_assume" in
          (* The generated function will first check whether it's given a correctly
             typed input. If not, report an error. *)
          (* TODO: The error reporting here isn't really working for this "wrap"
             case. *)
          let%bind fail_id = fresh_ident "fail" in
          let%bind assert_cls =
            new_instrumented_ed @@ Assert (new_expr_desc @@ Var fail_id)
          in
          let fail_cls =
            Let (fail_id, new_expr_desc @@ Bool false, assert_cls)
          in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed
               @@ RecordProj (gc_pair_dom_gen, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair_dom_gen, Label "checker")
          in
          let%bind appl_ed_1 =
            new_instrumented_ed
            @@ Appl (proj_ed_1, new_expr_desc @@ Var arg_assume)
          in
          (* let%bind proj_ed_2_inner =
               new_instrumented_ed
               @@ RecordProj (gc_pair_cod_gen, Label "~actual_rec")
             in *)
          let%bind proj_ed_2 =
            new_instrumented_ed
            @@ RecordProj (gc_pair_cod_gen, Label "generator")
          in
          let%bind appl_ed_2 =
            new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
          in
          let%bind inner_expr =
            new_instrumented_ed
            @@ If (appl_ed_1, appl_ed_2, new_expr_desc @@ fail_cls)
          in
          let gen_expr = Function ([ arg_assume ], inner_expr) in
          return @@ Function ([ Ident "~null" ], new_expr_desc gen_expr)
        in
        let%bind checker =
          (* For the checker, we need to fabricate an input value of the right type
             and feed it to the function we're checking, and determine whether
             the return type is correct. *)
          let%bind gc_pair_dom_check = semantic_type_of t1 in
          let%bind gc_pair_cod_check = semantic_type_of t2 in
          let%bind expr_id = fresh_ident "expr" in
          let%bind arg_assert = fresh_ident "arg_assert" in
          let%bind ret_id = fresh_ident "fun_ret" in
          let%bind appl_ed_1 =
            new_instrumented_ed
            @@ Appl
                 (new_expr_desc @@ Var expr_id, new_expr_desc @@ Var arg_assert)
          in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed
               @@ RecordProj (gc_pair_cod_check, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed
            @@ RecordProj (gc_pair_cod_check, Label "checker")
          in
          let%bind appl_ed_2 =
            new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var ret_id)
          in
          let codom_check = Let (ret_id, appl_ed_1, appl_ed_2) in
          (* let%bind proj_ed_2_inner =
               new_instrumented_ed
               @@ RecordProj (gc_pair_dom_check, Label "~actual_rec")
             in *)
          let%bind proj_ed_2 =
            new_instrumented_ed
            @@ RecordProj (gc_pair_dom_check, Label "generator")
          in
          let%bind appl_ed_3 =
            new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
          in
          let fun_body =
            Let (arg_assert, appl_ed_3, new_expr_desc codom_check)
          in
          return @@ Function ([ expr_id ], new_expr_desc fun_body)
        in
        let%bind wrapper =
          let tvs = get_poly_vars t1 [] in
          let tvs2 =
            get_poly_vars t2 []
            |> List.filter (fun t -> not @@ check_type_presence tvs t)
          in
          let%bind tvs_tids =
            tvs
            |> List.map (fun tv ->
                   let%bind tid = fresh_ident "t" in
                   return @@ (tv, tid))
            |> sequence
          in
          let%bind tvs_tids2 =
            tvs2
            |> List.map (fun tv ->
                   let%bind tid = fresh_ident "t" in
                   return @@ (tv, tid))
            |> sequence
          in
          let tids = List.map snd tvs_tids in
          let tids2 = List.map snd tvs_tids2 in
          let t1' =
            List.fold_left
              (fun acc (poly_var, tid) ->
                replace_tagless_expr_desc acc poly_var (new_expr_desc @@ Var tid))
              t1 tvs_tids
          in
          let t2' =
            List.fold_left
              (fun acc (poly_var, tid) ->
                replace_tagless_expr_desc acc poly_var (new_expr_desc @@ Var tid))
              t2 (tvs_tids @ tvs_tids2)
          in
          let%bind gc_pair_dom_check = semantic_type_of t1' in
          let%bind gc_pair_cod_check = semantic_type_of t2' in
          let%bind proj_ed_1 =
            new_instrumented_ed
            @@ RecordProj (gc_pair_dom_check, Label "checker")
          in
          let%bind expr_id = fresh_ident "expr" in
          let%bind arg_id = fresh_ident "arg" in
          let check_arg =
            new_expr_desc @@ Appl (proj_ed_1, new_expr_desc @@ Var arg_id)
          in
          let%bind assert_cls =
            new_instrumented_ed @@ Assert (new_expr_desc @@ Bool false)
          in
          let%bind proj_ed_2 =
            new_instrumented_ed
            @@ RecordProj (gc_pair_cod_check, Label "wrapper")
          in
          let eta_appl =
            new_expr_desc
            @@ Appl (new_expr_desc @@ Var expr_id, new_expr_desc @@ Var arg_id)
          in
          let wrap_res =
            if List.is_empty tids2
            then new_expr_desc @@ Appl (proj_ed_2, eta_appl)
            else
              new_expr_desc
              @@ Function (tids2, new_expr_desc @@ Appl (proj_ed_2, eta_appl))
          in
          let check_cls =
            new_expr_desc @@ If (check_arg, wrap_res, assert_cls)
          in
          let wrapped_fun =
            new_expr_desc @@ Function (tids @ [ arg_id ], check_cls)
          in
          return @@ Function ([ expr_id ], wrapped_fun)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeArrowD ((x1, t1), t2) ->
        (* For dependently typed functions, we need to make sure the co-domain can
           refer to the domain's value, thus this helper function here. *)
        let%bind generator =
          let%bind gc_pair_dom_g = semantic_type_of t1 in
          let%bind gc_pair_cod_g = semantic_type_of t2 in
          let%bind arg_assume = fresh_ident "arg_assume" in
          (* Same as a normal function type, we need to first check whether we have
             a correctly typed input given to us. *)
          (* TODO: Fix error reporting for incorrect input *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair_dom_g, Label "checker")
          in
          let%bind appl_ed_1 =
            new_instrumented_ed @@ mk_gc_pair_cod x1 gc_pair_cod_g arg_assume
          in
          let%bind proj_ed_2 =
            new_instrumented_ed @@ RecordProj (appl_ed_1, Label "generator")
          in
          let%bind appl_ed_2 =
            new_instrumented_ed
            @@ Appl (proj_ed_1, new_expr_desc @@ Var arg_assume)
          in
          let%bind appl_ed_3 =
            new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
          in
          let%bind assert_cls =
            new_instrumented_ed @@ Assert (new_expr_desc @@ Bool false)
          in
          let%bind inner_expr =
            new_instrumented_ed @@ If (appl_ed_2, appl_ed_3, assert_cls)
          in
          let gen_expr = Function ([ arg_assume ], inner_expr) in
          return @@ Function ([ Ident "~null" ], new_expr_desc gen_expr)
        in
        let%bind checker =
          let%bind gc_pair_dom_c = semantic_type_of t1 in
          let%bind gc_pair_cod_c = semantic_type_of t2 in
          let%bind expr_id = fresh_ident "expr" in
          let%bind arg_assert = fresh_ident "arg_assert" in
          let%bind ret_id = fresh_ident "fun_ret" in
          let%bind appl_ed_1 =
            new_instrumented_ed @@ mk_gc_pair_cod x1 gc_pair_cod_c arg_assert
          in
          let%bind gc_pair_cod' =
            new_instrumented_ed
            @@ Appl
                 ( new_expr_desc @@ Function ([ x1 ], appl_ed_1),
                   new_expr_desc @@ Var arg_assert )
          in
          let%bind appl_ed_2 =
            new_instrumented_ed
            @@ Appl
                 (new_expr_desc @@ Var expr_id, new_expr_desc @@ Var arg_assert)
          in
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair_cod', Label "checker")
          in
          let%bind appl_ed_3 =
            new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var ret_id)
          in
          let codom_check = Let (ret_id, appl_ed_2, appl_ed_3) in
          (* let%bind proj_ed_2_inner =
               new_instrumented_ed @@ RecordProj (gc_pair_dom_c, Label "~actual_rec")
             in *)
          let%bind proj_ed_2 =
            new_instrumented_ed @@ RecordProj (gc_pair_dom_c, Label "generator")
          in
          let%bind appl_ed_4 =
            new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
          in
          let fun_body =
            Let (arg_assert, appl_ed_4, new_expr_desc codom_check)
          in
          return @@ Function ([ expr_id ], new_expr_desc fun_body)
        in
        let%bind wrapper =
          let tvs = get_poly_vars t1 [] in
          let tvs2 =
            get_poly_vars t2 []
            |> List.filter (fun t -> not @@ check_type_presence tvs t)
          in
          let%bind tvs_tids =
            tvs
            |> List.map (fun tv ->
                   let%bind tid = fresh_ident "t" in
                   return @@ (tv, tid))
            |> sequence
          in
          let%bind tvs_tids2 =
            tvs2
            |> List.map (fun tv ->
                   let%bind tid = fresh_ident "t" in
                   return @@ (tv, tid))
            |> sequence
          in
          let t1' =
            List.fold_left
              (fun acc (poly_var, tid) ->
                replace_tagless_expr_desc acc poly_var (new_expr_desc @@ Var tid))
              t1 tvs_tids
          in
          let t2' =
            List.fold_left
              (fun acc (poly_var, tid) ->
                replace_tagless_expr_desc acc poly_var (new_expr_desc @@ Var tid))
              t2 (tvs_tids @ tvs_tids2)
          in
          let tids = List.map snd tvs_tids in
          let tids2 = List.map snd tvs_tids2 in
          let%bind gc_pair_dom = semantic_type_of t1' in
          let%bind gc_pair_cod = semantic_type_of t2' in
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair_dom, Label "checker")
          in
          let%bind expr_id = fresh_ident "expr" in
          let%bind arg_id = fresh_ident "arg" in
          let check_arg =
            new_expr_desc @@ Appl (proj_ed_1, new_expr_desc @@ Var arg_id)
          in
          let%bind assert_cls =
            new_instrumented_ed @@ Assert (new_expr_desc @@ Bool false)
          in
          let%bind appl_ed_1 =
            new_instrumented_ed @@ mk_gc_pair_cod x1 gc_pair_cod arg_id
          in
          let%bind gc_pair_cod' =
            new_instrumented_ed
            @@ Appl
                 ( new_expr_desc @@ Function ([ x1 ], appl_ed_1),
                   new_expr_desc @@ Var arg_id )
          in
          let%bind proj_ed_2 =
            new_instrumented_ed @@ RecordProj (gc_pair_cod', Label "wrapper")
          in
          let eta_appl =
            new_expr_desc
            @@ Appl (new_expr_desc @@ Var expr_id, new_expr_desc @@ Var arg_id)
          in
          let wrap_res =
            if List.is_empty tids2
            then new_expr_desc @@ Appl (proj_ed_2, eta_appl)
            else
              new_expr_desc
              @@ Function (tids2, new_expr_desc @@ Appl (proj_ed_2, eta_appl))
          in
          let check_cls =
            new_expr_desc @@ If (check_arg, wrap_res, assert_cls)
          in
          let wrapped_fun =
            new_expr_desc @@ Function (tids @ [ arg_id ], check_cls)
          in
          return @@ Function ([ expr_id ], wrapped_fun)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeSet (t, p) ->
        let%bind generator =
          (* For the generator, we will produce values of the right type first
             and then impose the constraint that it has to meet the predicate.
             If the value doesn't work, we just zero out this particular run via
             "assume false". *)
          let%bind gc_pair_g = semantic_type_of t in
          let%bind p_g = semantic_type_of p in
          let%bind candidate = fresh_ident "candidate" in
          let%bind appl_ed_1 =
            new_instrumented_ed @@ Appl (p_g, new_expr_desc @@ Var candidate)
          in
          let%bind pred_check =
            new_instrumented_ed
            @@ If
                 ( appl_ed_1,
                   new_expr_desc @@ Var candidate,
                   new_expr_desc @@ Assume (new_expr_desc @@ Bool false) )
          in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed @@ RecordProj (gc_pair_g, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair_g, Label "generator")
          in
          let%bind appl_ed_2 =
            new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Int 0)
          in
          let gen_expr = Let (candidate, appl_ed_2, pred_check) in
          return @@ Function ([ Ident "~null" ], new_expr_desc gen_expr)
        in
        let%bind checker =
          (* For the checker, we want to first check for the base type, and then
             we need to check whether it satisfies the predicate. *)
          let%bind gc_pair_c = semantic_type_of t in
          let%bind p_c = semantic_type_of p in
          let%bind expr_id = fresh_ident "expr" in
          let%bind t_check_id = fresh_ident "t_check" in
          let%bind pred_check_id = fresh_ident "pred_check" in
          let fail_pat_cls = new_expr_desc @@ Var pred_check_id in
          let expr_to_check = new_expr_desc @@ Var expr_id in
          let%bind check_pred_res =
            new_instrumented_ed @@ Appl (p_c, expr_to_check)
          in
          let%bind check_pred_inner =
            new_instrumented_ed
            @@ If (check_pred_res, new_expr_desc @@ Bool true, fail_pat_cls)
          in
          (* Note: To reduce complexity, we are not checking whether the predicate
             is of the right type. *)
          (* let%bind gc_pair_pred = semantic_type_of (TypeArrow (t, TypeBool)) in
             let%bind check_pred_id = fresh_ident "check_pred" in
             let pred_cond = If (Var check_pred_id, Record rec_map, Assert (Bool false)) in
             let check_pred = Let (check_pred_id,
                                   Appl (RecordProj (gc_pair_pred, Label "checker"), p'),
                                   pred_cond)
             in *)
          let check_pred =
            Let (pred_check_id, new_expr_desc @@ Bool false, check_pred_inner)
          in
          let%bind check_type_body =
            new_instrumented_ed
            @@ If
                 ( new_expr_desc @@ Var t_check_id,
                   new_expr_desc @@ check_pred,
                   new_expr_desc @@ Var t_check_id )
          in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed @@ RecordProj (gc_pair_c, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair_c, Label "checker")
          in
          let%bind appl_ed_1 =
            new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var expr_id)
          in
          let check_type = Let (t_check_id, appl_ed_1, check_type_body) in
          (* Since the predicate check could be a point of faliure, we need to
             record it. *)
          let%bind () = add_error_to_tag_mapping fail_pat_cls tag in
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls expr_to_check
          in
          return @@ Function ([ expr_id ], new_expr_desc check_type)
        in
        let%bind wrapper =
          let%bind sem_t = semantic_type_of t in
          new_instrumented_ed @@ RecordProj (sem_t, Label "wrapper")
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") wrapper
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeUnion (t1, t2) ->
        (* For union type, we want to be able to cover both cases, so input is used
           to generate control flows that explore different sides of the union. *)
        let%bind generator =
          let%bind gc_pair1_g = semantic_type_of t1 in
          let%bind gc_pair2_g = semantic_type_of t2 in
          let%bind select_int = fresh_ident "select_int" in
          let%bind geq_ed =
            new_instrumented_ed
            @@ Geq (new_expr_desc @@ Var select_int, new_expr_desc @@ Int 0)
          in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed @@ RecordProj (gc_pair1_g, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair1_g, Label "generator")
          in
          let%bind appl_ed_1 =
            new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Int 0)
          in
          (* let%bind proj_ed_2_inner =
               new_instrumented_ed @@ RecordProj (gc_pair2_g, Label "~actual_rec")
             in *)
          let%bind proj_ed_2 =
            new_instrumented_ed @@ RecordProj (gc_pair2_g, Label "generator")
          in
          let%bind appl_ed_2 =
            new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
          in
          let%bind branch =
            new_instrumented_ed @@ If (geq_ed, appl_ed_1, appl_ed_2)
          in
          let gen_expr = Let (select_int, new_expr_desc Input, branch) in
          return @@ Function ([ Ident "~null" ], new_expr_desc gen_expr)
        in
        let%bind checker =
          (* For the checker, to avoid we spend forever in one of the branches,
             we will use the same trick here to make sure that the checks alternate
             between doing one first then the other (and vice versa). *)
          let%bind gc_pair1_c = semantic_type_of t1 in
          let%bind gc_pair2_c = semantic_type_of t2 in
          let%bind gc_pair1_c' = semantic_type_of t1 in
          let%bind gc_pair2_c' = semantic_type_of t2 in
          let%bind expr_id = fresh_ident "expr" in
          let%bind select_int = fresh_ident "select_int" in
          let%bind fail_id = fresh_ident "fail" in
          let fail_pat_cls_1 = new_expr_desc @@ Var fail_id in
          let fail_pat_cls_2 = new_expr_desc @@ Var fail_id in
          let tested_expr_1 = new_expr_desc @@ Var expr_id in
          let tested_expr_2 = new_expr_desc @@ Var expr_id in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed @@ RecordProj (gc_pair2_c, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair2_c, Label "checker")
          in
          let%bind appl_ed_1 =
            new_instrumented_ed @@ Appl (proj_ed_1, tested_expr_1)
          in
          let%bind checker1_inner =
            new_instrumented_ed
            @@ If (appl_ed_1, new_expr_desc @@ Bool true, fail_pat_cls_1)
          in
          (* let%bind proj_ed_2_inner =
               new_instrumented_ed @@ RecordProj (gc_pair1_c, Label "~actual_rec")
             in *)
          let%bind proj_ed_2 =
            new_instrumented_ed @@ RecordProj (gc_pair1_c, Label "checker")
          in
          let%bind appl_ed_2 =
            new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Var expr_id)
          in
          let%bind checker1 =
            new_instrumented_ed
            @@ If (appl_ed_2, new_expr_desc @@ Bool true, checker1_inner)
          in
          (* let%bind proj_ed_3_inner =
               new_instrumented_ed @@ RecordProj (gc_pair1_c', Label "~actual_rec")
             in *)
          let%bind proj_ed_3 =
            new_instrumented_ed @@ RecordProj (gc_pair1_c', Label "checker")
          in
          let%bind appl_ed_3 =
            new_instrumented_ed @@ Appl (proj_ed_3, tested_expr_2)
          in
          let%bind checker2_inner =
            new_instrumented_ed
            @@ If (appl_ed_3, new_expr_desc @@ Bool true, fail_pat_cls_2)
          in
          (* let%bind proj_ed_4_inner =
               new_instrumented_ed @@ RecordProj (gc_pair2_c', Label "~actual_rec")
             in *)
          let%bind proj_ed_4 =
            new_instrumented_ed @@ RecordProj (gc_pair2_c', Label "checker")
          in
          let%bind appl_ed_4 =
            new_instrumented_ed @@ Appl (proj_ed_4, new_expr_desc @@ Var expr_id)
          in
          let%bind checker2 =
            new_instrumented_ed
            @@ If (appl_ed_4, new_expr_desc @@ Bool true, checker2_inner)
          in
          let%bind geq_ed =
            new_instrumented_ed
            @@ Geq (new_expr_desc @@ Var select_int, new_expr_desc @@ Int 0)
          in
          let%bind branch =
            new_instrumented_ed @@ If (geq_ed, checker1, checker2)
          in
          let fail_def = Let (fail_id, new_expr_desc @@ Bool false, branch) in
          let fun_body =
            Let (select_int, new_expr_desc Input, new_expr_desc fail_def)
          in
          (* Here, the point of error isn't the "false" returned by the base case,
             since the real error point is really a fabricated one that's the "or"
             of two check results. *)
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls_1 tested_expr_1
          in
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls_2 tested_expr_2
          in
          let%bind () = add_error_to_tag_mapping fail_pat_cls_1 tag in
          let%bind () = add_error_to_tag_mapping fail_pat_cls_2 tag in
          return @@ Function ([ expr_id ], new_expr_desc fun_body)
        in
        let%bind wrapper =
          let%bind expr_id = fresh_ident "expr" in
          return @@ Function ([ expr_id ], new_expr_desc @@ Var expr_id)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeIntersect (t1, t2) ->
        (* Note: Intersection of all records are now empty? *)
        let rec flatten_fun_intersection ed acc =
          match ed.body with
          | TypeIntersect (dom, cod) -> (
              match (dom.body, cod.body) with
              | TypeArrow _, TypeArrow _ | TypeArrowD _, TypeArrowD _ ->
                  dom :: cod :: acc
              | TypeArrow _, TypeIntersect _ | TypeArrowD _, TypeIntersect _ ->
                  let acc' = dom :: acc in
                  flatten_fun_intersection cod acc'
              | TypeIntersect _, TypeArrow _ | TypeIntersect _, TypeArrowD _ ->
                  let acc' = cod :: acc in
                  flatten_fun_intersection dom acc'
              | TypeIntersect _, TypeIntersect _ ->
                  let acc' = flatten_fun_intersection dom acc in
                  flatten_fun_intersection cod acc'
              | _ ->
                  failwith
                    "flatten_fun_intersection: Should be an intersection of \
                     functions!")
          | _ ->
              failwith
                "flatten_fun_intersection: Should be an intersection of \
                 functions!"
        in
        let rec domain_check ed =
          match ed.body with
          | TypeArrow (dom, cod) | TypeArrowD ((_, dom), cod) ->
              if is_fun_type dom then false else domain_check cod
          | _ -> true
        in
        let mk_fun_intersect_gen fun_types =
          let well_formed = List.for_all domain_check fun_types in
          if well_formed
          then
            (* let mk_gc_pair_cod x_id cod arg =
                 Appl
                   ( new_expr_desc @@ Function ([ x_id ], cod),
                     new_expr_desc @@ Var arg )
               in *)
            let%bind arg = fresh_ident "arg" in
            let rec folder t acc =
              match t.body with
              | TypeArrow (t1, t2) ->
                  let%bind gc_pair_dom_g = semantic_type_of t1 in
                  let%bind gc_pair_cod_g = semantic_type_of t2 in
                  (* let%bind proj_ed_1_inner =
                       new_instrumented_ed
                       @@ RecordProj (gc_pair_dom_g, Label "~actual_rec")
                     in *)
                  let%bind proj_ed_1 =
                    new_instrumented_ed
                    @@ RecordProj (gc_pair_dom_g, Label "checker")
                  in
                  let%bind appl_ed_1 =
                    new_instrumented_ed
                    @@ Appl (proj_ed_1, new_expr_desc @@ Var arg)
                  in
                  (* let%bind proj_ed_2_inner =
                       new_instrumented_ed
                       @@ RecordProj (gc_pair_cod_g, Label "~actual_rec")
                     in *)
                  let%bind proj_ed_2 =
                    new_instrumented_ed
                    @@ RecordProj (gc_pair_cod_g, Label "generator")
                  in
                  let%bind appl_ed_2 =
                    new_instrumented_ed
                    @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
                  in
                  let%bind if_ed =
                    new_instrumented_ed @@ If (appl_ed_1, appl_ed_2, acc)
                  in
                  return @@ if_ed
              | TypeArrowD ((x, t1), t2) ->
                  let%bind gc_pair_dom_g = semantic_type_of t1 in
                  let%bind gc_pair_cod_g =
                    let%bind cod_g = semantic_type_of t2 in
                    return @@ new_expr_desc @@ mk_gc_pair_cod x cod_g arg
                  in
                  (* let%bind proj_ed_1_inner =
                       new_instrumented_ed
                       @@ RecordProj (gc_pair_dom_g, Label "~actual_rec")
                     in *)
                  let%bind proj_ed_1 =
                    new_instrumented_ed
                    @@ RecordProj (gc_pair_dom_g, Label "checker")
                  in
                  let%bind appl_ed_1 =
                    new_instrumented_ed
                    @@ Appl (proj_ed_1, new_expr_desc @@ Var arg)
                  in
                  (* let%bind proj_ed_2_inner =
                       new_instrumented_ed
                       @@ RecordProj (gc_pair_cod_g, Label "~actual_rec")
                     in *)
                  let%bind proj_ed_2 =
                    new_instrumented_ed
                    @@ RecordProj (gc_pair_cod_g, Label "generator")
                  in
                  let%bind appl_ed_2 =
                    new_instrumented_ed
                    @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
                  in
                  let%bind if_ed =
                    new_instrumented_ed @@ If (appl_ed_1, appl_ed_2, acc)
                  in
                  return @@ if_ed
              | _ ->
                  failwith
                    "mk_fun_intersect_gen: should only handle function \
                     intersections!"
            in
            let%bind assert_cls =
              new_instrumented_ed @@ Assert (new_expr_desc @@ Bool false)
            in
            let%bind fun_body = list_fold_right_m folder fun_types assert_cls in
            return @@ Function ([ arg ], fun_body)
          else
            failwith
              "mk_fun_intersect_gen: ill-formed function intersection type"
        in
        let rec flatten_record_intersection ed =
          let combine_rec r1 r2 =
            Ident_map.fold
              (fun k v acc ->
                match Ident_map.find_opt k acc with
                | Some v2 ->
                    let v' = new_expr_desc @@ TypeIntersect (v, v2) in
                    Ident_map.add k v' acc
                | None -> Ident_map.add k v acc)
              r1 r2
          in
          match ed.body with
          | TypeIntersect (t1, t2) -> (
              match (t1.body, t2.body) with
              | TypeRecord r1, TypeRecord r2 -> combine_rec r1 r2
              | TypeIntersect _, TypeRecord r2 ->
                  let r1 = flatten_record_intersection t1 in
                  combine_rec r1 r2
              | TypeRecord r1, TypeIntersect _ ->
                  let r2 = flatten_record_intersection t2 in
                  combine_rec r1 r2
              | TypeIntersect _, TypeIntersect _ ->
                  let r1 = flatten_record_intersection t1 in
                  let r2 = flatten_record_intersection t2 in
                  combine_rec r1 r2
              | _ ->
                  failwith
                    "flatten_record_intersection: Should be an intersection of \
                     records!")
          | _ ->
              failwith
                "flatten_record_intersection: Should be an intersection of \
                 records!"
        in
        let%bind generator =
          (* For intersection type, we want to make sure that the value generated
             will indeed be in both types. Thus we will use the generator of one
             type, and then use the checker of the other to determine whether our
             fabricated value is valid. If not, simply zero out this execution with
             "assume false". *)
          if is_fun_type t1 || is_fun_type t2
          then
            let funs = flatten_fun_intersection e_desc [] in
            let%bind gen_body = mk_fun_intersect_gen funs in
            return @@ Function ([ Ident "~null" ], new_expr_desc gen_body)
          else if is_record_type t1 || is_record_type t2
          then
            let actual_type = flatten_record_intersection e_desc in
            let%bind gc_pair_g =
              semantic_type_of (new_expr_desc @@ TypeRecord actual_type)
            in
            (* let%bind proj_ed_1_inner =
                 new_instrumented_ed @@ RecordProj (gc_pair_g, Label "~actual_rec")
               in *)
            let%bind proj_ed_1 =
              new_instrumented_ed @@ RecordProj (gc_pair_g, Label "generator")
            in
            let%bind appl_ed_1 =
              new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Int 0)
            in
            return @@ Function ([ Ident "~null" ], appl_ed_1)
          else
            let%bind gc_pair1_g = semantic_type_of t1 in
            let%bind gc_pair2_g = semantic_type_of t2 in
            let%bind candidate_var = fresh_ident "candidate" in
            (* let%bind proj_ed_1_inner =
                 new_instrumented_ed @@ RecordProj (gc_pair2_g, Label "~actual_rec")
               in *)
            let%bind proj_ed_1 =
              new_instrumented_ed @@ RecordProj (gc_pair2_g, Label "checker")
            in
            let%bind appl_ed_1 =
              new_instrumented_ed
              @@ Appl (proj_ed_1, new_expr_desc @@ Var candidate_var)
            in
            let%bind validate =
              new_instrumented_ed
              @@ If
                   ( appl_ed_1,
                     new_expr_desc @@ Var candidate_var,
                     new_expr_desc @@ Assume (new_expr_desc @@ Bool false) )
            in
            (* let%bind proj_ed_2_inner =
                 new_instrumented_ed @@ RecordProj (gc_pair1_g, Label "~actual_rec")
               in *)
            let%bind proj_ed_2 =
              new_instrumented_ed @@ RecordProj (gc_pair1_g, Label "generator")
            in
            let%bind appl_ed_2 =
              new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
            in
            let gen_expr = Let (candidate_var, appl_ed_2, validate) in
            return @@ Function ([ Ident "~null" ], new_expr_desc gen_expr)
        in
        let%bind checker =
          (* To type check an intersection type, we want to make sure it passes
             the checker of both types. Since it's an "and" of result, there's no
             need to alternate the order as we did with union type. *)
          let%bind gc_pair1_c = semantic_type_of t1 in
          let%bind gc_pair2_c = semantic_type_of t2 in
          let%bind expr_id = fresh_ident "expr" in
          let%bind check_id = fresh_ident "check_1" in
          let%bind check_id2 = fresh_ident "check_2" in
          let fail_pat_cls_1 = new_expr_desc @@ Var check_id in
          let fail_pat_cls_2 = new_expr_desc @@ Var check_id2 in
          let tested_expr = new_expr_desc @@ Var expr_id in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed @@ RecordProj (gc_pair2_c, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair2_c, Label "checker")
          in
          let%bind appl_ed_1 =
            new_instrumented_ed @@ Appl (proj_ed_1, tested_expr)
          in
          let check_t2 = Let (check_id2, appl_ed_1, fail_pat_cls_2) in
          let%bind fun_body_inner =
            new_instrumented_ed
            @@ If
                 ( new_expr_desc @@ Var check_id,
                   new_expr_desc @@ check_t2,
                   fail_pat_cls_1 )
          in
          (* let%bind proj_ed_2_inner =
               new_instrumented_ed @@ RecordProj (gc_pair1_c, Label "~actual_rec")
             in *)
          let%bind proj_ed_2 =
            new_instrumented_ed @@ RecordProj (gc_pair1_c, Label "checker")
          in
          let%bind appl_ed_2 =
            new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Var expr_id)
          in
          let fun_body = Let (check_id, appl_ed_2, fun_body_inner) in
          (* Here, the point of error isn't the "false" returned by the base case,
             since the real error point is really a fabricated one that's the "and"
             of two check results. *)
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls_1 tested_expr
          in
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls_2 tested_expr
          in
          let%bind () = add_error_to_tag_mapping fail_pat_cls_1 tag in
          let%bind () = add_error_to_tag_mapping fail_pat_cls_2 tag in
          return @@ Function ([ expr_id ], new_expr_desc fun_body)
        in
        let%bind wrapper =
          let%bind expr_id = fresh_ident "expr" in
          return @@ Function ([ expr_id ], new_expr_desc @@ Var expr_id)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeRecurse (t_var, t') ->
        (* For recursive types, we're really setting up the bootstrap here. *)
        let%bind self_id = fresh_ident "self" in
        let%bind x_id = fresh_ident "x" in
        let%bind f_id = fresh_ident "f" in
        let%bind freeze_id = fresh_ident "freeze" in
        let%bind ycomb_id = fresh_ident "ycomb" in
        let%bind innermost_appl =
          new_instrumented_ed
          @@ Appl (new_expr_desc @@ Var x_id, new_expr_desc @@ Var x_id)
        in
        let%bind inner_inner_appl =
          new_instrumented_ed @@ Appl (new_expr_desc @@ Var f_id, innermost_appl)
        in
        let%bind inner_appl =
          new_instrumented_ed
          @@ Appl (inner_inner_appl, new_expr_desc @@ Var freeze_id)
        in
        let%bind innermost_appl_2 =
          new_instrumented_ed
          @@ Appl (new_expr_desc @@ Var x_id, new_expr_desc @@ Var x_id)
        in
        let%bind inner_inner_appl_2 =
          new_instrumented_ed
          @@ Appl (new_expr_desc @@ Var f_id, innermost_appl_2)
        in
        let%bind inner_appl_2 =
          new_instrumented_ed
          @@ Appl (inner_inner_appl_2, new_expr_desc @@ Var freeze_id)
        in
        let ycomb_inner = Function ([ x_id; freeze_id ], inner_appl) in
        let ycomb_inner_2 = Function ([ x_id; freeze_id ], inner_appl_2) in
        let%bind inner_appl_3 =
          new_instrumented_ed
          @@ Appl (new_expr_desc ycomb_inner, new_expr_desc ycomb_inner_2)
        in
        let ycomb = Function ([ f_id ], inner_appl_3) in
        (* let%bind appl_ed =
             new_instrumented_ed
             @@ Appl (new_expr_desc @@ Var primer_id, new_expr_desc @@ Var primer_id)
           in *)
        (* let res =
             new_expr_desc
             @@ Let
                   (primer_id, new_expr_desc @@ Function ([ t_var ], gc_pair), appl_ed)
           in *)
        let%bind generator =
          let%bind gc_pair_g = semantic_type_of t' in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed @@ RecordProj (gc_pair_g, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair_g, Label "generator")
          in
          let%bind appl_ed =
            new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Int 0)
          in
          let generator_body_1 =
            new_expr_desc @@ Function ([ t_var ], appl_ed)
          in
          let%bind unfreeze =
            new_instrumented_ed
            @@ Appl (new_expr_desc @@ Var self_id, new_expr_desc @@ Int 0)
          in
          let%bind generator_body_2 =
            new_instrumented_ed @@ Appl (generator_body_1, unfreeze)
          in
          return @@ Function ([ Ident "~null" ], generator_body_2)
        in
        let%bind checker =
          let%bind gc_pair_c = semantic_type_of t' in
          let%bind expr_id = fresh_ident "expr" in
          (* let%bind proj_ed_1_inner =
               new_instrumented_ed @@ RecordProj (gc_pair_c, Label "~actual_rec")
             in *)
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (gc_pair_c, Label "checker")
          in
          let%bind appl_ed =
            new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var expr_id)
          in
          let checker_body_1 = new_expr_desc @@ Function ([ t_var ], appl_ed) in
          let%bind unfreeze =
            new_instrumented_ed
            @@ Appl (new_expr_desc @@ Var self_id, new_expr_desc @@ Int 0)
          in
          let%bind checker_body_2 =
            new_instrumented_ed @@ Appl (checker_body_1, unfreeze)
          in
          return @@ Function ([ expr_id ], checker_body_2)
        in
        let%bind wrapper =
          let%bind expr_id = fresh_ident "expr" in
          return @@ Function ([ expr_id ], new_expr_desc @@ Var expr_id)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind type_map = new_instrumented_ed @@ Record rec_map in
        let type_rec =
          new_expr_desc @@ Function ([ self_id; Ident "~null" ], type_map)
        in
        let%bind appl_ycomb =
          new_instrumented_ed @@ Appl (new_expr_desc @@ Var ycomb_id, type_rec)
        in
        let%bind unfreeze_rec =
          new_instrumented_ed @@ Appl (appl_ycomb, new_expr_desc @@ Int 0)
        in
        let res =
          new_expr_desc @@ Let (ycomb_id, new_expr_desc ycomb, unfreeze_rec)
        in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeUntouched t' ->
        let%bind inner_map =
          let%bind empty_rec = new_instrumented_ed @@ Record Ident_map.empty in
          let encode_untouched =
            Ident_map.add (Ident ("~\'" ^ t')) empty_rec Ident_map.empty
          in
          return encode_untouched
        in
        let%bind untouched_v =
          let%bind inner_entry = new_instrumented_ed @@ Record inner_map in
          let encode_untouched_v =
            Ident_map.add (Ident "~untouched") inner_entry Ident_map.empty
          in
          return encode_untouched_v
        in
        let%bind generator =
          let%bind rec_ed = new_instrumented_ed @@ Record untouched_v in
          return @@ Function ([ Ident "~null" ], rec_ed)
        in
        let%bind fail_id = fresh_ident "fail" in
        let%bind checker =
          let%bind expr_id = fresh_ident "expr" in
          let%bind poly_var_id = fresh_ident "~poly_var" in
          let check_pat =
            Ident_map.empty
            |> Ident_map.add (Ident "~untouched") (Some poly_var_id)
            (* |> Ident_map.add (Ident ("~\'" ^ t')) None *)
          in
          let check_pat_inner =
            Ident_map.empty
            (* |> Ident_map.add (Ident "~untouched") (Some poly_var_id) *)
            |> Ident_map.add (Ident ("~\'" ^ t')) None
          in
          let fail_pat_cls_1 = new_expr_desc @@ Var fail_id in
          let fail_pat_cls_2 = new_expr_desc @@ Var fail_id in
          let matched_expr = new_expr_desc @@ Var expr_id in
          let%bind check_poly_var =
            new_instrumented_ed
            @@ Match
                 ( new_expr_desc @@ Var poly_var_id,
                   [
                     (RecPat check_pat_inner, new_expr_desc @@ Bool true);
                     (AnyPat, fail_pat_cls_2);
                   ] )
          in
          let%bind match_cls =
            new_instrumented_ed
            @@ Match
                 ( matched_expr,
                   [
                     (RecPat check_pat, check_poly_var); (AnyPat, fail_pat_cls_1);
                   ] )
          in
          let check_body = Function ([ expr_id ], match_cls) in
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls_1 matched_expr
          in
          let%bind () =
            add_error_to_value_expr_mapping fail_pat_cls_2 matched_expr
          in
          let%bind () = add_error_to_tag_mapping fail_pat_cls_1 tag in
          let%bind () = add_error_to_tag_mapping fail_pat_cls_2 tag in
          return
          @@ Let (fail_id, new_expr_desc @@ Bool false, new_expr_desc check_body)
        in
        let%bind wrapper =
          let%bind expr_id = fresh_ident "expr" in
          return @@ Function ([ expr_id ], new_expr_desc @@ Var expr_id)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeVariant vs ->
        let%bind generator =
          match vs with
          | [] -> failwith "Can't have empty variant types!"
          | (v_lbl, ve) :: [] ->
              let%bind gen_body =
                let%bind gc_pair1_g = semantic_type_of ve in
                let%bind proj_ed_1 =
                  new_instrumented_ed
                  @@ RecordProj (gc_pair1_g, Label "generator")
                in
                let%bind appl_ed_1 =
                  new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Int 0)
                in
                return @@ new_expr_desc @@ VariantExpr (v_lbl, appl_ed_1)
              in
              return @@ Function ([ Ident "~null" ], gen_body)
          | hd :: tl ->
              let tl_rev = List.rev tl in
              let last_v_lbl, last_ve = List.hd tl_rev in
              let tl' = List.rev @@ List.tl tl_rev in
              let%bind last_gen = semantic_type_of last_ve in
              let%bind last_proj_ed =
                new_instrumented_ed @@ RecordProj (last_gen, Label "generator")
              in
              let%bind last_appl_ed =
                new_instrumented_ed
                @@ Appl (last_proj_ed, new_expr_desc @@ Int 0)
              in
              let last_gen_val =
                new_expr_desc @@ VariantExpr (last_v_lbl, last_appl_ed)
              in
              let%bind gen_body =
                hd :: tl'
                |> list_fold_left_m
                     (fun acc (v_lbl, ve) ->
                       let cond =
                         GreaterThan
                           (new_expr_desc @@ Input, new_expr_desc @@ Int 0)
                       in
                       let%bind curr_gen = semantic_type_of ve in
                       let%bind curr_proj_ed =
                         new_instrumented_ed
                         @@ RecordProj (curr_gen, Label "generator")
                       in
                       let%bind curr_appl_ed =
                         new_instrumented_ed
                         @@ Appl (curr_proj_ed, new_expr_desc @@ Int 0)
                       in
                       new_instrumented_ed
                       @@ If
                            ( new_expr_desc @@ cond,
                              new_expr_desc @@ VariantExpr (v_lbl, curr_appl_ed),
                              acc ))
                     last_gen_val
              in
              return @@ Function ([ Ident "~null" ], gen_body)
        in
        let%bind checker =
          let%bind expr_id = fresh_ident "expr" in
          let%bind match_fail_id = fresh_ident "match_fail" in
          let match_fail_var = new_expr_desc @@ Var match_fail_id in
          let matched_expr = new_expr_desc @@ Var expr_id in
          let%bind match_body =
            list_fold_right_m
              (fun (v_lbl, ve) acc ->
                let%bind curr_val_id = fresh_ident "v_val" in

                let%bind curr_type = semantic_type_of ve in
                let%bind curr_proj_ed =
                  new_instrumented_ed @@ RecordProj (curr_type, Label "checker")
                in
                let%bind curr_check_ed =
                  new_instrumented_ed
                  @@ Appl (curr_proj_ed, new_expr_desc @@ Var curr_val_id)
                in
                return

                (* this patch doesn't totally work *)
                (* @@ ( VariantPat (v_lbl, curr_val_id),
                new_expr_desc @@ VariantExpr (v_lbl, curr_check_ed) ) :: acc) *)

                (* with this, need Mu tt. on all variants *)
                @@ ((VariantPat (v_lbl, curr_val_id), curr_check_ed) :: acc))

              vs
              [ (AnyPat, match_fail_var) ]
          in
          let%bind match_check =
            new_instrumented_ed @@ Match (matched_expr, match_body)
          in
          let full_expr =
            new_expr_desc
            @@ Let (match_fail_id, new_expr_desc @@ Bool false, match_check)
          in
          let%bind () =
            add_error_to_value_expr_mapping match_fail_var matched_expr
          in
          let%bind () = add_error_to_tag_mapping match_fail_var tag in
          return @@ Function ([ expr_id ], full_expr)
        in
        let%bind wrapper =
          let%bind expr_id = fresh_ident "expr" in
          let wrap_expr = new_expr_desc @@ Var expr_id in
          let%bind wrap_body =
            list_fold_right_m
              (fun (v_lbl, ve) acc ->
                let%bind curr_val_id = fresh_ident "v_val" in
                let%bind curr_type = semantic_type_of ve in
                let%bind curr_proj_ed =
                  new_instrumented_ed @@ RecordProj (curr_type, Label "wrapper")
                in
                let%bind curr_check_ed =
                  new_instrumented_ed
                  @@ Appl (curr_proj_ed, new_expr_desc @@ Var curr_val_id)
                in
                return
                @@ ( VariantPat (v_lbl, curr_val_id),
                     new_expr_desc @@ VariantExpr (v_lbl, curr_check_ed) )
                   :: acc)
              vs
              [ (AnyPat, new_expr_desc @@ Var expr_id) ]
          in
          let%bind match_to_wrap =
            new_instrumented_ed @@ Match (wrap_expr, wrap_body)
          in
          return @@ Function ([ expr_id ], match_to_wrap)
        in
        let rec_map =
          if !wrap_flag
          then
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
            |> Ident_map.add (Ident "wrapper") (new_expr_desc wrapper)
          else
            Ident_map.empty
            |> Ident_map.add (Ident "generator") (new_expr_desc generator)
            |> Ident_map.add (Ident "checker") (new_expr_desc checker)
        in
        let%bind res = new_instrumented_ed @@ Record rec_map in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Int n ->
        let res = new_expr_desc @@ Int n in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Bool b ->
        let res = new_expr_desc @@ Bool b in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Var x ->
        let res = new_expr_desc @@ Var x in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Input ->
        let res = new_expr_desc @@ Input in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | TypeError x ->
        let res = new_expr_desc @@ TypeError x in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    (* All other expressions are homomorphic *)
    | Function (id_lst, f_expr) ->
        let%bind f_expr' = semantic_type_of f_expr in
        let res = new_expr_desc @@ Function (id_lst, f_expr') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Appl (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Appl (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Let (x, e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Let (x, e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | LetRecFun (sig_lst, e) ->
        let%bind sig_lst' =
          sig_lst |> List.map (transform_funsig semantic_type_of) |> sequence
        in
        let%bind e' = semantic_type_of e in
        let res = new_expr_desc @@ LetRecFun (sig_lst', e') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | LetFun (fun_sig, e) ->
        let%bind fun_sig' = fun_sig |> transform_funsig semantic_type_of in
        let%bind e' = semantic_type_of e in
        let res = new_expr_desc @@ LetFun (fun_sig', e') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | LetWithType (x, e1, e2, t) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let%bind t' = semantic_type_of t in
        let res = new_expr_desc @@ LetWithType (x, e1', e2', t') in
        let%bind () = add_sem_to_syn_mapping e1' e1 in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | LetRecFunWithType (typed_sig_lst, e) ->
        let%bind typed_sig_lst' =
          typed_sig_lst
          |> List.map (transform_typed_funsig semantic_type_of)
          |> sequence
        in
        let%bind e' = semantic_type_of e in
        let res = new_expr_desc @@ LetRecFunWithType (typed_sig_lst', e') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | LetFunWithType (typed_fun_sig, e) ->
        let%bind typed_fun_sig' =
          typed_fun_sig |> transform_typed_funsig semantic_type_of
        in
        let%bind e' = semantic_type_of e in
        let res = new_expr_desc @@ LetFunWithType (typed_fun_sig', e') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Plus (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Plus (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Minus (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Minus (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Times (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Times (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Divide (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Divide (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Modulus (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Modulus (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Equal (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Equal (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Neq (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Neq (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | LessThan (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ LessThan (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Leq (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Leq (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | GreaterThan (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ GreaterThan (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Geq (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Geq (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | And (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ And (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Or (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ Or (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Not e ->
        let%bind e' = semantic_type_of e in
        let res = new_expr_desc @@ Not e' in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | If (e1, e2, e3) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let%bind e3' = semantic_type_of e3 in
        let res = new_expr_desc @@ If (e1', e2', e3') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Record m ->
        let%bind m' = ident_map_map_m semantic_type_of m in
        let res = new_expr_desc @@ Record m' in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | RecordProj (e, l) ->
        let%bind e' = semantic_type_of e in
        let res = new_expr_desc @@ RecordProj (e', l) in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Match (e, pattern_expr_lst) ->
        let%bind e' = semantic_type_of e in
        let mapper (pat, expr) =
          let%bind expr' = semantic_type_of expr in
          return @@ (pat, expr')
        in
        let%bind fail_id = fresh_ident "fail" in
        let%bind pattern_expr_lst' =
          let%bind og_pats = pattern_expr_lst |> List.map mapper |> sequence in
          let%bind assert_cls =
            new_instrumented_ed @@ Assert (new_expr_desc @@ Var fail_id)
          in
          let check_poly =
            let check_pat =
              Ident_map.empty |> Ident_map.add (Ident "~untouched") None
            in
            (RecPat check_pat, assert_cls)
          in
          return @@ (check_poly :: og_pats)
        in
        let transformed_match =
          new_expr_desc @@ Match (e', pattern_expr_lst')
        in
        let%bind instrumented_bool = is_instrumented tag in
        let%bind () =
          if instrumented_bool
          then
            (* let () =
                 Fmt.pr
                   "This is pre-instrumented: %a; \nThis is post-instrumented: %a \n"
                   Bluejay_ast_internal_pp.pp_expr_desc_with_tag e_desc
                   Bluejay_ast_internal_pp.pp_expr_desc_with_tag t'
               in *)
            add_instrumented_tag transformed_match.tag
          else return ()
        in
        let res =
          new_expr_desc
          @@ Let (fail_id, new_expr_desc @@ Bool false, transformed_match)
        in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | VariantExpr (lbl, e) ->
        let%bind e' = semantic_type_of e in
        let res = new_expr_desc @@ VariantExpr (lbl, e') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | List expr_lst ->
        let%bind expr_lst' =
          expr_lst |> List.map semantic_type_of |> sequence
        in
        let res = new_expr_desc @@ List expr_lst' in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | ListCons (e1, e2) ->
        let%bind e1' = semantic_type_of e1 in
        let%bind e2' = semantic_type_of e2 in
        let res = new_expr_desc @@ ListCons (e1', e2') in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Assert e ->
        let%bind e' = semantic_type_of e in
        let res = new_expr_desc @@ Assert e' in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
    | Assume e ->
        let%bind e' = semantic_type_of e in
        let res = new_expr_desc @@ Assume e' in
        let%bind () = add_sem_to_syn_mapping res e_desc in
        return res
  in
  let%bind instrumented_bool = is_instrumented tag in
  let%bind () =
    if instrumented_bool
    then
      (* let () =
           Fmt.pr
             "This is pre-instrumented: %a; \nThis is post-instrumented: %a \n"
             Bluejay_ast_internal_pp.pp_expr_desc_with_tag e_desc
             Bluejay_ast_internal_pp.pp_expr_desc_with_tag t'
         in *)
      add_instrumented_tag t'.tag
    else return ()
  in
  return t'

(* Phase two of the transformation: erasing all type signatures from the code.
   By the end of this phase, there should no longer be any (x : tau) present
   in the AST. *)
and bluejay_to_jay (e_desc : semantic_only expr_desc) : core_only expr_desc m =
  let mk_check_from_fun_sig fun_sig =
    match fun_sig with
    | Typed_funsig (f, typed_params, (_, ret_type)) ->
        let%bind arg_ids =
          list_fold_right_m
            (fun (Ident p, t) acc ->
              let%bind arg_id = fresh_ident p in
              return @@ ((arg_id, t) :: acc))
            typed_params []
        in
        let%bind mk_appl =
          let%bind appl_ed_1 =
            new_instrumented_ed
            @@ Appl
                 ( new_expr_desc @@ Var f,
                   new_expr_desc @@ Var (fst @@ List.hd arg_ids) )
          in
          list_fold_left_m
            (fun acc (arg, _) ->
              let%bind appl_ed_2 =
                new_instrumented_ed @@ Appl (acc, new_expr_desc @@ Var arg)
              in
              return @@ appl_ed_2)
            appl_ed_1 (List.tl arg_ids)
        in
        let%bind ret_type_core = bluejay_to_jay ret_type in
        (* let%bind proj_ed_1_inner =
             new_instrumented_ed @@ RecordProj (ret_type_core, Label "~actual_rec")
           in *)
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (ret_type_core, Label "checker")
        in
        let%bind check_ret = new_instrumented_ed @@ Appl (proj_ed_1, mk_appl) in
        let%bind check_expr =
          list_fold_right_m
            (fun (arg, t) acc ->
              let%bind t' = bluejay_to_jay t in
              (* let%bind proj_ed_2_inner =
                   new_instrumented_ed @@ RecordProj (t', Label "~actual_rec")
                 in *)
              let%bind proj_ed_2 =
                new_instrumented_ed @@ RecordProj (t', Label "generator")
              in
              let%bind appl_ed_3 =
                new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
              in
              return @@ new_expr_desc @@ Let (arg, appl_ed_3, acc))
            arg_ids check_ret
        in
        return check_expr
    | DTyped_funsig (f, ((Ident param as p), t), (_, ret_type)) ->
        let%bind arg_id = fresh_ident param in
        let%bind t' = bluejay_to_jay t in
        let%bind ret_type_core = bluejay_to_jay ret_type in
        let%bind appl_res =
          new_instrumented_ed
          @@ Appl (new_expr_desc @@ Var f, new_expr_desc @@ Var arg_id)
        in
        (* let%bind proj_ed_1_inner =
             new_instrumented_ed @@ RecordProj (ret_type_core, Label "~actual_rec")
           in *)
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (ret_type_core, Label "checker")
        in
        let%bind checker' =
          new_instrumented_ed
          @@ Appl
               ( new_expr_desc @@ Function ([ p ], proj_ed_1),
                 new_expr_desc @@ Var arg_id )
        in
        let%bind check_ret = new_instrumented_ed @@ Appl (checker', appl_res) in
        (* let%bind proj_ed_2_inner =
             new_instrumented_ed @@ RecordProj (t', Label "~actual_rec")
           in *)
        let%bind proj_ed_2 =
          new_instrumented_ed @@ RecordProj (t', Label "generator")
        in
        let%bind appl_ed_1 =
          new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
        in
        let check_expr = new_expr_desc @@ Let (arg_id, appl_ed_1, check_ret) in
        return check_expr
  in
  let e = e_desc.body in
  let tag = e_desc.tag in
  let%bind instrumented_bool = is_instrumented tag in
  let%bind transformed_ed =
    match e with
    | Int n ->
        let res = new_expr_desc @@ Int n in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Bool b ->
        let res = new_expr_desc @@ Bool b in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Var x ->
        let res = new_expr_desc @@ Var x in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Input ->
        let res = new_expr_desc @@ Input in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    (* | Untouched s ->
       let res = new_expr_desc @@ Untouched s in
       let%bind () = add_core_to_sem_mapping res e_desc in
       return res *)
    | TypeError x ->
        let res = new_expr_desc @@ TypeError x in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Function (id_lst, e) ->
        let%bind e' = bluejay_to_jay e in
        let res = new_expr_desc @@ Function (id_lst, e') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Appl (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Appl (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Let (x, e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Let (x, e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | LetRecFun (sig_lst, e) ->
        let%bind sig_lst' =
          sig_lst |> List.map (transform_funsig bluejay_to_jay) |> sequence
        in
        let%bind e' = bluejay_to_jay e in
        let res = new_expr_desc @@ LetRecFun (sig_lst', e') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | LetFun (fun_sig, e) ->
        let%bind sig' = fun_sig |> transform_funsig bluejay_to_jay in
        let%bind e' = bluejay_to_jay e in
        let res = new_expr_desc @@ LetFun (sig', e') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | LetWithType (x, e1, e2, type_decl) ->
        let%bind type_decl' = bluejay_to_jay type_decl in
        let%bind e1_transformed = bluejay_to_jay e1 in
        let (Ident x_str) = x in
        let%bind x' = fresh_ident x_str in
        let%bind e1' =
          let%bind bluejay_to_jay_maps = bluejay_to_jay_maps in
          let og_t =
            Bluejay_to_jay_maps.syn_bluejay_from_sem_bluejay bluejay_to_jay_maps
              type_decl
          in
          if is_record_type og_t
          then
            let%bind new_lbls =
              match og_t.body with
              | TypeRecord r ->
                  r |> Ident_map.key_list
                  |> list_fold_left_m
                       (fun acc k ->
                         let%bind empty_rec =
                           new_instrumented_ed @@ Record Ident_map.empty
                         in
                         return @@ Ident_map.add k empty_rec acc)
                       Ident_map.empty
              | _ ->
                  failwith
                    "semantic_type_of: Should only be invoked when t is a \
                     record type!"
            in
            let%bind actual_rec =
              new_instrumented_ed
              @@ RecordProj (new_expr_desc @@ Var x', Label "~actual_rec")
            in
            let%bind new_lbls_rec = new_instrumented_ed @@ Record new_lbls in
            let new_rec =
              Ident_map.empty
              |> Ident_map.add (Ident "~actual_rec") actual_rec
              |> Ident_map.add (Ident "~decl_lbls") new_lbls_rec
            in
            let%bind new_rec_ed = new_instrumented_ed @@ Record new_rec in
            (* FIXME: This might be buggy: this is adding the mapping between the
               newly typed record with the originally typed record.
               e.g.: let (x : {: a : int :}) = { a = 1, b = 2 } in ...
               {~actual_rec = { a = 1, b = 2 }, ~decl_lbls = { a = {}}} -> {a = 1, b = 2}
            *)
            let%bind () = add_core_to_sem_mapping new_rec_ed e1 in
            return new_rec_ed
          else return @@ new_expr_desc @@ Var x'
        in
        let e1_processed = new_expr_desc @@ Let (x', e1_transformed, e1') in
        let%bind e2' = bluejay_to_jay e2 in
        let%bind check_res = fresh_ident "check_res" in
        let%bind () = add_error_to_bluejay_mapping check_res e_desc in
        let%bind error_cls = new_instrumented_ed @@ TypeError check_res in
        (* let%bind proj_ed_1_inner =
             new_instrumented_ed @@ RecordProj (type_decl', Label "~actual_rec")
           in *)
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (type_decl', Label "checker")
        in
        let%bind appl_ed_1 =
          new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var x')
        in
        let%bind check_1 =
          new_instrumented_ed
          @@ If (new_expr_desc @@ Var check_res, e2', error_cls)
        in
        let check_cls = Let (check_res, appl_ed_1, check_1) in
        let res =
          new_expr_desc @@ Let (x, e1_processed, new_expr_desc check_cls)
        in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | LetRecFunWithType (sig_lst, e) ->
        let folder fun_sig acc =
          let%bind check_res = fresh_ident "check_res" in
          let%bind () = add_error_to_bluejay_mapping check_res e_desc in
          let fun_name =
            match fun_sig with
            | Typed_funsig (f, _, _) | DTyped_funsig (f, _, _) -> f
          in
          let%bind () = add_error_to_rec_fun_mapping check_res fun_name in
          let%bind error_cls = new_instrumented_ed @@ TypeError check_res in

          let%bind res_cls =
            new_instrumented_ed
            @@ If (new_expr_desc @@ Var check_res, acc, error_cls)
          in
          let%bind check_expr = mk_check_from_fun_sig fun_sig in
          let check_cls = Let (check_res, check_expr, res_cls) in
          return @@ new_expr_desc @@ check_cls
        in
        let%bind test_exprs =
          let%bind e' = bluejay_to_jay e in
          list_fold_right_m folder sig_lst e'
        in
        let%bind sig_lst' =
          sig_lst
          |> List.map (remove_type_from_funsig bluejay_to_jay)
          |> sequence
        in
        let res = new_expr_desc @@ LetRecFun (sig_lst', test_exprs) in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | LetFunWithType (fun_sig, e) ->
        let%bind (check_expr : core_only expr_desc) =
          mk_check_from_fun_sig fun_sig
        in
        let%bind e' = bluejay_to_jay e in
        let%bind check_res = fresh_ident "check_res" in
        let%bind () = add_error_to_bluejay_mapping check_res e_desc in
        let%bind error_cls = new_instrumented_ed @@ TypeError check_res in

        let%bind res_cls =
          new_instrumented_ed
          @@ If (new_expr_desc @@ Var check_res, e', error_cls)
        in
        let check_cls = Let (check_res, check_expr, res_cls) in
        let%bind fun_sig' = remove_type_from_funsig bluejay_to_jay fun_sig in
        let res = new_expr_desc @@ LetFun (fun_sig', new_expr_desc check_cls) in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Plus (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Plus (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Minus (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Minus (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Times (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Times (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Divide (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Divide (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Modulus (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Modulus (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Equal (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Equal (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Neq (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Neq (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | LessThan (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ LessThan (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Leq (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Leq (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | GreaterThan (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ GreaterThan (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Geq (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Geq (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | And (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ And (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Or (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ Or (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Not e ->
        let%bind e' = bluejay_to_jay e in
        let res = new_expr_desc @@ Not e' in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | If (e1, e2, e3) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let%bind e3' = bluejay_to_jay e3 in
        let res = new_expr_desc @@ If (e1', e2', e3') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Record m ->
        let%bind m' = ident_map_map_m (fun e -> bluejay_to_jay e) m in
        let%bind res =
          if instrumented_bool
          then return @@ new_expr_desc @@ Record m'
          else
            let wrapped_record =
              let decl_lbls =
                Ident_map.keys m'
                |> Enum.fold
                     (fun acc k ->
                       Ident_map.add k
                         (new_expr_desc @@ Record Ident_map.empty)
                         acc)
                     Ident_map.empty
              in
              Ident_map.empty
              |> Ident_map.add (Ident "~actual_rec") (new_expr_desc @@ Record m')
              |> Ident_map.add (Ident "~decl_lbls")
                   (new_expr_desc @@ Record decl_lbls)
            in
            new_instrumented_ed @@ Record wrapped_record
        in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | RecordProj (e, (Label l as lbl)) ->
        let%bind e' = bluejay_to_jay e in
        let%bind res =
          if instrumented_bool
          then return @@ new_expr_desc @@ RecordProj (e', lbl)
          else
            let%bind decl_lbls =
              new_instrumented_ed @@ RecordProj (e', Label "~decl_lbls")
            in
            let pat = RecPat (Ident_map.singleton (Ident l) None) in
            let%bind assert_cls =
              new_instrumented_ed @@ Assert (new_expr_desc @@ Bool false)
            in
            let%bind inner_projection =
              new_instrumented_ed @@ RecordProj (e', Label "~actual_rec")
            in
            let%bind proj_ed =
              new_instrumented_ed @@ RecordProj (inner_projection, lbl)
            in
            let%bind check_lbls =
              new_instrumented_ed
              @@ Match (decl_lbls, [ (pat, proj_ed); (AnyPat, assert_cls) ])
            in
            return @@ check_lbls
        in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Match (e, pattern_expr_lst) ->
        let%bind e' = bluejay_to_jay e in
        let mapper (pat, expr) =
          let%bind expr' = bluejay_to_jay expr in
          return @@ (pat, expr')
        in
        let%bind pattern_expr_lst' =
          pattern_expr_lst |> List.map mapper |> sequence
        in
        let rec_pat_expr_lst =
          List.filter
            (fun (p, _) -> Bluejay_ast_internal.is_record_pat p)
            pattern_expr_lst'
        in
        if instrumented_bool || List.is_empty rec_pat_expr_lst
        then
          let res = new_expr_desc @@ Match (e', pattern_expr_lst') in
          let%bind () = add_core_to_sem_mapping res e_desc in
          return res
        else
          let%bind actual_rec_str = fresh_ident "actual_rec" in
          let pat' =
            RecPat
              (Ident_map.singleton (Ident "~actual_rec") (Some actual_rec_str))
          in
          let%bind decl_lbls =
            new_instrumented_ed @@ RecordProj (e', Label "~decl_lbls")
          in
          let%bind rec_pat_expr_lst' =
            let mapper (p, ed) =
              match p with
              | StrictRecPat rec_pat | RecPat rec_pat ->
                  let lbl_to_vars = Ident_map.bindings rec_pat in
                  let%bind rebind_vars =
                    List.fold
                      (fun acc_m (Ident l, x_opt) ->
                        match x_opt with
                        | Some x ->
                            let%bind acc = acc_m in
                            let%bind get_cur_proj =
                              new_instrumented_ed
                              @@ RecordProj
                                   (new_expr_desc @@ Var actual_rec_str, Label l)
                            in
                            let new_binding =
                              new_expr_desc @@ Let (x, get_cur_proj, acc)
                            in
                            return @@ new_binding
                        | None -> acc_m)
                      (return ed) lbl_to_vars
                  in
                  return (p, rebind_vars)
              | _ ->
                  failwith
                    "bluejay_to_jay: should only be called with record \
                     patterns!"
            in
            rec_pat_expr_lst |> List.map mapper |> sequence
          in
          let inner_match =
            new_expr_desc @@ Match (decl_lbls, rec_pat_expr_lst')
          in
          let rec_pat_aggregate = (pat', inner_match) in
          let non_rec_pat_expr_lst =
            List.filter
              (fun (p, _) -> not @@ Bluejay_ast_internal.is_record_pat p)
              pattern_expr_lst'
          in
          let res =
            new_expr_desc
            @@ Match (e', rec_pat_aggregate :: non_rec_pat_expr_lst)
          in
          let%bind () = add_core_to_sem_mapping res e_desc in
          return res
    | VariantExpr (lbl, e) ->
        let%bind e' = bluejay_to_jay e in
        let res = new_expr_desc @@ VariantExpr (lbl, e') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | List expr_lst ->
        let%bind expr_lst' = expr_lst |> List.map bluejay_to_jay |> sequence in
        let res = new_expr_desc @@ List expr_lst' in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | ListCons (e1, e2) ->
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let res = new_expr_desc @@ ListCons (e1', e2') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Assert e ->
        let%bind e' = bluejay_to_jay e in
        let res = new_expr_desc @@ Assert e' in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Assume e ->
        let%bind e' = bluejay_to_jay e in
        let res = new_expr_desc @@ Assume e' in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
  in
  let%bind () =
    if instrumented_bool
    then add_instrumented_tag transformed_ed.tag
    else return @@ ()
  in
  return transformed_ed

let debug_transform_bluejay (trans_name : string)
    (transform : 'a expr_desc -> 'b expr_desc m) (e : 'a expr_desc) :
    'b expr_desc m =
  let%bind e' = transform e in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Result of %s:\n%s" trans_name
        (Pp_utils.pp_to_string Bluejay_ast_internal_pp.pp_expr e'.body)) ;
  return e'

let transform_bluejay ?(do_wrap = true) (e : syn_type_bluejay) :
    core_bluejay_edesc * Bluejay_to_jay_maps.t =
  let transformed_expr : (core_bluejay_edesc * Bluejay_to_jay_maps.t) m =
    let () = if do_wrap then wrap_flag := true else () in
    (* let () =
         Fmt.pr "This is pre-transformed: %a \n"
           Bluejay_ast_internal_pp.pp_expr_desc_with_tag (new_expr_desc e)
       in *)
    let%bind e' =
      if do_wrap
      then
        return (new_expr_desc e)
        >>= debug_transform_bluejay "initial" (fun e -> return e)
        >>= debug_transform_bluejay "bluejay wrap" wrap
        >>= debug_transform_bluejay "typed bluejay phase one" semantic_type_of
        >>= debug_transform_bluejay "typed bluejay phase two" bluejay_to_jay
      else
        return (new_expr_desc e)
        >>= debug_transform_bluejay "initial" (fun e -> return e)
        >>= debug_transform_bluejay "typed bluejay phase one" semantic_type_of
        >>= debug_transform_bluejay "typed bluejay phase two" bluejay_to_jay
    in
    let%bind bluejay_jay_map = bluejay_to_jay_maps in
    return (e', bluejay_jay_map)
  in
  run (new_translation_context ()) transformed_expr
