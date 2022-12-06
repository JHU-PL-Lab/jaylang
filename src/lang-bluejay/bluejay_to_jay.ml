open Batteries
open Jhupllib
open Bluejay_ast_internal
open Bluejay_to_jay_monad
open BluejayTranslationMonad

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
  return ed

(* Phase one of transformation: turning all syntactic types into its
   semantic correspondence.
   i.e. int -> { generator = fun _ -> input,
               , checker = fun e -> isInt e
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
  match t with
  | TypeVar tvar ->
      (* When it's a single type variable, we have the invariance that it comes
         from a recursive type. Therefore, we want to roll in the self application
         here.

         tv -> (tv tv) *)
      let%bind res =
        new_instrumented_ed
        @@ Appl (new_expr_desc (Var tvar), new_expr_desc (Var tvar))
      in
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
          Let (fail_id, new_expr_desc @@ Bool false, new_expr_desc @@ check_cls)
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
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
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
                   (BoolPat, new_expr_desc @@ Bool true); (AnyPat, fail_pat_cls);
                 ] )
        in
        let check_cls = Function ([ expr_id ], match_edesc) in
        let fail_cls =
          Let (fail_id, new_expr_desc @@ Bool false, new_expr_desc @@ check_cls)
        in
        (* We have another point of error here, thus the mapping. *)
        let%bind () = add_error_to_tag_mapping fail_pat_cls tag in
        let%bind () =
          add_error_to_value_expr_mapping fail_pat_cls matched_expr
        in
        return @@ fail_cls
      in
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
      let%bind () = add_sem_to_syn_mapping res e_desc in
      return res
  | TypeRecord r ->
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
        let%bind res_record = list_fold_left_m folder empty_record lbl_to_var in
        let folder' acc (_, lbl_var, cur_t) =
          let%bind gc_pair = semantic_type_of cur_t in
          let%bind proj_ed =
            new_instrumented_ed @@ RecordProj (gc_pair, Label "generator")
          in
          let%bind appl_ed =
            new_instrumented_ed @@ Appl (proj_ed, new_expr_desc @@ Int 0)
          in
          let res = new_expr_desc @@ Let (lbl_var, appl_ed, acc) in
          return res
        in
        let base_acc = new_expr_desc @@ Record res_record in
        let%bind gen_expr = list_fold_left_m folder' base_acc lbl_to_var in
        let actual_rec =
          let decl_lbls =
            Ident_map.keys r
            |> Enum.fold
                 (fun acc k ->
                   Ident_map.add k (new_expr_desc @@ Record Ident_map.empty) acc)
                 Ident_map.empty
          in
          Ident_map.empty
          |> Ident_map.add (Ident "~actual_rec") gen_expr
          |> Ident_map.add (Ident "~decl_lbls")
               (new_expr_desc @@ Record decl_lbls)
        in
        return
        @@ Function ([ Ident "~null" ], new_expr_desc @@ Record actual_rec)
      in
      let%bind checker =
        (* Building the intial check for whether it's a record value *)
        let%bind rec_fail_id = fresh_ident "rec_fail" in
        let%bind expr_id = fresh_ident "expr" in
        let fail_pat_cls = new_expr_desc @@ Var rec_fail_id in
        let matched_expr = new_expr_desc @@ Var expr_id in
        (* For the checker, we need to first check whether the value in
           question is a record. If not, returns false. Otherwise. we need
           to go through all the fields in this record to check whether it
           has the correct type for each field. *)
        let all_bindings = List.rev @@ Ident_map.bindings r in
        let rec_pat = Ident_map.singleton (Ident "~actual_rec") None in
        let type_dict =
          Ident_map.of_enum @@ Enum.map (fun k -> (k, None)) (Ident_map.keys r)
        in
        let fold_fun expr_a (Ident lbl, t) =
          let%bind lbl_check_id = fresh_ident "lbl_check" in
          let%bind cur_gc_pair = semantic_type_of t in
          let%bind proj_ed_1 =
            new_instrumented_ed @@ RecordProj (cur_gc_pair, Label "checker")
          in
          let%bind proj_ed_2 =
            new_instrumented_ed
            @@ RecordProj (new_expr_desc @@ Var expr_id, Label lbl)
          in
          let%bind appl_ed =
            new_instrumented_ed @@ Appl (proj_ed_1, proj_ed_2)
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
        let Ident first_lbl, first_type = List.hd all_bindings in
        let%bind gc_pair_fst = semantic_type_of first_type in
        let%bind proj_ed_3 =
          new_instrumented_ed @@ RecordProj (gc_pair_fst, Label "checker")
        in
        let%bind proj_ed_4 =
          new_instrumented_ed
          @@ RecordProj (new_expr_desc @@ Var expr_id, Label first_lbl)
        in
        let%bind init_acc =
          new_instrumented_ed @@ Appl (proj_ed_3, proj_ed_4)
        in
        let%bind fun_body =
          list_fold_left_m fold_fun init_acc (List.tl all_bindings)
        in
        let%bind actual_rec =
          new_instrumented_ed @@ RecordProj (matched_expr, Label "~actual_rec")
        in
        let%bind lbls_check =
          new_instrumented_ed
          @@ Match (actual_rec, [ (RecPat type_dict, fun_body) ])
        in
        (* Also, the record type we have here is like OCaml; it must have the
           labels with the corresponding types, and nothing more. That's why
           we require a strict pattern match here. *)
        let%bind match_body =
          new_instrumented_ed
          @@ Match
               ( matched_expr,
                 [ (RecPat rec_pat, lbls_check); (AnyPat, fail_pat_cls) ] )
        in
        let check_cls =
          Let (rec_fail_id, new_expr_desc @@ Bool false, match_body)
        in
        (* Since the initial record check could be a point of faliure, we need to
           record it as well. *)
        let%bind () =
          add_error_to_value_expr_mapping fail_pat_cls matched_expr
        in
        let%bind () = add_error_to_tag_mapping fail_pat_cls tag in
        return @@ Function ([ expr_id ], new_expr_desc check_cls)
      in
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
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
              new_expr_desc @@ ListCons (new_expr_desc @@ Var elm_id, appl_ed_2)
            )
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
        let gen_expr = LetRecFun ([ list_maker_fun ], new_expr_desc list_len) in
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
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
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
        let fail_cls = Let (fail_id, new_expr_desc @@ Bool false, assert_cls) in
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (gc_pair_dom_gen, Label "checker")
        in
        let%bind appl_ed_1 =
          new_instrumented_ed
          @@ Appl (proj_ed_1, new_expr_desc @@ Var arg_assume)
        in
        let%bind proj_ed_2 =
          new_instrumented_ed @@ RecordProj (gc_pair_cod_gen, Label "generator")
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
          @@ Appl (new_expr_desc @@ Var expr_id, new_expr_desc @@ Var arg_assert)
        in
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (gc_pair_cod_check, Label "checker")
        in
        let%bind appl_ed_2 =
          new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var ret_id)
        in
        let codom_check = Let (ret_id, appl_ed_1, appl_ed_2) in
        let%bind proj_ed_2 =
          new_instrumented_ed
          @@ RecordProj (gc_pair_dom_check, Label "generator")
        in
        let%bind appl_ed_3 =
          new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
        in
        let fun_body = Let (arg_assert, appl_ed_3, new_expr_desc codom_check) in
        return @@ Function ([ expr_id ], new_expr_desc fun_body)
      in
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
      let%bind () = add_sem_to_syn_mapping res e_desc in
      return res
  | TypeArrowD ((x1, t1), t2) ->
      (* For dependently typed functions, we need to make sure the co-domain can
         refer to the domain's value, thus this helper function here. *)
      let mk_gc_pair_cod cod arg =
        Appl (new_expr_desc @@ Function ([ x1 ], cod), new_expr_desc @@ Var arg)
      in
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
          new_instrumented_ed @@ mk_gc_pair_cod gc_pair_cod_g arg_assume
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
          new_instrumented_ed @@ mk_gc_pair_cod gc_pair_cod_c arg_assert
        in
        let%bind gc_pair_cod' =
          new_instrumented_ed
          @@ Appl
               ( new_expr_desc @@ Function ([ x1 ], appl_ed_1),
                 new_expr_desc @@ Var arg_assert )
        in
        let%bind appl_ed_2 =
          new_instrumented_ed
          @@ Appl (new_expr_desc @@ Var expr_id, new_expr_desc @@ Var arg_assert)
        in
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (gc_pair_cod', Label "checker")
        in
        let%bind appl_ed_3 =
          new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var ret_id)
        in
        let codom_check = Let (ret_id, appl_ed_2, appl_ed_3) in
        let%bind proj_ed_2 =
          new_instrumented_ed @@ RecordProj (gc_pair_dom_c, Label "generator")
        in
        let%bind appl_ed_4 =
          new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
        in
        let fun_body = Let (arg_assert, appl_ed_4, new_expr_desc codom_check) in
        return @@ Function ([ expr_id ], new_expr_desc fun_body)
      in
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
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
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
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
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (gc_pair1_g, Label "generator")
        in
        let%bind appl_ed_1 =
          new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Int 0)
        in
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
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
      let%bind () = add_sem_to_syn_mapping res e_desc in
      return res
  | TypeIntersect (t1, t2) ->
      (* Note: Intersection of all records are now empty? *)
      let rec flatten_fun_intersection ed acc =
        match ed.body with
        | TypeIntersect (t1, t2) -> (
            match (t1.body, t2.body) with
            | TypeArrow _, TypeArrow _ | TypeArrowD _, TypeArrowD _ ->
                t1 :: t2 :: acc
            | TypeArrow _, TypeIntersect _ | TypeArrowD _, TypeIntersect _ ->
                let acc' = t1 :: acc in
                flatten_fun_intersection t2 acc'
            | TypeIntersect _, TypeArrow _ | TypeIntersect _, TypeArrowD _ ->
                let acc' = t2 :: acc in
                flatten_fun_intersection t1 acc'
            | TypeIntersect _, TypeIntersect _ ->
                let acc' = flatten_fun_intersection t1 acc in
                flatten_fun_intersection t2 acc'
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
        | TypeArrow (t1, t2) | TypeArrowD ((_, t1), t2) ->
            if is_fun_type t1 then false else domain_check t2
        | _ -> true
      in
      let mk_fun_intersect_gen fun_types =
        let well_formed = List.for_all domain_check fun_types in
        if well_formed
        then
          let mk_gc_pair_cod x_id cod arg =
            Appl
              ( new_expr_desc @@ Function ([ x_id ], cod),
                new_expr_desc @@ Var arg )
          in
          let%bind arg = fresh_ident "arg" in
          let rec folder t acc =
            match t.body with
            | TypeArrow (t1, t2) ->
                let%bind gc_pair_dom_g = semantic_type_of t1 in
                let%bind gc_pair_cod_g = semantic_type_of t2 in
                let%bind proj_ed_1 =
                  new_instrumented_ed
                  @@ RecordProj (gc_pair_dom_g, Label "checker")
                in
                let%bind appl_ed_1 =
                  new_instrumented_ed
                  @@ Appl (proj_ed_1, new_expr_desc @@ Var arg)
                in
                let%bind proj_ed_2 =
                  new_instrumented_ed
                  @@ RecordProj (gc_pair_cod_g, Label "generator")
                in
                let%bind appl_ed_2 =
                  new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
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
                let%bind proj_ed_1 =
                  new_instrumented_ed
                  @@ RecordProj (gc_pair_dom_g, Label "checker")
                in
                let%bind appl_ed_1 =
                  new_instrumented_ed
                  @@ Appl (proj_ed_1, new_expr_desc @@ Var arg)
                in
                let%bind proj_ed_2 =
                  new_instrumented_ed
                  @@ RecordProj (gc_pair_cod_g, Label "generator")
                in
                let%bind appl_ed_2 =
                  new_instrumented_ed @@ Appl (proj_ed_2, new_expr_desc @@ Int 0)
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
          failwith "mk_fun_intersect_gen: ill-formed function intersection type"
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
        else
          let%bind gc_pair1_g = semantic_type_of t1 in
          let%bind gc_pair2_g = semantic_type_of t2 in
          let%bind candidate_var = fresh_ident "candidate" in
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
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
      let%bind () = add_sem_to_syn_mapping res e_desc in
      return res
  | TypeRecurse (t_var, t') ->
      (* For recursive types, we're really setting up the bootstrap here. *)
      let%bind gc_pair = semantic_type_of t' in
      let%bind primer_id = fresh_ident "primer" in
      let%bind appl_ed =
        new_instrumented_ed
        @@ Appl (new_expr_desc @@ Var primer_id, new_expr_desc @@ Var primer_id)
      in
      let res =
        new_expr_desc
        @@ Let
             (primer_id, new_expr_desc @@ Function ([ t_var ], gc_pair), appl_ed)
      in
      let%bind () = add_sem_to_syn_mapping res e_desc in
      return res
  | TypeUntouched t' ->
      let inner_map =
        Ident_map.empty
        |> Ident_map.add
             (Ident ("~\'" ^ t'))
             (new_expr_desc @@ Record Ident_map.empty)
      in
      let untouched_v =
        Ident_map.empty
        |> Ident_map.add (Ident "~untouched") (new_expr_desc @@ Record inner_map)
      in
      let generator =
        Function ([ Ident "~null" ], new_expr_desc @@ Record untouched_v)
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
      let rec_map =
        Ident_map.empty
        |> Ident_map.add (Ident "generator") (new_expr_desc generator)
        |> Ident_map.add (Ident "checker") (new_expr_desc checker)
      in
      let res = new_expr_desc @@ Record rec_map in
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
      let%bind e1' =
        let%bind e1_transformed = semantic_type_of e1 in
        if is_record_type t
        then
          let new_lbls =
            match t.body with
            | TypeRecord r ->
                r |> Ident_map.keys
                |> Enum.fold
                     (fun acc k ->
                       Ident_map.add k
                         (new_expr_desc @@ Record Ident_map.empty)
                         acc)
                     Ident_map.empty
            | _ ->
                failwith
                  "semantic_type_of: Should only be invoked when t is a record \
                   type!"
          in
          let%bind actual_rec =
            new_instrumented_ed
            @@ RecordProj (e1_transformed, Label "~actual_rec")
          in
          let new_rec =
            Ident_map.empty
            |> Ident_map.add (Ident "~actual_rec") actual_rec
            |> Ident_map.add (Ident "~decl_lbls")
                 (new_expr_desc @@ Record new_lbls)
          in
          return @@ new_expr_desc @@ Record new_rec
        else return e1_transformed
      in
      let%bind e2' = semantic_type_of e2 in
      let%bind t' = semantic_type_of t in
      let res = new_expr_desc @@ LetWithType (x, e1', e2', t') in
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
            (* |> Ident_map.add (Ident ("~\'" ^ t')) None *)
          in
          (RecPat check_pat, assert_cls)
        in
        return @@ (check_poly :: og_pats)
      in
      let transformed_match = new_expr_desc @@ Match (e', pattern_expr_lst') in
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
      let%bind expr_lst' = expr_lst |> List.map semantic_type_of |> sequence in
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
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (ret_type_core, Label "checker")
        in
        let%bind check_ret = new_instrumented_ed @@ Appl (proj_ed_1, mk_appl) in
        let%bind check_expr =
          list_fold_right_m
            (fun (arg, t) acc ->
              let%bind t' = bluejay_to_jay t in
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
        let%bind e1' = bluejay_to_jay e1 in
        let%bind e2' = bluejay_to_jay e2 in
        let%bind check_res = fresh_ident "check_res" in
        let%bind () = add_error_to_bluejay_mapping check_res e_desc in
        let%bind error_cls = new_instrumented_ed @@ TypeError check_res in
        let%bind res_cls =
          new_instrumented_ed
          @@ If (new_expr_desc @@ Var check_res, e2', error_cls)
        in
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (type_decl', Label "checker")
        in
        let%bind appl_ed_1 =
          new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var x)
        in
        let check_cls = Let (check_res, appl_ed_1, res_cls) in
        let res = new_expr_desc @@ Let (x, e1', new_expr_desc check_cls) in
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
        let res = new_expr_desc @@ Record m' in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | RecordProj (e, l) ->
        let%bind e' = bluejay_to_jay e in
        let%bind res =
          if instrumented_bool
          then return @@ new_expr_desc @@ RecordProj (e', l)
          else
            let%bind inner_projection =
              new_instrumented_ed @@ RecordProj (e', Label "~actual_rec")
            in
            return @@ new_expr_desc @@ RecordProj (inner_projection, l)
        in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
    | Match (e, pattern_expr_lst) ->
        (* if instrumented_bool
           then *)
        let%bind e' = bluejay_to_jay e in
        let mapper (pat, expr) =
          let%bind expr' = bluejay_to_jay expr in
          return @@ (pat, expr')
        in
        let%bind pattern_expr_lst' =
          pattern_expr_lst |> List.map mapper |> sequence
        in
        let res = new_expr_desc @@ Match (e', pattern_expr_lst') in
        let%bind () = add_core_to_sem_mapping res e_desc in
        return res
        (* else
           let%bind e' = bluejay_to_jay e in
           let mapper (pat, expr) =
             match pat with
             | AnyPat | IntPat | BoolPat | FunPat | StrictRecPat _ | VariantPat _
             | VarPat _ | EmptyLstPat | LstDestructPat _ ->
                 let%bind expr' = bluejay_to_jay expr in
                 return @@ (pat, expr')
             | RecPat rec_pat ->
                 let%bind expr' = bluejay_to_jay expr in
                 let%bind decl_lbls =
                   new_instrumented_ed @@ RecordProj (e', Label "~decl_lbls")
                 in
                 let%bind legal_match =
                   new_instrumented_ed
                   @@ Match (decl_lbls, [ (RecPat rec_pat, expr') ])
                 in
                 let pat' = RecPat
                 (Ident_map.singleton (Ident "~actual_rec") None) in
                 return @@ (pat', legal_match)
           in
           let%bind pattern_expr_lst' =
             pattern_expr_lst |> List.map mapper |> sequence
           in
           let res = new_expr_desc @@ Match (e', pattern_expr_lst') in
           let%bind () = add_core_to_sem_mapping res e_desc in
           return res *)
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

let rec wrap (e_desc : sem_bluejay_edesc) : sem_bluejay_edesc m =
  let mk_check_from_fun_sig fun_sig =
    match fun_sig with
    | Typed_funsig (f, typed_params, (f_body, ret_type)) ->
        let folder ((Ident p as param), t) acc =
          let%bind bluejay_jay_maps = bluejay_to_jay_maps in
          let t_syn =
            Bluejay_to_jay_maps.Intermediate_expr_desc_map.find t
              bluejay_jay_maps.sem_to_syn
          in
          if is_polymorphic_type t_syn
          then return acc
          else
            let%bind eta_arg = fresh_ident p in
            let%bind arg_check = fresh_ident "arg_check" in
            let%bind proj_ed_1 =
              new_instrumented_ed @@ RecordProj (t, Label "checker")
            in
            let%bind check_arg =
              new_instrumented_ed
              @@ Appl (proj_ed_1, new_expr_desc @@ Var eta_arg)
            in
            let%bind assert_cls =
              new_instrumented_ed @@ Assert (new_expr_desc @@ Bool false)
            in
            let%bind cond =
              new_instrumented_ed
              @@ If (new_expr_desc @@ Var arg_check, acc, assert_cls)
            in
            let eta_body = Let (arg_check, check_arg, cond) in
            let%bind wrapped_body =
              new_instrumented_ed
              @@ Appl
                   ( new_expr_desc
                     @@ Function ([ eta_arg ], new_expr_desc @@ eta_body),
                     new_expr_desc @@ Var param )
            in
            return wrapped_body
        in
        let%bind f_body' = wrap f_body in
        let%bind wrapped_f = list_fold_right_m folder typed_params f_body' in
        let%bind typed_params' =
          sequence
          @@ List.map
               (fun (p, t) ->
                 let%bind t' = wrap t in
                 return @@ (p, t'))
               typed_params
        in
        let%bind ret_type' = wrap ret_type in
        let fun_sig' =
          Typed_funsig (f, typed_params', (wrapped_f, ret_type'))
        in
        return fun_sig'
    | DTyped_funsig (f, ((Ident p as param), t), (f_body, ret_type)) ->
        let%bind eta_arg = fresh_ident p in
        let%bind arg_check = fresh_ident "arg_check" in
        let%bind proj_ed_1 =
          new_instrumented_ed @@ RecordProj (t, Label "checker")
        in
        let%bind check_arg =
          new_instrumented_ed @@ Appl (proj_ed_1, new_expr_desc @@ Var eta_arg)
        in
        let%bind f_body' = wrap f_body in
        let%bind assert_cls =
          new_instrumented_ed @@ Assert (new_expr_desc @@ Bool false)
        in
        let%bind cond =
          new_instrumented_ed
          @@ If (new_expr_desc @@ Var arg_check, f_body', assert_cls)
        in
        let eta_body = Let (arg_check, check_arg, cond) in
        let%bind wrapped_body =
          new_instrumented_ed
          @@ Appl
               ( new_expr_desc
                 @@ Function ([ eta_arg ], new_expr_desc @@ eta_body),
                 new_expr_desc @@ Var param )
        in
        let%bind t' = wrap t in
        let%bind ret_type' = wrap ret_type in
        let fun_sig' =
          DTyped_funsig (f, (Ident p, t'), (wrapped_body, ret_type'))
        in
        return fun_sig'
  in
  let e = e_desc.body in
  (* Using the original tag for now; may be buggy *)
  let tag = e_desc.tag in
  let%bind instrumented_bool = is_instrumented tag in
  let%bind transformed_ed =
    match e with
    | Int _ | Bool _ | Var _ | Input | TypeError _ -> return e_desc
    | Function (id_lst, e) ->
        let%bind e' = wrap e in
        let res = new_expr_desc @@ Function (id_lst, e') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Appl (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Appl (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Let (x, e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Let (x, e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | LetRecFun (sig_lst, e) ->
        let%bind sig_lst' =
          sig_lst |> List.map (transform_funsig wrap) |> sequence
        in
        let%bind e' = wrap e in
        let res = new_expr_desc @@ LetRecFun (sig_lst', e') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | LetFun (fun_sig, e) ->
        let%bind sig' = fun_sig |> transform_funsig wrap in
        let%bind e' = wrap e in
        let res = new_expr_desc @@ LetFun (sig', e') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    (* TODO: Will want to handle the function case here *)
    | LetWithType (x, e1, e2, type_decl) ->
        let%bind type_decl' = wrap type_decl in
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ LetWithType (x, e1', e2', type_decl') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | LetRecFunWithType (sig_lst, e) ->
        let%bind sig_lst' =
          sequence @@ List.map mk_check_from_fun_sig sig_lst
        in
        let%bind og_e' = wrap e in
        let res = new_expr_desc @@ LetRecFunWithType (sig_lst', og_e') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | LetFunWithType (fun_sig, e) ->
        let%bind fun_sig' = mk_check_from_fun_sig fun_sig in
        let%bind og_e' = wrap e in
        let res = new_expr_desc @@ LetFunWithType (fun_sig', og_e') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Plus (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Plus (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Minus (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Minus (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Times (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Times (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Divide (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Divide (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Modulus (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Modulus (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Equal (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Equal (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Neq (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Neq (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | LessThan (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ LessThan (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Leq (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Leq (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | GreaterThan (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ GreaterThan (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Geq (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Geq (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | And (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ And (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Or (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ Or (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Not e ->
        let%bind e' = wrap e in
        let res = new_expr_desc @@ Not e' in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | If (e1, e2, e3) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let%bind e3' = wrap e3 in
        let res = new_expr_desc @@ If (e1', e2', e3') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Record m ->
        let%bind m' = ident_map_map_m (fun e -> wrap e) m in
        let res = new_expr_desc @@ Record m' in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | RecordProj (e, l) ->
        let%bind e' = wrap e in
        let res = new_expr_desc @@ RecordProj (e', l) in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Match (e, pattern_expr_lst) ->
        let%bind e' = wrap e in
        let mapper (pat, expr) =
          let%bind expr' = wrap expr in
          return @@ (pat, expr')
        in
        let%bind pattern_expr_lst' =
          pattern_expr_lst |> List.map mapper |> sequence
        in
        let res = new_expr_desc @@ Match (e', pattern_expr_lst') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | VariantExpr (lbl, e) ->
        let%bind e' = wrap e in
        let res = new_expr_desc @@ VariantExpr (lbl, e') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | List expr_lst ->
        let%bind expr_lst' = expr_lst |> List.map wrap |> sequence in
        let res = new_expr_desc @@ List expr_lst' in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | ListCons (e1, e2) ->
        let%bind e1' = wrap e1 in
        let%bind e2' = wrap e2 in
        let res = new_expr_desc @@ ListCons (e1', e2') in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Assert e ->
        let%bind e' = wrap e in
        let res = new_expr_desc @@ Assert e' in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
    | Assume e ->
        let%bind e' = wrap e in
        let res = new_expr_desc @@ Assume e' in
        let%bind () = add_wrapped_to_unwrapped_mapping res e_desc in
        return res
  in
  let%bind () =
    if instrumented_bool
    then add_instrumented_tag transformed_ed.tag
    else return @@ ()
  in
  return transformed_ed

let transform_bluejay ?(do_wrap = true) (e : syn_type_bluejay) :
    core_bluejay_edesc * Bluejay_to_jay_maps.t =
  let transformed_expr : (core_bluejay_edesc * Bluejay_to_jay_maps.t) m =
    let%bind e' =
      if do_wrap
      then
        return (new_expr_desc e)
        >>= debug_transform_bluejay "initial" (fun e -> return e)
        >>= debug_transform_bluejay "typed bluejay phase one" semantic_type_of
        >>= debug_transform_bluejay "wrap" wrap
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