(* in
   let () = Fmt.pr "%a" Jayil.Pp.expr post_inst_ast in
   Ast_wellformedness.check_wellformed_expr post_inst_ast ;
   Fmt.pr "@?" ;
   (inst_jil, jil_inst_map, on_odefa_maps, bluejay_to_jay_map') *)

(* print_delimiter_line "Original program: " ;
   let () = print_endline @@ Bluejay_ast.show_expr_desc bluejay_ast in *)

(* let bluejay_ast_internal =
        Bluejay_ast_internal.to_internal_expr_desc bluejay_ast
      in
      let core_ast, bluejay_to_jay_map =
        (* Typed -> Untyped *)
        Bluejay_to_jay.transform_bluejay ~do_wrap bluejay_ast_internal.body
      in
            print_delimiter_line "Jay program after type elimination: " ;
   let () = print_endline @@ Jay_ast_pp.show_expr_desc jay_ast in
       let core_ast, bluejay_to_jay_map =
         Convert.bluejay_edesc_to_core_ast ~do_wrap bluejay_ast
       in
       let bluejay_to_jay_map' =
         bluejay_ast |> Bluejay_ast_internal.to_internal_expr_desc
         |> Bluejay_to_jay_maps.find_all_syn_tags bluejay_to_jay_map
       in
       let jay_ast = Bluejay_ast_internal.to_jay_expr_desc core_ast in *)

(* let bluejay_ast = Bluejay_ast.new_expr_desc raw_bluejay in
   let jay_ast, bluejay_jay_map =
     Convert.bluejay_edesc_to_jay ~do_wrap bluejay_ast
   in
   let bluejay_instruments = bluejay_jay_map.instrumented_tags in
   let post_inst_ast, odefa_inst_maps, on_odefa_maps =
     let init_consts = Convert.bluejay_edesc_to_consts bluejay_ast in
     Jay_translate.Jay_to_jayil.translate ~is_jay:true
       ~is_instrumented:do_instrument ~consts:init_consts
       ~bluejay_instruments jay_ast
   in *)
(* print_delimiter_line "Jayil program after instrumentation: " *)

(* === *)
(* let jay_ast = Jay_ast.new_expr_desc raw_jay in
      let consts = Jay_to_jayil.jay_edesc_to_consts jay_ast in
      print_delimiter_line "Jay program before instrumentation: " ;
      let () = print_endline @@ Jay_ast_pp.show_expr_desc jay_ast in
      let post_inst_ast, odefa_inst_maps, on_odefa_maps =
        Jay_to_jayil.translate ~is_instrumented:do_instrument ~consts jay_ast
      in
      print_delimiter_line "Jayil program after instrumentation: " ;
      (post_inst_ast, odefa_inst_maps, Some on_odefa_maps, None) *)
