open Batteries;;
open Swan_toploop_types;;

let no_op_callbacks =
  { cb_illformednesses = (fun _ -> ())
  ; cb_variable_analysis = (fun _ _ _ _ -> ())
  ; cb_errors = (fun _ -> ())
  ; cb_evaluation_result = (fun _ _ -> ())
  ; cb_evaluation_failed = (fun _ -> ())
  ; cb_evaluation_disabled = (fun _ -> ())
  ; cb_size_report_callback = (fun _ -> ())
  }
;;

let stdout_errors_callback errors =
  errors
  |> List.iter
    (fun error ->
       print_string @@ Swan_toploop_analysis_types.show_error error
    );
    flush stdout
;;

let stdout_callbacks =
  { cb_illformednesses =
      Core_toploop.stdout_illformednesses_callback
  ; cb_variable_analysis =
      Core_toploop.stdout_variable_analysis_callback
  ; cb_errors =
      stdout_errors_callback
  ; cb_evaluation_result =
      Core_toploop.stdout_evaluation_result_callback
  ; cb_evaluation_failed =
      Core_toploop.stdout_evaluation_failed_callback
  ; cb_evaluation_disabled =
      Core_toploop.stdout_evaluation_disabled_callback
  ; cb_size_report_callback =
      Core_toploop.stdout_size_report_callback
  }
;;


let handle_expression
    ?callbacks:(callbacks=no_op_callbacks)
    (conf : Core_toploop_options.configuration)
    (swan_expr : Swan_ast.expr)
  =
  let (nested_expr, nested_map) = Swan_translator.swan_to_nested_translation swan_expr in
  let nested_callbacks =
    { Nested_toploop_types.cb_illformednesses =
        (fun ills -> callbacks.cb_illformednesses ills)
    ; Nested_toploop_types.cb_variable_analysis =
        (fun x loc callstack fv ->
           callbacks.cb_variable_analysis x loc callstack fv)
    ; Nested_toploop_types.cb_errors =
        (fun es ->
           callbacks.cb_errors
             (List.of_enum
             (Swan_toploop_analysis.batch_translation
              @@ Egg_toploop_analysis.batch_translation
                nested_map
                (List.enum es))))
    ; Nested_toploop_types.cb_evaluation_result =
        (fun x env -> (callbacks.cb_evaluation_result x env))
    ; Nested_toploop_types.cb_evaluation_failed =
        (fun s -> callbacks.cb_evaluation_failed s)
    ; Nested_toploop_types.cb_evaluation_disabled =
        (fun u -> callbacks.cb_evaluation_disabled u)
    ; Nested_toploop_types.cb_size_report_callback =
        (fun u -> callbacks.cb_size_report_callback u)
    }
  in
  let result =
    Nested_toploop.handle_expression
      ~callbacks:nested_callbacks
      conf nested_expr
  in
  let egg_errors =
    List.of_enum @@
    Egg_toploop_analysis.batch_translation
      nested_map @@ List.enum result.Nested_toploop_types.errors
  in
  let swan_errors = Swan_toploop_analysis.batch_translation (List.enum egg_errors)
  in
  { illformednesses = result.Nested_toploop_types.illformednesses
  ; analyses = result.Nested_toploop_types.analyses
  ; errors = List.of_enum swan_errors
  ; evaluation_result = result.Nested_toploop_types.evaluation_result
  }
;;
