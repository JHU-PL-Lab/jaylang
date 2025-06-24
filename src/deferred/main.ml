
let unimplemented () = 
  failwith "unimplemented"

(*
  Still haven't...
    - defined values (but they should be an extension of embedded values in Lang.Value)
      - or it may be inspired by Analysis.Grammar
    - set up an executable
    - ensured an ordering on program points
*)

let rec eval (expr : Lang.Ast.Embedded.With_program_points.t) : unit =
  match expr with
  | EUnit -> unimplemented ()
  | EInt _i -> unimplemented ()
  | EBool _b -> unimplemented ()
  | EVar _id -> unimplemented ()
  | EId -> unimplemented () (* baked in identify function *)
  (* inputs *)
  | EPick_i { data = () ; point = _ } -> unimplemented ()
  | EPick_b { data = () ; point = _ } -> unimplemented ()
  (* operations *)
  | EBinop { left = _ ; binop = _ ; right = _ } -> unimplemented ()
  | ENot _expr -> unimplemented ()
  | EProject { record = _ ; label = _ } -> unimplemented ()
  (* control flow / branches *)
  | EMatch { subject = _ ; patterns = _ } -> unimplemented ()
  | EIf { cond = _ ; true_body = _ ; false_body = _ } -> unimplemented ()
  | ECase { subject = _ ; cases = _ ; default = _ } -> unimplemented ()
  (* closures and applications *)
  | EFunction { param = _ ; body = _ } -> unimplemented ()
  | EFreeze _expr -> unimplemented ()
  | ELet { var = _ ; defn = _ ; body = _ } -> unimplemented ()
  | EIgnore { ignored = _ ; body = _ } -> unimplemented () (* eval ignored, discard it, then eval body *)
  | EAppl { data = { func = _ ; arg = _ } ; point = _ } -> unimplemented ()
  | EThaw { data = _expr_closure ; point = _ } -> unimplemented ()
  (* modules, records, and variants  *)
  | ERecord _label_map -> unimplemented ()
  | EVariant { label = _ ; payload = _ } -> unimplemented ()
  | EModule _stmt_ls -> unimplemented ()
  | EUntouchable _e -> unimplemented ()
  (* termination *)
  | EDiverge { data = () ; point = _ } -> unimplemented ()
  | EAbort { data = _msg ; point = _ } -> unimplemented ()
  (* unhandled and currently ignored *)
  | EDet expr
  | EEscapeDet expr -> eval expr (* it is fine to ignore these and just eval what's inside for now *)
  (* unhandled and currently aborting -- okay to ignore for now because these are uncommon *)
  | EIntensionalEqual _ -> failwith "unhandled intensional equality in deferred evaluation"
  | ETable
  | ETblAppl _ -> failwith "unhandled table operations in deferred evaluation"