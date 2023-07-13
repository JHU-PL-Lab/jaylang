open Jay_ast

let id = Fmt.(using (fun (Ident s) -> s) string)
let var_ = Fmt.using (fun (Var (x, _)) -> x) id

let expr oc = function
  | Int n -> Fmt.int oc n
  | Bool b -> Fmt.bool oc b
  | Var x -> var_ oc x
  | _ -> Fmt.nop oc
