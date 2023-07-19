let list ?(sep = Fmt.any ", ") elem oc list =
  Fmt.iter ~sep (fun f list -> List.iter f list) elem oc list

let set elem iter oc set = Fmt.iter ~sep:(Fmt.any ", ") iter elem oc set

let map key elem iter oc map =
  let pp_entry oc (x, v) = Fmt.pf oc "%a = %a" key x elem v in

  Fmt.iter_bindings ~sep:(Fmt.any ", ") iter pp_entry oc map

let paren_if ~c pp = if c then Fmt.(any "(" ++ pp ++ any ")") else pp

(* let var_ = Fmt.using (fun (Var x) -> x) id

   let rec expr oc = function
     | Int n -> Fmt.int oc n
     | Bool b -> Fmt.bool oc b
     | Var x -> id oc x
     | Function (ids, e) -> Fmt.pr "(%a -> %a)" id_list ids expr_d e
     | Input -> Fmt.string oc "input"
     | Appl (e1, e2) -> Fmt.pr "(%a %a)" expr_d e1 expr_d e2
     | _ -> Fmt.nop oc ()

   and expr_d oc { body; _ } = expr oc body *)
