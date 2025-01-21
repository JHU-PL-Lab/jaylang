open Core

include Jayil.Ast.Ident_new
include Comparator.Make (Jayil.Ast.Ident_new)

let show (Ident s) = s
let pp oc (Ident s) = Fmt.pf oc "%s" s
let pp_list oc ids = Fmt.(pf oc "%a" (Dump.list pp) ids)
let tt = "$tt"
let ff = "$ff"
let cond_id_s b = if b then tt else ff
let cond_id b = Ident (cond_id_s b)
let cond_block_id (Ident condsite) b = Ident (condsite ^ cond_id_s b)
let name_main = "0_main"
let main_block = Ident name_main
let target_s = "target"
let default_target = Ident target_s
let s_ n = Ident n
