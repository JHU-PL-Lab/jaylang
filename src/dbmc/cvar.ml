open Core
open Helper

module T = struct
  type cat =
    | Fun_to_callsite of {
        block_f : Id.t;
        block_callsite : Id.t;
        callsite : Id.t;
      }
    | Callsite_to_fun of {
        callsite : Id.t;
        block_callsite : Id.t;
        block_f : Id.t;
      }
    | Condsite of { condsite : Id.t; beta : bool }

  and state = Core | Complete | Picked

  and t = { cat : cat; r_stk : Relative_stack.t; state : state }
  [@@deriving sexp, compare, equal, hash, show { with_path = false }]
end

include T
include Comparator.Make (T)

let print cvar =
  let cat_string =
    match cvar.cat with
    | Fun_to_callsite fc ->
        Fmt.str "%a$%a_to_%a" Id.pp fc.callsite Id.pp fc.block_f Id.pp
          fc.block_callsite
    | Callsite_to_fun cf ->
        Fmt.str "%a$%a_to_%a" Id.pp cf.callsite Id.pp cf.block_callsite Id.pp
          cf.block_f
    | Condsite cos -> Fmt.str "%a$%B" Id.pp cos.condsite cos.beta
  in
  let state_string =
    match cvar.state with Core -> "" | Complete -> "cc" | Picked -> "pp"
  in
  Fmt.str "%a_%s_%s" Relative_stack.pp cvar.r_stk cat_string state_string

let pp_print = Fmt.of_to_string print

let mk_condsite condsite r_stk =
  List.map [ true; false ] ~f:(fun beta ->
      let cat = Condsite { condsite; beta } in
      { cat; r_stk; state = Core })

let mk_fun_to_callsite fc =
  List.map fc.outs ~f:(fun out ->
      let cat =
        Fun_to_callsite
          {
            block_f = fc.fun_in;
            block_callsite = out.f_out;
            callsite = out.site;
          }
      in
      { cat; r_stk = fc.stk_in; state = Core })

let mk_callsite_to_fun cf =
  List.map cf.ins ~f:(fun in_ ->
      let cat =
        Callsite_to_fun
          {
            callsite = cf.site;
            block_callsite = cf.f_out;
            block_f = in_.fun_in;
          }
      in
      { cat; r_stk = cf.stk_out; state = Core })

let set_complete cvar = { cvar with state = Complete }

let set_picked cvar = { cvar with state = Picked }

let derive_complete_and_picked cvars =
  (List.map ~f:set_complete cvars, List.map ~f:set_picked cvars)
