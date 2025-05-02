
open Core

module type S = sig
  include Z3_api.S

  val solve : bool Expression.t list -> [ `Sat of Input_feeder.t | `Unsat ]
  (** [solve] overwrites the [Z3_api.solve] function. *)
end

module Make () = struct
  module Sudu = Z3_api.Make ()
  include Sudu

  module E = Expression.Solve (Sudu)

  module Feeder = Input_feeder.Make (Sudu)

  (* When solving, try simplifications and substitutions first *)
  let solve exprs =
    let subs, new_exprs = Expression.simplify exprs in
    match new_exprs with
    | [] -> `Sat (Feeder.from_model_and_subs Sudu.empty_model subs)
    | [ e ] when Expression.equal e Expression.true_ -> `Sat (Feeder.from_model_and_subs Sudu.empty_model subs)
    | [ e ] when Expression.equal e Expression.false_ -> `Unsat
    | _ ->
      let not_e = List.last_exn new_exprs |> Expression.not_ in (* get the constraint associated with the target *)
      if List.exists new_exprs ~f:(fun e' -> Expression.equal not_e e') (* check if this constraint is the exact negation of another constraint along the path *)
      then `Unsat
      else
        new_exprs
        |> List.map ~f:E.to_formula
        |> Sudu.solve (* the simplifications weren't enough, so need to actually call the solver *)
        |> function
          | Sudu.Solve_status.Unsat -> `Unsat
          | Unknown -> failwith "unimplemented solver timeout" (* would want to convey that the search wasn't complete, but this doesn't practically happen *)
          | Sat model -> `Sat (Feeder.from_model_and_subs model subs)
end

module Default = Make ()
