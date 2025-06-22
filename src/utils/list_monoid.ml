
open Core

(* We use this instead of Preface's standard list monoid because
  I like how the `Core.List.append` skips the work when RHS is `[]`. *)

module Make (X : T) = struct
  include Preface.Make.Monoid.Via_combine_and_neutral (struct
    type t = X.t list
    let neutral : t = []
    let combine : t -> t -> t = List.append (* skips work if either is empty *)
  end)
end