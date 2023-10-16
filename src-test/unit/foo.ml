[@@@warning "-34"]

open Core
(*
   module Int = struct
     module T = struct
       type t = int [@@deriving compare, sexp_of]
     end

     include T
     include Comparator.Make (T)
   end

   module Make (S : module type of Int) = struct
     type m = string Map.M(S).t [@@deriving compare, sexp_of]
   end

   module Case_sexp = struct
     type m = string Map.M(Int).t [@@deriving compare, sexp]

     module Make (S : module type of Int) = struct
       (* Error: Signature mismatch here for `S` *)
       (* type m = string Map.M(S).t [@@deriving compare, sexp] *)
     end
   end

   module Case_sexp_working1 = struct
     module Int = struct
       module T = struct
         type t = int [@@deriving compare, sexp]
       end

       include T
       include Comparator.Make (T)
     end

     type m = string Map.M(Int).t [@@deriving compare, sexp]

     module type TS = sig
       include module type of Int
       include Comparator.S with type t = t
     end

     module Make (S : TS) = struct
       type m = string Map.M(S).t [@@deriving compare, sexp]
     end
   end

   module Case_sexp_working2 = struct
     module Int = struct
       module T = struct
         type t = int [@@deriving compare, sexp]
       end

       include T
       include Comparator.Make (T)
     end

     type m = string Map.M(Int).t [@@deriving compare, sexp]

     module Make (S : module type of struct
       include Int
     end) =
     struct
       type m = string Map.M(S).t [@@deriving compare, sexp]
     end
   end *)

open Core

module Int = struct
  module T = struct
    type t = int [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

module Make (S : module type of Int) = struct
  (* Error: Signature mismatch here for `S` *)
  (* type m = string Map.M(S).t [@@deriving compare, sexp] *)
end

module Make1 (S : module type of struct
  include Int
end) =
struct
  type m = string Map.M(S).t [@@deriving compare, sexp]
end

module Make2 (S : module type of Int with module T := Int.T) = struct
  type m = string Map.M(S).t [@@deriving compare, sexp]
end

module Make3 (S : sig
  include Comparator.S with type t = Int.t
end) =
struct
  type m = string Map.M(S).t [@@deriving compare, sexp]
end
(*
   module type T = sig
     type t
   end

   module Copy (T : T with type t := float) = struct
     module T2 = struct
       include T
     end
   end

   module Foo = struct
     module T = struct
       type t = int

       module U = struct
         type u = string
       end
     end

     (* include T *)
     include Copy (T)
   end

   module Make (P : module type of Foo) = struct end
   module Baz = Make (Foo) *)

let () = ()
