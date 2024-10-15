open Core
open Core_bench
open Fix

(* The benchmark is to bench how to store a small map into a big map that allow multiple values. I am not sure whether the _multiple value_ is meanful here yet. The combination is from

      Big_map        = Map | Hash_tbl | Map ref
      Big_map_value  = Set | Multi_value
      Small_map      = Value | Hashcons

   One problem is not directly addressed here yet, the cost of hashcons is computed when a small map is updated, the benefit of hashcons is when it's added into the big map. It's corelated when any updated small map will be added into the big map immediately.
*)

module IL = struct
  module T = struct
    type t = Int.t list [@@deriving equal, compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

module IL_HC = struct
  module T = HashCons.ForHashedType (struct
    type t = IL.t

    let equal = IL.equal
    let hash = IL.hash
  end)

  module K = struct
    type t = IL.t HashCons.cell

    let compare = HashCons.compare

    (* let hash = HashCons.hash *)
    let sexp_of_t e = IL.sexp_of_t (HashCons.data e)
  end

  include K
  include Comparator.Make (K)
end

module Env = struct
  module T = struct
    type t = IL.t Map.M(Int).t [@@deriving equal, compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

module Env_HC = struct
  module T = HashCons.ForHashedType (struct
    type t = Env.t

    let equal = Env.equal
    let hash = Env.hash
  end)

  module K = struct
    type t = Env.t HashCons.cell

    let compare = HashCons.compare
    let hash = HashCons.hash
    let sexp_of_t e = Env.sexp_of_t (HashCons.data e)
  end

  include K
  include Comparator.Make (K)
end

module Table = struct
  module T = struct
    type t = IL.t Map.M(Int).t Map.M(Int).t
    [@@deriving equal, compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

(* TODO: hashcons *)

(* type the_table = Table.t
   type table_in_set = Set.M(Table).t
   type table_in_list = Table.t list
   type table_in_set_cell = Set.M(Table).t HashCons.cell
   type table_in_list_cell = Table.t list HashCons.cell
   type catalog1 = table_in_set Map.M(Int).t
   type catalog2 = table_in_list Map.M(Int).t
   type catalog3 = table_in_set_cell Map.M(Int).t
   type catalog4 = table_in_list_cell Map.M(Int).t *)

let iter n = List.init n ~f:Fn.id
let seq_from offset width = List.init width ~f:(fun wi -> offset + wi)
(* let seqi_from offset width = List.init width ~f:(fun wi -> (wi, offset + wi)) *)

let mk_env offset width =
  iter width |> List.map ~f:(fun wi -> (wi, seq_from (offset + wi) width))

(* let tables = List.init 1000 ~f:(fun i -> mk_env i 100) *)

let tables_env =
  List.init 1000 ~f:(fun i -> mk_env i 100 |> Map.of_alist_exn (module Int))

let tables_env_hc =
  List.init 1000 ~f:(fun i ->
      mk_env i 100 |> Map.of_alist_exn (module Int) |> Env_HC.T.make)

module Test1 = struct
  let do_one n t =
    iter n
    |> List.fold
         ~init:(Map.empty (module Int))
         ~f:(fun acc n ->
           let i = n + t in
           let env = List.nth_exn tables_env i in
           Map.update acc i ~f:(function
             | None -> Set.singleton (module Env) env
             | Some set -> Set.add set env))

  let test ~n = let table = do_one n 0 in

                ignore @@ table
end

module Test2 = struct
  let do_one n t =
    iter n
    |> List.fold
         ~init:(Map.empty (module Int))
         ~f:(fun acc n ->
           let i = n + t in
           let env = List.nth_exn tables_env i in
           let ans = Map.find_multi acc i in
           if List.mem ans env ~equal:Env.equal
           then acc
           else Map.add_multi ~key:i ~data:env acc)

  let test ~n = let table = do_one n 0 in

                ignore @@ table
end

module Test3 = struct
  let do_one n t =
    iter n
    |> List.fold
         ~init:(Map.empty (module Int))
         ~f:(fun acc n ->
           let i = n + t in
           let env = List.nth_exn tables_env_hc i in
           Map.update acc i ~f:(function
             | None -> Set.singleton (module Env_HC) env
             | Some set -> Set.add set env))

  let test ~n = let table = do_one n 0 in

                ignore @@ table
end

module Test4 = struct
  let do_one n t =
    iter n
    |> List.fold
         ~init:(Map.empty (module Int))
         ~f:(fun acc n ->
           let i = n + t in
           let env = List.nth_exn tables_env_hc i in
           Map.update acc i ~f:(function
             | None -> Hash_set.of_list (module Env_HC) [ env ]
             | Some set ->
                 Hash_set.add set env ;
                 set))

  let test ~n = let table = do_one n 0 in

                ignore @@ table
end

module Test5 = struct
  let htbl = Hashtbl.create (module Int)

  let do_one n t =
    iter n
    |> List.iter ~f:(fun n ->
           let i = n + t in
           let env = List.nth_exn tables_env i in
           Hashtbl.update htbl i ~f:(function
             | None -> Set.singleton (module Env) env
             | Some set -> Set.add set env))

  let test ~n = let () = do_one n 0 in

                ()
end

let tests ~n =
  let t name f = Bench.Test.create f ~name in
  [
    t "table_in_set" (fun () -> Test1.test ~n);
    t "table_in_multi" (fun () -> Test2.test ~n);
    t "table_in_set_hc" (fun () -> Test3.test ~n);
    t "hashtbl_in_hash_set_hc" (fun () -> Test4.test ~n);
    t "hashtbl_in_set_hc" (fun () -> Test5.test ~n);
  ]

let () = tests ~n:500 |> Bench.make_command |> Command_unix.run

(* let tests ~n =
   let t name f = Bench.Test.create f ~name in
   [
     t "map" (fun () -> map_iter ~n);
     t "tbl" (fun () -> tbl_iter ~n);
     t "hc_tbl" (fun () -> hc_tbl_iter ~n);
   ] *)

(*
   module Block = struct
     module T = struct
       type t = Set.M(Int).t [@@deriving equal, compare, sexp_of, hash]
     end

     include T
     include Comparator.Make (T)
   end

   module HC_block = struct
     module T = struct
       type t = Set.M(Int).t [@@deriving equal, compare, sexp_of, hash]
     end

     module T2 = HashCons.ForHashedType (struct
       type t = T.t

       let equal = T.equal
       let hash = T.hash
     end)

     module T3 = struct
       type t = T.t HashCons.cell

       let compare = HashCons.compare
       let hash = HashCons.hash
       let sexp_of_t e = T.sexp_of_t (HashCons.data e)
     end

     include T3
     include Comparator.Make (T3)
   end

   let block_from n k : Block.t = Set.of_list (module Int) (seq_from n k)

   let map_iter ~n =
     iter n
     |> List.fold
          ~init:(Map.empty (module Int))
          ~f:(fun acc n ->
            let block = block_from (n * 100) 100 in
            Map.add_exn ~key:n ~data:block acc)
     |> ignore

   let tbl_iter ~n =
     let tbl = Hashtbl.create (module Int) in
     iter n
     |> List.iter ~f:(fun n ->
            let block = block_from (n * 100) 100 in
            Hashtbl.add_exn ~key:n ~data:block tbl)
     |> ignore

   let hc_block_from n k : HC_block.t =
     HC_block.T2.make (Set.of_list (module Int) (seq_from n k))

   let hc_tbl_iter ~n =
     let tbl = Hashtbl.create (module HC_block) in
     iter n
     |> List.iter ~f:(fun n ->
            let block = hc_block_from (n * 100) 100 in
            Hashtbl.add_exn ~key:block ~data:block tbl)
     |> ignore *)

(* let tests ~n =
     let t name f = Bench.Test.create f ~name in
     [
       t "map" (fun () -> map_iter ~n);
       t "tbl" (fun () -> tbl_iter ~n);
       t "hc_tbl" (fun () -> hc_tbl_iter ~n);
     ]

   let () = tests ~n:200000 |> Bench.make_command |> Command_unix.run *)
