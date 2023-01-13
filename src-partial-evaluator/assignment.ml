(*

PL Assignment 1
 
Name                  : Ricky Cheng
List of Collaborators : 

Please make a good faith effort at listing people you discussed any 
problems with here, as per the course academic integrity policy.  
CAs/Prof need not be listed!

Fill in the function definitions below replacing the 

  unimplemented ()

with your code.  Feel free to add "rec" to any function listed to make
it recursive. In some cases, you will find it helpful to define
auxillary functions, feel free to.

Several of the questions below involve computing well-known mathematical functions;
if you are not familiar with the function named your trusty search engine
should be able to give you an answer, and feel free to ask on Piazza.

You must not use any mutation operations of OCaml for any of these
questions (which we have not taught yet in any case): no arrays,
for- or while-loops, references, etc.

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* Here is a simple function which gets passed unit, (), as argument
   and raises an exception.  It is the initial implementation below. *)

let unimplemented () =
	failwith "unimplemented"
	

(* ************** Section One: List operations ************** *)

(* 1a. Write a function that will count the number of occurrences of a specific element
			 in a list. 
*)

let rec count_occurrences elm l =
   match l with
   | [] -> 0
   | x :: xs -> count_occurrences elm xs
   + if x = elm then 1 else 0
;;

(*
# count_occurrences 3 [1;2;3;3;4;5;6] ;;
- : int = 2
# count_occurrences 0 [1;2;3;4;5;6] ;;
- : int = 0
*)

let count_occurrences_tail elm l =
   let rec inner_count_occurrences start = function
      | [] -> start
      | x :: xs -> inner_count_occurrences (start + if x = elm then 1 else 0) xs 
   in inner_count_occurrences 0 l
;;

(*
  1b. Write a function to reverse the first n elements of a list. If n is larger that the 
	    number of elements in the list, the entire list should be reversed. 
			You can assume that n >= 0.
*)

let reverse_n n lst = 
   let rec delay_reverse_n apl lst = function
      | 0 -> apl lst
      | n ->
         match lst with
         | [] -> apl []
         | x :: xs -> delay_reverse_n (fun cont_lst -> x :: apl cont_lst) xs (n-1)
   in delay_reverse_n (fun cont_lst -> cont_lst) lst n
;; 

(*
# reverse_n 3 [1;2;3;4;5;6] ;;
- : int list = [3;2;1;4;5;6]
# reverse_n 3 [1;2;3] ;;
- : int list = [3;2;1]
# reverse_n 3 [1;2;3;4] ;;
- : int list = [3;2;1;4]
# reverse_n 3 [1;2] ;;
- : int list = [2;1]
*)

(* 1c. Sometimes we wish to pick out certain elements in a list that meet a specified
			 condition from the rest, such as getting natural numbers from a list of integers.
			 We can write a general function that will split the list in such a way.

			 Note that the order in the resulting list does not matter; as long as you have the
			 right elements in the respective partition, the function will be considered as correct.
*)

let rec partition l cond_f = 
   match l with
   | [] -> ([],[])
   | x :: xs -> 
      let cont_accept, cont_reject = partition xs cond_f
      in if cond_f x
         then (x :: cont_accept,      cont_reject)
         else (     cont_accept, x :: cont_reject)
;;

(*
# partition [-5;-4;-3;-2;-1;0;1;2;3;4;5] (fun n -> n > 0) ;;
- : int list * int list = ([1;2;3;4;5], [-5;-4;-3;-2;-1;0]) 
# partition ["My"; "Name"; "Is"; "Jean"; "Valjean"] (fun s -> String.equal s "Javert") ;;
- : string list * string list = ([], ["My"; "Name"; "Is"; "Jean"; "Valjean"])
*)

let partition_tail l cond_f = 
   let rec inner_partition cont_accept cont_reject = function
      | [] -> (cont_accept, cont_reject)
      | x ::xs ->
         if cond_f x
            then inner_partition (x :: cont_accept)       cont_reject  xs
            else inner_partition       cont_accept  (x :: cont_reject) xs
   in inner_partition [] [] l
;;

(* 
	1d. Now let's try to write a Python-style list operation in Ocaml! List slicing 
			is a common and useful technique: the user will specify where the slice
			starts, where it ends, and the step they wish to take. For simplicity's sake,
			we will assume that we are only working with non-negative indices and positive step.
			
			Notes on corner cases:
			- If fin > length of the list, assume the operation will cover the entire list.
			- If end < init, return [].

*)

let rec slice l init fin jump = 
   match l with
   | [] -> []
   | x :: xs ->
      if fin <= 0 then []
      else let cont_l = slice xs (init-1) (fin-1) jump
      in if init > 0 || init mod jump <> 0
         then cont_l
         else x :: cont_l
;;

(*
# slice [0;1;2;3;4;5;6;7;8;9] 1 9 2 ;;
- : int list = [1;3;5;7]
# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 0 5 3 ;;
- : string list = ["a";"d"]
*)

let slice_tail l init fin jump =
   let rec inner_slice apl init fin = function
      | [] -> apl []
      | x :: xs ->
         if fin <= 0 then apl []
         else let cont_apl =
            if init > 0 || init mod jump <> 0
               then (fun cont_l -> apl cont_l)
               else (fun cont_l -> apl (x :: cont_l))
            in inner_slice cont_apl (init-1) (fin-1) xs
            
   in inner_slice (fun cont_l -> cont_l) init fin l
;;

(*
  1e. Many programming languages support the idea of a list comprehension - a mechanism
      to construct new lists from existing lists. The syntax is usually based on
      the mathematical set builder notation.

      For example the python expression lst = [ x*x for x in range(10) if x % 2 = 0 ]
      creates a list whose values are squares of even integers between 0 and 10 (exclusive).

      This construct is very functional in nature; so let us define a function - list_comprehension
      to help us with this. It takes 3 parameters - a source list of values (source), a predicate to
      filter values (pred) and a computation function (compute) to create a new value. The output is
      the list of values produced by the compute function for those source values that satisfy the
      predicate. The order of the items in the output list is based on the order in the source list.

      E.g. list_comprehension [0;1;2;3;4;5;6;7;8;9] (fun x -> (x mod 2) = 0) (fun x -> x * x) produces
      the list [0, 4, 16, 36, 64] similar to the python expression above.

*)

let rec list_comprehension source pred compute = 
   match source with
   | [] -> []
   | x :: xs ->
      let cont_result = list_comprehension xs pred compute
      in if pred x
         then compute x :: cont_result
         else cont_result
;; 

(*
# let rec range n = match n with 1 -> [0] | x -> (range (n-1)) @ [x-1] ;;
val range : int -> int list = <fun>
# list_comprehension (range 101) (fun x -> (x mod 5) = 0) (fun x -> x / 5) ;;
- : int list =
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20]
*)

let list_comprehension_tail source pred compute =
   let rec inner_list_comprehension apl = function
      | [] -> apl []
      | x :: xs -> let cont_apl =
         if pred x
            then (fun cont_result -> apl (compute x :: cont_result))
            else (fun cont_result -> apl cont_result)
         in inner_list_comprehension cont_apl xs
   in inner_list_comprehension (fun cont_result -> cont_result) source
;;

(* ************** Section Two: Kakurasu verifier ************** *)

(* Kakurasu is a Japanese logic puzzle. The goal of this question is
   to write a function that when presented with a kakurasu grid,
   returns whether it's been correctly solved. In case you're not
   familiar with the game, the rules and some examples can be found here:
   https://puzzlemadness.co.uk/kakurasu/medium#rules

   To verify a grid, we have to check that each row/column's filled out
   squares have combined value that is equal to the desired amount.

   For example, take a look at the second row in the example below:
   since the fourth, fifth, and eighth squares are marked as 1,
   we have 4 + 5 + 8 = 17, and it matches the expected row value on
   the right.

   Similarly, column one has the first, fourth, sixth, and seventh
   squares filled out, giving us 1 + 4 + 6 + 7 = 18.

      1  2  3  4  5  6  7  8
   1  ■  -  -  -  -  -  -  - -- 1
   2  -  -  -  ■  ■  -  -  ■ -- 17
   3  -  ■  ■  -  -  ■  -  - -- 11
   4  ■  -  ■  ■  ■  ■  ■  ■ -- 34
   5  -  -  ■  -  -  -  ■  ■ -- 18
   6  ■  -  -  -  -  ■  ■  ■ -- 22
   7  ■  -  -  -  -  -  ■  ■ -- 16
   8  -  -  -  ■  ■  ■  -  ■ -- 23
      |  |  |  |  |  |  |  |
     18  3 12 14 14 21 22 32

   A simple representation of an n x n Kakurasu grid is as a list of length n,
   each of the n element is itself a length-n list. We can use 0 to represent
   unfilled squares, and 1 for filled ones.

   e.g.: The above example will be represented as the list
   [[1; 0; 0; 0; 0; 0; 0; 0]; [0; 0; 0; 1; 1; 0; 0; 1];
    [0; 1; 1; 0; 0; 1; 0; 0]; [1; 0; 1; 1; 1; 1; 1; 1];
    [0; 0; 1; 0; 0; 0; 1; 1]; [1; 0; 0; 0; 0; 1; 1; 1];
    [1; 0; 0; 0; 0; 0; 1; 1]; [0; 0; 0; 1; 1; 1; 0; 1]]

   The function itself will take in four arguments: the dimension (represented by
   a single integer n for an n x n grid), the Kakurasu grid, a list containing
   desired sums for each row, and a list containing desired sums for each column.
*)

(* 2a. Since we will need to query a specific row from the desired sums list,
       getting a row value by index will come in handy. Note we will use 1-based
       indexing for convenience.
 *)

let rec nth lst n = 
   match lst with
   | [] -> failwith "nth row does not exist in provided lst!"
   | x :: xs ->
      match n with
      | 1 -> x
      | n -> nth xs (n-1)
;;

(*
# nth [1;0;0;0;0;0;0;0] 1 ;;
- : int = 1
# nth [1;0;0;0;0;0;0;0] 5 ;;
- : int = 0
*)

let fetch_row : 'a list list -> int -> 'a list = nth


(* 2b. The list representation makes it easy to access each row, but it can be
       tricky to get the columns. For this question, write a function that when
       given a column index (1-based) and a grid, extracts that specified column
			 as an integer list.
 *)

let rec fetch_column grid col = 
   match grid with
   | [] -> []
   | row :: rows
   -> nth row col :: fetch_column rows col
;;

let fetch_column_tail grid col = 
   let rec inner_fetch_column apl = function
   | [] -> apl []
   | row :: rows -> 
      let cur_col_val = nth row col
      in inner_fetch_column (fun cont_col -> apl (cur_col_val :: cont_col)) rows
   in inner_fetch_column (fun cont_col -> cont_col) grid

(*
# let test_grid = 
	 [[1; 0; 0; 0; 0; 0; 0; 0]; [0; 0; 0; 1; 1; 0; 0; 1];
    [0; 1; 1; 0; 0; 1; 0; 0]; [1; 0; 1; 1; 1; 1; 1; 1];
    [0; 0; 1; 0; 0; 0; 1; 1]; [1; 0; 0; 0; 0; 1; 1; 1];
    [1; 0; 0; 0; 0; 0; 1; 1]; [0; 0; 0; 1; 1; 1; 0; 1]]
	;;
# fetch_column test_grid 1 ;;
- : int list = [1; 0; 0; 1; 0; 1; 1; 0]
# fetch_column test_grid 7 ;;
- : int list = [0; 0; 0; 1; 1; 1; 1; 0]
*)

(* 2c. Write a function that, given a list and a desired sum, verifies that
       the combined value is equal to the sum.
			 e.g. verify_list [0; 0; 0; 1; 1; 1; 0; 1] 23 = true;;
			 			verify_list [1; 0; 0; 0; 1; 0; 0; 0] 19 = false;;
*)

let verify_list lst sum = 
   let rec val_list cur_val = function
      | [] -> 0
      | x :: xs ->
         val_list (cur_val+1) xs
         + if x = 1 then cur_val else 0
   in sum = val_list 1 lst
;;

(*
# verify_list [0; 0; 0; 1; 1; 1; 0; 1] 23 ;;
- : bool = true
# verify_list [1; 0; 0; 0; 1; 0; 0; 0] 19 ;;
- : bool = false
*)

let verify_list_tail lst sum = 
   let rec val_list_tail cur_val start = function
      | [] -> sum = start
      | x :: xs ->
         let x_val = if x = 1
            then cur_val
            else 0
         in val_list_tail (cur_val+1) (start+x_val) xs
   in val_list_tail 1 0 lst

let verify_list_tail_2 lst sum = 
   let rec verify_val_list cur_val sum = function
      | [] -> sum = 0
      | x :: xs ->
         let x_val = if x = 1
            then cur_val
            else 0
         in verify_val_list (cur_val+1) (sum-x_val) xs
   in verify_val_list 1 sum lst



(* generator stuff *)

type 'a gen     = (unit -> 'a gen_res)
and  'a gen_res = None | Some of 'a * 'a gen

let lst_gen lst : 'a gen = 
   let rec gen_next = function
      | [] -> None
      | x :: xs -> Some (x, (fun () -> gen_next xs))
   in (fun () -> gen_next lst)
;;

let rec gen_lst gen =
   match gen () with
   | None -> []
   | Some (x, next_gen) -> x :: gen_lst next_gen
;;

let map_gen (gen : 'a gen) f : 'b gen =
   let rec gen_next gen =
      match gen () with
      | None -> None
      | Some (x, next_gen) -> Some (f x, (fun () -> gen_next next_gen))
   in (fun () -> gen_next gen)
;;

let filter_gen (gen : 'a gen) f : 'a gen =
   let rec gen_next gen =
      match gen () with
      | None -> None
      | Some (x, next_gen) ->
         if f x
            then Some (x, (fun () -> gen_next next_gen))
            else gen_next next_gen
   in (fun () -> gen_next gen)
;;

let gen_comprehension (gen : 'a gen) pred compute : 'b gen =
   let rec gen_next gen = 
      match gen () with
      | None -> None
      | Some (x, next_gen) -> 
         if pred x
            then Some (compute x, (fun () -> gen_next next_gen))
            else gen_next next_gen
   in (fun () -> gen_next gen)
;;

let rec foldl_gen gen start f =
   match gen () with
   | None -> start
   | Some (x, next_gen) -> foldl_gen next_gen (f start x) f
;;

let rec foldr_gen gen start f =
   match gen () with
   | None -> start
   | Some (x, next_gen) -> f x (foldr_gen next_gen start f)
;;

type ('a, 'b) worker = End of 'a | Cont of 'b;;

let rec foldl_early_gen gen start f =
   match gen () with
   | None -> start
   | Some (x, next_gen) -> match f start x with
      | End final -> final
      | Cont cont_start -> foldl_early_gen next_gen cont_start f
;;

let rec foldr_early_gen gen start f =
   match gen () with
   | None -> start
   | Some (x, next_gen) -> match f x with
      | End final -> final
      | Cont combine -> combine (foldr_early_gen next_gen start f)
;;

let flatten_gen (gen_gen : 'a gen gen) : 'a gen =
   let rec gen_next gen_gen =
      match gen_gen () with
      | None -> None
      | Some (gen, next_gen_gen) ->
         let rec inner_gen_next gen =
            match gen () with
            | None -> gen_next next_gen_gen
            | Some (x, next_gen) ->
               Some (x, (fun () -> inner_gen_next next_gen))
         in inner_gen_next gen
   in (fun () -> gen_next gen_gen)
;;

let map_eager_gen (gen : 'a gen) f : 'b gen =
   let rec inner_map_eager_gen apl gen =
      match gen () with
      | None -> apl (fun () -> None)
      | Some (x, next_gen) -> let new_x = f x in
         inner_map_eager_gen (fun new_gen -> apl (fun () -> Some (new_x, new_gen))) next_gen
   in inner_map_eager_gen (fun new_gen -> new_gen) gen
;;

let zip_2_gen (g1 : 'a gen) (g2 : 'b gen) : ('a * 'b) gen =
   let rec gen_next g1 g2 =
      match (g1 (), g2 ()) with
      | (Some (x1, next_g1), Some (x2, next_g2)) ->
         Some ((x1, x2), (fun () -> gen_next next_g1 next_g2))
      | _ -> None
   in (fun () -> gen_next g1 g2)
;;

let zip_2_opt_gen (g1 : 'a gen) (g2 : 'b gen) : ('a option * 'b option) gen =
   (* let opt_gen_next : 'z. 'z gen = (fun () -> None)
   in *)
   let rec gen_next (g1 : 'a gen) (g2 : 'b gen) =
      let (x1, next_g1) = match g1 () with
      | (Some (x1, next_g1)) -> Option.Some x1, next_g1
      | None -> Option.None, g1 (* opt_gen_next *)
      in
      let (x2, next_g2) = match g2 () with
      | (Some (x2, next_g2)) -> Option.Some x2, next_g2
      | None -> Option.None, g2 (* opt_gen_next *)
      in
      match x1, x2 with
      | None, None -> None
      | _ -> Some ((x1, x2), (fun () -> gen_next next_g1 next_g2))
   in (fun () -> gen_next g1 g2)

let zip_2_def_gen (g1 : 'a gen) (def1 : 'a) (g2 : 'b gen) (def2 : 'b) : ('a * 'b) gen =
   (* let rec def_1_gen_next () : 'a gen_res = Some (def1, def_1_gen_next) in
      let rec def_2_gen_next () : 'b gen_res = Some (def2, def_2_gen_next) in 
   
      *)
   let rec gen_next (g1 : 'a gen) (g2 : 'b gen) =
      let (x1, next_g1) = match g1 () with
      | (Some (x1, next_g1)) -> Option.Some x1, next_g1
      | None -> Option.None, g1
      in
      let (x2, next_g2) = match g2 () with
      | (Some (x2, next_g2)) -> Option.Some x2, next_g2
      | None -> Option.None, g2
      in
      match x1, x2 with
      | None, None -> None
      | _ -> 
         let x1 = Option.value x1 ~default:def1 in
         let x2 = Option.value x2 ~default:def2 in
         
         Some ((x1, x2), (fun () -> gen_next next_g1 next_g2))
   in (fun () -> gen_next g1 g2)

let zip_n_gen (gen_gen : 'a gen gen) : 'a gen gen =
   let rec gen_next gen_gen =
      let rec gen_el apl gen_gen =
         match gen_gen () with
         | None -> apl ((fun () -> None), (fun () -> None))
         | Some (gen, cont_gen_gen) -> match gen () with
            | None -> None
            | Some (x, cont_gen) -> gen_el (fun (new_gen, new_gen_gen) -> apl ((fun () -> Some (x, new_gen)), (fun () -> Some (cont_gen, new_gen_gen))) ) cont_gen_gen
      in gen_el (fun (new_gen, new_gen_gen) -> Some (new_gen, fun () -> gen_next new_gen_gen)) gen_gen
   in (fun () -> gen_next gen_gen)
;;

let concat_gen (g1 : 'a gen) (g2 : 'a gen) : 'a gen =
   let rec gen_next g1 =
      match g1 () with
      | None -> g2 ()
      | Some (x, next_gen) -> Some (x, fun () -> gen_next next_gen)
   in (fun () -> gen_next g1)
;;

let limit_gen (gen : 'a gen) limit : 'a gen =
   let rec gen_next gen = function
      | 0 -> None
      | limit -> match gen () with
         | None -> None
         | Some (x, next_gen) -> Some (x, fun () -> gen_next next_gen (limit-1))
   in (fun () -> gen_next gen limit)
;;

type 'a infness = Inf | Finite of 'a

let rge_gen start step stop : 'a gen = 
   match stop with
   | Inf ->
      let rec gen_next start =
         Some (start, (fun () -> gen_next (start+step)))
      in (fun () -> gen_next start)
   | Finite stop ->
      let rec gen_next start =
         if start >= stop
            then None
            else Some (start, (fun () -> gen_next (start+step)))
      in (fun () -> gen_next start)
;;

let verify_gen (gen, sum) = 
   0 = foldl_gen
      ( map_gen
         (zip_2_gen gen (rge_gen 1 1 Inf))
         (fun (x, cur_val) ->
            if x = 1 then cur_val else 0
         )
      )
      sum
      (fun start x -> start-x)
;;


let all_gen gen = foldr_early_gen
   gen
   true
   (function
      | false -> End false
      | true -> Cont (fun cont -> cont)
   )
;;

let any_gen gen = foldr_early_gen
   gen
   false
   (function
      | true -> End true
      | false -> Cont (fun cont -> cont)
   )
;;

(* 2d. Now we can put it all together and verify the entire grid! *)

let verify_solution dimention grid row_vals col_vals =
   let rec verify_vals fetcher vals = function
   | -1 -> true
   | inv_val_num -> match vals with
      | [] -> failwith "number of vals should match dimension in grid!"
      | cur_val :: next_vals ->
         if verify_list (fetcher grid (dimention-inv_val_num)) cur_val
            then verify_vals fetcher next_vals (inv_val_num-1)
            else false
   in let dim_1 = dimention-1 in verify_vals fetch_row row_vals dim_1 && verify_vals fetch_column col_vals dim_1
;;

(* Explicitly catches when vals list is longer than grid *)
let verify_solution_2 dimention grid row_vals col_vals =
   let rec verify_vals fetcher vals inv_val_num = match vals, inv_val_num with
   | [], -1 -> true
   | cur_val :: next_vals, inv_val_num when inv_val_num > -1 ->
      if verify_list (fetcher grid (dimention-inv_val_num)) cur_val
         then verify_vals fetcher next_vals (inv_val_num-1)
         else false
   | _ -> failwith "number of vals should match dimension in grid!"
   in let dim_1 = dimention-1 in verify_vals fetch_row row_vals dim_1 && verify_vals fetch_column col_vals dim_1
;;
      
(* calling fetch_row and fetch_col in the above manner is somewhat inefficient; they are O(n) and O(n^2) operations respectively
   in the number of rows / cols; therefore the total run-time is cubic in the number of rows / cols *)

(* the below solution with lazy generators / streams is quadratic in the number of rows / cols; taking a dimension parameter is
   also then unnecessary. Of course, it comes with the bloat of deeply nested function calls, as is typical with lazy computation *)

(* There is also no length check...so if lengths don't match up it silently succeeds. But we can add it with 2 more O(n) operations *)

let verify_solution_gen grid row_vals col_vals = 
   let row_gen_gen = map_eager_gen (lst_gen grid) (fun row -> lst_gen row) (* O(n), creates an O(1) generator of n length of O(1) generators of n length *)
   in let col_gen_gen = zip_n_gen row_gen_gen (* O(1), creates an O(n) generator of n length of O(1) generators of n length *)
   in let concat_val_zipped_gen_gen = concat_gen (zip_2_gen row_gen_gen (lst_gen row_vals)) (zip_2_gen col_gen_gen (lst_gen col_vals)) (* O(1), creates an O(n) generator of 2n length of O(1) generators of n length *)
   in all_gen (map_gen concat_val_zipped_gen_gen verify_gen) (* O(n^2), do O(n) work, 2n = O(n) times, where work is getting value from O(n) generator, and O(n) verifying, where verifying is O(1) work, n = O(n) times *)
;;

(*
# verify_solution 8 test_grid [1;17;11;34;18;22;16;23] [18;3;12;14;14;21;22;32];;
- : bool = true
*)
