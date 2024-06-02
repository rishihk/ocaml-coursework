(* CS342 - Spring 2024
 * MP3
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(*Problem 1*)
let rec product l = match l with | [] -> 1.0
                    |(x::xs) -> x *. product xs;;

(*Problem 2*)
let rec double_all l = match l with | [] -> []
                       |(x::xs) -> (x *. 2.0)::double_all xs;;

(*Problem 3*)
let rec pair_with_all x l = match l with [] -> []
                            |(y::ys) -> (x, y)::pair_with_all x ys;;

(*Problem 4*)
let rec interleave l1 l2 = match l1, l2 with | [], [] -> []
                                             | [], (y::ys) -> y::interleave ys []
                                             | (x::xs), [] -> x::interleave xs []
                                             | (x::xs), (y::ys) -> x::y::interleave xs ys;;

(*Problem 5*)
let rec even_count_fr l = match l with | [] -> 0
                                       | (x::xs) -> let res = even_count_fr xs in
                                                      if x mod 2 = 0
                                                        then res + 1
                                                    else 
                                                      res;;

(*Problem 6*)
let rec pair_sums l = match l with | [] -> []
                                   | (x::xs) -> (match x with | (a,b) -> (a+b):: pair_sums xs);;
(*Problem 7*)
let rec remove_even list = match list with | [] -> []
                                      | (x::xs) -> let l = remove_even xs in
                                                    if x mod 2 = 0
                                                      then l
                                                  else x::l;;                      

(*Problem 8*)
let rec sift p l = match l with
  | [] -> ([], [])
  | (x::xs) -> let (lt, lf) = sift p xs in
                   if p x then 
                    (x::lt, lf)
                  else
                    (lt, x::lf);;


(*Problem 8 another way *)
let rec sift1 p l = match l with
  | [] -> ([], [])
  | (x :: xs) -> 
      if p x then 
        (x :: fst (sift1 p xs), snd (sift1 p xs))
      else 
        (fst (sift1 p xs), x :: snd (sift1 p xs));;

(*Problem 8 another way*)
let rec sift2 p l = match l with
  | [] -> ([], [])
  | (x::xs) -> 
    if p x then
      match sift2 p xs with 
      | (lt, lf) -> (x::lt, lf)
    else 
      match sift2 p xs with
      | (lt, lf) -> (lt, x::lf);;

  
(*Problem 9*)
let rec even_count_tr l = 
  let rec aux_even_count_tr l acc = 
    match l with | [] -> acc
                 | (x::xs) -> 
                    aux_even_count_tr xs (if x mod 2 = 0 then (1 + acc) else acc)
  in aux_even_count_tr l 0;;

(*Problem 10*)
let rec count_element l m = 
  let rec aux_count_element l m acc =
    match l with 
    | [] -> acc
    | (x::xs) -> aux_count_element xs m (if x = m then (1+acc) else acc)
  in aux_count_element l m 0;;
    

(*Problem 11*)
let rec all_nonneg list =
  let rec aux_all_nonneg list acc = 
    match list with 
    | [] -> acc
    | (x::xs) -> aux_all_nonneg xs (if x<0 then false else acc)
  in aux_all_nonneg list true;;

(*Problem 12*)
let rec split_sum l f = 
  let rec aux_split_sum l f acc = 
    match l with 
    | [] -> acc
    | (x::xs) -> if f x then aux_split_sum xs f ((fst acc + x), snd acc)
    else aux_split_sum xs f (fst acc, (snd acc + x))
  in aux_split_sum l f (0, 0);;



(*Problem 13*)
let even_count_fr_base = 101
let even_count_fr_rec x rec_val = raise(Failure "Function not implemented yet.")
  
(*Problem 14*)
let pair_sums_map_arg p = raise(Failure "Function not implemented yet.")

(*Problem 15*)
let remove_even_base = [101]
let remove_even_rec n r = raise(Failure "Function not implemented yet.")

(*Problem 16*)
let even_count_tr_start = (101)
let even_count_tr_step acc_val x = raise(Failure "Function not implemented yet.")

(*Problem 17*)
let split_sum_start = ((101),(101))
let split_sum_step f = raise(Failure "Function not implemented yet.")
