(* File: mp2.ml ocamlformat file.ml *)

(* Helper for Problem 1 *)
let distance (x1, y1) = (x1 *. x1) +. (y1 *. y1);;

(* Problem 1 *)
let closer_to_origin p1 p2 = match (p1, p2) with | ((x1, y1), (x2, y2)) -> if distance p1 < distance p2 then -1 else if distance p1 > distance p2 then 1 else 0;;

(* Problem 2 *)
let swap_eq p1 p2 = match p1, p2 with (x1, x2) , (y1, y2) -> x1 = y2 && x2 = y1;;

(* Problem 3 *)
let twist pp = match pp with ((x1, x2), (y1, y2)) -> ((y2, x1), (y1, x2));;

(* Problem 4 *)
let triple_pairs x trp = match trp with (x1, x2, x3) -> ((x, x1), (x, x2), (x, x3));;

(* Problem 5 *)
let triple_xprod trp pr = match trp, pr with (x1, x2, x3), (y1, y2) -> (((x1, y1), (x2, y1), (x3, y1)), ((x1, y2), (x2, y2), (x3, y2)));;

(*  Problem 6 *)
let two_funs fns ins = match fns, ins with (f, g), (x, y) -> (f x, g y);;

(*  Problem 7 *)
let triple_app (f,g,h) x = f(g(h(x)));;

(*  Problem 8 *)
let same_arg_twice f x = (f x) x;;

(*  Problem 9 *)
let rev_app x f = f x;;

(*  Problem 10 *)
let map_triple f (a,b,c) = (f a, f b, f c);;

(* Problem 11 *)
let rec ackermann m n = match m, n with 0, n -> n + 1 | m, 0 when m > 0 -> ackermann (m-1) 1 | m, n -> ackermann (m-1) (ackermann m (n-1));;

(* Other ways to do Problem 11 *)
let rec ackermann1 m n = if m = 0 then n + 1 else if m > 0 && n = 0 then ackermann1 (m-1) 1 else ackermann1 (m-1) (ackermann1 m (n-1));;

let rec ackermann2 m n = match m, n with 0, _ -> n + 1 | m , 0 when m > 0 -> ackermann2 (m-1) 1 | _, _ -> ackermann2 (m-1) (ackermann2 m (n-1));;

(* Problem 12 *)
let rec collatz n = match n with 1 -> 0 | _ -> if n mod 2 = 0 then 1 + collatz (n/2) else 1 + collatz (3*n+1);;

(* Problem 13 *)
let rec delannoy (m, n) = match (m, n) with (0, 0) -> 1 | (0, _) -> 1 | (_, 0) -> 1 | (_, _) -> delannoy(m-1, n) + delannoy(m, n-1) + delannoy(m-1, n-1);;

(* Problem 14 *)
let rec naive_fibonacci n = match n with 0 -> 1 | 1 -> 1 | _ -> naive_fibonacci (n-1) + naive_fibonacci (n-2);;

(* Problem 15 *)
let rec sum_evens_less_eq n = match n with 0 -> 0 | _ -> if n mod 2 = 0 then n + sum_evens_less_eq (n-1) else if n < 0 then 0 else sum_evens_less_eq (n-1);;

(* another way to do problem 15 *)
let sum_evens_less_eq1 n = n/2 * (n/2 + 1);;


