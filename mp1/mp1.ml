(* CS242 - Spring 2024
 * MP1 - Hrishikesha H Kyathsandra
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Common

(* 1 *)
let title = "MP 1 -- Basic OCaml";; 

(* 2 *)
let greetings = "Hi there.";; 

(* 3 *)
let address = "Greetings, my friend!";; 

(* 4 *)
let frozen = "Do you want to build a snowman?";; 

(* 5 *)
let daffy = "Th, th, that's all, Folks!";; 

(* 6 *)
let a = 17.5;; 

(* 7 *)
let pi = 3.14159;; 

(* 8 *)
let e = 2.71828;; 

(* 9 *)
let quarter = 0.25;; 

(* 10 *)
let x = 32.7;; 

(* 11 *)
let myFirstFun n = (n + 3) * 4;; 

(* 12 *)
let firstFun n = (n * 2) + 5;;

(* 13 *)
let square n = n * n;;

(* 14 *)
let times_13 n = n * 13;;

(* 15 *)
let cube n = n * n * n;;

(* 16 *)
let add_a n = a +. n;;

(* 17 *)
let circumference r = 2.0 *. pi *. r;;

(* 18 *)
let divide_e_by x = e /. x;;

(* 19 *)
let plus_quarter_times_3 y = (quarter +. y) *. 3.0;;

(* 20 *)
let square_plus_x y = x +. (y *. y);;

(* 21 *)
let salutations name = if name = "Elsa" then print_string "Halt! Who goes there!\n" else print_string("Hail, "^name^". We warmly welcome you!\n");;

(* 22 *)
let hail name = if name = "Elsa" then print_string("Wayell, hah theya, Ayelsa!") else print_string("Dear, " ^name^ ". I wish you the best in CS342.\n");;

(* 23 *)
let welcome name = if name = "Elsa" then print_string("Can you come out to play?\n") else print_string("Aw, come on, " ^name^ ". We're going to have a wonderful time!\n");;

(* 24 *)
let greet name = if name = "Elsa" then print_string("Hey Elsa, cool man!") else print_string("Hello, " ^name^ ". I hope you enjoy CS342.\n");;

(* 25 *)
let salute name = if name = "Elsa" then print_string("What's the low-down, man?") else print_string("Hey, " ^name^ "! Give me five, man.");;

(* 26 *)
let rectangle_area l w = if l >= 0.0 && w >=0.0 then l *. w else -1.0;;

(* 27 *)
let diff_square_9 m n = if m < n then (n *. n) -. 9.0 else if m /. 2.0 > n then (m *. m) -. 9.0 else ((m -. n) *. (m -. n)) -. 9.0;;

(* 28 *)
let make_bigger x y = if x > 0.0 then x +. y else if y < 1.0 then y +. 1.0 else y *. y;;

(* 29 *)
let has_smallest_square m n = if m * m < n * n then m else if n * n < m * m then n else if m < n then m else n;;

(* 30 *)
let sign_times n m = if n * m > 0 then 1 else if n * m = 0 then 0 else -1;;

