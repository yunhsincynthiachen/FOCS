

(*HOMEWORK 1

Name: Cynthia Chen (Yun-Hsin)

Email: yun-hsin.chen@students.olin.edu

Remarks, if any:

Nitya Dhanushkodi and I talked through how to do problems that we were having trouble with, like append and separate 
through discussing our ideas of how we should approach the problems. We helped each other when we got stuck, 
but we wrote code completely separately.

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)



(* Question 1 *)

(* gcd: Code a function gcd of type int * int -> int which takes two integers and returns the 
greatest common divisor of those integers.*)
(* Recursively using modulus, so you don't have to worry about whether a is greater than b.
Thought process: 
   gcd (2,4): 4 mod 2 is 0, so gcd (0,2): 2 is the gcd.
   gcd (4,2): 2 mod 4 is 2, so we need to find gcd (2,4): 4 mod 2 is 0, so gcd (0,2): 2 is gcd
   gcd (4,10): 10 mod 4 is 2, so we need to find gcd (2,4): 4 mod 2 is 0, so gcd (0,2): 2 is gcd  
   gcd (10,4): 4 mod 10 is 4, so we need to find gcd (4,10), which we know from above is 2
*)
let rec gcd (a,b) = 
   if a = 0
      then b
   else gcd(b mod a, a)

(* is_coprime: Two integers are coprime if they have only the trivial divisor in common — that is, if their greatest common divisor is 1.
Code a function is_coprime of type int * int -> bool which returns true if the two integers are coprime, and false otherwise.*)
let is_coprime (a,b) = 
   if gcd(a,b) = 1
      then true
   else false

(* euler: The Euler φ function is defined by taking φ(n) to be the number of integers 1≤ x ≤ n such that x and n are coprime.
Code a function euler of type int -> int which computes φ(n) of its input n.*)
let rec euler_helper (n, count, result) = 
   if count = 0
      then result
   else 
      if is_coprime(n, count) = true then euler_helper(n, count-1,result+1) else euler_helper(n, count-1,result)



let euler (n) = euler_helper(n,n,0)

(* coprimes: Code a function coprimes of type int -> int list which returns the list of all integers 1≤x≤n 
such that x and n are coprime. *)
let rec coprimes_helper (n, count,list_empty) = 
   if count = 0
      then list_empty
   else
      if is_coprime(n,count) = true then coprimes_helper(n,count-1,count :: list_empty) else coprimes_helper(n,count-1,list_empty)

let coprimes (n) = 
   coprimes_helper(n,n,[])



(* Question 2 *)

(* Append: Code a function append of type 'a list * 'a list -> 'a list which takes two 
lists and returns a new list consisting of the second list appended at the end of the first. *)
let rec append (xs,ys) =
   match xs with
       [] -> ys
      | first::rest -> first::append(rest,ys)

(* Flatten: Code a function flatten of type 'a list list -> 'a list which takes a list of lists 
and "flattens" it into a single list.

Didn't quite know how to do this without reversing original list of lists *)
let rec reverse (list2,new_list) =
   match list2 with [] -> new_list
      | first1::rest1 -> reverse(rest1,first1::new_list)

let rec flatten_helper (xss, new_list) = 
   match xss with [] -> new_list
      | first::rest -> flatten_helper(rest,append(first,new_list))

let flatten (xss) =
   flatten_helper (reverse(xss,[]), [])

(* nth: Code a function last of type 'a list -> 'a which returns the last element of a list.

If the list is empty, then there is no last element. Use built-in function failwith to return 
an error. (Function failwith takes a string as input, the error message to report.) *)
let rec nth (n,xs) = 
   match xs with [] -> failwith "out of bounds"
      | first::rest -> if n = 0 then first else nth(n-1,rest)

(* last: Code a function nth of type int * 'a list -> 'a where nth(n,xs) returns the element at 
position n in list xs, where 0 is the position of the first element.

If the position input n is out of bounds, use built-in function failwith to return an error. *)
let rec last (xs) = 
   match xs with [] -> failwith "empty list"
      | x::[] -> x
      | first::rest -> last(rest)

(* separate: Code a function separate of type ('a * 'b) list -> ('a list * 'b list) which takes a list 
   of pairs, and returns a pair of lists (L1,L2), where L1 is the list of all first components of the original pairs and L2 is the list of all second components of the original pairs. *)
let get_first_of_tuple ((xs,ys)) = xs

let get_second_of_tupe ((xs,ys)) = ys

let rec separate_helper (xs,first_list,second_list) = 
   match xs with [] -> (first_list,second_list)
      | first::rest -> separate_helper(rest,get_first_of_tuple(first)::first_list,get_second_of_tupe(first)::second_list)

let separate (xs) =
   separate_helper(reverse(xs,[]),[],[])


(* Question 3 *)

(* setIn: Code a function setIn of type 'a * 'a list -> bool 
   where setIn (a,S) returns true if element a is an element of set S, and false otherwise. *)
let rec setIn (e,xs) = 
   match xs with [] -> false
      | first::rest -> if e = first then true else setIn(e,rest)

(* setSub: Recall that a set S is a subset of T when every element of S is an element of T.

Code a function setSub of type 'a list * 'a list -> bool where setSub (S,T) returns true if 
S is a subset of T when S and T are interpreted as sets, and false otherwise. *)
let rec setSub (xs,ys) = 
   match xs with [] -> true
      | first::rest -> if setIn(first,ys) = true then setSub(rest,ys) else false

(* setEqual: Code a function setEqual of type 'a list * 'a list -> bool where setEqual (S,T) 
returns true if S and T are equal when interpreted as sets, and false otherwise. *)
let setEqual (xs,ys) = 
   if setSub(xs,ys) && setSub(ys,xs) then true else false

(* setUnion: Code a function setUnion of type 'a list * 'a list -> 'a list where setUnion (S,T) 
returns a list representing the union of S and T interpreted as sets. *)
let rec setUnion_helper (xs,ys) = 
   match xs with [] -> ys
      | first::rest -> if setIn(first,ys) then setUnion_helper(rest,ys) else setUnion_helper(rest,first::ys)

let rec setUnion(xs,ys) = 
   setUnion_helper(append(xs,ys),[])

(* setInter: Code a function setInter of type 'a list * 'a list -> 'a list where setInter (S,T) 
returns a list representing the intersection of S and T interpreted as sets. *)
let rec setInter_helper (xs,ys,new_set) = 
   match xs with [] -> new_set
      | first::rest -> if setIn(first,ys) then setInter_helper(rest,ys,first::new_set) else setInter_helper(rest,ys,new_set)

let setInter (xs,ys) =
   setInter_helper(xs,ys,[])

(* setSize: Code a function setSize of type 'a list -> int where setSize (S) returns the number of 
elements in S when interpreted as a set. *)
let rec setSize_helper (xs,new_set,length) = 
   match xs with [] -> length 
      | first::rest -> if setIn(first, new_set) then setSize_helper(rest,new_set,length) else setSize_helper(rest,first::new_set,length+1)

let setSize (xs) =
   setSize_helper(xs,[],0)