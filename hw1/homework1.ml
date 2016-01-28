

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

(* is_coprime *)
let is_coprime (a,b) = 
   if gcd(a,b) = 1
      then true
   else false

(* euler *)
let rec euler_helper (n, count, result) = 
   if count = 0
      then result
   else 
      if is_coprime(n, count) = true then euler_helper(n, count-1,result+1) else euler_helper(n, count-1,result)



let euler (n) = euler_helper(n,n,0)

(* coprimes *)

let rec coprimes_helper (n, count,list_empty) = 
   if count = 0
      then list_empty
   else
      if is_coprime(n,count) = true then coprimes_helper(n,count-1,count :: list_empty) else coprimes_helper(n,count-1,list_empty)

let coprimes (n) = 
   coprimes_helper(n,n,[])



(* Question 2 *)

(* Append *)
let rec append (xs,ys) =
   match xs with
       [] -> ys
      | first::rest -> first::append(rest,ys)

(* Flatten *)
let rec flatten_helper (xss, new_list) = 
   match xss with [] -> new_list
      | first::rest -> flatten_helper(rest,append(first,new_list))

let flatten (xss) =
   flatten_helper (reverse(xss,[]), [])

(* nth *)
let rec nth (n,xs) = 
   match xs with [] -> failwith "out of bounds"
      | first::rest -> if n = 0 then first else nth(n-1,rest)

(* last *)
let rec last (xs) = 
   match xs with [] -> failwith "empty list"
      | x::[] -> x
      | first::rest -> last(rest)

(* separate *)
let rec reverse (list2,new_list) =
   match list2 with [] -> new_list
      | first1::rest1 -> reverse(rest1,first1::new_list)

let get_first_of_tuple ((xs,ys)) = xs

let get_second_of_tupe ((xs,ys)) = ys

let rec separate_helper (xs,first_list,second_list) = 
   match xs with [] -> (first_list,second_list)
      | first::rest -> separate_helper(rest,get_first_of_tuple(first)::first_list,get_second_of_tupe(first)::second_list)

let separate (xs) =
   separate_helper(reverse(xs,[]),[],[])


(* Question 3 *)

(* setIn *)
let rec setIn (e,xs) = 
   match xs with [] -> false
      | first::rest -> if e = first then true else setIn(e,rest)

(* setSub *)
let rec setSub (xs,ys) = 
   match xs with [] -> true
      | first::rest -> if setIn(first,ys) = true then setSub(rest,ys) else false

(* setEqual *)
let setEqual (xs,ys) = 
   if setSub(xs,ys) && setSub(ys,xs) then true else false

(* setUnion *)
let rec setUnion (xs,ys) = 
   match xs with [] -> ys
      | first::rest -> if setIn(first,ys) then setUnion(rest,ys) else setUnion(rest,append(ys,first::[]))

(* setInter *)
let rec setInter_helper (xs,ys,new_set) = 
   match xs with [] -> new_set
      | first::rest -> if setIn(first,ys) then setInter_helper(rest,ys,first::new_set) else setInter_helper(rest,ys,new_set)

let setInter (xs,ys) =
   setInter_helper(xs,ys,[])

(* setSize *)
let rec setSize_helper (xs,new_set,length) = 
   match xs with [] -> length 
      | first::rest -> if setIn(first, new_set) then setSize_helper(rest,new_set,length) else setSize_helper(rest,first::new_set,length+1)

let setSize (xs) =
   setSize_helper(xs,[],0)