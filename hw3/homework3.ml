(* 

HOMEWORK 3

Name: Cynthia Chen

Email: yun-hsin.chen@students.olin.edu

Remarks, if any: 

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



(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = String.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (String.set str index c; loop(cs,index+1))
  in
    loop(cs,0)



(*
 *  The type of a finite automaton
 * 
 *  When the transition relation is a function
 *  (i.e., every p,a has q such that (p,a,q) is in 
 *  delta) then this is a deterministic finite automaton  
 * 
 *)

type 'a fa = { states: 'a list;
               alphabet: char list;
               delta: ('a * char * 'a) list;
               start : 'a;
               accepting : 'a list }




(* QUESTION 1 *)
let rec findTransitions_helper (fa,q,a) =
  match fa with
    |  [] -> []
    | (b,c,d)::rest ->  if b = q && c = a then (b,c,d)::findTransitions_helper(rest,q,a) else findTransitions_helper(rest,q,a)

let findTransitions (fa,q,a) = 
  findTransitions_helper(fa.delta,q,a)

let rec isAccepting_helper (fa,s) = 
  match fa with
    | [] -> false
    | first::[] -> if first = s then true else false
    | first2::rest -> if first2 = s then true else isAccepting_helper(rest,s)

let isAccepting (fa,s) = 
  isAccepting_helper(fa.accepting, s)

let step (fa,q,a) = 
  match findTransitions(fa,q,a) with
    | [] -> failwith "none"
    | (b,c,d)::rest -> d


let rec steps (fa,q,syms) = 
  match syms with 
    | [] -> q
    | first::rest -> steps(fa,step(fa,q,first),rest)


let rec isDFA_helper (fa_states, fa_alpha, fa) = 
  match fa_states with 
    | [] -> failwith "no states"
    | last::[] -> (match fa_alpha with
                    | [] -> failwith "no alphabet"
                    | first4::[] -> 
                      if List.length(findTransitions(fa,last,first4)) = 1
                        then true
                      else false
                    | first5::rest5 -> 
                        if List.length(findTransitions(fa,last,first5)) = 1
                          then isDFA_helper(last::[],rest5,fa)
                        else false)
    | first::rest -> (match fa_alpha with
                        | [] -> failwith "no alphabet"
                        | first2::[] -> 
                          if List.length(findTransitions(fa,first,first2)) = 1
                            then isDFA_helper(rest,fa.alphabet,fa)
                          else false
                        | first3::rest3 -> 
                            if List.length(findTransitions(fa,first,first3)) = 1
                              then isDFA_helper(first::rest,rest3,fa)
                            else false)
    

let isDFA (fa) =
  isDFA_helper(fa.states,fa.alphabet,fa)

let acceptDFA (fa,input) = 
  if isDFA(fa) = true
    then isAccepting(fa,steps(fa,fa.start,explode(input)))
  else failwith "not deterministic"





(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY AUTOMATA *)
(* REPLACE BY YOUR OWN DEFINITIONS *)


let dfa_q2_a = { states = [0;1;2;3];
		 alphabet = ['a';'b'];
		 delta = [(0,'a',0);
              (0,'b',1);
              (1,'a',0);
              (1,'b',2);
              (2,'a',0);
              (2,'b',3);
              (3,'a',3);
              (3,'b',3)];
		 start = 0;
		 accepting = [0;1;2]}


let dfa_q2_b = { states = [0;1;2;3;4;5;6;7;8];
		 alphabet = ['a';'b'];
		 delta = [(0,'a',1);
              (1,'a',2);
              (2,'a',3);
              (3,'a',4);
              (4,'a',4);
              (4,'b',4);
              (3,'b',5);
              (2,'b',5);
              (1,'b',5);
              (0,'b',5);
              (5,'b',6);
              (6,'b',7);
              (7,'b',8);
              (8,'a',8);
              (8,'b',8);
              (7,'a',1);
              (6,'a',1);
              (5,'a',1)];
		 start = 0;
		 accepting = [0;1;2;3;5;6;7]}


let dfa_q2_c = { states = [0;1;2;3;4;5;6;7];
		 alphabet = ['a';'b'];
		 delta = [(0,'a',1);
              (1,'a',2);
              (2,'a',3);
              (3,'a',3);
              (3,'b',4);
              (0,'b',4);
              (4,'b',5);
              (5,'b',6);
              (6,'b',6);
              (6,'a',1);
              (1,'b',7);
              (2,'b',7);
              (4,'a',7);
              (5,'a',7);
              (7,'a',7);
              (7,'b',7) ];
		 start = 0;
		 accepting = [3;6]}


let nfa_q2_d = { states = [0;1;2;3;4;5;6];
		 alphabet = ['a';'b'];
		 delta = [(0,'a',1);
              (1,'a',2);
              (2,'a',3);
              (3,'a',3);
              (3,'b',4);
              (4,'a',4);
              (4,'b',4);
              (4,'a',5);
              (2,'b',6);
              (1,'b',6);
              (0,'b',6);
              (6,'a',6);
              (6,'b',6)];
		 start = 0;
		 accepting = [1;2;3;5;6]}




(* QUESTION 3 *)

(* setIn used to check if value is already used in list*)
let rec setIn (e,xs) = 
   match xs with [] -> false
      | first::rest -> if e = first then true else setIn(e,rest)

let rec keepTarget (trs) = 
  match trs with 
    | [] -> []
    | (a,b,c)::rest -> if setIn(c,keepTarget(rest)) then keepTarget(rest) else c::keepTarget(rest)



let rec isAcceptingAny (fa,qs) = 
  match qs with 
    | [] -> false
    | first::rest -> if setIn(first,fa.accepting) then true else isAcceptingAny(fa,rest)


let rec stepAll_helper (fa,qs,a,deltas) = 
  match qs with 
    | [] -> []
    | first::rest -> (match fa with
                        | [] -> []
                        | (o,p,q)::[] -> if first = o && a = p 
                                              then (if setIn(q,stepAll_helper(deltas,rest,a,deltas))
                                                      then stepAll_helper(deltas,rest,a,deltas)
                                                    else q::stepAll_helper(deltas,rest,a,deltas))
                                            else stepAll_helper(deltas,rest,a,deltas)
                        | (x,y,z)::rest2 -> if first = x && a = y 
                                              then (if setIn(z,stepAll_helper(rest2,first::rest,a,deltas))
                                                      then stepAll_helper(rest2,first::rest,a,deltas)
                                                    else z::stepAll_helper(rest2,first::rest,a,deltas))
                                            else stepAll_helper(rest2,first::rest,a,deltas))

let stepAll (fa,qs,a) = stepAll_helper (fa.delta,qs,a,fa.delta)


let rec stepsAll (fa,qs,syms) = 
  match syms with 
    | [] -> qs
    | first::rest -> stepsAll(fa,stepAll(fa,qs,first),rest)


let acceptNFA (fa,input) = 
  if isAcceptingAny(fa,stepsAll(fa,fa.start::[],explode(input)))
    then true
  else false




(* 
 * A sample DFA for testing
 *
 * It accepts the language of all strings over {a,b} with a
 * multiple-of-3 number of a's.
 *
 *)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = [ ("start",'a',"one");
	    ("one",'a',"two");
	    ("two",'a',"start");
	    ("start",'b',"start");
	    ("one",'b',"one");
	    ("two",'b',"two") ];
  start = "start";
  accepting = ["start"]
} 



(* A sample NFA for testing
 *
 * It accepts the language of all strings over {a,b,c} 
 * whose last three symbols are b's.
 *
 *)

let nfaLastThreeB = {
  states = [0;1;2;3];
  alphabet = ['a';'b';'c'];
  delta = [ (0,'a',0);
	    (0,'b',0);
	    (0,'c',0);
	    (0,'b',1);
	    (1,'b',2);
	    (2,'b',3); ];
  start = 0;
  accepting = [3]
} 




(* This function is the base function that langDFA and
 * langNFA use -- it basically loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 *
 *)

let langFA accept (fa,n) = 

  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  
  let rec take n default l = 
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in
  
  let to_base_n base size n = 
    let rec loop n = 
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in
  
  let to_string alphabet size n = 
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in
  
  if n < 0 then ()
  else
    let print_str s = if s = "" then print_string "  <epsilon>\n"
  	              else print_string ("  "^s^"\n")  in
    let rec loop i = 
        if i <= n then 
  	  let ts = to_string fa.alphabet i  in
  	  let bound = expt (List.length fa.alphabet) i in
  	  let rec loop2 j = 
  	    if j < bound then (if accept(fa,ts j) 
                                 then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
    loop 0


(* 
 * Tester functions that dump the language accepted by a
 * finite automaton, either deterministic or not
 *
 *)
 
let langDFA x = langFA acceptDFA x
let langNFA x = langFA acceptNFA x

