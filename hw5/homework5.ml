(* 

HOMEWORK 5

Name: Cynthia Chen

Email: yun-hsin.chen@students.olin.edu

Remarks, if any: I got help from Sidd, especially on the last one (binary addition) to figure out
how to optimize the code and shorten the number of states, but I came up with the entire and transitions
of the Turing machine.

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * Do that in a _fresh_ OCaml shell 
 * It has to load without any errors.
 *
 *)




(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> string list
 *      returns the list of characters making up a string
 *
 *)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
  acc (String.length(str)-1) []


(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type symbol = string

type 'a tm = { states : 'a list;
	       input_alphabet : symbol list;
	       tape_alphabet : symbol list;
	       left_marker : symbol;
	       blank : symbol;
	       delta : ('a * symbol) -> ('a * symbol * int);   (* 0 = Left, 1 = Right *)
	       start : 'a;
	       accept : 'a;
	       reject : 'a }

type 'a config = { state : 'a;
		   before: symbol list;
		   after: symbol list }
      
(*
 * Helper function
 *
 * Pint a configuration (including newline) to standard output
 * and RETURN A VALUE
 * 
 *)

let printConfig m config value = 
    let mw = List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ = print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with 
             | [] -> Printf.printf "[%s]" m.blank
	     | a::v' -> let _ = Printf.printf "[%s]" a  in
	                print_syms v') in
    let _ = print_newline ()  in
    value




(* QUESTION 1 *)

let startConfig m w = {state = m.start; before = []; after = m.left_marker::explode(w)}


let acceptConfig m config = 
	if m.accept = config.state
		then true
	else false


let rejectConfig m config = 
	if m.reject = config.state
		then true
	else false


let haltConfig m c = 
	if m.reject = c.state || m.accept = c.state
		then true
	else false

let get_first_tuple (a,b,c) = a

let get_second_tuple (a,b,c) = b

let get_third_tuple (a,b,c) = c

let get_last_value orig_list = List.nth orig_list ((List.length orig_list) -1)

let remove_last orig_list = 
   match orig_list with
	   | []   -> []
	   | h::t -> t

let step m config = 
	match config.after with 
		| [] -> config
		| first::rest -> 	if get_third_tuple(m.delta (config.state,first)) = 1
								then {state = get_first_tuple(m.delta (config.state,first)); 
									before = config.before@((get_second_tuple(m.delta (config.state,first)))::[]); 
									after=rest}
							else {state = get_first_tuple(m.delta (config.state,first)); 
									before = List.rev (remove_last (List.rev config.before)); 
									after=((get_last_value config.before)::(get_second_tuple(m.delta (config.state,first)))::[])@rest}

let rec run_helper m config =  
	match config.after with 
		| [] -> if (haltConfig m config) then (if (acceptConfig m config) then (printConfig m config true) else (printConfig m config false)) else (printConfig m config false; (run_helper m (step m {state = config.state; before = config.before; after = [m.blank]})))
		| first::rest -> if (haltConfig m config) then (if (acceptConfig m config) then (printConfig m config true) else (printConfig m config false)) else  (printConfig m config false; (run_helper m (step m config)))

let run m w = run_helper m (startConfig m w)

(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { states = ["start"; "q1"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"_";">"];
	     blank = "_";
	     left_marker = ">";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", ">") -> ("start", ">", 1)
			 | ("start", "_") -> ("acc", "_", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "_") -> ("acc", "_", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", ">") -> ("acc", ">", 1)
			 | ("acc", "_") -> ("acc", "_", 1)
			 | (_,c) -> ("rej",c,1))}

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", "|") -> ("start", "|", 1)
			 | ("start", "/") -> ("q2", "/", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "/") -> ("q2", "/", 1)
			 | ("q2", "|") -> ("q3", "|", 1)
			 | ("q2", "a") -> ("q2", "a", 0)
			 | ("q2", "b") -> ("q2", "b", 0)
			 | ("q2", "X") -> ("q2", "X", 0)
			 | ("q2", "/") -> ("q2", "/", 0)
			 | ("q3", "X") -> ("q3", "X", 1)
			 | ("q3", "/") -> ("acc", "/", 1)
			 | ("q3", "a") -> ("q4", "X", 1)
			 | ("q4", "a") -> ("q4", "a", 1)
			 | ("q4", "X") -> ("q4", "X", 1)
			 | ("q4", "b") -> ("q2", "X", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", "|") -> ("acc", "|", 1)
			 | ("acc", "X") -> ("acc", "X", 1)
			 | ("acc", "/") -> ("acc", "/", 1)
			 | (_,c) -> ("rej",c,1))}


let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       input_alphabet = ["a";"b";"c"];
	       tape_alphabet = ["a";"b";"c";"X";"_";">"];
	       blank = "_";
	       left_marker = ">";
	       start = "start";
	       accept = "acc";
	       reject = "rej";
	       delta = (fun inp -> match inp with
	                | ("start", "a") -> ("start", "a", 1)
     			| ("start", "b") -> ("q1", "b", 1)
			| ("start", "c") -> ("q6", "c", 1)
			| ("start", ">") -> ("start", ">", 1)
			| ("start", "_") -> ("q2", "_", 1)
			| ("q1", "b") -> ("q1", "b", 1)
			| ("q1", "c") -> ("q6", "c", 1)
			| ("q1", "_") -> ("q2", "_", 1)
			| ("q2", ">") -> ("q3", ">", 1)
			| ("q2", "a") -> ("q2", "a", 0)
			| ("q2", "b") -> ("q2", "b", 0)
			| ("q2", "c") -> ("q2", "c", 0)
			| ("q2", "_") -> ("q2", "_", 0)
			| ("q2", "X") -> ("q2", "X", 0)
			| ("q3", "X") -> ("q3", "X", 1)
			| ("q3", "_") -> ("acc", "_", 1)
			| ("q3", "a") -> ("q4", "X", 1)
			| ("q4", "a") -> ("q4", "a", 1)
			| ("q4", "X") -> ("q4", "X", 1)
			| ("q4", "b") -> ("q5", "X", 1)
			| ("q5", "b") -> ("q5", "b", 1)
			| ("q5", "X") -> ("q5", "X", 1)
			| ("q5", "c") -> ("q2", "X", 1)
			| ("q6", "c") -> ("q6", "c", 1)
			| ("q6", "_") -> ("q2", "_", 1)
		        | ("acc", "a") -> ("acc", "a", 1)
		        | ("acc", "b") -> ("acc", "b", 1)
		        | ("acc", "c") -> ("acc", "c", 1)
		        | ("acc", ">") -> ("acc", ">", 1)
		        | ("acc", "X") -> ("acc", "X", 1)
		        | ("acc", "_") -> ("acc", "_", 1)
			| (_,c) -> ("rej", c,1))}



(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY TURING MACHINES *)
(* REPLACE BY YOUR OWN DEFINITIONS *)


let tm_q2_a = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"q8";"q9"];
		input_alphabet = ["c";"d"];
		tape_alphabet = ["c";"d";"_";">";"X"];
		blank = "_";
		left_marker = ">";
		start = "start";
		accept = "acc";
		reject = "rej";
		delta = (fun inp -> match inp with
					| ("start",">") -> ("q1", ">", 1)
					| ("q1","_") -> ("acc", "_", 1)
					| ("q1","c") -> ("q2","c",1)
					| ("q1","d") -> ("q2","d",1)
	            	| ("q2","c") -> ("q2","c",1)
	            	| ("q2","d") -> ("q2","d",1)
	            	| ("q2","_") -> ("q3","_",0)
	            	| ("q3","c") -> ("q4","X",0)
	            	| ("q3","d") -> ("q7","X",0)
	            	| ("q4","c") -> ("q4","c",0)
	            	| ("q4","d") -> ("q4","d",0)
	            	| ("q4","X") -> ("q4","X",0) 
	            	| ("q4",">") -> ("q5",">",1)
	            	| ("q5","X") -> ("q5","X",1)
	            	| ("q5","_") -> ("acc","_",0)
	            	| ("q5","c") -> ("q6","X",1)
	            	| ("q6","c") -> ("q6","c",1)
	            	| ("q6","d") -> ("q6","d",1)
	            	| ("q6","X") -> ("q3","X",0)
	            	| ("q7","c") -> ("q7","c",0)
	            	| ("q7","d") -> ("q7","d",0)
	            	| ("q7","X") -> ("q7","X",0)
	            	| ("q7",">") -> ("q8",">",1)
	            	| ("q8","X") -> ("q8","X",1) 
	            	| ("q8","_") -> ("acc","_",0)
	            	| ("q8","d") -> ("q9","X",1)
	            	| ("q9","c") -> ("q9","c",1)
	            	| ("q9","d") -> ("q9","d",1)
	            	| ("q9","X") -> ("q3","X",0)
	            	| ("q3","X") -> ("acc","X",0)
	            	| ("q8","c") -> ("rej","c",1)
	            	| ("q5","d") -> ("rej","d",1)
				        | ("acc", "c") -> ("acc", "c", 1)
				        | ("acc", "d") -> ("acc", "d", 1)
				        | ("acc", ">") -> ("acc", ">", 1)
				        | ("acc", "X") -> ("acc", "X", 1)
				        | ("acc", "_") -> ("acc", "_", 1)
	            	| (_,c) -> ("rej",c,1))}

let tm_q2_b = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6"];
		input_alphabet = ["a";"b"];
		tape_alphabet = ["a";"b";"_";">";"X"];
		blank = "_";
		left_marker = ">";
		start = "start";
		accept = "acc";
		reject = "rej";
		delta = (fun inp -> match inp with
					| ("start",">") -> ("q2",">",1)
					| ("q2","a") -> ("rej","a",1)
					| ("q2","_") -> ("acc","_",1)
					| ("q2","X") -> ("q2","X",1)
					| ("q2","b") -> ("q3","X",1)
					| ("q3","a") -> ("q3","a",1)
					| ("q3","b") -> ("q3","b",1)
					| ("q3","X") -> ("q3","X",1)
					| ("q3","_") -> ("q4","_",0)
					| ("q4","X") -> ("q4","X",0)
					| ("q4","a") -> ("q5","X",0)
					| ("q5","a") -> ("q6","X",0)
					| ("q6","a") -> ("q7","X",0)
					| ("q7","a") -> ("q7","a",0)
					| ("q7","b") -> ("q7","b",0)
					| ("q7","X") -> ("q7","X",0)
					| ("q7",">") -> ("q2",">",1)
					| (_,c) -> ("rej",c,1))}




(* QUESTION 3 *)


let binaryAddition = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"q8";"q9";
									"q10";"q11";"q12";"q13";"q14";"q15";"q16";"q17";"q18";"q19";"q20";
									"q21";"q22";"q23";"q24";"q25";"q26";"q27";"q28";"q29"];
		       input_alphabet = ["0";"1";"#"];
		       tape_alphabet = ["0";"1";"_";">";"X";"#"];
		       blank = "_";
		       left_marker = ">";
		       start = "start";
		       accept = "acc";
		       reject = "rej";
		       delta = (fun inp -> match inp with
					| ("start",">") -> ("q2",">",1)
					| ("q2","X") -> ("q2","X",1)
					| ("q2","1") -> ("q2","1",1)
					| ("q2","0") -> ("q2","0",1)
					| ("q2","#") -> ("q3","#",0)
					| ("q3","X") -> ("q3","X",0)
					| ("q3","0") -> ("q4","X",1)
					| ("q4","X") -> ("q4","X",1)
					| ("q4","#") -> ("q5","#",1)
					| ("q5","0") -> ("q5","0",1)
					| ("q5","1") -> ("q5","1",1)
					| ("q5","X") -> ("q5","X",1)
					| ("q5","#") -> ("q6","#",0)
					| ("q6","X") -> ("q6","X",0)
					| ("q6","0") -> ("q7","X",1)
					| ("q7","X") -> ("q7","X",1)
					| ("q7","0") -> ("q7","0",1)
					| ("q7","1") -> ("q7","1",1)
					| ("q7","#") -> ("q7","#",1)
					| ("q7","_") -> ("q8","_",0)
					| ("q8","X") -> ("q8","X",0)
					| ("q8","0") -> ("q9","X",0)
					| ("q9","#") -> ("acc","#",1)
					| ("q9","0") -> ("q10","0",0)
					| ("q9","1") -> ("q10","1",0)
					| ("q10","X") -> ("q10","X",0)
					| ("q10","#") -> ("q10","#",0)
					| ("q10","1") -> ("q10","1",0)
					| ("q10","0") -> ("q10","0",0)
					| ("q10",">") -> ("q2",">",1)
					| ("q6","1") -> ("q14","X",1)
					| ("q14","#") -> ("q14","#",1)
					| ("q14","X") -> ("q14","X",1)
					| ("q14","0") -> ("q14","0",1)
					| ("q14","1") -> ("q14","1",1)
					| ("q14","_") -> ("q15","_",0)
					| ("q15","X") -> ("q15","X",0)
					| ("q15","1") -> ("q9","X",0)
					| ("q3","1") -> ("q11","X",1)
					| ("q11","X") -> ("q11","X",1)
					| ("q11","#") -> ("q12","#",1)
					| ("q12","0") -> ("q12","0",1)
					| ("q12","1") -> ("q12","1",1)
					| ("q12","X") -> ("q12","X",1)
					| ("q12","#") -> ("q13","#",0)
					| ("q13","0") -> ("q14","X",1)
					| ("q13","X") -> ("q13","X",0)
					| ("q13","1") -> ("q16","X",1)
					| ("q16","X") -> ("q16","X",1)
					| ("q16","0") -> ("q16","0",1)
					| ("q16","1") -> ("q16","1",1)
					| ("q16","#") -> ("q16","#",1)
					| ("q16","_") -> ("q17","_",0)
					| ("q17","X") -> ("q17","X",0)
					| ("q17","0") -> ("q18","X",0)
					| ("q18","#") -> ("acc","#",0)
					| ("q18","0") -> ("q19","0",0)
					| ("q18","1") -> ("q19","1",0)
					| ("q19","#") -> ("q19","#",0)
					| ("q19","1") -> ("q19","1",0)
					| ("q19","0") -> ("q19","0",0)
					| ("q19","X") -> ("q19","X",0)
					| ("q19",">") -> ("q20",">",1)
					| ("q20","X") -> ("q20","X",1)
					| ("q20","1") -> ("q20","1",1)
					| ("q20","0") -> ("q20","0",1)
					| ("q20","#") -> ("q21","#",0)
					| ("q21","X") -> ("q21","X",0)
					| ("q21","0") -> ("q22","X",1)
					| ("q22","X") -> ("q22","X",1)
					| ("q22","#") -> ("q23","#",1)
					| ("q23","0") -> ("q23","0",1)
					| ("q23","1") -> ("q23","1",1)
					| ("q23","X") -> ("q23","X",1)
					| ("q23","#") -> ("q24","#",0)
					| ("q24","X") -> ("q24","X",0)
					| ("q24","0") -> ("q14","X",1)
					| ("q24","1") -> ("q16","X",1)
					| ("q21","1") -> ("q25","X",1)
					| ("q25","X") -> ("q25","X",1)
					| ("q25","#") -> ("q26","#",1)
					| ("q26","X") -> ("q26","X",1)
					| ("q26","0") -> ("q26","0",1)
					| ("q26","1") -> ("q26","1",1)
					| ("q26","#") -> ("q27","#",0)
					| ("q27","X") -> ("q27","X",0)
					| ("q27","0") -> ("q16","X",1)
					| ("q27","1") -> ("q28","X",1)
					| ("q28","X") -> ("q28","X",1)
					| ("q28","0") -> ("q28","0",1)
					| ("q28","1") -> ("q28","1",1)
					| ("q28","#") -> ("q28","#",1)
					| ("q28","_") -> ("q29","_",0)
					| ("q29","X") -> ("q29","X",0)
					| ("q29","1") -> ("q18","X",0)
					| (_,c) -> ("rej",c,1))}