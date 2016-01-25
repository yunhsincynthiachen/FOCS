let add(x,y) = x+y


(* let rec sumto_helper (index, n,result) = 
	if index <= n
		then sumto_helper(index+1,n,result+index)
	else result

let sumto (n) = sumto_helper(0,n,0) *)


(* let rec sumto (n) = if n = 0 then 0 else sumto(n-1) + n *)

let classify (lst) = 
	match lst with [] -> "empty"
		| a::b -> "not empty" 

let rec length (lst) = 
	match lst with [] -> 0
			| fst::rst -> length(rst) + 1 

let rec sum(lst) = 
	match lst with [] -> 0
		| first::rest -> sum(rest) + first 

List.nth ["a","b","c"] 1;;