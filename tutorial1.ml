let rec find_max (original_list, max_num) =
	match original_list with [] -> failwith "failed"
		| x::[] -> if x > max_num then x else max_num
		| first::rest -> if first > max_num then find_max(rest,first) else find_max(rest,max_num) 

let rec merge_lists (list_1,list_2, new_list) =
	match list_1 with [] -> new_list
		| first::rest -> 
			(match list_2 with [] -> failwith "failed"
				| first2::rest2 -> first::first2::merge_lists (rest,rest2,new_list)	)

let rec remove_consecs (orig_list, current, new_list) =
	match orig_list with [] -> new_list
		| first::rest -> if first = current then remove_consecs (rest, first, new_list) else first::remove_consecs (rest, first, new_list)

let remove_consecs_helper (orig_list) = 
	match orig_list with [] -> failwith "nothing in list"
		| first::rest -> first::remove_consecs(rest,first,[])

let rec compress lst = 
	match lst with 
		| [] -> []
		| [x] -> [x]
		| [x;y] -> if x=y then [x] else [x;y]
		| x1::x2::rest -> if x1 = x2 then compress(x2::rest) else x1::compress(x2::rest)

let rec append (list1,list2) =
	match list1 with
		 [] -> list2
		| first::rest -> first::append(rest,list2)