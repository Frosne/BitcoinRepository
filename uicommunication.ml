open String
open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;


let add a b = a + b;;
let sub a b = a - b;;

let stringConcat s1 s2 = 
	String.concat "" [s1;s2];;

let string_equal (a:string) (b:string) = 
	if String.compare a b == 0 then true else false;;

let print_bytes_m (x:bytes) : string =
    let s = get_cbytes x in
    let res = ref "" in
    for i = 0 to String.length s - 1 do
		let intR = int_of_char s.[i] in 
		let stringIntR = Printf.sprintf "%x" intR in 
		let stringIntR = if String.length stringIntR = 1 then stringConcat "0" stringIntR else stringIntR in 
		  res := !res ^ stringIntR;
		done;
    !res	


let stringCastOneElement(symbol:char): int =
		let el = symbol in 
	 	let result = 
		if  (el = 'a' || el = 'A') then 10 
		else if (el = 'b' || el = 'B') then 11
		else if (el = 'c' || el = 'C') then 12
		else if (el = 'd' || el = 'D') then 13
		else if (el = 'e' || el = 'E') then 14
		else if (el = 'f' || el = 'F') then 15
		else if (el >='0' && el <='9') then
			begin
			let code = Char.code el in 
			let one = Char.code '0' in 
		code - one
		end
		else 0
 in result;;

	let stringParse (line:string) =  
	let bytes = ref empty_bytes in 
	let count = ref (String.length line -1) in 
	while !count >= 0 do
		let result = 
			if !count = 0 then 
				let result = stringCastOneElement(line.[!count]) in result
			else 
				let first = stringCastOneElement (line.[!count]) in 
				let second = stringCastOneElement (line.[!count -1]) in 
				let result  = second * 16 + first in result 
		in
		count :=!count -2;  bytes :=  (bytes_of_int 1 result) @| !bytes done; !bytes;;
		
		

let chars_of_string str = 
	let rec exp i l = 
	if i < 0 then l else
	exp (i - 1) (str.[i] :: l ) in 
	exp (String.length str -1 ) [];;

let sha256 b = hash SHA256 b;;

let asciihashstr str = 
	let result = chars_of_string str in 
	let rec ascii line= 
		match line with [] -> ""
		| hd::tl ->  String.concat "" [Printf.sprintf "%X" (Char.code hd); (ascii tl)] in
	sha256(stringParse(ascii result));;

let asciihashlst lst = 
	let rec ascii lst = 
		match lst with [] -> "" | hd::tl -> String.concat "" [hd; ascii tl] in ascii lst;;

let hashCheck lst result = 
	let r = asciihashlst lst in 
	let r = asciihashstr r in 
	string_equal (print_bytes_m r) result;;

let printBytes bytes= print_string(print_bytes bytes); print_endline "" ;;

let take_list lst l r = 
	let a = ref [] in 
	for i = l to r do 
		a := List.append !a [(List.nth lst i)] done; !a;;
		

let print_list lst = 
	let rec print_list_pr lst =
		match lst with
		hd::tl -> print_endline hd; print_list_pr tl 
		| [] -> print_endline ""
	in print_list_pr lst;;

let main  = 
	let args = ref [] in 
		for i = 0 to Array.length Sys.argv - 1 do
     		args:= List.append !args [Sys.argv.(i)]
    	done;
	print_endline "Ocaml side";
	let a1 = take_list !args 1 ((List.length !args) -2)  in 
	let resultHash = hashCheck a1  (List.nth !args (List.length !args -1)) in 
	if 
		resultHash != true then  print_endline "Hash is not equal" 
	else if 
		List.length !args < 4 then begin print_endline "Wrong argument"; end 
	else
	begin
		let function_name = List.nth !args 1 in 
		let arg1 = List.nth !args 2 in 
		let arg1 = int_of_string arg1 in 
		let arg2 = List.nth !args 3 in 
		let arg2 = int_of_string arg2 in 
		if string_equal function_name "sub" == true
			then print_int (sub arg1 arg2)
		else if string_equal function_name "add" == true
			then print_int (add arg1 arg2)
		else print_endline "Function not found";
 		print_endline "";
	end;;

		
