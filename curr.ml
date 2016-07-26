(*32 bit version
a list of inputs
a list of outputs
32 bit locktime
*)


open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;

(* printing*)
let printBytes bytes= print_string(print_bytes bytes);;

(*main functions*)
let generateNVersion = bytes_of_int 4 1;;

let compareStrings s1 s2 : bool = 
	let result = (compare s1 s2) in
		if result == 0
			then true
		else false;;

let intOfBytesVarInput (number:bytes) : int = 
	let wholebyte = string_of_bytes number in
		let byteInfo = (String.sub wholebyte 0 2) in 	
			if (compareStrings "fd" byteInfo) && (number.length==3) then	
		let number = xor 3 number (bytes_of_int 3 16580608)
			in int_of_bytes number;
		(*Int is too big =/*)
		(*else if (compareStrings "fe" byteInfo) && (number.length==3) then let number = xor number (bytes_of_int 4 1090921693184)
		in int_of_bytes number*)
		else		
			 int_of_bytes number;;

let bytesVarIntOfInt (number: int) : bytes 	= 
	if number < 253 then bytes_of_int 1 number
	else if (number < 65536) then 
		let number = bytes_of_int 3 number in
let result = xor 3 number (bytes_of_int 3 16580608) in result
		else bytes_of_int 1 0;;
	(*else if (number < 4294967295) then 
		let number = bytes_of_int 5 number in 
			xor 5 number (bytes_of_int 3 1090921693184);;*)

let formatNumber value = bytesVarIntOfInt value;;

(*crypto*)

let createTransaction (ins: int) : bytes   =
	let nVersion = generateNVersion in 
	let vinCount = formatNumber(ins) in 
	let result = nVersion  @| vinCount in result;;

(*testing*)
	printBytes generateNVersion;;
	printBytes (bytesVarIntOfInt 1);;
	printBytes(createTransaction 1);;
