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

(* 
prevout_hash: 0000000000000000000000000000000000000000000000000000000000000000
prevout_n: ffffffff
scriptSig: 4d:04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73
sequence: ffffffff
*)



(*for variable = start_value downto end_value do
  expression
done*)

let scriptSignatureGeneration(ins:int)(hashed: bytes list)(inputs :int list) : bytes list = [empty_bytes];;
let publicKey: bytes = empty_bytes;;

let inputsBytes(ins:int) (hashed: bytes list) (inputs :int list) : bytes = 
	let counter = 0 in 
	let sequence = abytes "4294967295" in 
	let scripts = scriptSignatureGeneration ins hashed inputs in 
	let result = empty_bytes in 
	let hash = 
	while counter != ins do
		let hash = List.nth hashed counter in
		let input =bytesVarIntOfInt ( List.nth  inputs counter) in
		let script = List.nth scripts counter in
		counter = counter + 1; result = ((result @| hash) @| (input @| script))@|sequence done
	in result;;

let createTransaction (ins: int)(hashes:bytes list)(inputs:int list) : bytes   =
	let nVersion = generateNVersion in
	let vinCount = formatNumber(List.length hashes) in 
	let inputsSerialized = inputsBytes ins hashes inputs in 
	let result = nVersion  @| vinCount @| inputsSerialized in result;;

(*testing*)
	printBytes generateNVersion;;
	printBytes (bytesVarIntOfInt 1);;
	
