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
let formatNumber value = bytes_of_int 4 1;;

(*crypto*)



let createTransaction (ins: int) (outs: int) (hashes: bytes list)  (values: int list) : bytes   =
	let nVersion = generateNVersion in 
	let vincount = formatNumber(ins) in 
	let result = nVersion @| vinCount;;


(*testing*)
	printBytes generateNVersion;;
