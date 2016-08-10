open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;
open Cryptokit;;

type scriptSignature = {script : bytes; signature: bytes}

let scr_OP_DUP (stack : bytes Stack.t) = 
	let s = Stack.pop stack in 
	Stack.push s stack; Stack.push s stack;;

let rec print_stack (stack: int Stack.t) = 
	Stack.iter print_int stack;;

(*todo*)
let ripOriginal (s: string) = 
	hash_string (Hash.ripemd160()) s ;;

let sha256 b = hash SHA256 b;;

let scr_OP_HASH (stack : bytes Stack.t) = 
	let s = Stack.pop stack in 
	let sha = sha256 s in 
	let sharip = rip sha in 
	Stack.push sharip stack;;
(*
let split (b:bytes) i : bytes * bytes 
*)

let takeLeft (b: bytes) i = 
	let splitted = split b i in 
		List.nth 0 splitted;;
(*
let rec byteparse stack (lst:bytes) =
	let byte = int_of_bytes (takeLeft lst 0) in 
	match lst.bl with 
		[] -> stack
		| hd::tl -> 
			if byte == 118 then
				begin  scr_OP_DUP stack; byteparse stack tl end
			else if byte == 169 then
				begin scr_OP_HASH stack; byteparse stack tl end
			(*else if byte > 0 && byte < 76 then 
				begin  *)
			else byteparse stack tl;;
				
				


let parse (scr : scriptSignature) (lst : bytes) = 
		let s = Stack.create () in 
		(*!*)	Stack.push scr.script s; Stack.push scr.signature s;  byteparse s lst;;

*)










