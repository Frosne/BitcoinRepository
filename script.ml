open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;
open Cryptokit;;

(* printing*)


type transactionInputType = {prevout_hash: bytes; index : bytes; scriptSigB: bytes; sequence: bytes}

type transactionOutputType = {value: bytes; pkScript: bytes}

type transactionType = {nVersion:bytes; inputcount : int; inputs : transactionInputType list; outputcount: int; outputs: transactionOutputType list;nLockTime : bytes}

let stringConcat s1 s2 = 
	String.concat "" [s1;s2];;

let printBytes bytes= print_string(print_bytes bytes);;

(*number representation*)
let bytes_of_long_big_endian nb (i:int64) =
    let rec put_bytes bb lb n =
      if lb = 0 then failwith "not enough bytes"
      else
        begin
          String.set bb (nb - lb) (char_of_int (Int64.to_int(Int64.rem n  (Int64.of_int 256))));
          if (Int64.div n (Int64.of_int 256)) > Int64.zero then
            put_bytes bb (lb-1) (Int64.div n (Int64.of_int 256))
          else bb
        end
    in
    let b = String.make nb (char_of_int 0) in
      abytes(put_bytes b nb i);;

let bytes_of_int_big_endian nb i =
    let rec put_bytes bb lb n =
      if lb = 0 then failwith "not enough bytes"
      else
        begin
          String.set bb (nb-lb) (char_of_int (n mod 256));
          if n/256 > 0 then
            put_bytes bb (lb-1) (n/256)
          else bb
        end
    in
    let b = String.make nb (char_of_int 0) in
      abytes(put_bytes b nb i)

let long_of_bytes (b:bytes) : int64 = 
	let x = ref 0L in 
      	let c = get_cbytes b in
        for y = 0 to b.length-1 do
        x := Int64.add (Int64.mul 256L !x) (Int64.of_int(int_of_char (String.get c y)))
        done;
      !x



let bytes_of_long nb (i:int64) =
    let rec put_bytes bb lb n =
      if lb = 0 then failwith "not enough bytes"
      else
        begin
          String.set bb (lb - 1) (char_of_int (Int64.to_int(Int64.rem n  (Int64.of_int 256))));
          if (Int64.div n (Int64.of_int 256)) > Int64.zero then
            put_bytes bb (lb-1) (Int64.div n (Int64.of_int 256))
          else bb
        end
    in
    let b = String.make nb (char_of_int 0) in
      abytes(put_bytes b nb i);;
	
	let stringCastOneElement(symbol:char): int = 
		let el = symbol in 
	 	let result = 
		if  el = 'a' then 10 
		else if el = 'b' then 11
		else if el = 'c' then 12
		else if el = 'd' then 13
		else if el = 'e' then 14
		else if el = 'f' then 15
		else if el >='0' && el <='9' then
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
		
		
(* number representation*)

let compareStrings s1 s2 : bool = 
	let result = (compare s1 s2) in
		if result == 0
			then true
		else false;;

let intOfBytesVarInput (number:bytes) : int = 
	let wholebyte = string_of_bytes number in
		if String.length wholebyte  >= 2 then 
			let byteInfo = (String.sub wholebyte 0 2) in 	
				if (compareStrings "fd" byteInfo) && (number.length==3) then	
					let number = xor 3 number (bytes_of_int 3 16580608)
						in int_of_bytes number;
				else		
				 int_of_bytes number
		else 	
			int_of_bytes number;;

let bytesVarIntOfInt (number: int) : bytes 	= 
	if number < 253 then bytes_of_int 1 number
	else if (number < 65536) then 
		let number = bytes_of_int 3 number in
let result = xor 3 number (bytes_of_int 3 16580608) in result
		else bytes_of_int 1 0;;

let formatNumber value = bytesVarIntOfInt value;;

let computeTransactionHash (transaction:bytes) : bytes = 
empty_bytes;;
		
let takeLeft (b: bytes) i = 
	let splitted = split b i in
		let part = fst splitted in part;;

let takeRight(b:bytes) i = 
	 if (length b == 1) then	empty_bytes
else
	let splitted = split b i in
		let part = snd splitted in part;;

let print_stack_int (stack: int Stack.t) = 
	Stack.iter print_int stack;;

let print_stack_bytes (stack: bytes Stack.t) =
	Stack.iter printBytes stack;;

(* Main functionality *)



(*let op_put stack bytes = Stack.push bytes stack;;*)
(*let signature = Stack.pop stack in 
	let publicKey = Stack.pop stack in  *)

(* OPCODE 0*)
let _OpcodeZero (stack: bytes Stack.t ref) (data : bytes ref ) = 
	try 
		Stack.push empty_bytes !stack; 0
	with _ -> -1;;
(* OPCODE 1 - 75 *)
let _OpcodePush (stack: bytes Stack.t ref) (data: bytes ref) : int = 
	try
		let value = takeLeft !data 1 in data := takeRight !data 1; 
		let value = int_of_bytes value in 
		let dataToPut = takeLeft !data value in data:=takeRight !data value; Stack.push dataToPut !stack; 0
	with _ -> -1;;

(* We dont change the value of data, it means that the first byte will be 76 *) 
let _OpcodePushData1 (stack:bytes Stack.t ref) (data : bytes ref ) : int = 
	try
		data := takeRight !data 1; let value = takeLeft !data 1 in data:= takeRight !data 1; 
		let value = int_of_bytes value in 
		let dataToPut = takeLeft !data value in data:=takeRight !data value; Stack.push dataToPut !stack; 0
	with _ -> -1;;

let _OpcodePushData2(stack:bytes Stack.t ref) (data:bytes ref) : int = 
	try
		data := takeRight !data 1; let value = takeLeft !data 2 in data:=takeRight !data 2; 
		let value = int_of_bytes value in 
		let dataToPut = takeLeft !data value in data := takeRight !data value; Stack.push dataToPut !stack; 0
	with _ -> -1;; 

(* LONG??????? *) (*
let _OpcodePushData4 (stack::bytes Stack.t ref) (data:bytes ref) : int = 
	try
		data := takeRight !data 1; let value = takeLeft !data 4 in data := takeRight !data 4; 
		let value = long_of_bytes value in 
		let dataToPut = takeLeft !data 
*)

let _OpcodeNegateOne(stack:bytes Stack.t ref) (data:bytes ref) : int =
	try 
		data := takeRight !data 1;
		let num = -1 in 
		Stack.push (bytes_of_int 1 num) !stack; 0
	with _ -> -1;;


let _OpcodeOne(stack:bytes Stack.t ref) (data:bytes ref) : int  = 
	try 
		data := takeRight !data 1;
		let num = 1 in 
		Stack.push (bytes_of_int 1 num) !stack; 0
	with _ -> -1;;

let _OpcodeTwoSixteen(stack:bytes Stack.t ref) (data:bytes ref) : int = 
	try
		let value = takeLeft !data 1 in
		data:= takeRight !data 1; 
		let value = int_of_bytes value in 
		let value = value - 80 in 
		Stack.push (bytes_of_int 1 value) !stack; 0
	with _ -> -1;;

let _OpNop (stack:bytes Stack.t ref) (data:bytes ref) : int = 0;;

let _OpVerify (stack: bytes Stack.t ref) (data:bytes ref): int = 
	try
		let value = Stack.top !stack in 
		let value = int_of_bytes value in 
		let result =
			if value == 0 then 0 else -1 in result
	with _ -> -1;;

let _OpReturn (stack:bytes Stack.t ref) (data:bytes ref) : int = -1;;

let _OpDup(stack:bytes Stack.t ref) (data:bytes ref) : int =
	try
		let value = Stack.top !stack in 
		Stack.push value !stack; 0
	with _-> -1;;

let _OpSha256(stack:bytes Stack.t ref) (data:bytes ref) : int = 
	

let stack : bytes Stack.t = Stack.create ();;


print_string(Int64.to_string (long_of_bytes (stringParse "ffffffff")));;



































(* /Main functionality*) 



