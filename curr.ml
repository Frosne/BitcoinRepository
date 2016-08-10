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

let rec print_stack (stack: int Stack.t) = 
	Stack.iter print_int stack;;

type transactionInputType = {prevout_hash: bytes; index : bytes; scriptSigB: bytes; sequence: bytes}

type transactionOutputType = {value: bytes; pkScript: bytes}

type transactionType = {nVersion:bytes; inputcount : int; inputs : transactionInputType list; outputcount: int; outputs: transactionOutputType list;nLockTime : bytes}


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

(* 
	  let abytes (ba:cbytes) =
      {bl = [ba]; length = String.length ba; index = 0; max = String.length ba}

*)
	
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


(* 
let fucn =    
  let count = ref 0 in 
  while !count != 10 do
  print_int !count;
  count :=!count +1
  done;;
*)

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

(*main functions*)
let generateNVersion = bytes_of_int 4 16777216;;
let generateNLockTime = bytes_of_int 4 0;;

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

let scriptSignatureGeneration(ins:int)(hashed: bytes list)(inputs :int64 list) : bytes list = [empty_bytes];;
let publicKey: bytes = empty_bytes;;

let scriptSignatureGeneration(ins:int)(hashed: bytes list)(inputs :int64 list) : bytes list = 
	let scriptSig =stringParse "4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73" in [scriptSig];;

let inputsBytes(ins:int) (hashed: bytes list) (inputs :int64 list) : bytes = 
	let counter = ref 0 in 
	let sequence = stringParse "ffffffff" in 
	let scripts = scriptSignatureGeneration ins hashed inputs in 
	let result = ref empty_bytes in 
	let hash = 
	while !counter != ins do
		let hash = List.nth hashed !counter in
		let input =bytes_of_long_big_endian 4 ( List.nth inputs !counter) in
		let script = List.nth scripts !counter in
		counter := !counter + 1;  result := !result @| hash @| input @| script @|sequence done
	in !result;;


let scriptPubKey(outs: int) (publicKeys: bytes list) : bytes list = 
	let scriptPubKey =  stringParse "434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac" in [scriptPubKey];;

let outputBytes(outs: int)(values : int64 list) (publicKeys: bytes list) : bytes = 
	let scripts = scriptPubKey outs publicKeys in 
	let result = ref empty_bytes in 
	let counter = ref 0 in 
	let hash = 
	while !counter != outs do
		let value = List.nth values !counter in 
		let valueFormatted = bytes_of_long_big_endian 8 value in 
		let publicKey = List.nth scripts !counter in 
		counter := !counter + 1; result := !result @| valueFormatted @| publicKey done
	in !result;;
	
let createTransaction (ins: int)(hashes:bytes list)(inputs:int64 list)(outs: int)(values : int64 list)(publicKeys:bytes list) : bytes   =
	let nVersion = generateNVersion in
	let vinCount = formatNumber ins in 
	let voutCount = formatNumber outs in 
	let inputsSerialized = inputsBytes ins hashes inputs in 
	let outputsSerialized = outputBytes outs values publicKeys in 
	let nLockTime = generateNLockTime in 		
	let result = nVersion  @| vinCount @| inputsSerialized  @| voutCount @| outputsSerialized @| nLockTime in result;;

let takeLeft (b: bytes) i = 
	let splitted = split b i in
		let part = fst splitted in part;;

let takeRight(b:bytes) i = 
	 if (length b == 1) then	empty_bytes
else
	let splitted = split b i in
		let part = snd splitted in part;;


let parseTransaction (transaction:bytes)  = 
	let nVersion = takeLeft transaction 4 in 
	let transaction = takeRight transaction 4 in
		let num = takeLeft transaction 1 in 
			let numint = int_of_bytes num in
				let length = 
					if numint = 253 (*fd*) then 2
					else if numint = 254 then 4 
					else if numint = 255 then 8
					else  0 in 
					let vin =takeLeft transaction (length +1 ) in 
					let vinint = intOfBytesVarInput vin in
					let transaction = takeRight transaction (length +1) in 			
					let transactionInput : (transactionInputType list) = []  in
					let refTransactionInput = ref transactionInput in 
					let counter = ref 0 in 
					let l = ref 0 in 
					let temp = 
						while (!counter != vinint) do
							let hash = takeLeft transaction 32 in 
							let transaction = takeRight transaction 32 in 
							let n = takeLeft transaction 4 in 
							let transaction = takeRight transaction 4 in 
							let scriptSigLength = takeLeft transaction 1 in
							let scriptSigLengthInt = int_of_bytes scriptSigLength in 
							let scriptSigLengthIntAdd = 	
								if scriptSigLengthInt = 253 (*fd*) then 2
								else if scriptSigLengthInt = 254 then 4 
								else if scriptSigLengthInt = 255 then 8
								else  0 in
							let scriptSig = takeLeft transaction (scriptSigLengthIntAdd+1) in 
							let transaction = takeRight transaction (scriptSigLengthIntAdd +1 ) in 
							let seq = takeLeft transaction 4 in
							let transaction = takeRight transaction 4 in 
							let input = {prevout_hash = hash; index = n; scriptSigB = scriptSig; sequence = seq} 									in l := !l + 32 + 4 + 1 + scriptSigLengthInt + 4; 
						List.append !refTransactionInput [input]; counter:=!counter+1  done
					 in 
					let transaction = takeRight transaction !l in 
					(*ins*)
		let num = takeLeft transaction 1 in 
		let numint = int_of_bytes num in
		let length = 
			if numint = 253 (*fd*) then 2
			else if numint = 254 then 4 
			else if numint = 255 then 8
			else  0 in 
		let vout =takeLeft transaction (length +1 ) in 
			let voutint = intOfBytesVarInput vout in  
				let transaction = takeRight transaction (length +1) in 		
				let transactionOutput : (transactionOutputType list) = []  in
				let refTransactionOutput = ref transactionOutput in 
				let counter = ref 0 in 
				let l = ref 0 in 
					let temp = 
(* type transactionOutputType = {value: bytes; pkScript: bytes}*)
						while (!counter != voutint) do
							let valueCount = takeLeft transaction 8 in 	
							let transaction = takeRight transaction 8 in 	
							let scriptPubLength = takeLeft transaction 1 in
							let scriptPubLengthInt = int_of_bytes scriptPubLength in 
							let scriptPubLengthIntAdd = 	
								if scriptPubLengthInt = 253 (*fd*) then 2
								else if scriptPubLengthInt = 254 then 4 
								else if scriptPubLengthInt = 255 then 8
								else  0 in
							(*!*)let scriptPub = takeLeft transaction (scriptPubLengthIntAdd+1) in 
							let transaction = takeRight transaction (scriptPubLengthIntAdd +1 ) in 
							let output = {value = valueCount; pkScript = scriptPub;} 									in l := !l + 8 + 1 + scriptPubLengthInt; 
						List.append !refTransactionOutput [output]; counter:=!counter+1  done
					 in let transaction = takeRight transaction !l in 
					let nLockTime = takeLeft transaction 4 in 
	let result = {nVersion = nVersion;
inputcount  = numint;
inputs = transactionInput;
outputcount = voutint;
outputs = transactionOutput;
nLockTime = nLockTime}
 in result;;

let rec stringParsing (lst:bytes) (r:int list ref) (c: int ref)=
	if lst != empty_bytes then
	match lst.bl with
	[] -> ()
	| hd::tl -> let byte = int_of_bytes (takeLeft lst 1) in
		
	if byte > 0 && byte < 76 then
		begin 
			let bytes = takeRight lst (byte + 1) in c := !c + 1 + byte; stringParsing bytes r c end
	else if byte == 171 
		then begin 
			let bytes = takeRight lst 1 in c := !c +1; r := List.append !r [!c]; stringParsing bytes r c	
		end
	else	
		begin 
			let bytes = takeRight lst 1 in  c := !c +1; stringParsing bytes r c
		end
	else ();;	

let stringParsingMain (lst:bytes)  = 
	let counter = ref 0 in 
	let listSeparators : int list = [] in
	let listSeparatorsRef = ref listSeparators in
	let parsing = stringParsing lst listSeparatorsRef counter in !listSeparatorsRef;;

let rec generateSubScript (separators : int list) (counter : int) =
	match separators with 
	[] -> -1
	| hd::tl -> if counter > List.nth tl 0 && counter < hd then hd else generateSubScript tl counter;;

let rec generateSubScriptPrivate (lst: bytes) (separators: int list) (refList: bytes list ref) =
	match separators with 
	[] -> () 
	| hd::tl  -> 	let newByte = takeRight lst hd in 
	let endPosition = 
		if List.length tl = 0
		then length lst 
		else List.nth tl 0
	in let result = takeLeft newByte (endPosition - hd) in List.append !refList [result]; generateSubScriptPrivate lst tl refList;;
	
let rec listConcat(lst: bytes list) : bytes = 
	match lst with
	[] -> empty_bytes
	| hd::tl -> hd@|listConcat tl;;

let rec generateSubScript2 (lst:bytes) (separators: int list) (startIndex: int) = 
	if startIndex = -1 then lst
	else
		let refList : bytes list ref = ref [] in 
		generateSubScriptPrivate lst separators refList; listConcat !refList;;

let sigStrModification (signature:bytes) = 
	takeLeft signature 1;;

(* inputs : transactionInputType list;*)
(*type transactionType = {nVersion:bytes; inputcount : int; inputs : transactionInputType list; outputcount: int; outputs: transactionOutputType list;nLockTime : bytes}*)
let txCopyPrivate (transaction : transactionType) = 
	let newTransaction : transactionType = {nVersion = transaction.nVersion; inputcount = 0; inputs=[empty_bytes]; 
	outputcount = transaction.outputcount; outputs = transaction = transaction.outsputs; nLockTime = transaction.nLockTime;} in newTransaction;;
		

let op_false stack = Stack.push empty_bytes stack;;
let op_put stack bytes = Stack.push bytes stack;;
(*let op_checksig stack (scriptPubKey : bytes) (counter: int) = 
	let signature = Stack.pop stack in 
	let publicKey = Stack.pop stack in *)

(*let b = stringParse  "4104283338ffd784c198147f99aed2cc16709c90b1522e3b3637b312a6f9130e0eda7081e373a96d36be319710cd5c134aaffba81ff08650d7de8af332fe4d8cde20abac";;
*)
let rec byteparse (stack: bytes Stack.t) (lst:bytes) (scriptPubKey:bytes) (counter:int ref) (pkScriptPrev:bytes) =
	let byte = int_of_bytes (takeLeft lst 1)  in
	(*let counter : = !counter + 1 in *)
	let bts = takeRight lst 1 in 
	match lst.bl with 
		[] -> stack
		| hd::tl -> 
			if byte == 118 then
				begin byteparse stack bts scriptPubKey counter pkScriptPrev end
			else if byte == 0 then
				begin op_false stack; byteparse stack bts scriptPubKey counter pkScriptPrev end
			else if byte >0 && byte <76 then
				begin counter:=!counter +1 ; op_put stack (takeRight bts byte); let bts = takeRight bts byte in byteparse stack bts scriptPubKey counter pkScriptPrev end
			else if byte == 169 then
				begin byteparse stack bts scriptPubKey counter pkScriptPrev end
			else if byte == 171 then
				begin byteparse stack bts scriptPubKey counter pkScriptPrev end
			else if byte == 172 (*ac*)then
				begin print_endline "AC"; byteparse stack bts scriptPubKey counter pkScriptPrev end
			else byteparse stack bts scriptPubKey counter pkScriptPrev;;

let parsing_ptpk scriptPubKey scriptSig   =
	let stack : bytes Stack.t = Stack.create () in 
	let counter = ref 0 in byteparse stack scriptPubKey scriptPubKey counter scriptPubKey;;

(*pay to public key*)
(*let parse_ptpk (scriptPubKey:bytes) (scriptSig:bytes) = 
	let stack :  bytes Stack.t = Stack.create () in
	let counter= ref 0 in 
 	Stack.push scriptSig stack; byteparse stack counter counter scriptPubKey;;
*)
(*let a =parse_ptpk (stringParse "4104283338ffd784c198147f99aed2cc16709c90b1522e3b3637b312a6f9130e0eda7081e373a96d36be319710cd5c134aaffba81ff08650d7de8af332fe4d8cde20ac");;
*)
		
(*type transactionInputType = {prevout_hash: bytes; index : bytes; scriptSigB: bytes; sequence: bytes}*)
	

(*testing*)
	(*printBytes generateNVersion;;
	printBytes (bytesVarIntOfInt 1);;*)
	(*let bigNumber = 5000000000L;;
	printBytes (bytes_of_long_big_endian 8 bigNumber);;*)
	let ins = 1;;
	let outputs = 1;;
	let prevhash = [bytes_of_int 32 0];;	
	let prevAddress = [4294967295L];;
	let values = [5000000000L];;
	let transaction = createTransaction ins prevhash prevAddress outputs values [empty_bytes];;

	let test1 = parseTransaction transaction;;

	
	
	
	
	
