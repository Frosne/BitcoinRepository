open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;
open Cryptokit;;

(* printing*)


let rec print_stack (stack: int Stack.t) = 
	Stack.iter print_int stack;;

type transactionInputType = {prevout_hash: bytes; index : bytes; scriptSigB: bytes; sequence: bytes}

type transactionOutputType = {value: bytes; pkScript: bytes}

type transactionType = {nVersion:bytes; inputcount : int; inputs : transactionInputType list; outputcount: int; outputs: transactionOutputType list;nLockTime : bytes}

let stringConcat s1 s2 = 
	String.concat "" [s1;s2];;

 let print_bytes (x:bytes) : string =
    let s = get_cbytes x in
    let res = ref "" in
    for i = 0 to String.length s - 1 do
	let intR = int_of_char s.[i] in 
	let stringIntR = Printf.sprintf "%x" intR in 
	let stringIntR = if String.length stringIntR = 1 then stringConcat "0" stringIntR else stringIntR in 
      res := !res ^ stringIntR;
    done;
    !res

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

let formatNumber value = bytesVarIntOfInt value;;

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
						(*TODO!*)let scriptSigLengthIntAdd = 
							if scriptSigLengthIntAdd = 0 then scriptSigLengthInt 
							else let subTransaction = takeRight transaction 1 in let length =  takeRight transaction scriptSigLengthIntAdd in int_of_bytes length in						
							let scriptSig = takeLeft transaction (scriptSigLengthIntAdd+1) in 
							let transaction = takeRight transaction (scriptSigLengthIntAdd+1) in 
							let seq = takeLeft transaction 4 in
							let transaction = takeRight transaction 4 in
							let input = {prevout_hash = hash; index = n; scriptSigB = scriptSig; sequence = seq} 									in l := !l + 32 + 4 + 1 + scriptSigLengthInt + 4; 
						refTransactionInput := List.append !refTransactionInput [input]; 
 counter:=!counter+1  done
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
							let transaction = takeRight transaction !l in
							let valueCount = takeLeft transaction 8 in 	
							let transaction = takeRight transaction 8 in 	
							let scriptPubLength = takeLeft transaction 1 in
							let scriptPubLengthInt = int_of_bytes scriptPubLength in 
							let scriptPubLengthIntAdd = 	
								if scriptPubLengthInt = 253 (*fd*) then 2
								else if scriptPubLengthInt = 254 then 4 
								else if scriptPubLengthInt = 255 then 8
								else  0 in
let scriptPubLengthIntAdd = 
							if scriptPubLengthIntAdd = 0 then scriptPubLengthInt 
							else let subTransaction = takeRight transaction 1 in let length =  takeRight transaction scriptPubLengthIntAdd in int_of_bytes length in	
							(*!*)let scriptPub = takeLeft transaction (scriptPubLengthIntAdd+1) in 
							let transaction = takeRight transaction (scriptPubLengthIntAdd+1) in 
							let output = {value = valueCount; pkScript = scriptPub;}  in 
 							l := !l + 8 + 1 + scriptPubLengthInt;  
						refTransactionOutput := List.append !refTransactionOutput [output]; counter:=!counter+1  done
					 in let transaction = takeRight transaction !l) in 	
					let nLockTime = takeLeft transaction 4 in 
	let result = {nVersion = nVersion;
inputcount  = numint;
inputs = !refTransactionInput;
outputcount = voutint;
outputs = !refTransactionOutput;
nLockTime = nLockTime}
 in result;;



let string_of_bytes (x:bytes) : string =
    let s = get_cbytes x in
    let res = ref "" in
    for i = 0 to String.length s - 1 do
      res := !res ^ (Printf.sprintf "%x" (int_of_char s.[i]));
    done;
    !res;;

let transactionEndianChange(transaction:bytes)  = 
	let counter = ref 0 in 
	let ctransaction = transaction in 
	let str = string_of_bytes ctransaction in 
	let length = String.length str in 
	let length = length /4 in 
	let l = String.length str in  
	while !counter <= length do 
	let c1 = str.[!counter] in 
	let c2 = str.[!counter+1] in
	str.[!counter] <- str.[l-2 - !counter]; 
	str.[!counter+1] <- str.[l-1 - !counter]; 
	str.[l-1 - !counter] <- c2; 
	str.[l-2 - !counter] <- c1; 	
	counter := !counter +2; done; abytes str;; 		

let sha256 b = hash SHA256 b;;
let sha2 b = sha256 (sha256 b);;

let hex s = transform_string (Hexa.decode()) s
let tohex s = transform_string (Hexa.encode()) s

let transactionHash transaction =( sha2 transaction);;

let generateInput(transaction: bytes ) (index :int ) : bytes = 
	let index = index -1 in 
	let parsedTransaction = parseTransaction transaction in 
	let hash = transactionHash transaction in 
	let indexVar = bytes_of_int_big_endian 4 index  in 
	let outputs = parsedTransaction.outputs in
	let output = List.nth outputs index in 
	let script = output.pkScript in 
	let sequence = stringParse "ffffffff" in 
	hash @| indexVar @| script @|sequence;;

let generateInputS(transaction: bytes ) (index :int )(script:bytes) : bytes = 
	let index = index -1 in 
	let hash = transactionHash transaction in 
	let indexVar = bytes_of_int_big_endian 4 index  in 
	let script = script in 
	let length = length script in 
	let length = bytes_of_int 1 length in
	let script = length@|script in  print_endline " --- Script itself --- "; printBytes script; print_endline "" ;
	let sequence = stringParse "ffffffff" in 
	hash @| indexVar @| script @|sequence;;


let rec generateInputs (transactions : bytes list) (inputs : int list) : bytes = 
	let scripts =  ref empty_bytes in 
	let counter = ref 0 in let result = 
	while !counter <= (List.length transactions -1)  do 
	let inp = generateInput (List.nth transactions !counter) (List.nth  inputs  !counter ) in 
	scripts := !scripts @| inp;
	counter := !counter +1 ; done in !scripts;;

let rec generateInputsS (transactions : bytes list) (inputs : int list)(script:bytes) : bytes = 
	let scripts =  ref empty_bytes in 
	let counter = ref 0 in let result = 
	while !counter <= (List.length transactions -1)  do 
	let inp = generateInputS (List.nth transactions !counter) (List.nth  inputs  !counter )(script ) in 
	scripts := !scripts @| inp;
	counter := !counter +1 ; done in !scripts;;


let generateOutputPtPH (amount: int64) (address : bytes) : bytes = 
	let amount = bytes_of_long_big_endian 8 amount in 
	let script = amount @| bytes_of_int 1 25 @| bytes_of_int 1 118 @| bytes_of_int 1 169 @| address @| bytes_of_int 1 136@| bytes_of_int 1 172 in script;;

let rec generateOutputs(amounts : int64 list) (addresses : bytes list) : bytes = 
	let scripts = ref empty_bytes in 
	let counter = ref 0 in let result =
	while !counter <= (List.length amounts - 1) do
	let out = generateOutputPtPH (List.nth amounts !counter) (List.nth addresses !counter) in 
	scripts := !scripts @| out; 
	counter := !counter +1; done in !scripts;;

let usedCurve = ECC_P521;;
let params = {curve=usedCurve; point_compression = false;};;
let createKey (params:ec_params) : ec_key = ec_gen_key params;; 
let key = createKey params;;
let signTransaction transaction = ecdsa_sign None key transaction;;

let createTransaction (transactions : bytes list)(indexes : int list) (amounts : int64 list) (keys: bytes list)    =
	let nVersion = generateNVersion in
	let inputCount = List.length transactions in 
	let outputCount = List.length amounts in
	let vinCount = formatNumber inputCount in 
	let voutCount = formatNumber outputCount in
	let inputs = generateInputs transactions indexes in
	let outputs = generateOutputs amounts keys in 
	let locktime = bytes_of_int 4 0 in 
	let hash = bytes_of_int_big_endian 4 1 in 
	let transaction13 =  nVersion  @| vinCount @| inputs @| voutCount @| outputs @| locktime @| hash in 
	let transaction14 = sha2 transaction13 in 
		print_endline"Transaction before being hashed:"; printBytes(transaction13); print_endline "";
		print_endline"Transaction after being hashed:"; printBytes (transaction14); print_endline "";
	let signed = signTransaction transaction14 in
		print_endline"Transaction after being signed:"; printBytes(signed); print_endline "";
	let signed = signed @| bytes_of_int 1 1 in 
	let lengthScript = length signed in 
	let lengthScript = bytes_of_int 1 lengthScript in 
	let publicKey = bytes_of_int 1 4 @| (takeLeft key.ec_point.ecx 32) @| (takeLeft key.ec_point.ecy 32) in (*!!!!*)
	let publicKeyLength = length publicKey in 
	let publicKeyLength = bytes_of_int 1 publicKeyLength in 
		print_endline"Key:"; printBytes(takeLeft key.ec_point.ecx 32); print_endline""; printBytes (takeLeft key.ec_point.ecy 32); print_endline""; 
	let script = lengthScript @| signed @| publicKeyLength @| publicKey in
	let inputs = generateInputsS transactions indexes script in 
	nVersion @| vinCount @| inputs @| voutCount @| outputs @|locktime
	;;

let transactionOld = stringParse "010000000126c07ece0bce7cda0ccd14d99e205f118cde27e83dd75da7b141fe487b5528fb000000008b48304502202b7e37831273d74c8b5b1956c23e79acd660635a8d1063d413c50b218eb6bc8a022100a10a3a7b5aaa0f07827207daf81f718f51eeac96695cf1ef9f2020f21a0de02f01410452684bce6797a0a50d028e9632be0c2a7e5031b710972c2a3285520fb29fcd4ecfb5fc2bf86a1e7578e4f8a305eeb341d1c6fc0173e5837e2d3c7b178aade078ffffffff02b06c191e010000001976a9143564a74f9ddb4372301c49154605573d7d1a88fe88ac00e1f505000000001976a914010966776006953d5567439e5e39f86a0d273bee88ac00000000";;

(*let hashed = transactionHash transactionOld;;
print_endline "Hash: ";;
printBytes hashed;;
print_endline "Hash:";;

*)
let z : int64 = Int64.of_int 99900000;;
let transactionTest = createTransaction [transactionOld][2][z][stringParse "14097072524438d003d23a2f23edb65aae1bb3e469"];;
(*print_endline "";;
printBytes(transactionTest);;
print_endline "";;*)

print_endline "";;print_endline "";;
printBytes  ((parseTransaction transactionTest).nLockTime);;
