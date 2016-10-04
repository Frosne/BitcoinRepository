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

(*Crypto *)
let sha256 b = hash SHA256 b;;
let sha1 b = hash SHA1 b;;
let tohex s = transform_string (Hexa.encode()) s
let ripOriginal (s: string) = 
	tohex (hash_string (Hash.ripemd160()) s) ;;

let ripemd160 (data : bytes) =
	let dataString = get_cbytes data in 
	let hashed = ripOriginal dataString in 
	stringParse hashed;;

let _OpSHA256(stack:bytes Stack.t ref) (data:bytes ref) : int = 
	try 
		let value = Stack.pop !stack in 
		let hashedvalue = sha256 value in 
		Stack.push hashedvalue !stack; 0
	with _-> -1;;

let _OpRIPEMD160(stack: bytes Stack.t ref) (data: bytes ref) : int = 
	try 
		let value = Stack.pop !stack in 
		let hashedvalue = ripemd160 value in 
		Stack.push hashedvalue !stack; 0
	with _-> -1;;

let _OpSHA1 (stack: bytes Stack.t ref) (data:bytes ref) : int = 
	try 
		let value = Stack.pop !stack in 
		let hashedvalue = sha1 value in 
		Stack.push hashedvalue !stack; 0
	with _-> -1;;

let _OpHASH160 (stack:bytes Stack.t ref) (data:bytes ref) : int = 
	try 
		let value = Stack.pop !stack in 
		let hashedvalue = sha256 value in 
		let hashedvalue = ripemd160 hashedvalue in 
		Stack.push hashedvalue !stack; 0
	with _-> -1;;

let _OpHASH256 (stack:bytes Stack.t ref) (data:bytes ref) : int = 
	try
		let value = Stack.pop !stack in 
		let hashedvalue = sha256 value in 
		let hashedvalue = sha256 hashedvalue in 
		Stack.push hashedvalue !stack; 0
	with _-> -1;;

let _OpCodeSep (stack:bytes Stack.t ref) (data:bytes ref) : int = 0;;

(*let _OpCheckSigOneTransaction(stack:bytes Stack.t ref) (data:bytes ref)(transactionOld : transactionType)(transactionNew : transactionType) : int =
		let publicKey = Stack.pop !stack in 
		let signature = Stack.pop !stack in
*)


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
					 in 
					let nLockTime = takeLeft transaction 4 in 
	let result = {nVersion = nVersion;
inputcount  = numint;
inputs = !refTransactionInput;
outputcount = voutint;
outputs = !refTransactionOutput;
nLockTime = nLockTime}
 in result;;

(*let special_size l = 
   let rec size_aux previous l = match l with  
       [] -> 0 
     |  _::l1  -> size_aux (l::previous)
   in size_aux [] l ;;
*)
let transactionParse transactions =
	let lst  = [] in 
	let rec matching transactions  =
		match transactions with
			hd::tl -> List.append [(parseTransaction hd)] lst; matching tl
			| [] -> lst
	in matching transactions;;
	
(* type transactionInputType = {prevout_hash: bytes; index : bytes; scriptSigB: bytes; sequence: bytes}

type transactionOutputType = {value: bytes; pkScript: bytes}

type transactionType = {nVersion:bytes; inputcount : int; inputs : transactionInputType list; outputcount: int; outputs: transactionOutputType list;nLockTime : bytes}
*)

(*for variable = start_value to end_value do
  expression
done*)


let getByteInBytes (data : bytes) (i : int) = 
	let v = takeRight data i 
	in takeLeft v 1;;

let parseScript (script: bytes)(ckSig : int ref) = 
	let lstCodeSep = [] in 
	for i = 0 to (script.length -1) do
		let bts = getByteInBytes script i in 
		let bts = int_of_bytes bts in print_int bts;
		if ((bts > 0) && (bts <76))
			then i = i+ bts
		else if (bts == 171) 
			then begin lstCodeSep = List.append [1] lstCodeSep; i = i+1 end
		else if (bts == 172) 
			then begin ckSig := i; i = i+1 end
		else i = i+1
	done;lstCodeSep;;	

let findBetween position lst = 
	let max = ref (-1) in 
	let rec listSearch position  lst = 
		match lst with 
		hd::tl -> 
			if (hd<position) then 
				begin max := hd; listSearch position tl end 
			else listSearch position tl
		| [] -> !max
	in listSearch position lst;;

let regenerateList script : bytes= 
	let scriptAfterTreatment = ref empty_bytes in 
 	let positionOfCkSig = ref 0 in 
	let parseScr = parseScript script positionOfCkSig in 
		if (List.length parseScr > 0) then
			 begin
		let position = findBetween !positionOfCkSig parseScr in 
		let pred elem = elem > position in 
		let tup = List.partition pred parseScr in 
		let currentTuple = snd tup in 
			for i = List.nth currentTuple 0 to (script.length-1) do 
			begin 
				let tempPred elem = elem == i in 
					if not( List.exists tempPred currentTuple )
				then scriptAfterTreatment := !scriptAfterTreatment @| (getByteInBytes script i)
			end
 			done;
		end;
	!scriptAfterTreatment;;


let verifyOneInput (stack: bytes Stack.t ref) (data : bytes ref) (transactionNew : transactionType) (transactionOld : transactionType) (counter : int) =
	let pubKey = Stack.pop !stack in 
	let sign = Stack.pop !stack in 
(*now it's a list*)
	let scriptOutputs = transactionOld.outputs in 
	let numberOutput = transactionNew.inputs in 
	let numberOutput = List.nth numberOutput counter in 
	let numberOutput = numberOutput.index in 
	let numberOutput = int_of_bytes numberOutput in (* Here change endian*)
	let scriptOutput = List.nth scriptOutputs numberOutput in 
	let scriptOutput = scriptOutput.pkScript in 
	
 true;;	
		
let _OpCheckSig (stack:bytes Stack.t ref) (data:bytes ref) (transactionNew:transactionType)(transactions : bytes list) = 
	let flag = true in 
	let counter = 0 in 
		let transactionList = transactionParse transactions in 
		let rec all_transactions stack data transactionNew transactionList = 
			match transactionList with
				hd::tl -> flag = flag && verifyOneInput stack data transactionNew hd counter; counter = counter +1;  all_transactions stack data transactionNew tl
				| [] -> flag
		in all_transactions stack data transactionNew transactionList;;

let stack : bytes Stack.t = Stack.create ();;


(*print_string(Int64.to_string (long_of_bytes (stringParse "ffffffff")));;*)


(* /Main functionality*) 



