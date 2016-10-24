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

let int_of_bytes_big_endian(b:bytes) : int = 
	let x = ref 0 in
	let c = get_cbytes b in 
	for y = b.length -1 downto 0 do
	x := 256 * !x + (int_of_char (String.get c y))
	done;
	!x

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

let print_list_int (lst : int list) = 
	List.iter print_int lst;;

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
let sha2 b = sha256 (sha256 b);;
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
					 in let transaction = takeRight transaction !l in 	
					let nLockTime = takeLeft transaction 4 in 
	let result = {nVersion = nVersion;
inputcount  = numint;
inputs = !refTransactionInput;
outputcount = voutint;
outputs = !refTransactionOutput;
nLockTime = nLockTime}
 in result;;

let transactionParse transactions =
	let lst  = [] in 
	let rec matching transactions  =
		match transactions with
			hd::tl -> List.append [(parseTransaction hd)] lst; matching tl
			| [] -> lst
	in matching transactions;;

let getByteInBytes (data : bytes) (i : int) = 
	let v = takeRight data i 
	in  takeLeft v 1;;


let parseScript (script:bytes) (ckSig:int ref) =
	let a = 0 in let b = (script.length -1) in
	let lstCodeSep = ref [] in 
	let rec aux i = 
		if i<=b then begin 
			let bts = getByteInBytes script i in 
			let bts = int_of_bytes bts in 
				if ((bts > 0) && (bts <76))
					then  begin aux(i+bts+2) end 
				else if (bts == 171) 
					then begin lstCodeSep := List.append !lstCodeSep [i];  aux(i+1) end
				else if (bts == 172) 
					then begin ckSig := i; aux(i+1) end
				else aux(i+1)
		end
	in aux a; !lstCodeSep;;

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
		let pred elem = elem >= position in 
		let tup = List.partition pred parseScr in 
		let currentTuple = fst tup in 
			for i = ((List.nth currentTuple 0)+1) to (script.length-1) do 
				begin 
				let tempPred elem = elem == i in 
					if not( List.exists tempPred currentTuple )
					then scriptAfterTreatment := !scriptAfterTreatment @| (getByteInBytes script i)
				end
 			done;
			end
		else scriptAfterTreatment := script; 
	!scriptAfterTreatment;;

let outputToBytes b= empty_bytes;;
(*
type transactionInputType = {prevout_hash: bytes; index : bytes; scriptSigB: bytes; sequence: bytes}

type transactionOutputType = {value: bytes; pkScript: bytes}

type transactionType = {nVersion:bytes; inputcount : int; inputs : transactionInputType list; outputcount: int; outputs: transactionOutputType list;nLockTime : bytes}*)

let usedCurve = ECC_P521;;
let params = {curve=usedCurve; point_compression = false;};;
let createKey (params:ec_params) : ec_key = ec_gen_key params;; 
let key = createKey params;;
let signTransaction transaction = ecdsa_sign None key transaction;;


(*let ecdsa_verify hash_alg key input signature =
  let input = match hash_alg with
    | Some hash_alg -> hash hash_alg input
    | None -> input
  in
  let key = ssl_key_of_key key in
  ocaml_ecdsa_verify key (string_of_bytes input) (string_of_bytes signature)



ec_params = oldKey.ec_params; ec_point = oldKey.ec_point; ec_priv =
*)

let usedCurve = ECC_P521;;
let params = {curve=usedCurve; point_compression = false;};;

let verify transactionSigned transaction publicKey = ecdsa_verify None key transaction transactionSigned;;

let computeKey transaction input = 
	let transactionInput = transaction.inputs in 
	let transactionInput = List.nth transactionInput input in 
	let key = transactionInput.scriptSigB in 
	let key = takeRight key 1 in (*without length*) 
	let l = takeLeft key 1 in (*length of script*)
	let l = int_of_bytes l in 
	let key = takeRight key 1 in 
	let key = takeRight key l in 
	let l = takeLeft key 1 in (*length of keys *)
	let l = int_of_bytes l in
	let key = takeRight key 1 in (*length - length of keys*)
	let key = takeRight key 1 in (*we added 1 byte of 1*)
	let usedCurve = ECC_P521 in let params = {curve=usedCurve; point_compression = false;} in 
	let l = l/2 in 
	let x = takeLeft key l in 
	let y = takeRight key l in {ecx = x; ecy = y};;


let verify transactionSigned (transaction:bytes) transactionNew  =
	let point = computeKey transactionNew 0 in
	let key = 
	{ec_params = params; ec_point = point; ec_priv = None} in verify transactionSigned transaction key;;

let takeInputNumber transaction inputNumber =
	let inputNumber = 0 in 
	let transactionInputs = transaction.inputs in 
	let transactionInput = List.nth transactionInputs inputNumber in 
(* in human representaation it's input + 1 *)
	let input = transactionInput.index in
	int_of_bytes_big_endian input;;

let takeOutputScript transaction outputNumber = 
	let transactionOutputs = transaction.outputs in 
	let transactionOutput = List.nth transactionOutputs outputNumber in 
	transactionOutput.pkScript;;

let bytesVarIntOfInt (number: int) : bytes 	= 
	if number < 253 then bytes_of_int 1 number
	else if (number < 65536) then 
		let number = bytes_of_int 3 number in
let result = xor 3 number (bytes_of_int 3 16580608) in result
		else bytes_of_int 1 0;;

let rec inputToBytes inputs result = 
	match inputs with
	hd::l -> inputToBytes l (result @| hd.prevout_hash @| hd.index @| hd.scriptSigB @| hd.sequence) 
	| [] -> result;;

let rec outputToBytes outputs result = 
	match outputs with 
	hd::l -> outputToBytes l (result @| hd.value @| hd.pkScript) 
	| [] -> result;;

let transactionToBytes transaction = transaction.nVersion @| 
	(bytesVarIntOfInt transaction.inputcount) @| 
	(inputToBytes transaction.inputs empty_bytes) @|  
	(bytesVarIntOfInt transaction.outputcount ) @| 
	(outputToBytes transaction.outputs empty_bytes) @| transaction.nLockTime;;


let verifyOneInput (stack: bytes Stack.t ref) (data : bytes ref) (transactionNew : transactionType) (transactionOld : transactionType) (counter : int) =
	let pubKey = Stack.pop !stack in 
	let sign = Stack.pop !stack in 
(*now it's a list*)
	let numberOutput = takeInputNumber transactionNew counter in 
	let scriptOutput = takeOutputScript transactionOld numberOutput in 
	let scriptOutput = regenerateList scriptOutput in
	let hashSign = bytes_of_int_big_endian 4 1  in 
	(*let hashSign = takeRight  sign 1 in change*)
		let hashSign = int_of_bytes_big_endian hashSign in 
		let hashSign = bytes_of_int_big_endian 4 hashSign in 
	(*let sign = takeLeft sign 1 in*)
	let txcopy = transactionNew in 
	let txcopy = txcopy.inputs in 
	let txcopy = List.nth  txcopy 0 in 
	let newInput = {prevout_hash = txcopy.prevout_hash; index = txcopy.index; scriptSigB = scriptOutput; sequence = stringParse "ffffffff"} in 
	let newTransactionToSign = 
		{nVersion = transactionNew.nVersion; inputcount = transactionNew.inputcount; inputs = [newInput]; outputcount = transactionNew.outputcount; outputs = transactionNew.outputs; nLockTime = transactionNew.nLockTime} in 
	let newTransactionToSign = transactionToBytes newTransactionToSign @| hashSign in 
	let hashed = sha2 newTransactionToSign in true;;
	(*in verify sign hashed publicKey;;*)
(*let verify transactionSigned transaction (transactionInput:transactionInputType)*)
	(*verify sign txcopy transactionNew;; *)

	
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
let stackRef = ref stack;;
let data = ref empty_bytes ;;

let transactionOld = parseTransaction( stringParse("010000000126c07ece0bce7cda0ccd14d99e205f118cde27e83dd75da7b141fe487b5528fb000000008b48304502202b7e37831273d74c8b5b1956c23e79acd660635a8d1063d413c50b218eb6bc8a022100a10a3a7b5aaa0f07827207daf81f718f51eeac96695cf1ef9f2020f21a0de02f01410452684bce6797a0a50d028e9632be0c2a7e5031b710972c2a3285520fb29fcd4ecfb5fc2bf86a1e7578e4f8a305eeb341d1c6fc0173e5837e2d3c7b178aade078ffffffff02b06c191e010000001976a9143564a74f9ddb4372301c49154605573d7d1a88fe88ac00e1f505000000001976a914010966776006953d5567439e5e39f86a0d273bee88ac00000000"));;

let transactionNew = parseTransaction(stringParse("0100000001eccf7e3034189b851985d871f91384b8ee357cd47c3024736e5676eb2debb3f201000000cf8c308188024200ae0e580452d62234f8ee8b19495faabd0ab8261e1b3383459bea84e8dff7c29c62eecb2d8644431ab9d6cf767b0ab4d9153c4c858b3e87edf166fc3957ebfb3e0f0242013581052641b809882c41d080a6dfcf9e153e73b2870634d9acba7596d65f1294be0952133f7c8997dde48c4d3f13b3e819948e61bb6430af1087c891aeed781a4601410401ae48443586db2077211b21ccd5c11694203da633552ae95049c683efc7416e01863c5d4f056089742a641a7279b135d294a969449238c61ee618896507b13cffffffff01605af405000000001976a914097072524438d003d23a2f23edb65aae1bb3e46988ac00000000"));;

let ver = verifyOneInput stackRef data transactionNew transactionOld 0;;
	
let computeScript transaction input = 
	let script = transaction.inputs in 
	let script = List.nth script input in 
	let script = script.scriptSigB in 
	let script = takeRight script 1 in 
	let lengthOfScript = takeLeft script 1 in 
	let lengthOfScript = int_of_bytes lengthOfScript in
	let script = takeRight script 1 in 
	takeRight script lengthOfScript;;

let bytesToList script = [];;

let listToBytes = empty_bytes;;

let verifyOneInputTemp transactionNew transactionOld counter = 
	let input = List.nth transactionNew.inputs counter in 
	let input = input.index in 
	let input = int_of_bytes input in
	let script = computeScript transactionOld input in 
	let script = bytesToList script in 
	let flag = true in 
	let stack = Stack.create () in
	let stack = ref stack in 
	let data = ref empty_bytes in 
		let rec scriptParse script = 
			match script with 
			hd::tl -> 
				let elem = 5 in
				let result = 
					if elem == 168 then _OpSHA256 stack data 
					else if elem == 169 then _OpSHA256 stack data
					else if elem >1 and elem <75 then 
					let data = makeBytesFromList tl elem in 
		  _OpPush stack take
					else -1
				in
				if result == 0 then scriptParse tl else false
			| [] -> flag
		in scriptParse script;;

let getTransactionByHash hash = hash;; 

let verifyPublic transaction = 
	let rec verifyPrivate transaction transactionList counter flag  = 
		match transactionList with 
			hd::tl -> verifyPrivate transaction transactionList (counter+1) (flag && verifyOneInputTemp transaction (List.nth transactionList counter) counter)
			| [] -> flag in 
	let f a = a.scriptSigB in 
	let hashes = List.map f transaction.inputs in 
	let transactions = List.map getTransactionByHash hashes in 
	let transactions = List.map parseTransaction transactions
	in verifyPrivate transaction transactions 0 true;;

	(* in fact, there i need to get the transactions by hashed 
	for now i have no idea how to do this*)


