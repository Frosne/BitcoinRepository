open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;

type transactionInputType = {hash: bytes; index : int; signedTransaction: bytes; publicKey: ec_key;}

type transactionOutputType = {value: int; pkScript: ec_key}

type transactionType = {hash:bytes; inputs : transactionInputType list; outputs: transactionOutputType list;}




(*InputOutputCurve*)
let ssl_name_of_curve = function
  | ECC_P256 -> "prime256v1"
  | ECC_P384 -> "secp384r1"
  | ECC_P521 -> "secp521r1"
let printCurve curve =
	print_string (ssl_name_of_curve curve);;
let usedCurve = ECC_P521;;
(*printCurve usedCurve;;*)
(*InputOutputCurve*)

(*InputOutputKey*)
(*
type ec_key = {
     ec_params : ec_params;
     ec_point : ec_point; (* a.k.a. the public key *)
     ec_priv : bytes option;
}
*)

let params = {curve=usedCurve; point_compression = false;};;
let createKey (params:ec_params) : ec_key = 
	ec_gen_key params;;

let getPrivateKey key = 
	match key with 
	| None -> ""
	| Some v ->  string_of_bytes v;;

let printPrivateKey key =
	print_string(getPrivateKey key);;

let printEcPoint ec_point =
	print_string(string_of_bytes ec_point.ecx); print_string(string_of_bytes ec_point.ecy);;

let printKey (key:ec_key) =
	print_string "Curve: "; printCurve key.ec_params.curve; print_string("Private Key: "); printPrivateKey key.ec_priv; print_string("Public key: "); printEcPoint key.ec_point;;

let makePublicKeyPrivate (key:ec_key) =
	 let key = {ec_params = key.ec_params; ec_point = key.ec_point; ec_priv = None} in key;;

let serializeKey key = 
	key.ec_point.ecx @| key.ec_point.ecy;;

(*InputOutputKey*)


let outputEntity_to_bytes (outputEntity : transactionOutputType) : bytes =  
	let bytesValue = bytes_of_int 4 outputEntity.value in
		let bytesOut = bytesValue@|serializeKey outputEntity.pkScript in bytesOut;;

let rec output_to_bytes (output: transactionOutputType list) : bytes = 
	match output with 
		[] -> empty_bytes
		|hd::ls -> outputEntity_to_bytes hd @| output_to_bytes ls;;

let inputEntity_to_bytes (inputEntity : transactionInputType) : bytes = 
	let bytesIndex = bytes_of_int 4 inputEntity.index in 
		let bytesOut = inputEntity.hash @| bytesIndex @| inputEntity.signedTransaction @|serializeKey inputEntity.publicKey in bytesOut;;

let rec input_to_bytes (input : transactionInputType list) : bytes = 
	match input with 
		[] -> empty_bytes
		| hd::ls -> inputEntity_to_bytes hd @| input_to_bytes ls;;

let transaction_to_byte (transaction: transactionType) : bytes = 
	transaction.hash @| input_to_bytes transaction.inputs @| output_to_bytes transaction.outputs;;

let sha256 b = hash SHA256 b;;
let hashTransactionBytes b = sha256 (sha256 b);;

let hashTransaction (transaction: transactionType) =
	let transaction = transaction_to_byte transaction in
		hashTransactionBytes transaction;;

let signTransaction (privateKey:ec_key) (transaction:transactionType)  = 
	let transaction = transaction_to_byte transaction in 
		ecdsa_sign (Some SHA256) privateKey transaction;;

let verifyTransaction (privateKey:ec_key) (transaction:transactionType) (transactionToVerify : bytes) =
	let transaction = transaction_to_byte transaction in
			ecdsa_verify (Some SHA256) privateKey transaction transactionToVerify;;

(*wtf*)
(*let verifyTransactionInput (publicKey:ec_key) (transactionInput: bytes) (transactionToVerify : transactionType) = 
	let transactionToVerify = transaction_to_byte transactionToVerify in
		ecdsa_verify (Some SHA256) publicKey transactionInput transactionToVerify;;
*)


let verifyInputEntTrans (transactionInput : transactionInputType ) (transaction:transactionType) : bool = 
	verifyTransaction transactionInput.publicKey transaction transactionInput.signedTransaction && 
		transactionInput.publicKey.ec_params = (List.nth transaction.outputs transactionInput.index).pkScript.ec_params && transactionInput.publicKey.ec_point =  (List.nth transaction.outputs transactionInput.index).pkScript.ec_point;;


let rec listFindTransaction (transactionList: transactionType list) (transactionInput:transactionInputType) : transactionType = 
	let hash = transactionInput.hash in
		match transactionList with
			[] -> raise Not_found	
			|hd::lst -> if hd.hash = hash then hd else listFindTransaction lst transactionInput;;

let verifyInputEnt (transactionInput: transactionInputType) (transactionList : transactionType list) : bool = 
	let transactionReferenced = listFindTransaction transactionList transactionInput in 
		verifyInputEntTrans transactionInput transactionReferenced;;

let rec verifyInput (transactionInputList: transactionInputType list) (transactionList : transactionType list) : bool =
	match transactionInputList with 
	[] -> true
	| hd:: lst -> if verifyInputEnt hd transactionList = true then verifyInput lst transactionList else false;;


	

let verifyOutputEnt (outputvar: transactionOutputType) : bool = 
	if outputvar.value > 0 then true else false;;


let rec verifyOutputPrivate(outputs : transactionOutputType list) (index:int) : int =
	match outputs with 
	[] -> -1
	| e::t -> 
		let currectVerification =  verifyOutputEnt e in 
			let index = index + 1 in 
				if currectVerification = true 
					then verifyOutputPrivate t index 
				else index;;

let verifyOutputPublic (outputs : transactionOutputType list) = verifyOutputPrivate outputs 0;;

let rec verifyOutput(outputs : transactionOutputType list) : bool =
	match outputs with 
	[] -> true	
	| e::t -> 
		let currectVerification =  verifyOutputEnt e in 
			if currectVerification = true 
				then verifyOutput t 
			else false;;



(*let rec verifyInput (transactionInputList: transactionInputType list) (transactionList : transactionType list) : bool =
	match transactionInputList with 
	[] -> true
	| hd:: lst -> if verifyInputEnt hd transactionList = true then verifyInput lst transactionList else false;;
*)
let verifyTransaction (transaction: transactionType) (transactionList : transactionType list) = 
	(verifyInput transaction.inputs transactionList)  && (verifyOutput transaction.outputs);;




let print_boolean (a:bool)= 
	print_string(string_of_bool a);;

let userKey = createKey params;;

let input = [{hash = bytes_of_int 4 0; index = 1; signedTransaction = bytes_of_int 4 1; publicKey = createKey params}];;
let output = [{value = 10; pkScript = userKey};{value = 15; pkScript = userKey}];;
let transaction = {hash = bytes_of_int 4 0; inputs = input; outputs = output;};;

let input1 = [{hash = bytes_of_int 4 0; index = 1; signedTransaction = bytes_of_int 4 1; publicKey = createKey params}];;
let output1 = [{value = 10; pkScript = userKey};{value = 15; pkScript = userKey}];;
let transaction1 = {hash = bytes_of_int 4 1; inputs = input1; outputs = output1;};;

let input2 = [{hash = bytes_of_int 4 0; index = 1; signedTransaction = bytes_of_int 4 1; publicKey = createKey params}];;
let output2 = [{value = 10; pkScript = userKey};{value = 15; pkScript = userKey}];;
let transaction2 = {hash = bytes_of_int 4 2; inputs = input2; outputs = output2;};;

let lstTr = [transaction; transaction1; transaction2];;


let a = signTransaction userKey transaction;;
let b = signTransaction userKey transaction1;;

let inputPart = {hash = bytes_of_int 4 0; index = 0; signedTransaction = a; publicKey = userKey;};;
let inputPart2  = {hash = bytes_of_int 4 1; index = 0; signedTransaction = b; publicKey = userKey;};;
let input_ = [inputPart; inputPart2];;
let output_ = [{value = 10; pkScript = createKey params}];;
let transaction_ = {hash = empty_bytes; inputs = input_; outputs = output_;};;

(*print_string (print_bytes htest);;
print_string(string_of_bytes (hashTransactionBytes ft));;*)



