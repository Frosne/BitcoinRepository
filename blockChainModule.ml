open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;
open Cryptokit;;
open StdLabels;;



type transactionInputType = {prevout_hash: bytes; index : bytes;}
type transactionType = {inputcount : int; inputs : transactionInputType list; flagExit:bytes }

(*standart *)
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

let random bound = 
	let temp = int_of_bytes (CoreCrypto.random 1) in 
	((temp mod bound)+1);;

let printBytes bytes= print_string(print_bytes bytes);;

let takeLeft (b: bytes) i = 
	let splitted = split b i in
		let part = fst splitted in part;;

let takeRight(b:bytes) i = 
	 if (length b == 1) then	empty_bytes
else
	let splitted = split b i in
		let part = snd splitted in part;;

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
(*standart*)

(* Set *)
type bytes = Platform.Bytes.bytes;;

let compare (a:bytes) (b:bytes) = 
	let temp = equalBytes a b in
	match temp with 
	true -> 0
	| false -> 1;;

module HashSet = Set.Make (struct 
let compare = compare
type t = bytes
end);;

let h = ref HashSet.empty;;

let hashAdd ~elem ~setRef = 
	let set = HashSet.add elem !setRef in setRef:=set;;

let hashTakeRandom ~set = 
	let lst = HashSet.elements set in print_int(List.length lst);
	let rnd = random (List.length lst) in List.nth lst rnd;;

(* Set *)

(* IO *)

let parseTransaction transaction = 
	let inputCount = takeLeft transaction 4 in 
	let inputCount = int_of_bytes inputCount in 
	let transaction = takeRight transaction 4 in 
		printBytes transaction; print_endline"";
	let transactionInput : (transactionInputType list) = []  in
	let refTransactionInput = ref transactionInput in 
		let counter = ref 0 in 
		while !counter != inputCount do
			let hash = takeLeft transaction 20 in 
			let transaction = takeRight transaction 20 in 
			let index = takeLeft transaction 4 in
			let transaction = takeRight transaction 4 in
			let input =  {prevout_hash = hash; index = index} in 
		 refTransactionInput := List.append !refTransactionInput [input]; counter := !counter+1
		done;
	{inputcount = inputCount; inputs = !refTransactionInput; flagExit = bytes_of_int 4 0};;
							
let printInput input = print_string
	"Hash: "; 
	print_string (print_bytes input.prevout_hash);
	print_string "  ";
	print_string "Index: ";
	print_endline (print_bytes input.index);;

let printTransaction transaction = 
	let rec printInputs inputs= 
		match inputs with
		hd::tl -> printInput hd; printInputs tl
		| [] -> print_endline ""
	in print_endline  "Transaction: input_count: " ; print_int transaction.inputcount; print_endline""; printInputs transaction.inputs;;
	
let writeToFile ~line ~fileName =
    let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 fileName in
    output_string oc "\n";
    output_string oc line;
    close_out oc;;
(*writeToFile ~line:"hello\n" ~fileName: "./input_output";;*)
let readFile ~fileName = 
	let lines = ref [] in
	let chan = open_in fileName in
	try
	  while true; do
	    lines := input_line chan :: !lines
	  done; []
	with End_of_file ->
	  close_in chan;
	  List.rev !lines;;

(*takes transaction as bytes*)
let printTransactionsFromFile transactions = 
	let lst = List.filter (fun a -> String.length a > 3) transactions in
	let lst = List.map stringParse lst in 
	let lst = List.map parseTransaction lst in 
	let rec print lst = 
		match lst with 
		hd::tl -> printTransaction hd; 
		print tl
		| [] -> print_string "" 
	in print lst;;

let printTransactionsFromList transactions = 
	let rec print lst = 
		match lst with
		hd::tl -> printTransaction hd; print tl
		| []->print_string ""
	in print transactions;;

let serializeTransaction transaction = 
	let rec transactionInputSerialize transactionInputs = 
			match transactionInputs with 
			hd::tl -> hd.prevout_hash @| hd.index @| transactionInputSerialize tl 
			| [] -> empty_bytes
		in (bytes_of_int 4 transaction.inputcount) @| (transactionInputSerialize transaction.inputs) @| transaction.flagExit;;


let writeTransactionToFile ~transaction ~fileName = 
	let serialized = serializeTransaction transaction in 
	let transaction = print_bytes serialized  in 
	writeToFile ~line:  transaction  ~fileName: fileName;;
(* IO *) 

let rec writeTransactionsToFile ~transactions ~fileName =
	match transactions with
	hd::tl -> writeTransactionToFile ~transaction: hd ~fileName: fileName; writeTransactionsToFile ~transactions: tl ~fileName: fileName
	| [] -> ();;

let computeHash (transaction : transactionType) : bytes = 
	let transaction = serializeTransaction transaction in
	printBytes (hash SHA256(hash SHA256 transaction));
	hash SHA256(hash SHA256 transaction);;

let createStartTransaction = 
	let lst = [{prevout_hash = bytes_of_int 20 1; index = bytes_of_int 4 1;}] in {inputcount = List.length lst; inputs = lst; flagExit = bytes_of_int 4 0};;


let createTransaction hash = 
	let counter = random 3 in 
	let lst = ref []  in 
	while (List.length !lst< counter) do
	print_int (List.length !lst); 
	let temp = 
	[{prevout_hash = hash; 
	index = bytes_of_int 4 (random 3)}] 
	in lst :=  List.append !lst temp
	done; {inputcount = List.length !lst; inputs = !lst; flagExit = bytes_of_int 4 0};;
 
let createRoundTransaction ~lst ~globalSetRef  = 
	let localSet = ref HashSet.empty in 
	let temp = ref [] in 
	List.map (fun elem -> 
		let r = random 3 in 
		let hash = computeHash elem in 
		while(List.length !temp != r) do
			let transactionNew = createTransaction hash in
			hashAdd ~elem: hash ~setRef: localSet; 
			temp := List.append [transactionNew] !temp done) lst;
	globalSetRef := HashSet.union !globalSetRef !localSet;	
	 !temp;;

let start = createStartTransaction ;;
writeTransactionToFile ~transaction: start ~fileName: "./input_output" ;;
(*let round = createRoundTransaction [start];;*)

let createBlockchain ~startPoint ~length ~fileName =
	let globalSet = ref HashSet.empty in  
	let counter = ref 0 in 
	let source = [startPoint] in
	while (!counter != length) do
	let temp = 	createRoundTransaction ~lst: source ~globalSetRef: globalSet in 
	writeTransactionsToFile ~transactions: temp ~fileName: fileName; 
	counter := !counter +1 done;; 

createBlockchain ~startPoint: start  ~fileName: "./input_output" ~length: 2;;


(*

let createBlockchain = 
let createCheckPointRound = 
let createCheckPoint = 
*)

(*writeTransactionToFile ~transaction: createStartTransaction ~fileName: "./input_output";;
printTransactionsFromFile (readFile ~fileName:"./input_output");;*)
