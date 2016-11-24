open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;
open Cryptokit;;
open StdLabels;;

(* Сrypto *)
let sha256 b = hash SHA256 b
let sha256Double b = sha256 (sha256 b)		

let hex s = transform_string (Hexa.decode()) s
let tohex s = transform_string (Hexa.encode()) s

let random ?(boundL=0) ~boundH = 
	let temp = int_of_bytes (CoreCrypto.random 2) in 
	let v = boundH - boundL in 
	let temp = temp mod v in temp + boundL

(* Misc *)
let stringCompare s1 s2 = 
	let result = (compare s1 s2) in
		if result == 0
			then true
		else false
		
let bytesTakeLeft (b: bytes) i = 
	let splitted = split b i in
		let part = fst splitted in part

let bytesTakeRight(b:bytes) i = 
	if (length b == 1) 
		then empty_bytes
	else
		let splitted = split b i in
			let part = snd splitted in part

(* Printing functions *)
	
let stringConcat s1 s2 = 
		String.concat "" [s1;s2]

(*overrided from Planform.Bytes*)	
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
	
(*Slightly different*)	
let string_of_bytes (x:bytes) : string =
    let s = get_cbytes x in
    let res = ref "" in
    for i = 0 to String.length s - 1 do
      res := !res ^ (Printf.sprintf "%x" (int_of_char s.[i]));
    done;
    !res
	
let printBytes bytes= print_string(print_bytes bytes);;

let printStackInt stack = 
	Stack.iter print_int stack

let printStackBytes stack =
	Stack.iter printBytes stack

let printListInt lst = 
	List.iter print_int lst

let printListBytes lst = 
	List.iter printBytes lst
	

(*Number representation*)
let bytes_of_long_big_endian ~nb ~(i:int64) =
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
      abytes(put_bytes b nb i)
	  
let bytes_of_int_big_endian ~nb ~i =
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
	  
let int_of_bytes_big_endian b : int = 
	let x = ref 0 in
	let c = get_cbytes b in 
	for y = b.length -1 downto 0 do
	x := 256 * !x + (int_of_char (String.get c y))
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
      abytes(put_bytes b nb i)
	  
let long_of_bytes (b:bytes) : int64 = 
	let x = ref 0L in 
      	let c = get_cbytes b in
        for y = 0 to b.length-1 do
        x := Int64.add (Int64.mul 256L !x) (Int64.of_int(int_of_char (String.get c y)))
        done;
      !x
	  
let intOfBytesVarInput (number:bytes) : int = 
	let wholebyte = string_of_bytes number in
		if String.length wholebyte  >= 2 then 
			let byteInfo = (String.sub wholebyte 0 2) in 	
				if (stringCompare "fd" byteInfo) && (number.length==3) then	
					let number = xor 3 number (bytes_of_int 3 16580608)
						in int_of_bytes number;
				else		
				 int_of_bytes number
		else 	
			int_of_bytes number

let bytesVarIntOfInt (number: int) : bytes 	= 
	if number < 253 then bytes_of_int 1 number
	else if (number < 65536) then 
		let number = bytes_of_int 3 number in
let result = xor 3 number (bytes_of_int 3 16580608) in result
		else bytes_of_int 1 0
		
let bytesEndianInverse (entity:bytes)  = 
	let counter = ref 0 in 
	let str = string_of_bytes entity in 
	let length = ((String.length str)/4) in 
	let l = String.length str in  
	while !counter <= length do 
		let c1 = str.[!counter] in 
		let c2 = str.[!counter+1] in
		str.[!counter] <- str.[l-2 - !counter]; 
		str.[!counter+1] <- str.[l-1 - !counter]; 
		str.[l-1 - !counter] <- c2; 
		str.[l-2 - !counter] <- c1; 	
		counter := !counter +2; done; 
	abytes str		
	
let stringCastOneElement (symbol:char): int = 
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
	in result  
	  
let convertFromHexdecimalToBytes (line:string) = 
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
		count :=!count -2;  bytes :=  (bytes_of_int 1 result) @| !bytes done; !bytes

let compareBytes (a:bytes) (b:bytes) = 
	let temp = equalBytes a b in
	match temp with 
	true -> 0
	| false -> 1

(* Hash set *)	
	module HashSet = Set.Make (struct 
	let compare = compareBytes
	type t = Platform.Bytes.bytes
	end);;

let hashAdd ~elem ~setRef = 
	let set = HashSet.add elem !setRef in setRef:=set;;

let hashTakeRandom ~set = 
	let lst = HashSet.elements !set in 
	let rnd = random ~boundL: 0 ~boundH: (List.length lst) in List.nth lst rnd;;

let hashEnumerate ~set = 
	HashSet.iter (fun s -> printBytes s; print_string "  ") set; print_endline "";;
		  
		
