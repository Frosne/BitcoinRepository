open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;

type transactionInputType = {prevout_hash: bytes; index : bytes; signedTransaction: bytes; publicKey: ec_key; sequence: bytes}

type transactionOutputType = {value: int; pkScript: ec_key}

type transactionType = {hash:bytes; nVersion:int; inputcount : bytes; inputs : transactionInputType list; outputcount: bytes; outputs: transactionOutputType list;}


let string_of_bytes (x:bytes) : string =
    let s = get_cbytes x in
    let res = ref "" in
    for i = 0 to String.length s - 1 do
      res := !res ^ (Printf.sprintf "%x" (int_of_char s.[i]));
    done;
    !res;;

let compareStrings s1 s2 : bool = 
	let result = (compare s1 s2) in
		if result == 0
			then true
		else false;;

let intOfBytesVarInput (number:bytes) : int = 
	let wholebyte = string_of_bytes number in
		let byteInfo = (String.sub wholebyte 0 2) in 	
			if (compareStrings "fd" byteInfo) && (number.length==3) then	
		let number = xor 3 number (bytes_of_int 3 16580608)
			in int_of_bytes number;
(*Int is too big =/*)
		(*else if (compareStrings "fe" byteInfo) && (number.length==3) then		let number = xor number (bytes_of_int 4 1090921693184)
		in int_of_bytes number
			*)
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


(*
type transactionInputType = {prevout_hash: bytes; index : bytes; signedTransaction: bytes; publicKey: ec_key; sequence: bytes}

type transactionOutputType = {value: int; pkScript: ec_key}

type transactionType = {hash:bytes; nVersion:int; inputcount : bytes; inputs : transactionInputType list; outputcount: bytes; outputs: transactionOutputType list;}*)

let serializeKey (key: ec_key) : bytes = 
	let x = key.ec_point.ecx in
		let y = key.ec_point.ecy in 
			let serializedKey = bytes_of_int 1 4 in 
				let xy =  x @| y in serializedKey @|xy;;

  let bytes_of_intBigEndian nb i =
    let rec put_bytes bb lb n =
      if lb = 0 then failwith "not enough bytes"
      else
        begin
          String.set bb (nb - lb) (char_of_int (n mod 256));
          if n/256 > 0 then
            put_bytes bb (lb-1) (n/256)
          else bb
        end
    in
    let b = String.make nb (char_of_int 0) in
      abytes(put_bytes b nb i)

(*let a = bytes_of_int 8 33540;;
print_endline(print_bytes a);;
let a = bytes_of_intBigEndian 8 33540;;
print_endline (print_bytes a);;*)

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

let longNum = 1000L;;
let bytes = bytes_of_long_big_endian 4 longNum;;
print_endline(print_bytes bytes);;















