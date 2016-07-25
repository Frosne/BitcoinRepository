open Cryptokit

let sum a b = a +b ;;
print_int (sum 2 3);;

(* Test harness *)

let error_occurred = ref false

let function_tested = ref ""

let testing_function s =
    function_tested := s;
    print_newline();
    print_string s;
    print_newline()

let test test_number answer correct_answer =
 if answer <> correct_answer then 
	print_endline "BAD"
else 
print_endline "Successful";;

(* Useful auxiliaries *)

let hex s = transform_string (Hexa.decode()) s
let tohex s = transform_string (Hexa.encode()) s

let hash s = hash_string (Hash.ripemd160()) s ;;
let dehash s = tohex (hash s);;
let res = dehash "";;

print_string res;;
