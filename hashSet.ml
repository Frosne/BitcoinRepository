open CoreCrypto;;
open Platform.Bytes;;
open Platform.Option;;
open Cryptokit;;
open StdLabels;;

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

