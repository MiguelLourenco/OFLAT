# 1 "src/OCamlFlat.ml"
(*
 * OCamlFlat.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * feb/2021 (amd) - New module.
 *)

let ocamlFlatVersion = "1.0"
# 1 "src/Configuration.ml"
(*
 * Configuration.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)
 
module type ConfigurationSig =
sig
	val diagnosticsOn : unit -> bool
end


module Configuration : ConfigurationSig =
struct
	let automaticDiagnostics = ref true

	let diagnosticsOn () = !automaticDiagnostics
end
# 1 "src/Error.ml"
(*
 * Error.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Simplified module.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Supports a log of errors. Probably, a text-based application
 * will use the error log differently from a graphical-based application.
 * The errors are handled in a imperative style to simplify the signature of
 * many functions - this modules implements a kind of error log monad.
 *)

module type ErrorSig =
sig
	val start : unit -> unit
	val error : string -> string -> 'a -> 'a
	val get: unit -> string list
	val show : string -> string -> unit
end

module Error : ErrorSig =
struct

	let showImmediately =
		false (* for debugging *)

	let errors: string list ref =
		ref []

	let start () =
		errors := []

	let makeMesg (culprit: string) (str: string) =
		if culprit = "_" then "" ^ str
		else "\"" ^ culprit ^ "\": " ^ str

	let printMesg (mesg: string) =
		print_string "	";
		print_string mesg;
		print_string "\n"

	let debugMesg (mesg: string) =
		if showImmediately && Configuration.diagnosticsOn () then
			printMesg ("==> "^mesg)

	let error (culprit: string) (str: string) (res: 'a): 'a =
		let mesg = makeMesg culprit str in
			errors := !errors @ [mesg];
			debugMesg mesg;
			res

	let get (): string list =
		!errors

	let show (expectedKind: string) (name: string): unit =
		if !errors <> [] && Configuration.diagnosticsOn () then begin
			print_string (expectedKind^" \""^name^ "\" has errors:\n");
			List.iter printMesg (!errors)
		end
end
# 1 "src/Set.ml"
(*
 * Set.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * mar/2022 (amd) - More functions; stable ordering.
 * may/2021 (amd) - New projection functions; new fixed point function.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Polymorphic sets. Naive implementation.
 *
 * TODO: Improve the implementation or move to the functorized sets of
 * the ocaml standard library.
 *)

module type SetSig =
sig
	(*type 'a t = 'a list*)
	type 'a t (* opaque *)
	val make : 'a list -> 'a t
	val toList : 'a t -> 'a list
	val empty : 'a t
	
	val makeSorted : 'a list -> 'a t
	val sort: 'a t -> 'a t

	val isEmpty : 'a t -> bool
	val size : 'a t -> int
	val compare_sizes: 'a t -> 'b t -> int
	val compare_size_with : 'a t -> int -> int
	val cons : 'a -> 'a t -> 'a t
	val hd : 'a t -> 'a
	val tl : 'a t -> 'a t
	val cut : 'a t -> 'a * 'a t
	val match_ : 'a t -> (unit -> 'b) -> ('a -> 'a t -> 'b) -> 'b
	val nth : 'a t -> int -> 'a
	val nth_opt : 'a t -> int -> 'a option
	val init : int -> (int -> 'a) -> 'a t
	val flatten : 'a t t -> 'a t
	
	val iter : ('a -> unit) -> 'a t -> unit
	val iteri : (int -> 'a -> unit) -> 'a t -> unit
	val map : ('a -> 'b) -> 'a t -> 'b t
	val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
	val flatMap : ('a -> 'b t) -> 'a t -> 'b t
	val flat_map : ('a -> 'b t) -> 'a t -> 'b t
	val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
	val fold_right: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val fold_left_s: ('a t -> 'b -> 'a t) -> 'a t -> 'b t -> 'a t
	val fold_right_s: ('a -> 'b t -> 'b t) -> 'a t -> 'b t -> 'b t

	val for_all : ('a -> bool) -> 'a t -> bool
	val exists : ('a -> bool) -> 'a t -> bool
	val belongs : 'a -> 'a t -> bool
	val subset : 'a t -> 'a t -> bool
	val equals : 'a t -> 'a t -> bool
	
	val find : ('a -> bool) -> 'a t -> 'a
	val find_opt : ('a -> bool) -> 'a t -> 'a option
	val filter : ('a -> bool) -> 'a t -> 'a t
	
	val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
	
	val split : ('a * 'b) t -> 'a t * 'b t
	val combine : 'a t -> 'b t -> ('a * 'b) t

	val add : 'a -> 'a t -> 'a t
	val remove : 'a -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val union : 'a t -> 'a t -> 'a t
	
	val unionUnsafe : 'a t -> 'a t -> 'a t


	val combinations : 'a t -> 'b t -> ('a * 'b) t
	val star : 'a list t -> int -> 'a list t
	val allDistinct : ('a -> 'b) -> 'a t -> bool
	val hasDuplicates : 'a t -> bool
	val validate : 'a list -> string -> 'a t
	val historicalFixedPoint : ('a t -> 'a t) -> ('a t) -> 'a t
	val historicalFixedPointTracing : ('a t -> 'a t) -> ('a t) -> 'a t list
	
	val proj3_1 : ('a * 'b * 'c) t -> 'a t
	val proj3_2 : ('a * 'b * 'c) t -> 'b t
	val proj3_3 : ('a * 'b * 'c) t -> 'c t
	val proj3_12 : ('a * 'b * 'c) t -> ('a * 'b) t
	val proj3_23 : ('a * 'b * 'c) t -> ('b * 'c) t
	
	val test: unit -> int list list
end

module Set : SetSig =
struct
	type 'a t = 'a list
	let delX (v :'a) = List.filter (fun x -> x <> v)

	let rec make (l: 'a list): 'a t =
		match l with
		| [] -> []
		| x::xs -> x::make (delX x xs)
	let toList (s: 'a t): 'a list = s
	let empty: 'a t = []
	
	let makeSorted (l: 'a list): 'a t = List.sort_uniq compare l
	let sort (s: 'a t): 'a list = List.sort compare s

	let isEmpty (s: 'a t): bool = s = []
	let size: 'a t -> int = List.length
	let compare_sizes: 'a t -> 'b t -> int = List.compare_lengths
	let compare_size_with: 'a t -> int -> int = List.compare_length_with
(* cons: add 'x' at the begin if 'x' is new in 's' *)
	let cons (v :'a) (s: 'a t): 'a t = if List.mem v s then s else v::s
(* add: add 'x' at the end if 'x' is new in 's' *)
	let add (v :'a) (s: 'a t): 'a t = if List.mem v s then s else s@[v]
	let hd: 'a t -> 'a = List.hd
	let tl: 'a t -> 'a t = List.tl
	let cut (s: 'a t) = (List.hd s, List.tl s)
	let match_ s e n = if isEmpty s then e () else n (hd s) (tl s)
	let nth: 'a t -> int -> 'a = List.nth
	let nth_opt: 'a t -> int -> 'a option = List.nth_opt
	let init: int -> (int -> 'a) -> 'a t = List.init
	let flatten (ss: 'a t t): 'a t = make (List.flatten ss)
	
	let iter: ('a -> unit) -> 'a t -> unit = List.iter	
	let iteri: (int -> 'a -> unit) -> 'a t -> unit = List.iteri	
	let map (f: 'a -> 'b) (s: 'a t): 'b t = make (List.map f s)
	let mapi (f: int -> 'a -> 'b) (s: 'a t): 'b t = make (List.mapi f s)
	let flatMap (f: 'a -> 'b t) (s: 'a t): 'b t = flatten (List.map f s)
	let flat_map: ('a -> 'b t) -> 'a t -> 'b t = flatMap
	let fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a = List.fold_left
	let fold_right: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b = List.fold_right
	let fold_left_s f u (s: 'a t): 'b t = make (List.fold_left f u s)
	let fold_right_s f u (s: 'a t): 'b t = make (List.fold_right f u s)
	
	let for_all: ('a -> bool) -> 'a t -> bool = List.for_all
	let exists: ('a -> bool) -> 'a t -> bool = List.exists
(* The following three functions use the equality '=' and may not work well for sets of sets *)
	let belongs: 'a -> 'a t -> bool = List.mem
	let subset (s1: 'a t) (s2: 'a t): bool = List.for_all (fun v -> belongs v s2) s1
	let equals (s1: 'a t) (s2: 'a t): bool = compare_sizes s1 s2 = 0 && subset s1 s2

	let find: ('a -> bool) -> 'a t -> 'a = List.find
	let find_opt: ('a -> bool) -> 'a t -> 'a option = List.find_opt	
	let filter: ('a -> bool) -> 'a t -> 'a t = List.filter	(* already distinct *)
	
	let partition: ('a -> bool) -> 'a t -> 'a t * 'a t = List.partition	(* already distinct *)
	
	let split (s: ('a * 'b) t): 'a t * 'b t = let (a, b) = List.split s in (make a, make b)
	let combine: 'a t -> 'b t -> ('a * 'b) t = List.combine
	
	let remove: 'a -> 'a t -> 'a t = delX
	let inter (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun v -> belongs v s2) s1
	let diff (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun v -> not (belongs v s2)) s1
(* union: join s1 with the new elements of s2 *)
	let union (s1: 'a t) (s2: 'a t): 'a t = s1 @ (diff s2 s1)
	(* pre: inter s1 s2 = [] *)
	let unionUnsafe (s1: 'a t) (s2: 'a t): 'a t = s1 @ s2
	let combinations (s1: 'a t) (s2: 'b t): ('a * 'b) t =
		flatMap (fun x -> List.map (fun y -> (x,y)) s2) s1	(* already distinct *)
	let starOne (s: 'a list t) (n: int) (l: 'a t): 'a list t = (* private auxiliary *)
		let z = n - (List.length l) in
		let sel = filter (fun k -> List.length k <= z) s in
			map (fun k -> k @ l) sel
	let rec fixedPoint (f: 'a -> 'a) (x: 'a): 'a =
		let next = f x in
			if x = next then x
			else fixedPoint f next
	let star (s: 'a list t) (n: int): 'a list t =
		fixedPoint (fun v -> union v (flatMap (starOne v n) s)) [[]]

	let allDistinct f (s: 'a t): bool = size s = size (map f s)
	let hasDuplicates (s: 'a t): bool = size s <> size (make s)
	let validate (l: 'a list) (culprit: string): 'a t =
		if hasDuplicates l
			then Error.error culprit "Repetitions in set" empty
			else make l

	let rec acumFixedPoint (f: 'a t -> 'a t) (v: 'a t): 'a t =
		let next = union v (f v) in
			if v = next then v
			else acumFixedPoint f next

	let historicalFixedPoint (f: 'a t -> 'a t) (v: 'a t): 'a t =
		let rec historicalFixedPointX (f: 'a t -> 'a t) (v: 'a t) (acum: 'a t): 'a t =
			let next = f v in
			let newAcum = union v acum in
			if acum = newAcum then v
			else historicalFixedPointX f next newAcum
		in
			historicalFixedPointX f v empty

	let historicalFixedPointTracing (f: 'a t -> 'a t) (v: 'a t): 'a t list =
		let rec historicalFixedPointX (f: 'a t -> 'a t) (v: 'a t) (acum: 'a t) (trace: 'a t list): 'a t list =
			let next = f v in
			let newTrace = trace@[next] in
			let newAcum = union v acum in
			if acum = newAcum then trace
			else historicalFixedPointX f next newAcum newTrace
		in
			historicalFixedPointX f v empty [v]

	let proj3_1 s3 = map (fun (a,_,_) -> a) s3
	let proj3_2 s3 = map (fun (_,b,_) -> b) s3
	let proj3_3 s3 = map (fun (_,_,c) -> c) s3
	let proj3_12 s3 = map (fun (a,b,_) -> (a,b)) s3
	let proj3_23 s3 = map (fun (_,b,c) -> (b,c)) s3

	let test (): int list list =	(* Set.test () *)
		toList (star (make[ [1]; [2;3]]) 4)
end

module type UPSetSig = (* unordered pair set *)
sig
	type 'a t
	val make : ('a * 'a) list -> 'a t
	val toList : 'a t -> ('a * 'a) list
	val empty : 'a t
	val size : 'a t -> int
	val belongs : 'a * 'a -> 'a t -> bool
	val union : 'a t -> 'a t -> 'a t
	val add : 'a * 'a -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val subset : 'a t -> 'a t -> bool
	val map : ('a * 'a -> 'b * 'b) -> 'a t -> 'b t
	val filter : ('a * 'a -> bool) -> 'a t -> ('a * 'a) Set.t
	val for_all : ('a * 'a -> bool) -> 'a t -> bool
	val exists : ('a * 'a -> bool) -> 'a t -> bool
	val exists : ('a * 'a -> bool) -> 'a t -> bool
	val flatten : 'a t t -> 'a t
	val flatMap : ('a -> 'b t) -> 'a t -> 'b t
	val iter : ('a * 'a -> unit) -> 'a t -> unit
	val partition : ('a * 'a -> bool) -> 'a t -> ('a * 'a) Set.t * ('a * 'a) Set.t
	val combinations : 'a t -> 'b t -> ('a * 'b) t
	val star : 'a list t -> int -> 'a list t
	val allDistinct : ('a * 'a -> 'b) -> 'a t -> bool
	val hasDuplicates : 'a t -> bool
	val validate : ('a * 'a) list -> string -> 'a t
	val test: unit -> (int * int) list
end

module UPSet : UPSetSig =
struct
	type 'a t = ('a*'a) Set.t

	(* invariant: a < b for all pairs (a,b) *)
	let ord (a,b) = if a < b then (a, b)		(* keep *)
					else if b < a then (b, a)	(* swap *)
					else failwith "UPSet.ord"	(* error *)

	let make (l: ('a*'a) list): 'a t =
		let l1 = List.filter (fun (a,b) -> a <> b) l in
		let l2 = List.map ord l1 in
			Set.make l2
	let toList (s: 'a t): ('a*'a) list = Set.toList s

	let empty: 'a t = Set.empty
	let size (s: 'a t): int = Set.size s
	let belongs (v: 'a*'a) (s: 'a t): bool = Set.belongs (ord v) s
	let union (s1: 'a t) (s2: 'a t): 'a t = Set.union s1 s2
	let add (v: 'a*'a) (s: 'a t): 'a t = Set.add (ord v) s
	let inter (s1: 'a t) (s2: 'a t): 'a t = Set.inter s1 s2
	let diff (s1: 'a t) (s2: 'a t): 'a t = Set.diff s1 s2
	let subset (s1: 'a t) (s2: 'a t): bool = Set.subset s1 s2

	let map f (s: 'a t) = make (Set.toList (Set.map f s))
	let filter f (s: 'a t) = Set.filter f s
	let for_all f (s: 'a t) = Set.for_all f s
	let exists f (s: 'a t) = Set.exists f s
	let flatten (ss: 'a t t) = failwith "UPSet.flatten"
	let flatMap f (s: 'a t) = failwith "UPSet.flatMap"
	let iter f (s: 'a t) = Set.iter f s
	let partition f (s: 'a t) = Set.partition f s
	let combinations (s1: 'a t) (s2: 'b t): ('a * 'b) t = failwith "UPSet.combinations"
	let star (s: 'a list t) (n: int): 'a list t = failwith "UPSet.star"

	let allDistinct f (s: 'a t) = Set.allDistinct f s
	let hasDuplicates (s: 'a t): bool = Set.hasDuplicates s
	let validate (l: ('a*'a) list) (culprit: string): 'a t = failwith "UPSet.validate"
	let test () =	(* UPSet.test () *)
		toList (make [(1,1);(1,2);(2,2);(3,2);(3,2);(2,3)])
end


# 1 "src/BasicTypes.ml"
(*
 * BasicTypes.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * apr/2022 (amd) - The type 'symbol' is now opaque type an can be internally
 *						represented using a char or a string. Required several changes
 *						all over the code of several modules.
 * mar/2021 (amd) - New types 'property', 'properties'.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Some types and constants used across all the modules.
 *)


# 35 "src/BasicTypes.ml"
module type BasicTypesSig =
sig
	type 'a set = 'a Set.t
	
	type symbol (* opaque *)
	
# 41 "src/BasicTypes.ml"
 type symbolX = char
	
# 45 "src/BasicTypes.ml"
 type symbols = symbol set
	type variable = symbol
	type variableX = symbolX
	type variables = variable set
	
	val char2symb : char -> symbol
	val symb2char : symbol -> char
	val str2symb : string -> symbol
	val symb2str : symbol -> string
	val symb2symbX : symbol -> symbolX
	val symbX2symb : symbolX -> symbol
	val symb : string -> symbol

	val symbols2symbolsX : symbols -> symbolX list
	val symbolsX2symbols : symbolX list -> symbols

	val epsilon : symbol
	val dollar : symbol
	val draftVar : variable
	val symbolTypeName : string
	val empty: symbol

	(* WORDS *)
	type word = symbol list
	type words = word set
	val str2word : string -> word
	val word : string -> word
	val word2str : word -> string
	val strs2words : string list -> word list
	val words2strs : word list -> string list
	val symbols : string -> symbols
	
	(* STATES *)
	(* type state *) (* opaque *)
	type state = string
	type states = state set
	val state2str : state -> string
	val str2state : string -> state
	val state : string -> state
	val draftState: state

	(* TRANSITIONS3 *)	
	type transition3 = state * symbol * state
	type transition3X = state * symbolX * state
	type transitions3 = transition3 set

	val transsX2transs3 : transition3X list -> transitions3
	val transs2transsX3 : transitions3 -> transition3X list

	(* PROPERTIES *)	
	type property = string
	type properties = property set

	(* OTHER *)	
	type direction = L | R

	val direction2string: direction -> string
	val string2direction: string -> direction

	val char2direction: char -> direction

	val stringIsDirection: char -> bool

	(* DISPLAY *)	
	val str2display : string -> string
	val symb2display : symbol ->  string
	val symbX2display : symbolX ->  string
	val state2display : state -> string

	val symbols2display : symbols ->  string
	val symbolsX2display : symbolX list ->  string
	val statesX2display : state list ->  string
	val strings2display : string list ->  string
	val transsX2display3 : transition3X list -> string
end

module BasicTypes : BasicTypesSig =
struct
	type 'a set = 'a Set.t
	
	
# 126 "src/BasicTypes.ml"
 type symbol = char
	type symbolX = char
	let symbolTypeName = "char"
	
# 134 "src/BasicTypes.ml"
 type symbols = symbol set
	type variable = symbol
	type variableX = symbolX
	type variables = variable set
	
	
# 140 "src/BasicTypes.ml"
 let symbDisplayQuote = "'" 
	let char2symb c: symbol = c
	let symb2char s: char = s
	let str2symb s: symbol = if String.length s > 0 then String.get s 0 else '?'
	let symb2str s: string = Char.escaped s	

	
# 153 "src/BasicTypes.ml"
 let symb2symbX (s: symbol): symbolX = s
	let symbX2symb (x: symbolX): symbol = x
	let symb (s: string): symbol  = str2symb s

	let symbols2symbolsX ss: symbolX list =
		List.map symb2symbX (Set.toList ss)
	let symbolsX2symbols ss: symbols =
		Set.make (List.map symbX2symb ss)
	

	let epsilon: symbol = symb "~" (* used for representing the empty transitions *)
	let dollar: symbol = symb "$"
	let draftVar: variable = symb "_"
	let nothing: symbol = symb "_"
	let empty: symbol = symb "B"

	(* WORDS *)

	type word = symbol list
	type words = word set
	
	(* let str2word s =      //only ocaml >= 4.06
		List.init (String.length s) (String.get s) ; *)
	let str2word (s:string): word =
		let n = String.length s in
		let rec iterStr i =
			if i < n then (char2symb s.[i])::iterStr (i+1)
			else []
		in
			iterStr 0

	let word s = str2word s

	let word2str (w:word): string =
		let strs = List.map symb2str w in
			String.concat "" strs

	let strs2words ss =
		List.map str2word ss
		
	let words2strs ws =
		List.map word2str ws
		
	let symbols s =
		Set.make (word s)

	(* STATES *)
	
	type state = string
	type states = state set

	let state2str s: string = s

	let str2state s: state = s
	let state s = str2state s

	let draftState: state = state "_"

	(* TRANSITIONS3 *)
		
	type transition3 = state * symbol * state
	type transition3X = state * symbolX * state
	type transitions3 = transition3 set

	let transX2trans3 (a,b,c): transition3 =
		(a, symbX2symb b, c)
	let transs2transsX3 s: transition3X list =
		List.map transX2trans3 (Set.toList s)
	let transsX2transs3 l: transitions3 =
		Set.make (List.map transX2trans3 l)

	(* PROPERTIES *)	
	
	type property = string
	type properties = property set

	(* OTHER *)
	
	type direction = L | R
	type 'c trail = 'c set list
	type 'c path = 'c list

	let direction2string dir : string = if dir = L then "L" else "R"
	let string2direction dirS : direction = if dirS = "L" then L else R 

	let char2direction dirC : direction = if dirC = 'L' then L else R 

	let stringIsDirection dirC: bool = if dirC = 'L' || dirC = 'R' then true else false

	(* DISPLAY *)

	let str2display s: string = "\"" ^ s ^ "\""
	let state2display s: string = "\"" ^ (state2str s) ^ "\""
	let symb2display s: string = symbDisplayQuote ^ (symb2str s) ^ symbDisplayQuote
	let symbX2display s: string = symb2display s

	let list2display f l =
		let l = List.map f l in
		let core = String.concat "; " l in
			"[" ^ core ^ "]"	
	let symbolsX2display l: string = list2display symb2display l
	let symbols2display s: string = list2display symbX2display (Set.toList s)
	let statesX2display l = list2display state2display l
	let strings2display l = list2display str2display l
	let transsX2display3 l =
		let t2d (a,b,c) =
			Printf.sprintf "(%s, %s, %s)" (state2display a) (symbX2display b) (state2display c)
		in list2display t2d l
end
# 1 "src/Util.ml"
(*
 * Util.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * may/2021 (amd) - Lots of miscellaneous new stuff.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Miscellaneous utility functions. 
 *
 * TODO: Check if this is the right place for some of this functions.
 *)
 
open BasicTypes

module type UtilSig =
sig
	val stripChars : string -> string -> string
	val stripHead : string -> string

	val flatMap:  ('a -> 'b list) -> 'a list -> 'b list
	val concatAll : 'a list -> 'a list list -> 'a list list
	val distrib2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
	val indexOf : 'a -> 'a list -> int
	val fixedPoint : ('a -> 'a) -> 'a -> 'a
	
	val loadFile : string -> string
	val print : string list -> unit
	val println : string list -> unit
	val header : string -> unit
	val printAlphabet : symbols -> unit
	val printStates : states -> unit
	val printTransition : string -> symbol -> string -> unit
	val printTransitionTM : state -> symbol -> state -> symbol -> direction -> unit 
	val printTransitionPDA :state -> symbol -> symbol -> state -> symbol list -> unit
	val printWords : words -> unit
	val printStrings : string set -> unit
	val show : string -> unit

	val handleHomeDir : string -> string
	val testing : bool -> string -> bool
end

module Util : UtilSig =
struct
	let stripChars s cs =
		let len = String.length s in
		let j = ref 0 in
		let res = Bytes.create len in
			for i = 0 to len-1 do
				if not (String.contains cs s.[i]) then begin
					Bytes.set res !j s.[i];
					j := !j + 1
				end
			done;
			Bytes.to_string (Bytes.sub res 0 !j)

	let stripHead s =
		let len = String.length s in
		let n = ref 0 in
		let skip = ref (-1) in
		let j = ref 0 in
		let res = Bytes.create len in
			for i = 1 to len-1 do
				if !skip < 0 then begin
					if s.[i] = '\t' then
						n := !n + 1
					else
						skip := 0
				end;
				if !skip >= 0 then begin
					if !skip > 0 && s.[i] = '\t' then
						skip := !skip - 1
					else begin
						if s.[i] = '\n' then
							skip := !n
						else ();
						Bytes.set res !j s.[i];
						j := !j + 1
					end
				end
			done;
			Bytes.to_string (Bytes.sub res 0 !j)
		
	let flatMap f l =
		List.flatten (List.map f l)

	let addAll symb =
		List.map (fun l -> symb::l)

	let concatAll w =
		List.map (fun l -> w@l)

	let distrib2 f (a,b) =
		f a b

	let indexOf e l =
		let rec index e l n =
			match l with
				[] -> -1
				|x::xs -> if e = x then n else index e xs (n+1)
		in
		index e l 0

	let rec fixedPoint (f: 'a -> 'a) (x: 'a): 'a =
		let next = f x in
			if x = next then x
			else fixedPoint f next
			
			
	let loadFile (filename: string): string =
		try
			let ic = open_in filename in
			let n = in_channel_length ic in
			let s = Bytes.create n in
				really_input ic s 0 n;
				close_in ic;
				Bytes.to_string s
		with
			Sys_error str ->
				Error.error "file" str ""


	let rec print (l: string list) =
		match l with
		| [] -> ()
		| x::xs -> print_string x; print xs

	let println (l: string list) =
		print l ;
		print_newline()

	let header (str: string) =
		println ["------------------------------------------------"] ;
		println [str]

	let printAlphabet (a: symbols) =
		Set.iter (fun x -> print [symb2str x; ", "]) a;
		println []

	let printStates (st:states) =
		Set.iter (fun x -> print [state2str x; ", "]) st;
		println []

	let printTransition (a:string) (b:symbol) (c:string) =
		println ["("; a; ", "; symb2str b; ", "; c; ")"]

	let printTransitionTM (a:state) (b:symbol) (c:state) (d:symbol) (e:direction) =
		println ["("; state2str a; ", "; symb2str b; ", "; state2str c; ", "; symb2str d; ", "; direction2string e ; ")"]

	let printTransitionPDA (a:state) (b:symbol) (c:symbol) (d:state) (e: symbol list) =
		println ["("; state2str a; ", "; symb2str b; ", "; symb2str c; ", "; state2str d; ", ["];
		Set.iter (fun x -> print [symb2str x; ", "]) (Set.make e);
		print ["])"]

	let printWord (w:word) =
		println ["'"; word2str w; "'"]

	let printWords (s: words) =
		Set.iter printWord s
		
	let printString (s: string) =
		println ["'"; s; "'"]

	let printStrings (s: string set) =
		Set.iter printString s
		
	let show s =
		print_string ("|" ^ s ^ "|\n")
		
	let handleHomeDir s =
		match String.length s with
		| 0 ->
			""
		| 1 ->
			if s = "~" then Sys.getenv("HOME") else s
		| n ->
			if s.[0] = '~' then
				if s.[1] = '/' then
					Sys.getenv("HOME") ^ String.sub s 1 (n - 1)
				else
					"/home/" ^ String.sub s 1 (n - 1)
			else s

	let testing active moduleName =
		let forceActive = false in
		let regularActive = (active && try ignore (Sys.getenv("TESTING")); true with _ -> false) in
		let active = forceActive || regularActive in
			if active then
				header ("### Testing " ^ moduleName ^ " ###");
			active
end

module UtilTests =
struct
	let active = false

	let test0 () =
		Util.println [Util.loadFile "examples/fa_abc.json"]

	let test1 () =
		let a = word2str [symb "e";symb "r";symb "t"] in
		let b = word2str [symb "4";symb "5";symb "y"] in
			Util.println [a; b]

	let runAll : unit =
		if Util.testing active "Util" then begin
			test0 ();
			test1 ()
		end
end

module IdGenerator =
struct
	let current = ref 0;;

	let reset () =
		current := 0

	let gen (s: string) =
		let res = Printf.sprintf "%s%02d" s (!current) in
			current := !current+1;
			res
end

module type RuntimeControlSig =
sig
	val start: unit -> unit
	val giveUp: int -> bool
	val stats: unit -> bool * int * float
end

module RuntimeControl : RuntimeControlSig =
struct
	let _CONFIGS_ALLOWANCE = 10
	let _TIME_ALLOWANCE = 10.0
	
	let timeStart = ref 0.0
	let exactResult = ref false
	let runconfigs = ref 0
	let runtime = ref 0.0		(* in seconds *)

	let start () =
		timeStart := Sys.time();
		exactResult := true;
		runconfigs := 0;
		runtime := 0.0

	let giveUp n =	
		runconfigs := n;
		runtime := Sys.time() -. !timeStart;
		(*Printf.printf "(%6d, %f)\n" !configs !time;*)
		if !runtime > _TIME_ALLOWANCE || !runconfigs > _CONFIGS_ALLOWANCE then begin
			exactResult := false;
			true
		end
		else
			false

	let stats () =
		(!exactResult, !runconfigs, !runtime)
end
# 1 "src/Scanner.ml"
(*
 * Scanner.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Initial version, for the JSon parser.
 *)

(*
 * Description: Simple lexical analyzer that assumes that the tokens are
 * the individual non-blank characters. The function getToken is available
 * to handle the rare cases where we need a multi-char token. This module is
 * suitable for the three parsers defined in the OCamlFLAT library.
 * The tokens are handled in a imperative style to simplify the signature of
 * of the client parsing functions.
 *)

module type ScannerSig =
sig
	val start : string -> string -> unit
	val skip : unit -> unit
	val curr : unit -> char
	val getToken : (char -> bool) -> string
	val expecting : string -> char -> 'a
	val invalid : string -> 'a
end

module Scanner : ScannerSig =
struct
	let parserName = ref ""
	let inputString = ref ""
	let inputStringLength = ref 0
	let inputStringPosition = ref 0

	let start name s =
		parserName := name;
		inputString := s;
		inputStringLength := String.length s;
		inputStringPosition := 0;
		Error.start ()

	let isInside () =
		!inputStringPosition < !inputStringLength

	let getThis () =
		String.get !inputString !inputStringPosition

	let skip () =
		inputStringPosition := !inputStringPosition + 1

	let skipWhile good =
		while isInside () && good (getThis ()) do skip() done

	let curr () =
		skipWhile (fun c -> c = ' ' || c = '\t' || c = '\n');
		if isInside () then
			getThis ()
		else
			' '

	let getToken good =
		let start = !inputStringPosition in
			skipWhile good;
			String.sub !inputString start (!inputStringPosition - start)

	let expecting exp got =
		let g = if got = ' ' then "'EOF'" else "'" ^ Char.escaped got ^ "'" in
		let mesg ="Expecting " ^ exp ^ ", got " ^ g in
			Error.error !parserName mesg ();
			raise Not_found

	let invalid str =
		Error.error !parserName str ();
		raise Not_found
end
# 1 "src/JSon.ml"
(*
 * JSon.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * apr/2022 (amd) - Added the "make" family of functions. Required several changes
 *						all over the code of several modules.
 * may/2021 (amd) - Added a second parser, for OCaml values syntax. The output
 *                  is regular JSon.
 * jan/2021 (amd) - Added a very simple recursive descent parser for JSon.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Very simple JSon parser, plus some JSon handling functions.
 *)

open BasicTypes

module type JSonSig =
sig
	type t =
		| JNull
		| JString of string
		| JAssoc of (string * t) list
		| JList of t list

	val parse : string -> t
	val parseOon : string -> t
	val fromFile : string -> t

	val toStringN : int -> t -> string
	val toString : t -> string
	val show : t -> unit
	val remove : t -> string list -> t

	val isNull : t -> bool
	val hasField : t -> string -> bool
	
	val fieldSymbol : t -> string -> symbol
(*	val fieldSymbolList : t -> string -> symbol list *)
	val fieldSymbolSet : t -> string -> symbol set
	
	val fieldString : t -> string -> string
(*	val fieldStringList : t -> string -> string list *)
	val fieldStringSet : t -> string -> string set

	val fieldState : t -> string -> state
(*	val fieldStateList : t -> string -> state list *)
	val fieldStateSet : t -> string -> state set
	
	val fieldBool : t -> string -> bool

(*	val fieldTriplesList : t -> string -> (state * symbol * state) list *)
	val fieldTriplesSet : t -> string -> (state * symbol * state) set

(*	val fieldQuintupletsList : t -> string -> (state * symbol * symbol * state * symbol set) list *)
	val fieldQuintupletsSet : t -> string -> (state * symbol * symbol * state * word) set


	val fieldTMTransitionSet : t -> string -> (state * symbol * state * symbol * direction) set
	
	val append: t -> t -> t
	
	val makeSymbol : symbol -> t
	val makeSymbolSet : symbol set -> t
	val makeString : string -> t
	val makeStringSet : string set -> t
	val makeState : state -> t
	val makeStateSet : states -> t
	val makeBool : bool -> t
    val makeTriplesSet : (state * symbol * state) set -> t
    val makeQuintupletsSet : (state * symbol * symbol * state * word) set -> t
	val makeTMTransitionsSet : (state * symbol * state * symbol * direction) set -> t
	val makeAssoc : (string * t) list -> t
end

module JSon : JSonSig =
struct
	open Scanner

	type t =
		| JNull
		| JString of string
		| JAssoc of (string * t) list
		| JList of t list

	let parseString delim =
		skip();	(* skip quotation mark *)
		let tk = getToken (fun c -> c <> delim) in
			match curr () with
				| x when x = delim -> skip(); tk
				| err -> expecting ("closing '" ^ (Char.escaped delim) ^ "'") err
				
	let parseWord () =
		getToken (fun c -> 'a' <= c && c <= 'z'
					|| 'A' <= c && c <= 'Z'
					|| '0' <= c && c <= '9'
					|| c = '_')

	let parseLabel () =
		match curr() with
			| '"' -> parseString '"'
			| 'a'..'z' -> parseWord ()
			| err -> expecting "'STRING' or '}'" err

	let checkEOF () =
		match curr() with
			| ' ' -> ()
			| err -> expecting "'EOF'" err


	module JSonParsing = (* JSon syntax *)
	struct
		let rec parsePair () =
			let label = parseLabel () in
				match curr() with
					| ':' -> skip(); (label, parseJSon ())
					| err -> expecting "':'" err

		and parseAssocCont () =
			let p = parsePair () in
				match curr() with
					| ',' -> skip(); p::parseAssocCont ()
					| '}' -> skip(); [p]
					| err -> expecting "',' or '}'" err

		and parseAssoc () =
			skip();	(* skip { *)
			match curr() with
				| '}' -> skip(); []
				| ' ' -> expecting "'}' or 'STRING'" ' '
				| _ -> parseAssocCont ()

		and parseListCont () =
			let j = parseJSon () in
				match curr() with
					| ',' -> skip(); j::parseListCont ()
					| ']' -> skip(); [j]
					| err -> expecting "',' or ']'" err

		and parseList () =
			skip();	(* skip [ *)
			match curr() with
				| ']' -> skip(); []
				| ' ' -> expecting "']' or 'JSON'" ' '
				| _ -> parseListCont ()

		and parseJSon s =
			match curr() with
				| '"' -> JString (parseString '"')
				| '[' -> JList (parseList ())
				| '{' -> JAssoc (parseAssoc ())
				| err -> expecting "'JSON'" err

		let parse s =
			Scanner.start "JSon" s;
			try
				let j = parseJSon () in
					checkEOF (); j
			with Not_found ->
				JNull
	end

	module OCamlValueParsing = (* OCaml value syntax *)
	struct
		let rec parsePair () =
			let label = parseLabel () in
				match curr() with
					| '=' -> skip(); (label, parseOon ())
					| err -> expecting "'='" err

		and parseAssocCont () =
			let p = parsePair () in
				match curr() with
					| ';' -> skip(); p::parseAssocCont ()
					| '}' -> skip(); [p]
					| err -> expecting "';' or '}'" err

		and parseAssoc () =
			skip();	(* skip { *)
			match curr() with
				| '}' -> skip(); []
				| ' ' -> expecting "'}' or 'STRING'" ' '
				| _ -> parseAssocCont ()

		and parseListCont () =
			let j = parseOon () in
				match curr() with
					| ';' -> skip(); j::parseListCont ()
					| ']' -> skip(); [j]
					| err -> expecting "';' or ']'" err

		and parseList () =
			skip();	(* skip [ *)
			match curr() with
				| ']' -> skip(); []
				| ' ' -> expecting "']' or 'Oon'" ' '
				| _ -> parseListCont ()

		and parseTupleCont () =
			let j = parseOon () in
				match curr() with
					| ',' -> skip(); j::parseTupleCont ()
					| ')' -> skip(); [j]
					| err -> expecting "',' or ')'" err

		and parseTuple () =
			skip();	(* skip [ *)
			match curr() with
				| ')' -> skip(); []
				| ' ' -> expecting "')' or 'Oon'" ' '
				| _ -> parseTupleCont ()

		and parseOon s =
			match curr() with
				| '"' -> JString (parseString '"')
				| '\''-> JString (parseString '\'')
				| '[' -> JList (parseList ())
				| '(' -> JList (parseTuple ())
				| '{' -> JAssoc (parseAssoc ())
				| err -> expecting "'OON'" err
				
		let parse s =
			Scanner.start "OON" s;
			try
				let j = parseOon () in
					checkEOF (); j
			with Not_found ->
				JNull
	end

	let parse s =
		JSonParsing.parse s

	let parseOon s =
		OCamlValueParsing.parse s

	let fromFile filename =
		parse (Util.loadFile filename)

(* PRETTY PRINT *)
	let tab n =
		String.make n '\t'

	let isComplex j =
		match j with
		| JList l -> true
		| JAssoc l -> true
		| _ -> false

	let rec textual (tab1: int) (tab2: int) (j: t) : string =
		tab tab1
		^
		match j with
		| JNull ->
			"null"
		| JString s ->
				"\"" ^ s ^ "\""
		| JList l when List.exists isComplex l ->
				let elems = List.map (textual (tab2+1) (tab2+1)) l in (
						"[\n"
						^ String.concat (",\n") elems ^ "\n"
						^ tab tab2 ^ "]"
					)
		| JList l ->
				let elems = List.map (textual 0 0) l in
					("[" ^ String.concat ", " elems ^ "]")
		| JAssoc [] ->
				"{}"
		| JAssoc l ->
				let field (s,j) = tab (tab2+1) ^ s ^ " : " ^ textual 0 (tab2+1) j in
					let elems = List.map field l in (
						"{\n"
						^ String.concat ",\n" elems ^ "\n"
						^ tab tab2 ^ "}"
					)

	let rec textualOCaml (tab1: int) (tab2: int) (j: t) : string =
		tab tab1
		^
		match j with
		| JNull ->
			"null"
		| JString s ->
				"\"" ^ s ^ "\""
		| JList l when List.exists isComplex l ->
				let elems = List.map (textualOCaml (tab2+1) (tab2+1)) l in (
						"[\n"
						^ String.concat (",\n") elems ^ "\n"
						^ tab tab2 ^ "]"
					)
		| JList l ->
				let elems = List.map (textualOCaml 0 0) l in
					("[" ^ String.concat ", " elems ^ "]")
		| JAssoc [] ->
				"{}"
		| JAssoc l ->
				let field (s,j) = tab (tab2+1) ^ s ^ " : " ^ textualOCaml 0 (tab2+1) j in
					let elems = List.map field l in (
						"{\n"
						^ String.concat ",\n" elems ^ "\n"
						^ tab tab2 ^ "}"
					)

	let toStringN n j =
		textual 0 n j

	let toString j =
		toStringN 0 j

	let show (j: t) =
		Util.println [toString j]

	let remove (j: t) r =
		match j with
		| JAssoc l ->
			JAssoc (List.filter (fun (a,_) -> not (List.mem a r)) l)
		| _ ->
			j


(* MEMBERSHIP *)
	let isNull j =
		j = JNull

	let hasField j name =
		match j with
		| JAssoc obj -> (
				try
					ignore (List.assoc name obj); true
				with Not_found -> false
			)
		| _ ->
			false

	let getField name j =
		match j with
		| JAssoc obj -> (
				try
					List.assoc name obj
				with Not_found -> JNull
			)
		| _ ->
			JNull

(* MORE *)

	let error = Error.error
	
	let dummySymb = symb "#"
	let dummyState = state "#"
	let dummyDirection = L

	let fieldSymbol (j: t) (field: string): symbol =
		match j |> getField field with
		| JNull -> error field "Missing field" dummySymb
		| JString s -> str2symb s
		| _ -> error field "Expected symbol" dummySymb

	let asSymbol (j: t) (field: string): symbol =
		match j with
		| JString s -> str2symb s
		| _ -> error field "Expected symbol" dummySymb

	let fieldSymbolList (j: t) (field: string): symbol list =
		match j |> getField field with
		| JNull -> error field "Missing field" []
		| JList l -> List.map (fun j -> asSymbol j field) l
		| _ -> error field "Expected symbol list" []

	let fieldSymbolSet (j: t) (field: string): symbol set =
		Set.validate (fieldSymbolList j field) field


	let fieldString (j: t) (field: string) =
		match j |> getField field with
		| JNull -> error field "Missing field" "#"
		| JString s -> s
		| _ -> error field "Expected string" "#"

	let asString (j: t) (field: string) =
		match j with
		| JString s -> s
		| _ -> error field "Expected string" "#"

	let fieldStringList (j: t) (field: string) =
		match j |> getField field with
		| JNull -> error field "Missing field" []
		| JList l -> List.map (fun j -> asString j field) l
		| _ -> error field "Expected string list" []

	let fieldStringSet (j: t) (field: string) =
		Set.validate (fieldStringList j field) field


	let asState (j: t) (field: string) =
		match j with
		| JString s -> state s
		| _ -> error field "Expected state" dummyState

	let fieldState (j: t) (field: string) =
		state (fieldString j field)
		
	let fieldStateList (j: t) (field: string) =
		List.map state (fieldStringList j field)
		
	let fieldStateSet (j: t) (field: string) =
		Set.validate (fieldStateList j field) field

	let fieldBool (j: t) (field: string) =
		match fieldString j field with
		| "false" -> false
		| "true" -> true
		| _ -> error field "Expected bool" false

	let fieldDirection (j: t) (field: string) =
		match fieldString j field with
		| "L" -> L
		| "R" -> R
		| _ -> error field "Expected L|R" dummyDirection


	let asStateSymbolState (j: t) (field: string) =
		match j with
		| JList [a; b; c] -> (asState a field, asSymbol b field, asState c field)
		| _ -> error field "Malformed triple" (dummyState,dummySymb,dummyState)

	let fieldTriplesList (j: t) (field: string) =
		match j |> getField field with
		| JNull -> error field "Missing field" []
		| JList l -> List.map (fun j -> asStateSymbolState j field) l
		| _ -> []

	let fieldTriplesSet (j: t) (field: string) =
		Set.validate (fieldTriplesList j field) field


	let asWord (j: t) (field: string): word =
		match j with
			| JString s -> str2word s
			| _ -> error field "Expected word" []

	let asStateSymbolSymbolStateWord (j: t) (field: string) =
		match j with
		| JList [a; b; c; d; e] ->
			(	asState a field,
				asSymbol b field,
				asSymbol c field,
				asState d field,
				asWord e field
			)
		| _ -> error field "Malformed quintuplet" (dummyState,dummySymb,dummySymb,dummyState,[])


	let asDirection (j: t) (field: string): direction =
		match j with
		| JString "L" -> L
		| JString "R" -> R
		| _ -> error field "Expected L|R" dummyDirection


	let asStateSymbolStateSymbolDirection (j: t) (field: string) =
		match j with
		| JList [a; b; c; d; e] ->
			(	asState a field,
				asSymbol b field,
				asState c field,
				asSymbol d field,
				asDirection e field
			)
		| _ -> error field "Malformed TM transition" (dummyState,dummySymb,dummyState,dummySymb,dummyDirection)
	
	let fieldQuintupletsList (j: t) (field: string) =
		match j |> getField field with
		| JNull -> error field "Missing field" []
		| JList l -> List.map (fun j -> asStateSymbolSymbolStateWord j field) l
		| _ -> []

	let fieldQuintupletsSet (j: t) (field: string) =
		Set.validate (fieldQuintupletsList j field) field


	let fieldTMTransitionList (j: t) (field: string) =
		match j |> getField field with
		| JNull -> error field "Missing field" []
		| JList l -> List.map (fun j -> asStateSymbolStateSymbolDirection j field) l
		| _ -> []

	let fieldTMTransitionSet (j: t) (field: string) =
		Set.validate (fieldTMTransitionList j field) field


	let append j1 j2 =
		match j1, j2 with
		| JAssoc l1, JAssoc l2 -> JAssoc (l1 @ l2)
		| _, _ -> failwith "JSon.append: not Assoc"

	let makeSymbol s =
		JString (symb2str s)

	let makeSymbolSet s =
		JList (List.map makeSymbol (Set.toList s))
		
	let makeString s =
		JString s

	let makeStringSet s =
		JList (List.map makeString (Set.toList s))
		
	let makeState s =
		makeString (state2str s)
		
	let makeStateSet s =
		JList (List.map makeState (Set.toList s))

	let makeBool b =
		makeString (if b then "true" else "false")
		
	let makeTriplesSet s =
		JList (List.map (fun (a,b,c) ->
				JList [JString (state2str a); JString (symb2str b);
						JString (state2str c)]) (Set.toList s))

	let makeQuintupletsSet s =
		JList (List.map (fun (a,b,c,d,e) ->
							JList [	JString (state2str a);
									JString (symb2str b);
									JString (symb2str c);
									JString (state2str d);
									JString (word2str e)])
							(Set.toList s))

	let makeTMTransitionsSet s =
		JList (List.map (fun (a,b,c,d,e) ->
							JList [	JString (state2str a);
									JString (symb2str b);
									JString (state2str c);
									JString (symb2str d);
									JString (if e = L then "L" else "R")])
							(Set.toList s))


	let makeAssoc l =
		JAssoc l
end



module JSonTests =
struct
	let active = false

	let jsonSample = {| {
		name: {
			first: "aa",
			last: "22",
			fullname: "33"
		},
		age: "33",
		hobbies: [ "44", "55" ]
	} |}
	
	let jsonSample2 = {| "aa" |}
	
	let test0 () =
		let json = JSon.parse jsonSample in
		let json2 = JSon.parse jsonSample2 in
			JSon.show json; JSon.show json2
	
	let oonSample = {| {
		alphabet = ['a';'b'];
		states = ["START"; "33"];
		initialState = "START";
		transitions = [("START", 'a', "START"); ("START", 'a', "START")];
		acceptStates = ["START"]
	} |}
	
	let oonSample2 = {| "z*" |}

	let oonSample3 = {| ("START", ["ee"], "yu") |}

	let test1 () =
		let oon = JSon.parseOon oonSample in
		let oon2 = JSon.parseOon oonSample2 in
		let oon3 = JSon.parseOon oonSample3 in
			JSon.show oon; JSon.show oon2; JSon.show oon3

	let test () =
		let oon2 = JSon.parseOon oonSample2 in
			JSon.show oon2

	let runAll =
		if Util.testing active "JSon" then begin
			test ()
		end

end
# 1 "src/Examples.ml"
(*
 * Examples.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * dec/2020 (amd) - Collected the examples in a single module.
 * sep/2019 (jg) - Initial version, each example in an individual file.
 *)

(*
 * Description: A set of good predefined examples.
 *
 * TODO: Check if these examples are really good and improve.
 *)

module type ExamplesSig =
sig
	val examples : string list
	val example : string -> string
	val jsonExample : string -> JSon.t
	val see : string -> unit
end

module Examples : ExamplesSig =
struct
	(* Entity definitions *)

	let dfa_1 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "dfa_1",
			alphabet: ["a", "b"],
			states : ["START", "A", "B", "C"],
			initialState : "START",
			transitions : [
					["START", "a", "A"], ["A", "b", "B"], ["B", "a", "C"], ["C", "b", "B"],
					["C", "a", "A"]
				],
			acceptStates : ["START", "B", "C"]
		} |}

	let dfa_2 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "dfa_2",
			alphabet: ["0", "1"],
			states : ["START", "1", "2", "3"],
			initialState : "START",
			transitions : [
				["START", "1", "1"], ["1", "1", "START"], ["1", "0", "2"], ["2", "0", "1"],
				["2", "1", "3"], ["3", "1", "2"], ["3", "0", "START"], ["START", "0", "3"]
			],
			acceptStates : ["1"]
			} |}

	let dfa_astar = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "dfa_astar",
			alphabet: ["a"],
			states : ["START"],
			initialState : "START",
			transitions : [
				["START", "a", "START"]
			],
			acceptStates : ["START"]
			} |}

	let fa_abc = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "fa_abc",
			alphabet : ["a", "b", "c", "d"],
			states : ["START", "A", "AB", "SUCCESS"],
			initialState : "START",
			transitions : [
					["START","a","A"], ["START","b","START"], ["START","c","START"], ["START","d","START"],
					["A","a","A"], ["A","b","AB"], ["A","c","START"], ["A","d","START"],
					["AB","a","A"], ["AB","b","START"], ["AB","c","SUCCESS"], ["AB","d","START"],
					["SUCCESS","a","SUCCESS"], ["SUCCESS","b","SUCCESS"], ["SUCCESS","c","SUCCESS"], ["SUCCESS","d","SUCCESS"]
				],
			acceptStates : ["SUCCESS"]
		} |}

	let fa_error = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "fa_error",
			alphabet : ["a"],
			states : ["A"],
			initialState : "START",
			transitions : [],
			acceptStates : ["SUCCESS"]
		} |}

	let nfa_1 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "nfa_1",
			alphabet: ["a", "b"],
			states : ["START", "A", "B"],
			initialState : "START",
			transitions : [
					["START", "a", "A"], ["A", "b", "B"], ["A", "b", "START"], ["B", "a", "START"]
				],
			acceptStates : ["START"]
			} |}

	let nfa_2 = {| {
			kind : "finite automaton",
			description : "this is an example",
			name : "nfa_2",
			alphabet : ["a", "b", "c", "d", "e"],
			states : ["START", "A", "AB", "SUCCESS", "UNREACHABLE", "UNPRODUCTIVE"],
			initialState : "START",
			transitions : [
					["START","a","A"], ["START","b","START"], ["START","c","START"], ["START","d","START"],
					["A","a","A"], ["A","b","AB"], ["A","c","START"], ["A","d","START"],
					["AB","a","A"], ["AB","b","START"], ["AB","c","SUCCESS"], ["AB","d","START"],
					["SUCCESS","a","SUCCESS"], ["SUCCESS","b","SUCCESS"], ["SUCCESS","c","SUCCESS"], ["SUCCESS","d","SUCCESS"], ["A","a","AB"], ["UNREACHABLE", "a", "SUCCESS"],
					["SUCCESS", "e", "UNPRODUCTIVE"], ["UNPRODUCTIVE", "a", "UNPRODUCTIVE"]
				],
			acceptStates : ["SUCCESS"]
		} |}

	let re_abc = {| {
			kind : "regular expression",
			description : "this is an example",
			name : "re_abc",
			re : "((a+b)*(cd)*)*"
		} |}

	let re_complex = {| {
			kind : "regular expression",
			description : "this is a complex example",
			name : "re_complex",
			re : "(a+(b(c+d)+ea))*f*g"
		} |}

	let re_convoluted = {| {
			kind : "regular expression",
			description : "this is a convoluted example",
			name : "re_convoluted",
			re : "((((a+b)*(cd)*)*+(e(f+gh*i)*jk)*+lmn)op+q)"
		} |}

	let re_simple = {| {
			kind : "regular expression",
			description : "this is a simple example",
			name : "re_simple",
			re : "a+a*+bc*"
		} |}

	let re_astar = {| {
			kind : "regular expression",
			description : "this is a simple example",
			name : "re_astar",
			re : "a*"
		} |}

	let fe_colors = {| {
		kind : "finite enumeration",
		description : "this is an example",
		name : "fe_colors",
		words : ["Red", "Yellow", "Blue"]
	} |}

	let cfg_simple = {| {
			kind : "context free grammar",
			description : "this is an example",
			name : "cfg_simple",
			alphabet : ["0", "1"],
			variables : ["S", "P"],
			initial : "S",
			rules : [	"S -> 1S0 | P",
						"P -> 0P1 | ~" ]
		} |}
		
	let cfg_balanced = {| {
			kind : "context free grammar",
			description : "CFG: Language of balanced square bracket parentheses",
			name : "cfg_balanced",
			alphabet : ["[", "]"],
			variables : ["S"],
			initial : "S",
			rules : [	"S -> [S] | SS | ~"]
		} |}

	let exer_balanced_cfg = {| {
			kind : "exercise",
			description : "CFG: Language of balanced square bracket parentheses",
			name : "exer_balanced_cfg",
			problem : "CFG for the language of balanced parentheses",
			inside : ["","[]","[[]]","[][]","[[][][][[]]][]"],
			outside : ["[","][","[[]","[[]]]"],
			properties : []
		} |}

	let exer_astar_fa = {| {
			kind : "exercise",
			description : "FA: all sequences of 'a's",
			name : "exer_astar_fa",
			problem : "Finite automaton for all sequences of 'a's",
			inside : ["","a","aa","aaa","aaaaaaa"],
			outside : ["d","b","ava"],
			properties : []
		} |}

	let exer_astar_re = {| {
			kind : "exercise",
			description : "RE: all sequences of 'a's",
			name : "exer_astar_re",
			problem : "Regular expression for all sequences of 'a's",
			inside : ["","a","aa","aaa","aaaaaaa"],
			outside : ["d","b","ava"],
			properties : []
		} |}

	let exer_abcd = {| {
			kind : "exercise",
			description : "this is an example",
			name : "exer_abcd",
			problem : "Convert the regular expression (a+b)*(c+d) to finite automaton.",
			inside : ["abc","c","ab","b","abac"],
			outside : ["","aba","bab","abba","baab","abcd"],
			properties : []
		} |}

	let exer_ab = {| {
			kind : "exercise",
			description : "this is an example",
			name : "exer_ab",
			problem : "Convert the regular expression ab*+ba* to finite automaton.",
			inside : ["a","ab","abb","abbbbbbbb","b","ba","baa","baaaaaa"],
			outside : ["","aba","bab","abba","baab","c"],
			properties : []
		} |}

	let exer_re2fa = {| {
			kind : "exercise",
			description : "this is an example",
			name : "exer_re2fa",
			problem : "Converta o autómato finito com alfabeto: [x, y, z], estados: [S, T, V], estado inicial: S, transições [[S, x, S], [S, y, T], [S, z, V], [T, x, T], [T, z, T], [T, y, V], [V, x, T]], e estados finais: [V] em expressão regular.",
			inside : ["z", "xz", "yy", "yzy", "xyy", "zxxy"],
			outside : ["x","y","xy", "xyz", "yyx", "xzxz", "xyxz"],
			properties : []
		} |}

	let exer_readwrite = {| {
			kind : "exercise",
			description : "this is an example",
			name : "exer_readwrite",
			problem : "open,close,read,write",
			inside : ["","orc","owc","orwc","owwrwrrc","ocorwc"],
			outside : ["or","oo","o","w","r","c","orw","owrrww","corwc"],
			properties : []
		} |}

	let exer_tm = {| {
		kind : "exercise",
		description : "this is an example",
		name : "exer_tm",
		problem : "open,close,read,write",
		inside : ["","orc","owc","orwc","owwrwrrc","ocorwc"],
		outside : ["or","oo","o","w","r","c","orw","owrrww","corwc"],
		properties : []
	} |}
			
	let tm_astar1 = {| {
		kind: "turing machine",
		description: "this is an example",
		name: "tm_astar1",
		entryAlphabet: ["a", "b"],
		tapeAlphabet: ["a", "b", "B"],
		empty: "B",
		states: ["q1", "q2"],
		initialState: "q1",
		transitions: [
			["q1", "B", "q2", "B", "L"],
			["q1", "a", "q1", "b", "R"],
			["q1", "b", "q1", "a", "R"],
			["q2", "a", "q2", "a", "L"],
			["q2", "b", "q2", "b", "L"]
		],
		acceptStates: [],
		criteria: "false",
		markers: []
		} |}

	
	let tm_astar2 = {| {
		kind: "turing machine",
		description: "this is an example",
		name: "tm_astar2",
		entryAlphabet: ["a", "b"],
		tapeAlphabet: ["a", "b", "X", "Y","B"],
		empty: "B",
		states: ["q1", "q2", "q3", "q4", "q5", "q6", "q7"],
		initialState: "q1",
		transitions: [
			["q1", "a", "q2", "X", "R"],
			["q1", "b", "q5", "Y", "R"],
			["q1", "B", "q7", "B", "L"],

			["q2", "a", "q2", "a", "R"],
			["q2", "b", "q2", "b", "R"],
			["q2", "B", "q3", "B", "R"],

			["q3", "a", "q3", "a", "R"],
			["q3", "b", "q3", "b", "R"],
			["q3", "B", "q4", "a", "L"],

			["q4", "a", "q4", "a", "L"],
			["q4", "b", "q4", "b", "L"],
			["q4", "B", "q4", "B", "L"],
			["q4", "X", "q1", "X", "R"],
			["q4", "Y", "q1", "Y", "R"],

			["q5", "a", "q5", "a", "R"],
			["q5", "b", "q5", "b", "R"],
			["q5", "B", "q6", "B", "R"],

			["q6", "a", "q6", "a", "R"],
			["q6", "b", "q6", "b", "R"],
			["q6", "B", "q4", "b", "L"],

			["q7", "X", "q7", "a", "L"],
			["q7", "Y", "q7", "b", "L"]
		],
		acceptStates: [],
		criteria: "false",
		markers: []
		} |}

	let tm_astar3 = {| {
			kind: "turing machine",
			description: "this is an example changed",
			name: "tm_astar3",
			entryAlphabet: ["a", "b"],
			tapeAlphabet: ["a", "b", "B"],
			empty: "B",
			states: ["q1", "q2", "q3"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q2", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q2", "a", "q3", "a", "R"],
				["q2", "b", "q1", "b", "R"]
			],
			acceptStates: ["q3"],
			criteria: "true",
			markers: []
			} |}

	let tm_astar4 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar4",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "X", "Y", "Z", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4", "q5", "q6"],
			initialState: "q1",
			transitions: [
				["q1", "B", "q6", "B", "R"],
				["q1", "Y", "q5", "Y", "R"],
				["q1", "a", "q2", "X", "R"],

				["q2", "a", "q2", "a", "R"],
				["q2", "Y", "q2", "Y", "R"],
				["q2", "b", "q3", "Y", "R"],

				["q3", "b", "q3", "b", "R"],
				["q3", "Z", "q3", "Z", "R"],
				["q3", "c", "q4", "Z", "L"],

				["q4", "Z", "q4", "Z", "L"],
				["q4", "Y", "q4", "Y", "L"],
				["q4", "b", "q4", "b", "L"],
				["q4", "a", "q4", "a", "L"],

				["q5", "Y", "q5", "Y", "R"],
				["q5", "Z", "q5", "Z", "R"],
				["q5", "B", "q6", "B", "R"]
			],
			acceptStates: ["q6"],
			criteria: "true",
			markers: []
			} |}

	let tm_astar5 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar5",
			entryAlphabet: ["a", "b"],
			tapeAlphabet: ["a", "b", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q2", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q1", "B", "q4", "B", "R"],

				["q2", "a", "q3", "a", "R"],
				["q2", "b", "q1", "b", "R"],
				["q2", "B", "q4", "B", "R"],

				["q4", "a", "q4", "a", "R"],
				["q4", "b", "q4", "b", "R"],
				["q4", "B", "q4", "B", "R"]
			],
			acceptStates: [],
			criteria: "false",
			markers: []
			} |}

	let tm_astar6 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar6",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4", "q5", "q6", "q7"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q1", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q1", "c", "q1", "c", "R"],

				["q1", "c", "q2", "c", "R"],
				["q1", "c", "q5", "c", "L"],

				["q2", "a", "q3", "a", "R"],

				["q3", "b", "q4", "b", "R"],

				["q5", "b", "q6", "b", "L"],

				["q6", "a", "q7", "a", "L"]
			],
			acceptStates: ["q4", "q7"],
			criteria: "true",
			markers: []
			} |}

	let tm_astar7 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar7",
			entryAlphabet: ["a", "b", "c", "d", "e"],
			tapeAlphabet: ["a", "b", "c", "d", "e", "B"],
			empty: "B",
			states: ["q1", "q2", "q3"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q2", "a", "R"],

				["q1", "a", "q1", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q1", "c", "q1", "c", "R"],
				["q1", "d", "q1", "d", "R"],
				["q1", "e", "q1", "e", "R"],

				["q2", "c", "q3", "c", "R"]
			],
			acceptStates: ["q3"],
			criteria: "true",
			markers: []
			} |}

	let tm_astar8 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar8",
			entryAlphabet: ["a"],
			tapeAlphabet: ["a", "B"],
			empty: "B",
			states: ["q1", "q2", "q3"],
			initialState: "q1",
			transitions: [
				["q1", "B", "q2", "B", "R"],
				["q2", "B", "q1", "B", "L"],

				["q2", "a", "q3", "a", "R"]
			],
			acceptStates: ["q3"],
			criteria: "true",
			markers: []
			} |}

	let tm_astar9 = {| {
			kind: "turing machine",
			description : "this is an example",
			name: "tm_astar9",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "X", "Y", "Z", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"],
			initialState: "q1",
			transitions: [

				["q1", "B", "q6", "B", "R"],
				["q1", "Y", "q5", "Y", "R"],
				["q1", "a", "q2", "X", "R"],

				["q2", "a", "q2", "a", "R"],
				["q2", "Y", "q2", "Y", "R"],
				["q2", "b", "q3", "Y", "R"],

				["q3", "b", "q3", "b", "R"],
				["q3", "Z", "q3", "Z", "R"],
				["q3", "c", "q4", "Z", "L"],

				["q4", "Z", "q4", "Z", "L"],
				["q4", "Y", "q4", "Y", "L"],
				["q4", "b", "q4", "b", "L"],
				["q4", "a", "q4", "a", "L"],
				
				["q4", "X", "q1", "X", "R"],

				["q5", "Y", "q5", "Y", "R"],
				["q5", "Z", "q5", "Z", "R"],
				["q5", "B", "q6", "B", "R"],

				["q5", "b", "q9", "c", "R"],

				["q7", "b", "q8", "c", "R"],
				["q7", "B", "q6", "B", "R"]

			],
			acceptStates: ["q6"],
			criteria: "true",
			markers: []
		} |}

	let tm_astar10 = {| {
		kind: "turing machine",
		description: "this is an example",
		name: "tm_astar10",
		entryAlphabet: ["a", "b", "c"],
		tapeAlphabet: ["a", "b", "c", "B"],
		empty: "B",
		states: ["q1"],
		initialState: "q1",
		transitions: [
			["q1", "B", "q1", "c", "R"],
			["q1", "a", "q1", "a", "R"],
			["q1", "b", "q1", "b", "R"],
			["q1", "c", "q1", "c", "R"]
		],
		acceptStates: [],
		criteria: "false",
		markers: []
		} |}

	let tm_astar11 = {| {
		kind : "turing machine",
		description : "this is an example",
		name : "tm_astar11",
		entryAlphabet: ["a", "b", "c"],
		tapeAlphabet: ["a", "b", "c", "B"],
		empty: "B",
		states : ["q1", "q2", "q3"],
		initialState : "q1",
		transitions : [
			["q1", "a", "q2", "c", "R"],
			["q1", "b", "q1", "b", "R"],
			["q1", "c", "q1", "a", "R"],
			["q2", "b", "q1", "b", "L"],
			["q2", "c", "q3", "c", "R"]
		],
		acceptStates : [],
		criteria : "false",
		markers: []
		} |}



	(* Examples table *)

	let oflatExamplesTable = [
		("tm_astar1", tm_astar1);
		("tm_astar2", tm_astar2);
		("tm_astar3", tm_astar3);
		("tm_astar4", tm_astar4);
		("tm_astar5", tm_astar5);
		("tm_astar6", tm_astar6);
		("tm_astar7", tm_astar7);
		("tm_astar8", tm_astar8);
		("tm_astar9", tm_astar9);
		("tm_astar10", tm_astar10);
		("tm_astar11", tm_astar11);
		("dfa_1", dfa_1);
		("dfa_2", dfa_2);
		("dfa_astar", dfa_astar);
		("fa_abc", fa_abc);
		("nfa_1", nfa_1);
		("nfa_2", nfa_2);

		("re_abc", re_abc);
		("re_complex", re_complex);
		("re_convoluted", re_convoluted);
		("re_simple", re_simple);
		("re_astar", re_astar);

		("fe_colors", fe_colors);

		("cfg_simple", cfg_simple);
		("cfg_balanced", cfg_balanced);

		("exer_balanced_cfg", exer_balanced_cfg);
		("exer_astar_fa", exer_astar_fa);
		("exer_astar_re", exer_astar_re);
		("exer_abcd", exer_abcd);
		("exer_ab", exer_ab);
		("exer_re2fa", exer_re2fa);
		("exer_readwrite", exer_readwrite)
	]

	let examples =
		List.map fst oflatExamplesTable

	let example name =
		List.assoc name oflatExamplesTable

	let jsonExample name =
		let j = JSon.parse (example name) in
			if JSon.isNull j then
				Error.show "example" name;
			j

	let see name =
		Util.println [example name]

	let validate () =
		List.iter (fun n -> ignore (jsonExample n)) examples

	let _ =
		if false then
			validate ()

end
# 1 "src/Entity.ml"
(*
 * Entity.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Improved error handling.
 * may/2021 (amd) - Added support for an extern representation.
 * may/2021 (amd) - Centralized the handling of kind/description/name.
 * feb/2021 (amd) - Added the alternative Predef.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: An entity is a named instance of a concept. As now, the entities
 * are the exercises and the FLAT models. The type "alternatives" is to allow
 * the constructor to be used with several kind of parameter forms.
 *)

module Arg =
struct	
	type ('r, 'x) alternatives =
		| JSon of JSon.t
		| Text of string
		| File of string
		| Predef of string
		| Representation of 'r
		| RepresentationX of 'x

	let fromAlternatives alt =
		match alt with
		| JSon j -> j
		| Text str -> JSon.parse str
		| File str -> JSon.fromFile str
		| Predef str -> JSon.parse (Examples.example str)
		| _ -> JSon.JNull
end

module Entity =
struct

	type t = {
		kind : string;
		description : string;
		name : string
	}

	let dummyId (k: string): t = {
		kind = k;
		description = "_";
		name = "_"
	}

	let fromJSon (j: JSon.t) (expectedKind: string): t =
		if JSon.isNull j then
			dummyId expectedKind
		else {
			kind = JSon.fieldString j "kind";
			description = JSon.fieldString j "description";
			name = JSon.fieldString j "name"
		}
		
	let toJSon (rep: t): JSon.t =
		JSon.makeAssoc [
			("kind", JSon.makeString rep.kind);
			("description", JSon.makeString rep.description);
			("name", JSon.makeString rep.name)
		]

	class virtual entity (arg: ('r,'x) Arg.alternatives) (expectedKind: string) =
		object(self)
		
			val id: t =
				Error.start ();
				match arg with
				| Arg.Representation r -> dummyId expectedKind
				| Arg.RepresentationX r -> dummyId expectedKind
				| _ -> fromJSon (Arg.fromAlternatives arg) 	expectedKind

			val errors = ref []

			method id: t =
				id

			method errors : string list =
				!errors
			
			method virtual validate: unit
			
			method toJSon =
				toJSon id
			
			method handleErrors = (
				if id.kind <> expectedKind then
					Error.error id.kind "Wrong kind" ();
				self#validate;
				errors := Error.get ();
				Error.show expectedKind id.name
			)
			
			method moduleName =
				"Entity"
	end
end

module EntityTests : sig end =
struct
	let active = false

	let test0 () =
		()

	let runAll =
		if Util.testing active "Entity" then begin
			test0 ()
		end
end

# 1 "src/Exercise.ml"
(*
 * Exercise.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Improved error handling.
 * mar/2021 (amd) - Added semantic constrains (properties) to the exercises.
 * jan/2021 (amd) - Module in an independent file.
 * set/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml"
 *)

(*
 * Description: Support to pedagogical exercises. The solutions are validated
 * using unit tests.
 *)

open BasicTypes

module ExerTypes =
struct
	type tx =
		unit
	type t = {
		problem : string;
		inside : words;
		outside : words;
		properties : properties
	}
end

module type ExerciseSig =
sig
	open ExerTypes

	class exercise :
		(t,t) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon : JSon.t
				method representation : t
				method validate : unit
				
				method tracing : unit
				
				method moduleName : string
			end
end

module Exercise : ExerciseSig =
struct
	open ExerTypes
	
	let fromJSon j =
		if JSon.isNull j || not (JSon.hasField j "kind") then {
			problem = "_";
			inside = Set.empty;
			outside = Set.empty;
			properties = Set.empty
		}
		else {
			problem = JSon.fieldString j "problem";
			inside = Set.map str2word (JSon.fieldStringSet j "inside");
			outside = Set.map str2word (JSon.fieldStringSet j "outside");
			properties = JSon.fieldStringSet j "properties"
		}
	
	let toJSon (rep: t): JSon.t =
		JSon.makeAssoc [
			("problem", JSon.makeString rep.problem);
			("inside", JSon.makeStringSet (Set.map word2str rep.inside));
			("outside", JSon.makeStringSet (Set.map word2str rep.outside));
			("properties", JSon.makeStringSet rep.properties)
		]

	class exercise (arg: (t,t) Arg.alternatives ) =
		object(self) inherit Entity.entity arg "exercise" as super

			val representation: t =
				match arg with
					| Arg.Representation r -> r
					| Arg.RepresentationX r -> r
					| _ -> fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation =
				representation

			method validate = ()
			
			method toJSon: JSon.t =
				JSon.append (super#toJSon) (toJSon representation)

			method tracing = ()
			
			method moduleName =
				"Exercice"
	end
end

module ExerciseTests : sig end =
struct
	let active = false

	let test0 () =
		let e = new Exercise.exercise (Arg.Predef "exer_balanced") in
		let je = e#toJSon in
			JSon.show je

	let runAll =
		if Util.testing active "Exercices" then begin
			test0 ()
		end
end


# 1 "src/Model.ml"
(*
 * Model.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jun/2023 (amd) - Added generic 'accept' and 'generate' methods.
 * set/2022 (amd) - Full restructuration.
 * jul/2021 (amd) - Improved Learn-OCaml support.
 * mar/2021 (amd) - Added support for semantic constrains (properties) in
 *                  the exercises, in this class and in all its subclasses.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Abstract FLAT model.
 *
 * TODO: Probably add a new method "canonical" to generate a
 * normalized/simplified version of the FLAT model.
 *)

 open BasicTypes
 type 'c trail = 'c set list
 type 'c path = 'c list

 module ModelBasics =
 struct
 end
 
 module ModelExercises =
 struct
	 let checkProperty (prop: string) =
		 match prop with
			 | "fail" | "false" -> false
			 | "true" -> true
			 | _ ->
				 let mesg = "checkProperty: unknown property ("
								 ^ prop ^ ")" in
					 failwith mesg
 
	 let checkExercise (ex: ExerTypes.t) accept checkProperty =
				Set.for_all accept ex.inside
		 && Set.for_all (fun w -> not (accept w)) ex.outside
		 && Set.for_all checkProperty ex.properties
 
	 let checkExerciseFailures (ex: ExerTypes.t) ac cp = (
		 Set.filter (fun w -> not (ac w)) ex.inside,
		 Set.filter ac ex.outside,
		 Set.filter (fun w -> not (cp w)) ex.properties
	 )
 end
 
 module ModelSupport =
 struct
	 include ModelBasics
	 include ModelExercises
 end
 
 module ModelPrivate =
 struct
	 let stats () =
		 RuntimeControl.stats ()
 
	 (* The result is true is the word is accepted. *)
	 let accept (m: 'm) (w: word)
				 (initial: 'm -> word -> 'c set)
				 (next: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool): bool =
		 let rec acceptX (configs: 'c set) (seen: 'c set): bool =
			 let newConfigs = Set.diff configs seen in
			 let newSeen = Set.unionUnsafe newConfigs seen in
			 if Set.isEmpty newConfigs then
				 false
			 else if Set.exists (isAccepting m) newConfigs then
				 true
			 else if RuntimeControl.giveUp (Set.size seen) then
				 false
			 else
				 let nextConfigs = Set.flatMap (next m) newConfigs in
					 acceptX nextConfigs newSeen
		 in	
		 let _ = RuntimeControl.start () in
		 let initialConfigs = initial m w in
			 acceptX initialConfigs Set.empty
 
	 (* The result is a triple: acceptance, one path, trail with all the alternatives.  *)
	 let acceptFull (m: 'm) (w: word)
				 (initial: 'm -> word -> 'c set)
				 (next: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool): bool * 'c path * 'c trail =
		 let base (r: bool) (configs: 'c set): bool * 'c path * 'c trail =
			 let accepting = Set.filter (isAccepting m) configs in
			 let c = Set.hd (if Set.isEmpty accepting then configs else accepting) in
				 (r, [c], [configs])
		 in
		 let rec acceptX (configs: 'c set) (seen: 'c set) : bool * 'c path * 'c trail =
			 let newConfigs = Set.diff configs seen in
			 let newSeen = Set.unionUnsafe newConfigs seen in
			 if Set.isEmpty newConfigs then
				 (false, [], [])
			 else if Set.exists (isAccepting m) newConfigs then
				 base true newConfigs
			 else if RuntimeControl.giveUp (Set.size newSeen) then
				 base false newConfigs
			 else
				 let nextConfigs = Set.flatMap (next m) newConfigs in
				 let (r,p,t) = acceptX nextConfigs newSeen in
					 match p with
					 | [] ->
						 base r newConfigs
					 | x::_ ->
						 let c = Set.find (fun c -> Set.belongs x (next m c)) newConfigs in
							 (r, c::p, newConfigs::t)
		 in
		 let _ = RuntimeControl.start () in
		 let initialConfigs = initial m w in
			 acceptX initialConfigs Set.empty
	 
	 (* invariant - for_all c in seen: c <= len *)
	 let generate (m: 'm) (len: int)
				 (initial: 'm -> word -> 'c set)
				 (next2: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool)
				 (getWord: 'c -> word): words =
 
		 let strict = len < 0 in
		 let len = abs len in
		 let lenWord c = List.length (getWord c) in
		 let isNew seen c = lenWord c <= len && not (Set.belongs c seen) in
		 let isExpanding c = lenWord c < len || not (isAccepting m c) in
		 let finalSelection =
			 if strict then (fun c -> isAccepting m c && lenWord c = len) 
					 else (fun c -> isAccepting m c) 
		 in
		 let rec generateX (configs: 'c set) (seen: 'c set): 'c set =
			 let newConfigs = Set.filter (isNew seen) configs in
			 let newSeen = Set.unionUnsafe newConfigs seen in
			 let toExpand = Set.filter isExpanding newConfigs in
				 if Set.isEmpty toExpand || RuntimeControl.giveUp (Set.size newSeen) then
					 newSeen
				 else
					 let nextConfigs = Set.flatMap (next2 m) toExpand in
						 generateX nextConfigs newSeen
		 in
		 let _ = RuntimeControl.start () in
		 let initialConfigs = initial m (word "") in
		 let collected = generateX initialConfigs Set.empty in
		 let selected = Set.filter finalSelection collected in
			 Set.map getWord selected
 
	 (* generate and test. Will be improved. *)
	 let generateDumb (m: 'm) (alphabet : symbols) (len: int)
				 (initial: 'm -> word -> 'c set)
				 (next: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool): words =
				 
		 let addAll symb = List.map (fun l -> symb::l) in
		 let rec combinations n l =
			 if n = 0 then [[]]
			 else let p = combinations (n-1) l in
					 List.flatten (List.map  (fun x -> addAll x p) l)
		 in
		 let rec combinations2 n l =
			 if n = 0 then [[]] else combinations2 (n-1) l @ combinations n l in
		 let strict = len < 0 in
		 let len: int = abs len in
		 let s: word = Set.toList alphabet in
		 let comb: word list = if strict then combinations len s else combinations2 len s in
		 let accept w: bool = accept m w initial next isAccepting in
		 let selected: word list = List.filter accept comb in
			 Set.make selected
 
 end
 
 module Model =
 struct
	 include ModelSupport
 
	 let stats = ModelPrivate.stats 
	 let accept = ModelPrivate.accept
	 let acceptFull = ModelPrivate.acceptFull
	 let generate = ModelPrivate.generate	
	 let generateDumb =  ModelPrivate.generateDumb
 
	 class virtual model (arg: ('r,'x) Arg.alternatives) (expectedKind: string) =
		 object(self) inherit Entity.entity arg expectedKind
 
			 method virtual accept: word -> bool
			 (*method virtual acceptFull: 'c. word ->  bool * 'c path * 'c trail*)
			 method virtual generate: int -> words
			 (*method virtual generateDumb: int -> words*)
			 
		 (* Exercices support *)
			 method checkProperty (prop: string) = checkProperty prop
			 method checkExercise (exercise: Exercise.exercise) =
				 checkExercise exercise#representation self#accept self#checkProperty
			 method checkExerciseFailures (exercise: Exercise.exercise) =
				 checkExerciseFailures exercise#representation self#accept self#checkProperty
 
		 (* Learn-OCaml support *)
			 method virtual moduleName: string
			 method virtual xTypeName: string
			 method virtual xTypeDeclString : string
			 method virtual toDisplayString: string -> string
			 method virtual example : JSon.t
	 end
 
 end
 
 (* this is only a test *)
 class virtual cModel (arg: ('r,'x) Arg.alternatives) (expectedKind: string) =
	 let open Model in
	 object(self) inherit Entity.entity arg expectedKind
 
		 method virtual accept: word -> bool
		 method virtual acceptFull: 'c. word ->  bool * 'c path * 'c trail
		 method virtual generate: int -> words
		 method virtual generateDumb: int -> words
		 
	 (* Exercices support *)
		 method checkProperty (prop: string) = checkProperty prop
		 method checkExercise (exercise: Exercise.exercise) =
			 checkExercise exercise#representation self#accept self#checkProperty
		 method checkExerciseFailures (exercise: Exercise.exercise) =
			 checkExerciseFailures exercise#representation self#accept self#checkProperty
 
	 (* Learn-OCaml support *)
		 method virtual moduleName: string
		 method virtual xTypeName: string
		 method virtual xTypeDeclString : string
		 method virtual toDisplayString: string -> string
		 method virtual example : JSon.t
 end
 
 
 
 (*
 SAVE- old versions that might be useful again
 
	 (* trail alone *)
	 let acceptTrail (m: 'm) (w: word)
				 (initial: 'm -> word -> 'c set)
				 (next: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool): bool * 'c trail =
 
		 let rec acceptX (configs: 'c set) (seen: 'c set) (trail: 'c trail): bool * 'c trail =
			 let newConfigs = Set.diff configs seen in
			 let newSeen = Set.unionUnsafe newConfigs seen in
			 let newTrail = newConfigs::trail in
			 if Set.isEmpty newConfigs then (false, trail)
			 else if Set.exists (isAccepting m) newConfigs then (true, newTrail)
			 else if RuntimeControl.giveUp (Set.size newSeen) then (false, newTrail)
			 else
				 let nextConfigs = Set.flatMap (next m) newConfigs in
					 acceptX nextConfigs newSeen newTrail
		 in
		 let _ = RuntimeControl.start () in
		 let initialConfigs = initial m w in
		 let (b, trail) = acceptX initialConfigs Set.empty [] in
			 (b, List.rev trail)
 
	 (* path calculated from the trail *)
	 let acceptPath (m: 'm) (w: word)
				 (initial: 'm -> word -> 'c set)
				 (next: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool): 'c path =
 
		 let rec acceptX (trail: 'c trail): 'c path =
			 match trail with
			 | [] -> failwith "acceptX"
			 | [c] ->
				 let a = Set.filter (isAccepting m) c in
					 [Set.hd (if Set.isEmpty a then c else a)]
			 | c::cs ->
				 (match acceptX cs with
				 | [] ->  failwith "acceptX"
				 | p::ps ->
					 let n = Set.find (fun c -> Set.belongs p (next m c)) c in
						 n::p::ps)
		 in
		 let (_, trail) = acceptTrail m w initial next isAccepting in
			 acceptX trail
 
			 let acceptPaths (m: 'm) (w: word)
				 (initial: 'm -> word -> 'c)
				 (next: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool): 'c path set =
 
 
	 (* all the paths alone *)
		 let rec acceptX (paths: 'c path set) (seen: 'c set): 'c path set =
			 let configs = Set.map List.hd paths in
			 let newConfigs = Set.diff configs seen in				(* select the new *)
			 let seen = Set.unionUnsafe newConfigs seen in			(* build new seen *)
			 if Set.isEmpty newConfigs then							(* case repetition *)
				 Set.map List.tl paths
			 else if Set.exists (isAccepting m) newConfigs then		(* case accept *)
				 Set.filter (fun p -> isAccepting m (List.hd p)) paths
			 else
				 let isNewPath p = Set.belongs (List.hd p) newConfigs in
				 let nextPathsOne p = Set.map (fun c -> c::p) (next m (List.hd p))  in
				 let newPaths = Set.filter isNewPath paths in
				 let nextPaths = Set.flatMap nextPathsOne newPaths in
				 if Set.size nextPaths = 0 then paths				(* case no-followup *)
				 else	acceptX nextPaths seen
		 in	
		 let initialConfig = initial m w in
		 let paths = acceptX (Set.make [[initialConfig]]) Set.empty in
			 Set.map List.rev paths
	 let acceptPath (m: 'm) (w: word)
				 (initial: 'm -> word -> 'c)
				 (next: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool): 'c path =
		 let ps = acceptPaths m w initial next isAccepting in
		 let min p1 p2 = if List.length p1 <= List.length p2 then p1 else p2 in
			 Set.fold_left min (Set.hd ps) (Set.tl ps)
 
	 (* trail and all the paths  *)
	 let acceptFull (m: 'm) (w: word)
				 (initial: 'm -> word -> 'c set)
				 (next: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool): bool * 'c path * 'c trail =
 
		 let rec acceptX (paths: 'c path set) (trail: 'c trail) (seen: 'c set)
													 : bool * 'c path set * 'c trail =
			 let configs = Set.map List.hd paths in
				 if Set.exists (isAccepting m) configs then			(* case accept *)
					 (true, paths, trail)
				 else if RuntimeControl.giveUp (Set.size seen) then
					 (false, paths, trail)
				 else
					 let nextConfigsOne p = next m (List.hd p) in
					 let newConfigsOne p = Set.diff (nextConfigsOne p) seen in
					 let newPathsOne p = Set.map (fun c -> c::p) (newConfigsOne p) in
					 let newPaths = Set.flatMap newPathsOne paths in
					 let newConfigs = Set.map List.hd newPaths in
					 let newTrail = newConfigs::trail in
					 let newSeen = Set.unionUnsafe newConfigs seen in
						 if Set.isEmpty newConfigs then				(* case reject *)
							 (false, paths, trail)
						 else
							 acceptX newPaths newTrail newSeen
		 in
		 let _ = RuntimeControl.start () in
		 let initialConfigs = initial m w in
		 let initialPaths = Set.map (fun c -> [c]) initialConfigs in
		 let initialTrail = [initialConfigs] in
		 let initialSeen = initialConfigs in
		 let (r, ps, t) = acceptX initialPaths initialTrail initialSeen in
		 let (r, ps, t) = (r, Set.map List.rev ps, List.rev t) in
		 let fps = Set.filter (fun p -> isAccepting m (List.hd p)) ps in
			 (r, Set.hd (if Set.isEmpty fps then ps else fps), t)
 
	 (* full from trail and path  *)
	 let acceptFull (m: 'm) (w: word)
				 (initial: 'm -> word -> 'c)
				 (next: 'm -> 'c -> 'c set)
				 (isAccepting: 'm -> 'c -> bool): bool * 'c path * 'c trail =
		 let p = acceptPath m w initial next isAccepting in
		 let (r,t) = acceptTrail m w initial next isAccepting in
			 (r, p, t)
 *)
# 1 "src/FinAutSyntax.ml"
(*
 * FinAutSyntax.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * sep/2022 (amd) - New module
 *)

(*
 * Description: Support types and functions for FAs.
 *)

open BasicTypes

module FinAutTypes =
struct
	type tx = {
		alphabet : symbolX list;
		states : state list;
		initialState : state;
		transitions : transition3X list;
		acceptStates : state list
	}
	type finiteAutomaton =
		tx
	type t = {
		alphabet : symbols;
		states : states;
		initialState : state;
		transitions : transitions3;
		acceptStates : states
	}
	type fa =
		t
end

module FinAutConversions =
struct
	open FinAutTypes

	let internalize (fa: tx): t = {
		alphabet = symbolsX2symbols fa.alphabet;
		states = Set.make fa.states;
		initialState = fa.initialState;
		transitions = transsX2transs3 fa.transitions;
		acceptStates = Set.make fa.acceptStates
	}

	let externalize (fa: t): tx = {
		alphabet = symbols2symbolsX fa.alphabet;
		states = Set.toList fa.states;
		initialState = fa.initialState;
		transitions = transs2transsX3 fa.transitions;
		acceptStates = Set.toList fa.acceptStates
	}

	let fromJSon (j: JSon.t): t =
		if JSon.isNull j || not (JSon.hasField j "kind") then {
			alphabet = Set.empty;
			states = Set.make [draftState];
			initialState = draftState;
			transitions = Set.empty;
			acceptStates = Set.empty
		}
		else {
			alphabet = JSon.fieldSymbolSet j "alphabet";
			states = JSon.fieldStateSet j "states";
			initialState = JSon.fieldState j "initialState";
			transitions = JSon.fieldTriplesSet j "transitions";
			acceptStates = JSon.fieldStateSet j "acceptStates"
		}

	let toJSon (id: JSon.t) (rep: t): JSon.t =
		let body =
			JSon.makeAssoc [
				("alphabet", JSon.makeSymbolSet rep.alphabet);
				("states", JSon.makeStateSet rep.states);
				("initialState", JSon.makeState rep.initialState);
				("transitions", JSon.makeTriplesSet rep.transitions);
				("acceptStates", JSon.makeStateSet rep.acceptStates)
			]
		in JSon.append id body
end

module FinAutForLearnOCaml =
struct
	open FinAutTypes

	let moduleName =
		"FiniteAutomaton"

	let xTypeName =
		"finiteAutomaton"

	let displayHeader (name: string) (xTypeName: string) =
		if name = "" then ""
		else ("let " ^ name ^ ": " ^ xTypeName ^ " =\n\t\t")

	let solution (name: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s{
			alphabet = %s;
			states = %s;
			initialState = %s;
			transitions = %s;
			acceptStates = %s
		}
		|zzz}	(* please, do not change this line *)
			(displayHeader name xTypeName)
			(symbolsX2display repx.alphabet)
			(statesX2display repx.states)
			(state2display repx.initialState)
			(transsX2display3 repx.transitions)
			(statesX2display repx.acceptStates)


	let prelude : string =
		Printf.sprintf {zzz|
		type symbol = %s
		type state = string
		type finiteAutomaton = {
			alphabet : symbol list;
			states : state list;
			initialState : state;
			transitions : (state * symbol * state) list;
			acceptStates : state list
		}
		|zzz}	(* please, do not change this line *)
			symbolTypeName

	let example : JSon.t =
		JSon.parse {|
		{
			kind : "finite automaton",
			description : "this is an example",
			name : "example",
			alphabet: ["w", "z"],
			states : ["START", "X", "Z"],
			initialState : "START",
			transitions : [
				["START", "w", "X"], ["X", "z", "X"]
			],
			acceptStates : ["Z"]
		}
		|}	(* please, do not change this line *)
end
# 1 "src/FiniteAutomaton.ml"
(*
 * FiniteAutomaton.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Improved Learn-OCaml support and error handling.
 * jun/2021 (amd) - Added checks for epsilon ('~') in the #validate method.
 * may/2021 (amd) - Added support for an extern representation.
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * dec/2019 (jg) - Main functionalities.
 * jun/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Finite automata functionality.
 *
 * TODO: More cleanup.
 *)

open BasicTypes

module type FiniteAutomatonSig = sig
	open FinAutTypes

	val modelDesignation : string
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation : t
				method representationx : tx
				method validate : unit

				method tracing : unit

				method acceptBreadthFirst: word -> bool
				method accept : word -> bool
				method acceptWithTracing : word -> unit
				method generate : int -> words
				method generateUntil : int -> words
				method reachable : state -> states
				method productive : states
				method getUsefulStates : states
				method getUselessStates : states
				method cleanUselessStates: model
				method areAllStatesUseful: bool

				method toDeterministic : model
				method isDeterministic : bool
				method equivalencePartition: states set
				method minimize : model
				method isMinimized : bool

			(* Exercices support *)
				method checkProperty : string -> bool
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties

			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t
			end
end

module FiniteAutomaton : FiniteAutomatonSig =
struct	
	open FinAutTypes

	let modelDesignation = "finite automaton"

	(*------Auxiliary functions---------*)

	(* get starting state, symbol, and/or end state of all transitions in set *)
	let transitionGet1 trns = Set.map ( fun (a,_,_) -> a ) trns
	let transitionGet2 trns = Set.map ( fun (_,b,_) -> b ) trns
	let transitionGet3 trns = Set.map ( fun (_,_,c) -> c ) trns
	let transitionGet23 trns = Set.map (fun (_,b,c) -> (b,c)) trns

	(* fuse all states into a new state *)
	let fuseStates sts =
		let l = List.map state2str sts in
			state (String.concat "_" l)


	(* checks if set ts has at least one transition from state st through symbol sy *)
	let hasTrans st sy ts = Set.exists (fun (x,y,_) -> x = st && y = sy) ts

	(* returns the set of state st and all its states reachable by an epsilon transition *)
	let nextEpsilon1 st ts =
		let trns = Set.filter (fun (a,b,c) -> st = a && b = epsilon) ts in
		let nextStates = transitionGet3 trns in
			Set.add st nextStates

	(* returns the set of states sts and all states reachable from sts through epsilon transitions *)
	let rec closeEmpty sts t =
		let ns = Set.flatMap (fun st -> nextEpsilon1 st t) sts in
			if (Set.subset ns sts) then ns else closeEmpty (Set.union sts ns) t

	(* returns states reachable from st through symbol sy *)
	let nextStates st sy t =
		let n = Set.filter (fun (a,b,c) -> st = a && sy = b) t in
			transitionGet3 n
		
	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super
		
			val representation: t =
				match arg with
				| Arg.Representation r -> r
				| Arg.RepresentationX r -> FinAutConversions.internalize r
				| _ -> FinAutConversions.fromJSon (Arg.fromAlternatives arg)
					
			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation: t =
				representation

			method representationx: tx =
				FinAutConversions.externalize representation

			method toJSon: JSon.t =
				FinAutConversions.toJSon (super#toJSon) representation

			(**
			* This method verifies if the automaton is valid.
			* An automaton is considered valid if its initial and acceptance states belong to the set of all its states
			* and if all its transitions have states and symbols belonging to the set of all its states and its alphabet respectively.
			*
			* Desc: If the automaton is invalid, the cause could derive from any combination of the following
			* three options: either the initial state, one of the acceptance states, or one of the transitions does not follow the
			* previously discussed predicate. This method will print to the console stating which combination of these options caused
			* the automaton to be invalid
			*)
			method validate: unit = (

				(* the alphabet must not contain " " *)
				let validAlphabet = not (Set.belongs epsilon representation.alphabet) in

				(* does initial state belong to the set of all states *)
				let validInitSt = Set.belongs representation.initialState representation.states in

				(* are all accepted states members of all states *)
				let validAccSts = Set.subset representation.acceptStates representation.states in

				let fromSt = transitionGet1 representation.transitions in
				let sy = transitionGet2 representation.transitions in
				let toSt = transitionGet3 representation.transitions in
				let alpha = Set.add epsilon representation.alphabet in

				(* do all transitions have states belonging to all states and symbols belonging to the alphabet *)
				let validTrns = (Set.subset fromSt representation.states) &&
					(Set.subset sy alpha) && (Set.subset toSt representation.states) in

				if not validAlphabet then
					Error.error self#id.Entity.name
						"The alphabet contains epsilon '~', and it should not" ()
				;

				if not validInitSt then
					Error.error self#id.Entity.name
						"The initial state does not belong to the set of all states" ()
				;

				if not validAccSts then
					Error.error self#id.Entity.name
						"Some accept states do not belong to the set of all states" ()
				;

				if not validTrns then
					Error.error self#id.Entity.name
						"Some transitions are invalid" ()
				)

			method tracing : unit = ()



			(**
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be tested for acceptance
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			* Desc: Checks if the automaton accepts word w using configurations (that is, pairs formed by a state and
			* a remaining word) and a breadth-first approach as to deal with potential non-termination
			*)
			method acceptBreadthFirst(w: word): bool = false
			(*
				let rec acc cf t sta =
					match cf with
						[] -> false
						|(st,[])::ls ->
							let accepts = (Set.inter (closeEmpty (Set.make [st]) t) sta) <> Set.empty in
								accepts || acc ls t sta
						|(st,x::xs)::ls ->
							let n = nextStates st x t in
							let cfn = Set.map (fun c -> (c,xs)) n in
							let n2 = nextStates st epsilon t in
							let cfn2 = Set.map (fun c -> (c,x::xs)) n2 in
								acc (Set.flatten (Set.make [ls;cfn;cfn2])) t sta in
				acc (Set.make [(representation.initialState,w)]) representation.transitions representation.acceptStates
			*)

			(**
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be accepted
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			* Desc: Checks if the automaton accepts word w using functions over sets of states
			*)
			method accept (w: word): bool =

				let transition sts sy t =
					let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
						Set.union nsts (closeEmpty nsts t) in

				let rec acceptX sts w t =
					match w with
						[] -> (Set.inter sts representation.acceptStates) <> Set.empty
						|x::xs -> let nextSts = transition sts x t in
							nextSts <> Set.empty && acceptX nextSts xs t in

				let i = closeEmpty (Set.make [representation.initialState]) representation.transitions in
					acceptX i w representation.transitions



			method acceptWithTracing (w:word): unit =

				let transition sts sy t =
					let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
						Set.union nsts (closeEmpty nsts t) in

				let rec acceptX sts w t =
					match w with
						[] -> [(w,sts)]
						|x::xs -> let nextSts = transition sts x t in
									let res = acceptX nextSts xs t in
										(w,sts)::res
				in

				let i = closeEmpty (Set.make [representation.initialState]) representation.transitions in

				let res = acceptX i w representation.transitions in

				let printRes w sts =
					Util.print ["('"; word2str w; "',["];
					Set.iter (fun st -> Util.print [state2str st; ";"]) sts;
					Util.print ["])"];

				in

				List.iter (fun (w,sts) -> printRes w sts; Util.print [";"]) res; Util.println []




			(**
			* This method generates all words of the given size which are accepted by the automaton
			*
			* Precondition -> length >= 0
			*
			* @param length:int -> size of all words to be generated
			*
			* @returns words -> the set of all words with size length
			*)
			method generate (length: int): words =

				(* adds symbol to the left of all words *)
				let addSyToRWords symb ws = Set.map (fun l -> symb::l) ws in

				let hasAcceptState sts accSts = Set.exists (fun st -> Set.belongs st accSts) sts in
				let nxtNonEmptyTrns st ts = Set.filter (fun (a,b,_) -> a = st && b <> epsilon) ts in

				let rec gen n state transitions accSts =

					let clsEmpty = (closeEmpty (Set.make [state]) transitions) in
					if n = 0 then
						if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty

					else
						let trnsSet = Set.flatMap (fun st -> nxtNonEmptyTrns st transitions ) clsEmpty in

						let rwords st1 l1 = gen (l1-1) st1 transitions accSts in
						let genX sy st l = addSyToRWords sy (rwords st l) in

								Set.flatMap (fun (_,sy,st) -> genX sy st n) trnsSet
				in
				gen length representation.initialState representation.transitions representation.acceptStates


			(**
			* This method generates all words up to a given size which are accepted by the automaton
			*
			* Precondition -> length >= 0
			*
			* @param length:int -> maximum size of all words to be generated
			*
			* @returns words -> the set of all words with size length or less
			*)
			method generateUntil (length: int): words =

				(* adds symbol to the left of all words *)
				let addSyToRWords symb ws = Set.map (fun l -> symb::l) ws in

				let hasAcceptState sts accSts = Set.exists (fun st -> Set.belongs st accSts) sts in
				let nxtNonEmptyTrns st ts = Set.filter (fun (a,b,_) -> a = st && b <> epsilon) ts in

				let rec gen n state transitions accSts =

					let clsEmpty = (closeEmpty (Set.make [state]) transitions) in
					if n = 0 then
						if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty

					else
						let trnsSet = Set.flatMap (fun st -> nxtNonEmptyTrns st transitions ) clsEmpty in
						let genX sy st l = addSyToRWords sy (gen (l-1) st transitions accSts) in
						let lenOneOrMore = Set.flatMap (fun (_,sy,st) -> genX sy st n) trnsSet in
						let lenZero = if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty in
							Set.union lenOneOrMore lenZero
				in
				gen length representation.initialState representation.transitions representation.acceptStates


			(**
			* This method generates all states that are reachable from the given state. A state is reachable from s if there
			* exists a word that starting on s will lead to that state
			*
			* @param s:state -> the given state
			*
			* @returns states -> the set of all states reachable from s.
			*)
			method reachable (s:state): states =

				let neighbourSts st t = transitionGet3 (Set.filter (fun (a,_,_) -> a = st) t) in
				let nextStates sts t = Set.flatMap (fun st -> neighbourSts st t) sts in
				let remain s t = Set.filter (fun (a,_,_) -> not (Set.belongs a s)) t in

				let rec reach visited s t = if visited = s then Set.empty else Set.union s ( reach s (nextStates s t) (remain s t) ) in
					reach Set.empty (Set.make [s]) representation.transitions



			(**
			* This method generates all productive states. A state is productive if there exists a word that will lead said state
			* to an acceptance state
			*
			* @returns states -> the set of all productive states
			*
			* Desc: For each state of the automaton, this method applies the reachable method and checks if any of the resulting
			* states is an acceptance state, and if it is then that state will belong to the resulting set of productive states
			*)
			method productive: states =

				let reachsAccSt st = Set.exists (fun s -> Set.belongs s representation.acceptStates ) (self#reachable st) in
					Set.filter (fun st -> reachsAccSt st) representation.states

			(**
			* This method generates the set of all useful states
			*
			* @returns states -> the set of all useful states
			*)
			method getUsefulStates: states =
				Set.inter self#productive (self#reachable representation.initialState)


			(**
			* This method generates the set of all non useful states
			*
			* @returns states -> the set of all non useful states
			*)
			method getUselessStates: states =
				Set.diff representation.states self#getUsefulStates


			(**
			* This method creates the equivalent automaton where all states are useful
			*
			* @returns FiniteAutomaton.model -> the new equivalent automaton where all states are useful
			*
			* Desc: The new automaton is created by eliminating from the original automaton all its non useful states, all transitions
			* that have a non useful state, and all symbols of the alphabet that only appear in said transitions
			*)

			method cleanUselessStates: model =

				let usfSts = self#getUsefulStates in
				let usfTrs = Set.filter (fun (a,_,c) -> Set.belongs a usfSts &&
														Set.belongs c usfSts)
								representation.transitions in

				let alf = transitionGet2 usfTrs in
				let usfAlf = Set.diff alf (Set.make [epsilon]) in
				let newAccSts = Set.inter representation.acceptStates usfSts in
				let usfSts = Set.add representation.initialState usfSts in

				let fa = {
						alphabet = usfAlf;
						states = usfSts;
						initialState = representation.initialState;
						transitions = usfTrs;
						acceptStates = newAccSts
				} in
					new model (Arg.Representation fa)


			(**
			* This method verifies if all the automaton's states are useful
			*
			* @returns bool -> true if all states of the automaton are useful, false otherwise
			*)
			method areAllStatesUseful: bool =

				let usfSts = self#getUsefulStates in
					Set.size representation.states = Set.size usfSts


			(**
			* This method converts the non-deterministic automaton into its deterministic equivalent
			*
			* @returns FiniteAutomaton.model -> the new deterministic automaton
			*
			* Desc: If the automaton to determinize is already deterministic,
			* the resulting automaton will be equal to the original
			*)
			method toDeterministic: model =

				let move sts sy ts = Set.flatMap (fun st -> nextStates st sy ts ) sts in

				(* generates the set of states reachable from the given state set though the given symbol *)
				let newR oneR sy ts =
					let nxtSts = move oneR sy ts in
					let clsempty = closeEmpty nxtSts ts in
					Set.union nxtSts clsempty in

				(* creates all transitions (given state set, a given symbol, states reachable from set through given symbol) *)
				let rToTs r =
					let nxtTrans = Set.map (fun sy -> (r,sy,newR r sy representation.transitions)) representation.alphabet in
						Set.filter (fun (_,_,z) -> not (z = Set.empty)) nxtTrans in

				(* applies previous function to all state sets until no new set is generated *)
				let rec rsToTs stsD rD trnsD alph =
					let nxtTs = Set.flatMap (fun stSet -> rToTs stSet ) rD in
					let nxtRs = Set.map (fun (_,_,z) -> z) nxtTs in
					let newRs = Set.filter (fun r -> not (Set.belongs r stsD)) nxtRs in
					if newRs = Set.empty then (Set.union trnsD nxtTs) else
						rsToTs (Set.union newRs stsD) newRs (Set.union trnsD nxtTs) alph in


				let r1 = closeEmpty (Set.make [representation.initialState]) representation.transitions in

				(* all transitions of the new deterministic automaton *)
				let trnsD = rsToTs (Set.make [r1]) (Set.make [r1]) Set.empty representation.alphabet in

				let tds = Set.map (fun (a,b,c) -> (fuseStates (Set.toList a), b, fuseStates (Set.toList c))) trnsD in

				let newInitialState = fuseStates (Set.toList r1) in

				let stSet1 = Set.map (fun (a,_,_) -> a) trnsD in
				let stSet2 = Set.map (fun (_,_,c) -> c) trnsD in
				let stSet = Set.union stSet1 stSet2 in

				let isAccepState st = Set.belongs st representation.acceptStates in
				let hasAnAccepSt set = Set.exists (fun st -> isAccepState st ) set in
				let newAccStsSet = Set.filter (fun set -> hasAnAccepSt set) stSet in

				let newAllSts = Set.map (fun set -> fuseStates (Set.toList set)) stSet in
				let newAccSts = Set.map (fun set -> fuseStates (Set.toList set)) newAccStsSet in


				new model (Arg.Representation {
								alphabet = representation.alphabet;
								states = newAllSts;
								initialState = newInitialState;
								transitions = tds;
								acceptStates = newAccSts
						} )

			(**
			* This method verifies if the automaton is deterministic
			*
			* @returns bool -> true if automaton is deterministic, false otherwise
			*
			* Desc: For each state s, this method checks if there exists 2 or more transitions with the same symbol from any
			* state belonging to closeempty of s, independently of the state which said transitions will lead to.
			* If there is no state for which this property is true, then the automaton is deterministic
			*)
			method isDeterministic: bool =

				let trnsFromSt st ts = Set.filter (fun (st1,sy,_) -> st1 = st && sy <> epsilon) ts in

				let isStDeter st ts =
					let allSts = closeEmpty (Set.make [st]) ts in
					let allTs = Set.flatMap (fun st -> trnsFromSt st ts) allSts in
					let sys = transitionGet2 allTs in
						Set.size allTs = Set.size sys in

				let hasNondeterSt = Set.exists (fun st -> not (isStDeter st representation.transitions) )
										representation.states in
					not hasNondeterSt



			(* partition states by equivalence *)
			method equivalencePartition: states set =

				let fa = self#toDeterministic in

				let fa2 = fa#cleanUselessStates in


				let rep = fa2#representation in

				let (inF, notF) = Set.partition (fun x -> Set.belongs x rep.acceptStates) rep.states in
				let distI1 = Set.combinations inF notF in

				let hasTransMulti sts sy ts = Set.partition (fun st -> hasTrans st sy ts) sts in
				let distI2 = Set.flatMap (fun sy -> Util.distrib2 Set.combinations
													(hasTransMulti rep.states sy rep.transitions))
								rep.alphabet in


				let distI = Set.union distI1 distI2 in

				let stsXSts = Set.combinations rep.states rep.states in

				(* generates all pairs of states that can reach the pair (st1,st2) through a transition with symbol sy *)
				let reachingSts st1 st2 sy p =
					let t1 = Set.filter (fun (_,y,z) -> z = st1 && y = sy) rep.transitions in
					let t2 = Set.filter (fun (_,y,z) -> z = st2 && y = sy) rep.transitions in
					let s1 = transitionGet1 t1 in
					let s2 = transitionGet1 t2 in
						Set.diff (Set.combinations s1 s2) p in

				let findAR p q = Set.flatMap (fun (a,b) -> Set.flatMap (fun sy -> reachingSts a b sy p) rep.alphabet) q in

				let distA = findAR distI distI in

				let rec aped p q = if (q = Set.empty || (Set.union p q) = stsXSts) then Set.union p q
					else aped (Set.union p q) (findAR (Set.union p q) q ) in

				let dist = aped distI distA in


				(* given for example states a b c d generates (a,a) (a,b) (a,c) (a,d) (b,b) (b,c) (b,d) (c,c) (c,d) (d,d) *)
				let rec halfCombs sts =
					match sts with
						[] -> Set.empty
						|x::xs -> Set.union (Set.combinations (Set.make [x]) (Set.make sts)) (halfCombs xs) in

				let halfTriang = halfCombs (Set.toList rep.states) in

				(* given set of equivalent states dicti, substitutes state st for its leftmost equivalent state according to dicti *)
				let rec translate st dicti =
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in

				(* the set of equivalent state pairs are those not present in the set of distinct state pairs *)
				let equiv = Set.filter ( fun (a,b) -> not (Set.belongs (a,b) dist) &&
														not (Set.belongs (b,a) dist) ) halfTriang in

				let equivList = Set.toList equiv in
				let hasAny st1 st2 sta stb = (translate st1 equivList) = sta || (translate st2 equivList) = sta
											|| (translate st1 equivList) = stb || (translate st2 equivList) = stb in


				let rec agroup eq =
					match eq with
						| [] -> Set.empty
						| (a,b)::ls ->
							let (part1,part2) = Set.partition (fun (x,y) -> hasAny x y a b) (Set.make eq) in
							let gRemain = Set.flatMap (fun (c,d) -> Set.make [c;d]) part1 in
								Set.add (Set.union (Set.make [a;b]) gRemain) (agroup (Set.toList part2))
				in

				agroup equivList


			(**
			* This method minimizes the automaton
			*
			* @returns FiniteAutomaton.model -> the new minimal equivalent automaton
			*
			* Desc: The given automaton is minimized according to the process described in lecture a15.
			*)
			method minimize: model =

				let fa = self#toDeterministic in

				let fa2 = fa#cleanUselessStates in


				let rep = fa2#representation in

				let (inF, notF) = Set.partition (fun x -> Set.belongs x rep.acceptStates) rep.states in
				let distI1 = Set.combinations inF notF in

				let hasTransMulti sts sy ts = Set.partition (fun st -> hasTrans st sy ts) sts in
				let distI2 = Set.flatMap (fun sy -> Util.distrib2 Set.combinations
													(hasTransMulti rep.states sy rep.transitions))
								rep.alphabet in


				let distI = Set.union distI1 distI2 in

				let stsXSts = Set.combinations rep.states rep.states in

				(* generates all pairs of states that can reach the pair (st1,st2) through a transition with symbol sy *)
				let reachingSts st1 st2 sy p =
					let t1 = Set.filter (fun (_,y,z) -> z = st1 && y = sy) rep.transitions in
					let t2 = Set.filter (fun (_,y,z) -> z = st2 && y = sy) rep.transitions in
					let s1 = transitionGet1 t1 in
					let s2 = transitionGet1 t2 in
						Set.diff (Set.combinations s1 s2) p in

				let findAR p q = Set.flatMap (fun (a,b) -> Set.flatMap (fun sy -> reachingSts a b sy p) rep.alphabet) q in

				let distA = findAR distI distI in

				let rec aped p q = if (q = Set.empty || (Set.union p q) = stsXSts) then Set.union p q
					else aped (Set.union p q) (findAR (Set.union p q) q ) in

				let dist = aped distI distA in


				(* given for example states a b c d generates (a,b) (a,c) (a,d) (b,c) (b,d) (c,d) *)
				let rec halfCombs sts =
					match sts with
						[] -> Set.empty
						|x::xs -> Set.union (Set.combinations (Set.make [x]) (Set.make xs)) (halfCombs xs) in
				let halfTriang = halfCombs (Set.toList rep.states) in

				(* given set of equivalent states dicti, substitutes state st for its leftmost equivalent state according to dicti *)
				let rec translate st dicti =
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in

				(* the set of equivalent state pairs are those not present in the set of distinct state pairs *)
				let equiv = Set.filter ( fun (a,b) -> not (Set.belongs (a,b) dist) &&
														not (Set.belongs (b,a) dist) ) halfTriang in

				let equivList = Set.toList equiv in

				let eq = Set.map (fun (a,b) -> b) equiv in
				let newSts = Set.diff rep.states eq in
				let newInitSt = translate rep.initialState equivList in
				let newAccSts = Set.inter rep.acceptStates newSts in
				let newTrans = Set.map (fun (x,y,z) -> (translate x equivList,y,translate z equivList) ) rep.transitions in


				new model (Arg.Representation {
								alphabet = rep.alphabet;
								states = newSts;
								initialState = newInitSt;
								transitions = newTrans;
								acceptStates = newAccSts
						} )


			(**
			* This method verifies if the automaton is minimal
			*
			* @returns boolean -> true if automaton is minimal, false otherwise
			*
			* Desc: The given automaton is considered minimal if the result of minimizing it is an automaton with the same
			* number of states
			*)
			method isMinimized: bool =

				let fa = self#minimize in
				let rep = fa#representation in
					Set.size representation.states = Set.size rep.states

			method checkProperty (prop: string) =
				match prop with
					| "deterministic" -> self#isDeterministic
					| "minimized" -> self#isMinimized
					| "finite automaton" -> true
					| _ -> super#checkProperty prop		
					
		(* Learn-OCaml support *)
			method moduleName = FinAutForLearnOCaml.moduleName
			method xTypeName = FinAutForLearnOCaml.xTypeName
			method xTypeDeclString : string = FinAutForLearnOCaml.prelude
			method toDisplayString (name: string): string =
				FinAutForLearnOCaml.solution name self#representationx
			method example : JSon.t = FinAutForLearnOCaml.example

		end
end

module FiniteAutomatonTests : sig end =
struct
	let active = false

	let test0 () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
			let j = fa#toJSon in
				JSon.show j

	let testBug () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let fa2 = fa#toDeterministic in
			let j = fa2#toJSon in
				JSon.show j;
		let fa3 = fa2#cleanUselessStates in
			let j = fa3#toJSon in
				JSon.show j

	let testBug2 () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let fa2 = fa#toDeterministic in
			Util.println ["productive states:"];
			Util.printStates fa2#productive;
			Util.println []

	let faAccept = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "ab123",
		alphabet : ["a", "b"],
		states : ["1", "2", "3"],
		initialState : "1",
		transitions : [
				["1","a","2"], ["1","b","3"],
				["2","b","2"],
				["3","a","3"]
			],
		acceptStates : ["2", "3"]
	} |}

	let faAccept2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b", "c", "d"],
		states : ["START", "A", "AB", "C", "SUCCESS", "D"],
		initialState : "START",
		transitions : [
				["START","a","A"], ["START","~","AB"],
				["A","~","C"],
				["AB","b","SUCCESS"], ["AB","~","SUCCESS"],
				["C","~","SUCCESS"], ["C","d","C"],
				["SUCCESS","~","START"]
			],
		acceptStates : ["SUCCESS"]
	} |}

	let check f w =
		let msg = 
			if f w then "word was accepted" else "word was not accepted"
		in Util.println [msg]

	let testAcceptBF () =
		let fa = new FiniteAutomaton.model (Arg.Text faAccept) in
			check fa#acceptBreadthFirst [];
			check fa#acceptBreadthFirst (word "a");
			check fa#acceptBreadthFirst (word "ab");
			check fa#acceptBreadthFirst (word "b");
			check fa#acceptBreadthFirst (word "ba");
			check fa#acceptBreadthFirst (word "abb");
			check fa#acceptBreadthFirst (word "aba");
			check fa#acceptBreadthFirst (word "baa");
			check fa#acceptBreadthFirst (word "bab");
			Util.println []

	let testAcceptBF2 () =
		let fa = new FiniteAutomaton.model (Arg.Text faAccept2) in
			check fa#acceptBreadthFirst [];
			check fa#acceptBreadthFirst (word "a");
			check fa#acceptBreadthFirst (word "ad");
			check fa#acceptBreadthFirst (word "abad");
			check fa#acceptBreadthFirst (word "c");
			Util.println []

	let testAccept () =
		let fa = new FiniteAutomaton.model (Arg.Text faAccept) in
			check fa#accept [];
			check fa#accept (word "a");
			check fa#accept (word "ab");
			check fa#accept (word "b");
			check fa#accept (word "ba");
			check fa#accept (word "abb");
			check fa#accept (word "aba");
			check fa#accept (word "baa");
			check fa#accept (word "bab");
			Util.println []

	let testAccept2 () =
		let fa = new FiniteAutomaton.model (Arg.Text faAccept2) in
			check fa#accept [];
			check fa#accept (word "a");
			check fa#accept (word "ad");
			check fa#accept (word "abad");
			check fa#accept (word "c");
			Util.println []

	let testAccTrace () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
			fa#acceptWithTracing (word "abe")

	let faGenerate = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","b","S2"], ["S1","a","S3"], ["S1","~","S3"],
				["S2","~","S3"],
				["S3","~","S3"]
			],
		acceptStates : ["S3"]
	} |}

	let faGenerate2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2"],
		initialState : "S1",
		transitions : [
				["S1","a","S1"], ["S1","b","S2"],
				["S2","a","S2"], ["S2","b","S1"]

			],
		acceptStates : ["S2"]
	} |}

	let faGenerate3 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a"],
		states : ["S1"],
		initialState : "S1",
		transitions : [
				["S1","a","S1"]
			],
		acceptStates : ["S1"]
	} |}

	let faGenerate4 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2"],
		initialState : "S1",
		transitions : [
				["S1","a","S1"], ["S1","b","S2"],
				["S2","a","S2"]
			],
		acceptStates : ["S1"]
	} |}

	let testGenerate () =
		let fa = new FiniteAutomaton.model (Arg.Text faGenerate) in
			Util.println ["generated words size 0:"]; Util.printWords (fa#generate 0);
			Util.println ["generated words size 1:"]; Util.printWords (fa#generate 1);
			Util.println ["generated words size 2:"]; Util.printWords (fa#generate 2);
			Util.println ["generated words size 100:"]; Util.printWords (fa#generate 100);
			Util.println []

	let testGenerate2 () =
		let fa = new FiniteAutomaton.model (Arg.Text faGenerate2) in
			Util.println ["generated words size 0:"]; Util.printWords (fa#generate 0);
			Util.println ["generated words size 1:"]; Util.printWords (fa#generate 1);
			Util.println ["generated words size 2:"]; Util.printWords (fa#generate 2);
			Util.println ["generated words size 3:"]; Util.printWords (fa#generate 3);
			Util.println ["generated words size 4:"]; Util.printWords (fa#generate 4);
			Util.println ["generated words size 18:"]; Util.printWords (fa#generate 18);

			Util.println []

	let testGenerate3 () =
		let fa = new FiniteAutomaton.model (Arg.Text faGenerate3) in
			Util.println ["generated words size 0:"]; Util.printWords (fa#generate 0);
			Util.println ["generated words size 1:"]; Util.printWords (fa#generate 1);
			Util.println ["generated words size 10:"]; Util.printWords (fa#generate 10);
			Util.println ["generated words size 50:"]; Util.printWords (fa#generate 50);
			Util.println ["generated words size 100:"]; Util.printWords (fa#generate 100);
			Util.println ["generated words size 1000:"]; Util.printWords (fa#generate 1000);
			Util.println []

	let testGenerate4 () =
		let fa = new FiniteAutomaton.model (Arg.Text faGenerate4) in
			Util.println ["generated words size 0:"]; Util.printWords (fa#generate 0);
			Util.println ["generated words size 1:"]; Util.printWords (fa#generate 1);
			Util.println ["generated words size 10:"]; Util.printWords (fa#generate 10);
			Util.println ["generated words size 100:"]; Util.printWords (fa#generate 100);
			Util.println []

	let testGenerateUntil () =
		let fa = new FiniteAutomaton.model (Arg.Text faGenerate) in
			Util.println ["generated words size 5:"]; Util.printWords (fa#generateUntil 5);
			Util.println [];
		let fa = new FiniteAutomaton.model (Arg.Text faGenerate2) in
			Util.println ["generated words size 5:"]; Util.printWords (fa#generateUntil 5);
			Util.println [];
		let fa = new FiniteAutomaton.model (Arg.Text faGenerate3) in
			Util.println ["generated words size 5:"]; Util.printWords (fa#generateUntil 5);
			Util.println [];
		let fa = new FiniteAutomaton.model (Arg.Text faGenerate4) in
			Util.println ["generated words size 5:"]; Util.printWords (fa#generateUntil 5);
			Util.println []

	let faReach = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3"],
		initialState : "S1",
		transitions : [
			],
		acceptStates : ["S1"]
	} |}

	let faReach2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a","b"],
		states : ["S1","S2","S3","S4","S5","S6"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","a","S3"],
				["S2","a","S2"],
				["S3","~","S4"],
				["S4","~","S5"],
				["S5","~","S3"], ["S5","b","S6"], ["S5","~","S5"]
			],
		acceptStates : ["S1"]
	} |}

	let testReachable () =
			let open FiniteAutomaton in
			let fa = new FiniteAutomaton.model (Arg.Text faReach) in
			let fa2 = new FiniteAutomaton.model (Arg.Text faReach2) in
			let start = fa#representation.initialState in
			let start2 = fa2#representation.initialState in
				Util.println ["reachable states:"]; Util.printStates (fa#reachable start); Util.println [];
				Util.println ["reachable states:"]; Util.printStates (fa#reachable start2); Util.println []

	let faProductive = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","b","S3"],
				["S4","a","S2"], ["S4","b","S3"], ["S3","a","S3"]
			],
		acceptStates : ["S4"]
	} |}

	let faProductive2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1","S2","S3","S4","S5","S6","S7"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","a","S2"], ["S1","~","S3"], ["S1","a","S3"], ["S1","~","S5"], ["S1","a","S5"],
				["S2","~","S1"], ["S2","a","S1"],
				["S4","~","S3"], ["S4","a","S3"],["S4","~","S4"], ["S4","a","S4"],
				["S5","~","S2"], ["S5","a","S2"],["S5","~","S6"], ["S5","a","S6"],
				["S6","~","S6"], ["S6","a","S6"],["S6","~","S7"], ["S6","a","S7"],
				["S7","~","S3"], ["S7","a","S3"],["S7","~","S5"], ["S7","a","S5"]
			],
		acceptStates : ["S2","S4"]
	} |}

	let testProductive () =
		let fa = new FiniteAutomaton.model (Arg.Text faProductive) in
		let fa2 = new FiniteAutomaton.model (Arg.Text faProductive2) in
			Util.println ["productive states:"]; Util.printStates fa#productive; Util.println [];
			Util.println ["productive states:"]; Util.printStates fa2#productive; Util.println []


	let faClean = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","b","S3"],
				["S4","a","S2"], ["S4","b","S3"], ["S3","a","S3"]
			],
		acceptStates : ["S4"]
	} |}

	let faClean2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1","S2","S3","S4"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","~","S3"],
				["S3","a","S2"], ["S3","~","S1"], ["S3","a","S4"]
			],
		acceptStates : ["S2"]
	} |}

	let testClean () =
		let fa = new FiniteAutomaton.model (Arg.Text faClean) in
		let fa2 = new FiniteAutomaton.model (Arg.Text faClean2) in
		let mfa = fa#cleanUselessStates in
		let mfa2 = fa2#cleanUselessStates in
		let j = mfa#toJSon in
		let j2 = mfa2#toJSon in
			JSon.show j; Util.println [];
			JSon.show j2; Util.println []

	let faIsDeter = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "isDeter",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","a","S3"],
				["S2","a","S3"], ["S2","b","S2"]
			],
		acceptStates : ["S3"]
	} |}

	let faIsDeter2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "isDeter",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","~","S2"], ["S1","b","S3"],
				["S2","a","S4"], ["S4","b","S5"],
				["S3","b","S5"]
			],
		acceptStates : ["S5"]
	} |}

	let faIsDeter3 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "isDeter",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","a","S3"],
				["S2","b","S4"],
				["S3","b","S4"]
			],
		acceptStates : ["S4"]
	} |}

	let faToDeter = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["0", "1"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","1","S2"], ["S1","1","S3"] , ["S1","0","S5"],
				["S2","~","S4"],
				["S4","0","S3"],
				["S5","1","S2"], ["S5","0","S3"], ["S5","0","S4"]
			],
		acceptStates : ["S3"]
	} |}

	let testIsDeterministic () =
		let fa = new FiniteAutomaton.model (Arg.Text faIsDeter) in
		let fa2 = new FiniteAutomaton.model (Arg.Text faIsDeter2) in
		let fa3 = new FiniteAutomaton.model (Arg.Text faIsDeter3) in
			if fa#isDeterministic then
				Util.println ["automata is deterministic"] else Util.println ["automata is non-deterministic"];
		if fa2#isDeterministic then
				Util.println ["automata is deterministic"] else Util.println ["automata is non-deterministic"];
			if fa3#isDeterministic then
				Util.println ["automata is deterministic"] else Util.println ["automata is non-deterministic"]



	let testToDeterministic () =
		let fa = new FiniteAutomaton.model (Arg.Text faToDeter) in
		let mfa = fa#toDeterministic in
		let j = mfa#toJSon in
			JSon.show j;
		let fa = new FiniteAutomaton.model (Arg.Text faIsDeter) in
		let mfa = fa#toDeterministic in
		let j = mfa#toJSon in
			JSon.show j


	let testEquivalence () =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let s = fa#equivalencePartition in
			Set.iter (fun s -> Util.print ["set: "]; Util.printStates s) s


	let faMinimize = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b"],
		states : ["S1", "S2", "S3", "S4", "S5"],
		initialState : "S1",
		transitions : [
				["S1","a","S2"], ["S1","b","S3"],
				["S2","b","S4"], ["S2","a","S3"],
				["S3","a","S2"], ["S3","b","S4"],
				["S4","b","S3"], ["S4","a","S2"],
				["S4","a","S5"]
			],
		acceptStates : ["S4"]
	} |}

	let faMinimize2 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "min",
		alphabet : ["i", "c", "1", "2"],
		states : ["S01", "S02", "S03", "S04", "S05",
				"S06", "S07", "S08", "S09", "S10"],
		initialState : "S01",
		transitions : [
				["S01","i","S02"],
				["S02","1","S03"], ["S02","i","S02"],
				["S03","1","S04"], ["S03","i","S04"],
				["S04","1","S03"], ["S04","2","S05"], ["S04","i","S04"],
				["S05","i","S06"], ["S05","c","S07"],
				["S06","i","S06"], ["S06","1","S03"],
				["S07","1","S04"], ["S07","i","S08"],
				["S08","i","S08"], ["S08","1","S03"], ["S08","2","S09"],
				["S09","c","S03"], ["S09","i","S10"],
				["S10","1","S03"], ["S10","i","S10"]
			],
		acceptStates : ["S10"]
	} |}

	let faMinimize3 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["a", "b","c"],
		states : ["S0","S1", "S2", "S3", "S4", "S5"],
		initialState : "S0",
		transitions : [
				["S0","a","S1"], ["S0","b","S2"],
				["S1","b","S0"], ["S1","a","S1"], ["S1","c","S4"],
				["S2","b","S0"], ["S2","a","S2"], ["S2","c","S5"],
				["S3","b","S1"], ["S3","a","S3"], ["S3","c","S4"],
				["S4","b","S5"],
				["S5","b","S4"]
			],
		acceptStates : ["S4","S5"]
	} |}

	let faMinimize4 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["0", "1"],
		states : ["00","01", "10", "11"],
		initialState : "00",
		transitions : [
				["00","1","01"], ["00","0","10"],
				["01","1","00"], ["01","0","11"],
				["10","0","00"], ["10","1","11"],
				["11","1","10"], ["11","0","01"]
			],
		acceptStates : ["01"]
	} |}

	let testMinimize () =
		let fa = new FiniteAutomaton.model (Arg.Text faMinimize) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let testMinimize2 () =
		let fa = new FiniteAutomaton.model (Arg.Text faMinimize2) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let testMinimize3 () =
		let fa = new FiniteAutomaton.model (Arg.Text faMinimize3) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j

	let testMinimize4 () =
		let fa = new FiniteAutomaton.model (Arg.Text faMinimize4) in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j
			
	let testExercice () =
		let e = new Exercise.exercise (Arg.Predef "exer_astar") in
		let fa = new FiniteAutomaton.model (Arg.Predef "dfa_astar") in
		let (ins,outs,props) = fa#checkExerciseFailures e in	
			Util.printWords ins;
			Util.printWords outs;
			Util.printStrings props

	let faMinimize4 = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "abc",
		alphabet : ["0", "1"],
		states : ["00","01", "10", "11"],
		initialState : "00",
		transitions : [
				["00","1","01"], ["00","0","10"],
				["01","1","00"], ["01","0","11"],
				["10","0","00"], ["10","1","11"],
				["11","1","10"], ["11","0","01"]
			],
		acceptStates : ["01"]
	} |}

	let testRepresentationX =
		let fa = new FiniteAutomaton.model (Arg.Predef "fa_abc") in
		let x = fa#representationx in
			new FiniteAutomaton.model (Arg.RepresentationX x)
	
	let runAll =
		if Util.testing active "FiniteAutomaton" then begin
			testExercice ();
			test0 ();
			testBug ();
			testBug2 ();
			testAcceptBF ();
			testAcceptBF2 ();
			testAccept ();
			testAccept2 ();
			testAccTrace ()
		end
end

# 1 "src/RegExpSyntax.ml"
(*
 * RegExpSyntax.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * sep/2022 (amd) - New submodules RegExpConversions and RegExpForLearnOCaml
 * jul/2021 (amd) - Now this module is client of the Scanner module and
 *                  the erros are registered using the Error module.
 * jan/2021 (amd) - Module in an independent file.
 * jun/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Support types and functions for REs including a parser for REs.
 *)

open BasicTypes

module RegExpTypes =
struct
	type tx =
		string
	type regularExpression =
		tx
	type t =
		| Plus of t * t
		| Seq of t * t
		| Star of t
		| Symb of symbol
		| Empty
		| Zero
	type re =
		t
	type reTree =
		| Fail
		| Tree of word * t * reTree list
end

module type RegExpSyntaxSig =
sig
	open RegExpTypes
	
	val parse : string -> t
	val toString : t -> string
	val show : t -> unit
end

module RegExpSyntax : RegExpSyntaxSig =
struct
	open Scanner
	open RegExpTypes

	(*	Grammar:
			E -> E + E | E E | E* | c | (E) | ()

		Grammar with priorities:
			E -> T | T + E
			T -> F | F T
			F -> A | A*
			A -> P | c
			P -> (E) | ()
	*)
	let rec parseExp () =
		let t = parseTerm () in
			match curr() with
				| '+' -> skip(); Plus (t, parseExp ())
				| _ -> t

	and parseTerm () =
		let f = parseFactor () in
			match curr() with
				| '+' | ')' | ' ' -> f
				| _ -> Seq (f, parseTerm ())

	and parseFactor () =
		let a = parseAtom () in
			match curr() with
				| '*' -> skip(); (Star a)
				| _ -> a

	and parseAtom () =
		match curr() with
			| '~' -> skip(); Empty
			| '!' -> skip(); Zero
			| '(' -> skip(); parseParentised ()
			| '+' | '*' -> invalid "Invalid use of wildcard\n"
			| ' ' -> invalid "Premature end of expression\n"
			| c -> skip(); (Symb (char2symb c))

	and parseParentised () =
		let e = parseExp () in (
			match curr() with
				| ')' -> skip(); e
				| _ -> invalid "Right-parenthesis expected\n"
		)

	let parse s =
		Scanner.start "RegExpSyntax" s;
		try
			parseExp ()
		with Not_found ->
			Zero

	let rec toStringN n re =
		match re with
			| Plus(l, r) ->
					(if n > 0 then "(" else "") ^
					toStringN 0 l ^ "+" ^ toStringN 0 r
					^ (if n > 0 then ")" else "")
			| Seq(l, r) ->
					(if n > 1 then "(" else "") ^
					toStringN 1 l ^ toStringN 1 r
					^ (if n > 1 then ")" else "")
			| Star(r) ->
					toStringN 2 r ^ "*"
			| Symb(c) -> symb2str c
			| Empty -> "~"
			| Zero -> "!"

	let toString re =
		toStringN 0 re

	let show re =
		Util.println [toString re]
end

module RegExpConversions =
struct
	open RegExpTypes

	let internalize (re: tx): t =
		RegExpSyntax.parse re

	let externalize (re: t): tx =
		RegExpSyntax.toString re

	let fromJSon j =
		if JSon.isNull j || not (JSon.hasField j "kind") then
			Zero
		else
			let re = JSon.fieldString j "re" in
				RegExpSyntax.parse re

	let toJSon (id: JSon.t) (rep: t): JSon.t =
		let body =
			JSon.makeAssoc [
				("re", JSon.makeString (RegExpSyntax.toString rep));
			]
		in JSon.append id body
end

module RegExpForLearnOCaml =
struct
	open RegExpTypes

	let moduleName =
		"RegularExpression"

	let xTypeName =
		"regularExpression"

	let solution (name: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s	%s
		|zzz}	(* please, do not change this line *)
			(FinAutForLearnOCaml.displayHeader name xTypeName)
			(state2display repx)

	let prelude : string = {|
		type regularExpression = string
		|}	(* please, do not change this line *)

	let example : JSon.t =
		JSon.parse {| {
			kind : "regular expression",
			description : "this is a simple example",
			name : "example",
			re : "w*+(w+yz)*"
		}
		|}	(* please, do not change this line *)
end

module RegExpSyntaxTests =
struct
	let active = false

	let test0 () =
		let re = RegExpSyntax.parse "ab+~*" in
			RegExpSyntax.show re

	let test1 () =
		let re = RegExpSyntax.parse "~((a+b)*(cd)*)*" in
			RegExpSyntax.show re

	let runAll =
		if Util.testing active "RegExpSyntax" then begin
			test0 ();
			test1 ()
		end
end
# 1 "src/RegularExpression.ml"
(*
 * RegularExpression.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Improved Learn-OCaml support and error handling.
 * may/2021 (amd) - Added support for an extern representation.
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * dec/2019 (jg) - Main functionalities.
 * jun/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Regular expressions functionality.
 *
 * TODO: More cleanup. Improve the regular expression simplifier.
 *)

open BasicTypes

module type RegularExpressionSig =
sig
	open RegExpTypes

	val modelDesignation : string
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation : t
				method representationx : tx
				method validate : unit

				method accept : word -> bool
				method allTrees : word -> unit
				method generate : int -> words
				method tracing : unit

				method alphabet : symbols
				method quasiLanguage : words

				method simplify : model

				method checkProperty : string -> bool
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties
											
			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t
			end
end

module RegularExpression : RegularExpressionSig =
struct
	open RegExpTypes

	let modelDesignation = "regular expression"

	(* auxiliary functions *)
	let seqConcat aset bset = Set.flatMap (fun s1 -> Set.map (fun s2 -> s1@s2) bset) aset

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super

			val representation: t =
				match arg with
					| Arg.Representation r -> r
					| Arg.RepresentationX r -> RegExpConversions.internalize r
					| _ -> RegExpConversions.fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method toJSon: JSon.t =
				RegExpConversions.toJSon (super#toJSon) representation

			method representation: t =
				representation

			method representationx: tx =
				RegExpConversions.externalize representation

			method validate = (

				(*
				let representation = RegExpSyntax.parse "(((xx+ut)+(aaa+dss+ghf)+(xx+uu))ee)+bgc*+(jgg+bgcd)" in

				let rec lang rep =
					match rep with
						| RegExpSyntax.Plus(l, r) -> Util.print "pls: "; Util.print (RegExpSyntax.toString l); Util.print ", ";
						Util.print (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Seq(l, r) -> Util.print "seq: "; Util.print (RegExpSyntax.toString l); Util.print ", ";
						Util.print (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Star(r) -> Util.print "str: "; Util.print (RegExpSyntax.toString r); Util.println ""; (lang r)
						| RegExpSyntax.Symb(c) -> Set.make [c]
						| RegExpSyntax.Empty -> Set.empty
						| RegExpSyntax.Zero -> Set.empty
				in
				let a = lang representation in
				()
				*)
			)

			method tracing: unit = ()


			(**
			* This method generates the alphabet of all symbols in the expression
			*
			* @returns symbols -> the set of all symbols in the expression's alphabet
			*
			*)
			method alphabet: symbols =

				let rec alf rep =
					match rep with
						| Plus(l, r) -> Set.union (alf l) (alf r)
						| Seq(l, r) -> Set.union (alf l) (alf r)
						| Star(r) -> alf r
						| Symb(c) -> Set.make [c]
						| Empty -> Set.empty
						| Zero -> Set.empty
				in
					alf representation


			(**
			* This method generates the language of the regular expression for when klenne is always zero
			*
			* @returns words -> set of generated words
			*
			*)
			method quasiLanguage: words =

				let rec lang rep =
					match rep with
						| Plus(l, r) -> Set.union (lang l) (lang r)
						| Seq(l, r) -> seqConcat (lang l) (lang r)
						| Star(r) -> Set.make [[]]
						| Symb(c) -> Set.make [[c]]
						| Empty -> Set.empty
						| Zero -> Set.empty
				in
					lang representation




			(**
			* This method tests if a given word is accepted by the regular expression
			*
			* @param w:word -> word to be tested for acceptance
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			*)
			method accept (w: word): bool =

				let partition w =
					let rec partX w pword =
						match w with
							[] -> Set.empty
							| x::xs -> let fwp = pword@[x] in
											Set.add (fwp, xs) (partX xs fwp) in
					Set.add ([],w) (partX w []) in

				let rec acc rep w =
					match rep with
						| Plus(l, r) -> (acc l w) || (acc r w)
						| Seq(l, r) -> let wpl = partition w in
											Set.exists (fun (wp1,wp2) -> (acc l wp1) && (acc r wp2)) wpl
						| Star(re) -> w = [] ||
									(let wpl = Set.remove ([],w) (partition w) in
											Set.exists (fun (wp1,wp2) -> (acc re wp1) && (acc (Star re) wp2)) wpl)
						| Symb(c) -> w = [c]
						| Empty -> w = []
						| Zero -> false
				in

					acc representation w



			(**
			* This method returns the derivation tree for the word acceptance
			*
			* @param w:word -> word to be tested for acceptance
			*
			* @returns reTree list -> list of derivation trees
			*
			*)
			method allTrees w : unit =


				let partition w =
					let rec partX w pword =
						match w with
							[] -> Set.empty
							| x::xs -> let fwp = pword@[x] in
										Set.add (fwp, xs) (partX xs fwp)
					in
					Set.add ([],w) (partX w [])
				in

				let rec acc w rep =
					match rep with
						| Plus(l, r) ->
							let l1 = acc w l in
							let r1 = acc w r in
								List.map (fun t -> Tree (w, rep, [t])) (l1 @ r1)

						| Seq(l, r) ->
							let wps = partition w in
							let wpl = Set.toList wps in
							List.flatten ( List.map (fun (wp1, wp2) ->
								let tl = acc wp1 l in
								let tr = acc wp2 r in
									List.flatten (List.map (fun x -> List.map
										(fun y -> Tree (w, rep, [x; y])) tr)tl)
							) wpl)

						| Star(re) ->
							if w = [] then
								[Tree ([epsilon], rep, [])]
							else
								(let wps = Set.remove ([],w) (partition w) in
								let wpl = Set.toList wps in
								List.flatten (List.map (fun (wp1, wp2) ->
									let tl = acc wp1 re in
									let tr = acc wp2 (Star re) in
									List.flatten (List.map (fun x -> List.map
										(fun y -> Tree (w, rep, [x; y])) tr) tl)) wpl))

						| Symb(c) ->
							if w = [c] then
								[Tree (w, rep, [])]
							else
								[Tree (w, rep, [Fail])]

						| Empty ->
							if w = [] then
								[Tree ([epsilon], rep, [])]
							else
								[Tree (w, rep, [Fail])]

						| Zero -> [Tree (w, rep, [Fail])]

				in

				let ac = acc w self#representation in



				let rec isNotFail t =
					match t with
						Fail -> false
						| Tree ([], re, []) -> true
						| Tree (w, re, []) -> true
						| Tree ([], re, x::xs) -> (isNotFail x) && (isNotFail (Tree ([], re, xs)))
						| Tree (w, re, x::xs) -> (isNotFail x) && (isNotFail (Tree (w, re, xs)))
				in

				let ts = List.filter (fun t -> isNotFail t) ac in


				let printTreeX w re n =
					let s = String.make (3*n) ' ' in
					Util.println [s; word2str w; " -> "; RegExpSyntax.toString re]
				in

				let rec printTree t n =
					match t with
						Fail -> Util.println ["Fail"]
						| Tree ([], re, []) -> Util.println ["TREH "]
						| Tree (w, re, []) -> printTreeX w re n
						| Tree ([], re, x::xs) -> printTreeX [] re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
						| Tree (w, re, x::xs) -> printTreeX w re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
				in

					List.iter (fun t -> printTree t 0) ts



			(**
			* This method generates all words up to the given length that are generated by the regular expression
			*
			* @param length:int -> maximum length of all generated words
			*
			* @returns words -> set of generated words
			*)
			method generate (length: int): words =

				let rec lang rep ln =
					match rep with
						| Plus(l, r) ->
								Set.union (lang l ln) (lang r ln)
						| Seq(l, r) ->
								let left = lang l ln in
								let rigth w = lang r (ln - (List.length w)) in
								let conc w = Util.concatAll w (Set.toList (rigth w)) in
									Set.flatMap (fun lw -> Set.make (conc lw)) left
						| Star r ->
								let exp = lang r ln in
									Set.star exp ln

							(* alternate version of star, leave 4 now

							let rec starX ws sz =
								if sz <= 0 then Set.make [[]]
								else
									let ws = Set.filter (fun x -> sz >= (List.length x)) ws in
									let newLn w = sz - (List.length w) in
									let tail w ws = Set.toList (starX ws (newLn w)) in
									let conc w ws = Util.concatAll w (tail w ws) in
									let track w ws = Set.add w (Set.make (conc w ws)) in
										Set.flatMap (fun w -> if w = [] then Set.make [[]] else track w ws) ws in
							let exp = lang r ln in
								Set.add [] (starX exp ln)*)

						| Symb(c) -> if ln > 0 then Set.make [[c]] else Set.empty
						| Empty -> Set.make [[]]
						| Zero -> Set.empty
				in
					lang representation length


			(**
			* This method simplifies the regular expression
			*
			* @returns RegularExpression.model -> the new simplified, equivalent expression
			*)
			method simplify : model =

				(* various base case simplification rules to apply to the given expressions *)
				let rec simpX re =
					match re with
						(* plus *)
						(* a* + empty *)
						| Plus(Star(l), Empty) -> Star(l)
						| Plus(Empty, Star(r)) -> Star(r)
						(* a* + zero *)
						| Plus(Zero, r) -> r
						| Plus(l, Zero) -> l
						(* ~ + aa* *)
						| Plus(Empty, Seq(l, Star(r))) when l = r -> Star(r)
						| Plus(Empty, Seq(Star(l), r)) when l = r -> Star(l)
						| Plus(Seq(l, Star(r)), Empty) when l = r -> Star(r)
						| Plus(Seq(Star(l), r), Empty) when l = r -> Star(l)
						(* a* + a + empty *)
						| Plus(Star(l), Plus(Empty, r)) when l = r -> Star(l)
						| Plus(Star(l), Plus(r, Empty)) when l = r -> Star(l)
						| Plus(Plus(Empty, l), Star(r)) when l = r -> Star(r)
						| Plus(Plus(l, Empty), Star(r)) when l = r -> Star(r)
						(* a* + a *)
						| Plus(Star(l), r) when l = r -> Star(l)
						| Plus(l, Star(r)) when l = r -> Star(r)
						(* a + b = a||b when a = b *)
						| Plus(l, r) when l = r -> l
						(* seq *)
						| Seq(Empty, Empty) -> Empty
						| Seq(Zero, Zero) -> Zero
						| Seq(Empty, r) -> r
						| Seq(l, Empty) -> l
						| Seq(Zero, r) -> Zero
						| Seq(l, Zero) -> Zero
						(* (~+a)a* *)
						| Seq(Plus(Empty, l),Star(r)) when l = r -> Star(r)
						| Seq(Plus(l, Empty),Star(r)) when l = r -> Star(r)
						| Seq(Star(l),Plus(Empty, r)) when l = r -> Star(l)
						| Seq(Star(l),Plus(r, Empty)) when l = r -> Star(l)
						| Seq(Star(l),Star(r)) when l = r -> Star(l)
						(* star *)
						| Star(Star(r)) -> Star(r)
						| Star(Plus(Empty, r)) -> Star(r)
						| Star(Plus(r, Empty)) -> Star(r)
						| Star(Empty) -> Empty
						| Star(Zero) -> Empty
						| Star(r) -> re
						(* symb *)
						| Symb(c) -> Symb c
						(* empty *)
						| Empty -> Empty
						(* zero *)
						| Zero -> Zero
						| _ -> re
				in

				(* applies various base case simplifications to the various sub-expressions of regular expression re *)
				let rec simplify re =

					match re with
						| Plus(l,r) -> simpX (Plus(simplify l, simplify r))
						| Seq(l,r) -> simpX (Seq(simplify l, simplify r))
						| Star(re) -> simpX (Star(simplify re))
						| Symb(c) -> Symb c
						| Empty -> Empty
						| Zero -> Zero
				in

				let sre = simplify representation in

				new model (Arg.Representation (sre))

			method checkProperty (prop: string) =
				match prop with
					| "regular expression" -> true
					| _ -> super#checkProperty prop

		(* Learn-OCaml support *)
			method moduleName = RegExpForLearnOCaml.moduleName
			method xTypeName = RegExpForLearnOCaml.xTypeName
			method xTypeDeclString : string = RegExpForLearnOCaml.prelude
			method toDisplayString (name: string): string =
				RegExpForLearnOCaml.solution name self#representationx
			method example : JSon.t = RegExpForLearnOCaml.example
		end
end

module RegularExpressionTests: sig end =
struct
	let active = false

	let test0 () =
		let m = new RegularExpression.model (Arg.Predef "re_abc") in
			let j = m#toJSon in
				JSon.show j

	let test1 () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			let j = re#toJSon in
				JSon.show j

	let testAlphabet () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			Util.println ["alphabet: "]; Util.printAlphabet re#alphabet;
			Util.println []

	let testAlphabet2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			Util.println ["alphabet: "]; Util.printAlphabet re#alphabet;
			Util.println []

	let testAlphabet3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
			Util.println ["alphabet: "]; Util.printAlphabet re#alphabet;
			Util.println []

	let testAlphabet4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
			Util.println ["alphabet: "]; Util.printAlphabet re#alphabet;
			Util.println []

	let testQuasiLang () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			let ws = re#quasiLanguage in
			Util.printWords ws

	let testQuasiLang2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			let ws = re#quasiLanguage in
			Util.printWords ws

	let testQuasiLang3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
			let ws = re#quasiLanguage in
			Util.printWords ws

	let testQuasiLang4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
			let ws = re#quasiLanguage in
			Util.printWords ws

	let check f w =
		let msg = 
			if f w then "word was accepted" else "word was not accepted"
		in Util.println [msg]

	let testAccept () =
		let m = new RegularExpression.model (Arg.Predef "re_abc") in
			check m#accept (word "aa")

	let testAccept2 () =
		let m = new RegularExpression.model (Arg.Predef "re_simple") in
			check m#accept (word "aa")

	let testAccept3 () =
		let m = new RegularExpression.model (Arg.Predef "re_complex") in
			check m#accept (word "aa")

	let testAccept4 () =
		let m = new RegularExpression.model (Arg.Predef "re_convoluted") in
			check m#accept (word "aa")

	let testGenerate () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
			Util.println ["generated words size 0:"]; Util.printWords (re#generate 0);
			Util.println ["generated words size 1:"]; Util.printWords (re#generate 1);
			Util.println ["generated words size 2:"]; Util.printWords (re#generate 2);
			Util.println ["generated words size 3:"]; Util.printWords (re#generate 3);
			Util.println ["generated words size 4:"]; Util.printWords (re#generate 4);
			Util.println []

	let testGenerate2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			Util.println ["generated words size 0:"]; Util.printWords (re#generate 0);
			Util.println ["generated words size 1:"]; Util.printWords (re#generate 1);
			Util.println ["generated words size 2:"]; Util.printWords (re#generate 2);
			Util.println ["generated words size 3:"]; Util.printWords (re#generate 3);
			Util.println ["generated words size 4:"]; Util.printWords (re#generate 4);
			Util.println []

	let testGenerate3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
			Util.println ["generated words size 0:"]; Util.printWords (re#generate 0);
			Util.println ["generated words size 1:"]; Util.printWords (re#generate 1);
			Util.println ["generated words size 2:"]; Util.printWords (re#generate 2);
			Util.println ["generated words size 3:"]; Util.printWords (re#generate 3);
			Util.println ["generated words size 4:"]; Util.printWords (re#generate 4);
			Util.println []

	let testGenerate4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
			Util.println ["generated words size 0:"]; Util.printWords (re#generate 0);
			Util.println ["generated words size 1:"]; Util.printWords (re#generate 1);
			Util.println ["generated words size 2:"]; Util.printWords (re#generate 2);
			Util.println ["generated words size 3:"]; Util.printWords (re#generate 3);
			Util.println ["generated words size 4:"]; Util.printWords (re#generate 4);
			Util.println []

	let testSimplify2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let res = re#simplify in
			JSon.show res#toJSon

	let testEnum () =
		let e = new Exercise.exercise (Arg.Predef "exer_re2fa") in
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let result = re#checkExercise e in
			if result then Util.print ["it works"] else Util.print ["it does not work"]

	let testTrace () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
			re#allTrees (word "acbacb")
	
	let re_more = {| {
			kind : "regular expression",
			description : "this is an example",
			name : "re_more",
			re : "a*"
	} |}
				
	let testMore () =
		let re = new RegularExpression.model (Arg.Text re_more) in
			re#allTrees (word "aa")

	let runAll =
		if Util.testing active "RegularExpression" then begin
			testMore ()
		end
end
# 1 "src/FinEnuSyntax.ml"
(*
 * FinEnuSyntax.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * sep/2022 (amd) - New module
 *)

 (*
 * Description: Support types and functions for FEs.
 *)

open BasicTypes

module FinEnuTypes =
struct
	type tx =
		string list
	type finiteEnumeration =
		tx
	type t =
		words
	type fe =
		t
end

module FinEnuConversions =
struct
	open FinEnuTypes

	let internalize (fe: tx): t =
		Set.make (strs2words fe)

	let externalize (fe: t): tx =
		words2strs (Set.toList fe)

	let fromJSon j =
		if JSon.isNull j || not (JSon.hasField j "kind") then
			Set.empty
		else
			let strings = JSon.fieldStringSet j "words" in
			let words = Set.map str2word strings in
				words
	
	let toJSon (id: JSon.t) (rep: t): JSon.t =
		let body =
			JSon.makeAssoc [
				("words", JSon.makeStringSet (Set.map word2str rep))
			]
		in JSon.append id body
end

module FinEnuForLearnOCaml =
struct
	open FinEnuTypes

	let moduleName =
		"FiniteEnumeration"

	let xTypeName =
		"finiteEnumeration"

	let solution (name: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s	%s
		|zzz}	(* please, do not change this line *)
			(FinAutForLearnOCaml.displayHeader name xTypeName)
			(strings2display repx)

	let prelude : string = {| {
		type symbol = char
		type state = string
		type transition = state * symbol * state

		type finiteAutomaton = {
			alphabet : symbol list;
			states : state list;
			initialState : state;
			transitions : transition list;
			acceptStates : state list
		}
		|}	(* please, do not change this line *)

	let example : JSon.t =
		JSon.parse {| {
			kind : "finite enumeration",
			description : "this is an example",
			name : "example",
			words : ["Red", "Yellow", "Blue"]
		}
		|}	(* please, do not change this line *)

end
# 1 "src/FiniteEnumeration.ml"
(*
 * FiniteEnumeration.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Improved Learn-OCaml support and error handling.
 * may/2021 (amd) - Added support for an extern representation.
 * mar/2021 (amd) - New module
 *)

(*
 * Description: Finite language, directly defined as a set of words.
 *)

open BasicTypes

module FiniteEnumeration =
struct
	open FinEnuTypes

	let modelDesignation = "finite enumeration"

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super
		
			val representation: t =
				match arg with
					| Arg.Representation r -> r
					| Arg.RepresentationX r -> FinEnuConversions.internalize r
					| _ -> FinEnuConversions.fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation: t =
				representation

			method representationx: tx =
				FinEnuConversions.externalize representation

			method toJSon: JSon.t =
				FinEnuConversions.toJSon (super#toJSon) representation

			method validate: unit = ()

			method tracing : unit = ()

			method accept (w: word): bool =
				Set.belongs w representation

			method generate (length: int): words =
				Set.filter (fun w -> List.length w == length) representation

			method checkProperty (prop: string) =
				match prop with
					| "finite enumeration" -> true
					| _ -> super#checkProperty prop

		(* Learn-OCaml support *)
			method moduleName = FinEnuForLearnOCaml.moduleName
			method xTypeName = FinEnuForLearnOCaml.xTypeName
			method xTypeDeclString : string = FinEnuForLearnOCaml.prelude
			method toDisplayString (name: string): string =
				FinEnuForLearnOCaml.solution name self#representationx
			method example : JSon.t = FinEnuForLearnOCaml.example
		end
end

module FiniteEnumerationTests : sig end =
struct
	let active = false
	
	let fe_colors = {| {
		kind : "finite enumeration",
		description : "this is an example",
		name : "colors",
		words : ["Red", "Yellow", "Blue"]
	} |}
	
	let exer_colors = {| {
		kind : "exercise",
		description : "this is an example",
		name : "exer_colors",
		problem : "Colors of length 3",
		inside : ["Red"],
		outside : ["","Yellow","Blue"],
		properties : ["true"]
	} |}

	let test0 () =
		let fe = new FiniteEnumeration.model (Arg.Text fe_colors) in
		let e = new Exercise.exercise (Arg.Text exer_colors) in
		let (ins,outs,props) = fe#checkExerciseFailures e in	
			Util.printWords ins;
			Util.printWords outs;
			Util.printStrings props
		
	let runAll =
		if Util.testing active "FiniteEnumeration" then begin
			test0 ();
		end
end

# 1 "src/CFGSyntax.ml"
(*
 * CFGSyntax.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * sep/2022 (amd) - New submodules CFGConversions and CFGForLearnOCaml
 * jul/2021 (amd) - Now this module is client of the Scanner module and
 *                  the erros are registered using the Error module.
 * jan/2021 (amd) - Module moved to an independent file.
 * dec/2019 (amd) - Initial version, inside the big file "OCamlFlatSupport.ml".
 *)

(*
 * Description: Support types and functions for CFGs including a parser for CFGs.
 *)
 
open BasicTypes

module CFGTypes =
struct
	type rule =
		{ head : symbol; body : word; }
	type rules =
		rule Set.t
	type tx = {
		alphabet : symbolX list;
		variables : variableX list;
		initial : variableX;
		rules : string list
	}
	type contextFreeGrammar =
		tx
	type t = {
		alphabet : symbols;
		variables : variables;
		initial : variable;
		rules : rules
	}
	type cfg =
		t
	type cfgTree =
		  Leaf of symbol
		| Root of symbol * cfgTree list
end

module type CFGSyntaxSig =
sig
	open CFGTypes

	val parse : string Set.t -> rule Set.t
	val parseLine : string -> rule Set.t
	val toStringList : rule Set.t -> string list
	val (-->) : symbol -> string -> rule
	val show : rule Set.t -> unit
end

module CFGSyntax: CFGSyntaxSig =
struct
	open Scanner
	open CFGTypes

	let isWhite c =
		List.mem c [' '; '\t']
	
	let rec parseHead () : symbol =
		match curr() with
			| ' ' -> invalid "Premature end of expression\n"
			| c -> skip() ; char2symb c

	let rec parseNeck (): unit =
		match curr() with
			| ' ' -> invalid "Premature end of expression\n"
			| '-' -> skip();
					if curr() = '>' then skip()
					else invalid "Bad neck\n"
			| _ -> invalid "Bad neck\n"

	let rec parseBody (): word list =
		match curr() with
			| ' ' -> [[]]
			| '|' -> skip(); []::parseBody ()
			| '~' -> skip(); parseBody ()
			| c -> skip();
					match parseBody () with
						| [] -> invalid "never happens"
						| x::xs -> ((char2symb c)::x)::xs

	let parseLine line: rule Set.t =
		if String.trim line = "" then
			Set.empty
		else (
			Scanner.start "CFGSyntax" line;
			try
				let h = parseHead () in
				let _ = parseNeck () in
				let bs = Set.make (parseBody ()) in
					Set.map (fun b -> {head=h; body=b}) bs
			with Not_found ->
				Set.empty
		)

	let parse rs: rule Set.t =
		Set.flatMap parseLine rs

	let toString1 r: string =
		let h = r.head in
		let b = if r.body = [] then [epsilon] else r.body in
		let full = [symb2str h; " -> "] @ (List.map symb2str b) in
			String.concat "" full

	let toString rs: string =
		let rl = Set.toList rs in
		String.concat "\n" (List.map toString1 rl)

	let toStringList rs: string list =
		let rl = Set.toList rs in
			List.map toString1 rl
	
	let (-->) h b : rule =
		{ head = h; body = str2word b } ;;

	let show rs =
		Util.println [toString rs]
end

module CFGConversions =
struct
	open CFGTypes

	let internalize (cfg: tx): t = {
		alphabet = symbolsX2symbols cfg.alphabet;
		variables = symbolsX2symbols cfg.variables;
		initial = symbX2symb cfg.initial;
		rules = CFGSyntax.parse (Set.make cfg.rules)
	}

	let externalize (cfg: t): tx = {
		alphabet = symbols2symbolsX cfg.alphabet;
		variables = symbols2symbolsX cfg.variables;
		initial = symb2symbX cfg.initial;
		rules = CFGSyntax.toStringList cfg.rules
	}

	let fromJSon j =
		if JSon.isNull j || not (JSon.hasField j "kind") then {
			alphabet = Set.empty;
			variables = Set.make [draftVar];
			initial = draftVar;
			rules = Set.empty;
		}
		else {
			alphabet = JSon.fieldSymbolSet j "alphabet";
			variables = JSon.fieldSymbolSet j "variables";
			initial = JSon.fieldSymbol j "initial";
			rules = CFGSyntax.parse (JSon.fieldStringSet j "rules");
		}

	let rule2str {head=h; body=b} =
		let bb = if b = [] then [epsilon] else b in
			(symb2str h) ^ " -> " ^ (word2str bb)

	let toJSon (id: JSon.t) (rep: t): JSon.t =
		let body =
			JSon.makeAssoc [
				("alphabet", JSon.makeSymbolSet rep.alphabet);
				("variables", JSon.makeSymbolSet rep.variables);
				("initial", JSon.makeSymbol rep.initial);
				("rules", JSon.makeStringSet (Set.map rule2str rep.rules))
			]
		in JSon.append id body
end

module CFGForLearnOCaml =
struct
	open CFGTypes

	let moduleName =
		"ContextFreeGrammar"

	let xTypeName =
		"contextFreeGrammar"

	let solution (name: string) (repx: tx): string =
		Printf.sprintf {zzz|
		%s{
			alphabet = %s;
			variables = %s;
			initial = %s;
			rules = %s
		}
		|zzz}	(* please, do not change this line *)
			(FinAutForLearnOCaml.displayHeader name xTypeName)
			(symbolsX2display repx.alphabet)
			(symbolsX2display repx.variables)
			(symbX2display repx.initial)
			(strings2display repx.rules)

	let prelude : string =
		Printf.sprintf {zzz|
			type symbol = %s
			type variable = %s
			type rule = string
			type contextFreeGrammar = {
				alphabet : symbol list;
				variables : variable list;
				initial : variable;
				rules : rule list
			}
		|zzz}	(* please, do not change this line *)
				symbolTypeName symbolTypeName

		let example : JSon.t =
			JSon.parse {| {
				kind : "context free grammar",
				description : "this is an example",
				name : "cfg_simple",
				alphabet : ["0", "1"],
				variables : ["S", "X"],
				initial : "S",
				rules : [ "S -> 1S0 | X", "X -> 0X1 | ~" ]
			}
			|}	(* please, do not change this line *)
end

module CFGSyntaxTests =
struct
	let active = false

	let test0 () =
		let cfg = Set.make [ "S -> aTb | ~"; "T -> aSb" ] in
		let rules = CFGSyntax.parse cfg in
			CFGSyntax.show rules

	let test1 () =
		let cfg = Set.make ["S -> aSb | ~"] in
		let rules = CFGSyntax.parse cfg in
			CFGSyntax.show rules

	let runAll =
		if Util.testing active "CFGSyntax" then begin
			Util.header "test0";
			test0 ();
			Util.header "test1";
			test1 ();
			Util.header ""
		end
end
# 1 "src/CFGChomsky.ml"
(*
 * ChomskyNormalForm.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Guilherme Fernandes (gf)
 *)

(*
 * ChangeLog:
 *
 * sep/2022 (amd) - Adapted the Guilherme code to the context of OCamlFLAT
 * jul/2022 (gf) - New module (original code inside the "contributions" dir)
 *)

(*
 * Description: ???
 *)
 
open BasicTypes

module type ChomskyNormalFormSig =
sig
	open CFGTypes

	val chomsky : cfg -> cfg	
	val cykAccept : cfg -> word -> bool	
	val accept : cfg -> word -> bool	
end

module ChomskyNormalForm : ChomskyNormalFormSig =
struct
	open CFGTypes
(*Remove rule rule from grammar cfg
  and return a new grammar*)
let rec removeRule (rule : rule) (cfg : cfg) : cfg = 
  let newRules = Set.filter (fun x -> x <> rule) cfg.rules in 
  {alphabet = cfg.alphabet;
  variables = cfg.variables;
  initial = cfg.initial;
  rules = newRules
  }
;;

(*Generate a random char*)
let randVar (forbiddenSymbols : symbols) : symbol =
	let var = ref (Char.uppercase_ascii (Char.chr (97 + (Random.int 26)))) in
	while Set.belongs (char2symb !var) forbiddenSymbols do 
		var := Char.uppercase_ascii (Char.chr (97 + (Random.int 26))) 
	done;
	char2symb !var
;;

(* return the symbols used in grammar cfg *)
let usedSymbols (cfg : cfg) : symbols = 
	Set.union cfg.variables cfg.alphabet
;;

(* return the direct derivations of the 
   variable var in grammar cfg*)
let directDeriv (var : symbol) (cfg : cfg) : rules =
	Set.filter ( fun x -> x.head = var) cfg.rules
;;

(* return the number of direct derivations
 of the variable var in grammar cfg*)
let numberOfProdVar (var : symbol) (cfg : cfg) : int = 
	Set.size (directDeriv var cfg)
;;


(*returns the rule with a body equal to bdy and,
  if it does not exist, generates a new one with the body bdy*)
  (* in rulesWithSameBdy it is necessary to check
  	if numberOfProdVar x.head is equal to 1
	because if not, we may be changing
	the grammar ex: S -> a
	S -> b
	if we look for bdy 'a' we will find the S
	but we will also add production b
	the grammar
	 *)   
let getRule (bdy : word) (cfg : cfg) : rule = 
	let rulesWithSameBdy =
			Set.filter ( fun x -> x.body = bdy && numberOfProdVar x.head cfg = 1) cfg.rules in
		if Set.size rulesWithSameBdy > 0 then 
			Set.hd rulesWithSameBdy
		else
			{head = randVar (usedSymbols cfg); body = bdy}
;;


(*checks if there is a rule in grammar cfg that 
  contains the initial variable on the right-hand side*)
let containsSInRHS (cfg : cfg) : bool = 
	Set.exists (fun x -> List.mem cfg.initial x.body) cfg.rules
;;

(* adds a new rule to grammar cfg and sSymb indicates if 
  variable in the left-hand side is the initial variable*)
let addRule (rule : rule) (cfg : cfg) (sSymb : bool): cfg = 
  {alphabet = cfg.alphabet;
  variables = Set.union (Set.make [rule.head]) cfg.variables;
  initial = if sSymb then rule.head else cfg.initial;
  rules = Set.cons rule cfg.rules
  }
;;

(*START*)
(*Eliminate the start symbol from right-hand side *)
let delSFromRHS (cfg : cfg) : cfg = 
  if containsSInRHS cfg then 
    let newS = randVar (usedSymbols cfg) in
    addRule {head = newS; body = [cfg.initial]} cfg true
  else cfg
;;

(*------------------------------------------------------------------------------------------------*)

(*checks if symbol var is a cfg grammar variable*)
let isVariable (var : symbol) (cfg : cfg) : bool = 
  Set.belongs var cfg.variables
;;

(*checks if symbol symbol is a terminal symbol*)
let isTerminalSymbol (symbol : symbol) (cfg : cfg) : bool = 
  Set.belongs symbol cfg.alphabet
;;
(*checks if var produces the word in grammar cfg
  and seen are the rules already parsed*)
let rec prodWord (var : symbol) (word : word) (cfg : cfg) (seen : rules): bool = 
  let direct = directDeriv var cfg in 
  let words = Set.map (fun x -> x.body) direct in 
		Set.belongs word words 
	|| Set.exists (fun x -> not (Set.belongs x seen) 
		&& List.for_all (fun y -> prodWord y word cfg (Set.cons x seen)) x.body) direct
;; 

(*checks if var produces the word in grammar cfg*)
let varProdWord (var : symbol) (word : word) (cfg : cfg) : bool = 
  prodWord var word cfg Set.empty
;;

(*checks if var produces the empty word in grammar cfg*)
let prodEpsilon (var : symbol) (cfg : cfg) : bool =
  varProdWord var [] cfg 
;;

(*generates all possible words by applying the epsilon
  transformation, if the variable we are analyzing derives epsilon*)
let rec epsilonProdsCombs (word : word) (cfg : cfg) : words = 
  match word with
  | [] -> Set.make [[]]
  | hd::tl -> let a = epsilonProdsCombs tl cfg in
              if prodEpsilon hd cfg then 
                let b = Set.map ( fun x -> hd::x) a in
                if numberOfProdVar hd cfg > 1 then
                  Set.union a b
                else a
              else
                Set.map (fun x -> hd::x) a
;;


(*test*)
let rec print rules =
  match rules with
  | [] -> Format.printf "\n"
  | hd::tl ->
		Format.printf "{head = %s; body = " (symb2str hd.head); 
		List.iter (fun x -> Format.printf "%s" (symb2str x)) hd.body;
		Format.printf "}\n";
		Format.print_flush ();
		print tl
;;

(*for each element x in bodies 
  add a rule with head leftSide and body x*)
let rec addNewRules (leftSide : symbol) (bodies : words) (cfg : cfg) : cfg =
 	Set.match_ bodies
		(fun () -> cfg)
		(fun hd tl ->
			let nCfg = addNewRules leftSide tl cfg in
              if hd <> [leftSide] then
                addRule {head = leftSide; body = hd} nCfg false
              else nCfg)
  ;;

(*verifies if symb is the cfg grammar start symbol *)
let isStartSymbol (symb : symbol) (cfg : cfg) : bool = 
  symb = cfg.initial
;;

(*Eliminate ε-rules of cfgRules
  and returns a new grammar without ε-rules *)
  let rec delEpsilonRules (cfgRules: rules) (cfg : cfg) : cfg =
 	Set.match_ cfgRules
		(fun () -> cfg)
		(fun hd tl ->
			let nCfg = delEpsilonRules tl cfg in
              let epsilonProd = epsilonProdsCombs hd.body cfg in 
              let cfgWithRules = addNewRules hd.head epsilonProd nCfg in
              if isStartSymbol hd.head cfgWithRules then 
                cfgWithRules 
              else 
                removeRule {head = hd.head; body = []} cfgWithRules)
;;




(*DEL*)
(*Eliminate ε-rules of cfg grammar*)
let cleanEpsilonRules (cfg : cfg) : cfg = 
  delEpsilonRules cfg.rules cfg 
;;





(*------------------------------------------------------------------------------------------------*)

(*checks if rule derivates only one variable*)
let isAnUnitProduction (rule : rule) (cfg : cfg) : bool = 
  List.length rule.body = 1 && isVariable (List.hd rule.body) cfg
;;

let rec addRules (rules : rules ) (cfg : cfg) : cfg =
	Set.match_ rules
		(fun () -> cfg)
		(fun hd tl -> addRules tl (addRule hd cfg false))

(*UNIT*)
(*let unitFor1 (seen : rules) (rule : rule) (cfg : cfg) : rules * cfg = 
  let nCfg = removeRule rule cfg in
  let direct = directDeriv (List.hd rule.body) nCfg in 
  let words = List.map (fun x -> x.body) direct in
  let nW = List.filter( fun x -> not(List.mem {head = rule.head; body = x} seen)) words in
  let analyzed = List.map( fun x-> {head = rule.head; body = x})nW in
  (analyzed, addNewRules rule.head nW nCfg)
;;

(* Eliminate unit rules and seen
  are the rules already parsed*)
let rec processUnitProduction (seen : rules) (cfgRules: rules) (cfg : cfg) : cfg = 
  match cfgRules with
  | [] -> cfg
  | hd::tl -> if isAnUnitProduction hd cfg then
                let (a, b) = unitFor1 seen hd cfg in
                processUnitProduction (seen@a) b.rules b
              else 
                processUnitProduction seen tl cfg
;;*)
let unitFor1 (seen : rules) (rule : rule) (cfg : cfg) : rules  = 
  let direct = directDeriv (List.hd rule.body) cfg in 
  let words = Set.map (fun x -> x.body) direct in
  let nW = Set.filter ( fun x -> not (Set.belongs {head = rule.head; body = x} seen)) words in
  Set.map ( fun x-> {head = rule.head; body = x}) nW 
;;

(* Eliminate unit rules and seen
  are the rules already parsed*)
let rec processUnitProduction (seen : rules) (cfgRules: rules) (cfg : cfg) : cfg = 
	Set.match_ cfgRules
		(fun () -> cfg)
		(fun hd tl ->
			let nCfg = processUnitProduction seen tl cfg in
              if isAnUnitProduction hd nCfg then
                let sdnCfg = removeRule hd nCfg in
                let rules = unitFor1 seen hd sdnCfg in
                processUnitProduction (Set.union rules seen) rules (addRules rules sdnCfg)
              else 
                nCfg)
;;

(*UNIT*)  
(*Eliminate unit rules from cfg grammar*)
let rec delUnitProductions (cfgRules: rules) (cfg : cfg) : cfg = 
  processUnitProduction Set.empty cfgRules cfg;;

(*------------------------------------------------------------------------------------------------*)

(*checks if the right-hand side of the 
  rule has one isolated terminal symbol*)
let isNonSolitaryTerminalsRule (rule : rule) (cfg : cfg) : bool = 
  let terminals = List.filter ( fun x -> isTerminalSymbol x cfg) rule.body in
        List.length terminals > 1 
    ||  (List.length terminals > 0 && List.length rule.body > 1)
;;

(*removes the non solitary terminal symbols from word and
  add a new rule to cfg grammar for each one
  and returns a ( new word * new grammar)*)
let rec addRulesFromNonSolitary (bdy : word) (cfg : cfg) : word * cfg = 
  match bdy with
  | [] -> (bdy, cfg)
  | hd::tl -> let (a, b) = addRulesFromNonSolitary tl cfg in
              if isTerminalSymbol hd b then
                let rule = getRule [hd] b in
                let nCfg = addRule rule b false in
                (rule.head::a, nCfg)
              else
                (hd::a, b)
;;

(*TERM*)
(*Eliminate rules with nonsolitary terminals*)
(*let rec delRulesNonSolitaryTerminals (cfgRules: rules) (cfg : cfg) : cfg = (*trocar nome*)
  match cfgRules with
  | [] -> cfg
  | hd::tl -> if isNonSolitaryTerminalsRule hd cfg then 
                let (a, b) = cleanNonSolitary hd.body cfg in
                let nCfg = addRule {head = hd.head; body = a} b false in
                delRulesNonSolitaryTerminals tl (removeRule hd nCfg)
              else
                delRulesNonSolitaryTerminals tl cfg

;;*)


let rec processRulesWithNonSolitaryTerminals (cfgRules: rules) (cfg : cfg) : cfg = 
	Set.match_ cfgRules
		(fun () -> cfg)
		(fun hd tl ->
			let nCfg = processRulesWithNonSolitaryTerminals tl cfg in
              if isNonSolitaryTerminalsRule hd nCfg then 
                let (a, b) = addRulesFromNonSolitary hd.body nCfg in
                addRule {head = hd.head; body = a} (removeRule hd b) false 
              else
                nCfg)
;;


(*------------------------------------------------------------------------------------------------*)
(* checks if the rule has more than 2 
  non terminal symbols in right-hand side*)
let hasMoreThan2NonTerminalsInRHS (rule : rule) (cfg : cfg) : bool = 
    let nonTerminalsInRHS = List.filter ( fun x -> isVariable x cfg) rule.body in
    List.length nonTerminalsInRHS > 2
;;

(* split the word when it finds the first variable*)
let rec splitBodyByVariables (body:word) (cfg : cfg) : word * word = 
	match body with
	| [] -> ([], [])
	| hd::tl -> let (a, b) = splitBodyByVariables tl cfg in
							if isVariable hd cfg then 
								([hd], tl)
							else
								(hd::a, b)
;;

(*let binFor1 (rule : rule) (cfg : cfg) : cfg = 
  let cfgWithoutHd = removeRule rule cfg in 
  let (a, b) = splitBodyByVariables rule.body cfgWithoutHd in
  let var = randVar (usedSymbols cfgWithoutHd) in 
  let nCfg = addRule {head = rule.head; body = a@[var]} cfgWithoutHd false in
  addRule {head = var; body = b} nCfg false
;;*)


let binFor1 (rule : rule) (cfg : cfg) : rules = 
  let (a, b) = splitBodyByVariables rule.body cfg in
  let var = randVar (usedSymbols cfg) in 
	Set.make [{head = rule.head; body = a@[var]}; {head = var; body = b}]
;;

let rec processRHSwithMoreThan2NonTerminals (cfgRules: rules) (cfg : cfg) : cfg = 
	Set.match_ cfgRules
		(fun () -> cfg)
		(fun hd tl ->
			let nCfg = processRHSwithMoreThan2NonTerminals tl cfg in
              if hasMoreThan2NonTerminalsInRHS hd nCfg then 
                let cfgWithoutHd = removeRule hd nCfg in 
                let rules = binFor1 hd cfgWithoutHd in
                let sdCfg = addRules rules cfgWithoutHd in
                processRHSwithMoreThan2NonTerminals rules sdCfg
              else
                nCfg)
;;

(*BIN*)
(*Eliminate right-hand sides with more than 2 nonterminals*)
(*let rec processRHSwithMoreThan2NonTerminals (cfgRules: rules) (cfg : cfg) : cfg = 
  match cfgRules with
  | [] -> cfg
  | hd::tl -> if hasMoreThan2NonTerminalsInRHS hd cfg then 
                let sdCfg = binFor1 hd cfg in
                processRHSwithMoreThan2NonTerminals sdCfg.rules sdCfg
              else
                processRHSwithMoreThan2NonTerminals tl cfg
;;*)

(*let rec delRHSwithMoreThan2NonTerminals (cfgRules: rules) (cfg : cfg) : cfg = 
  match cfgRules with
  | [] -> cfg
  | hd::tl -> let nCfg = delRHSwithMoreThan2NonTerminals tl cfg in 
              if hasMoreThan2NonTerminalsInRHS hd nCfg then 
                binFor1 hd nCfg   // It doesn't work because we need to
                                  // analyze the new rules added in binFor1
              else
                nCfg

            
;;*)



(*------------------------------------------------------------------------------------------------*)
(*Convert cfg grammar to Chomsky normal form*)
let chomsky (cfg: cfg) : cfg =
  let start = delSFromRHS cfg in
  let term = processRulesWithNonSolitaryTerminals start.rules start in
  let bin = processRHSwithMoreThan2NonTerminals term.rules term in 
  let del = cleanEpsilonRules bin in
  let unit = delUnitProductions del.rules del in
  unit 
;;


(* IMPERATIVE FORM of cykAccept*)

(* https://www.geeksforgeeks.org/cocke-younger-kasami-cyk-algorithm/ *)

(*check if rule derivate variables
  pre: rule is in chomsky form*)
let prodVars (rule : rule) : bool =
    List.length rule.body = 2
;;

let cykAccept (cfg: cfg) (w: word) =
  if w = [] then
	prodEpsilon cfg.initial cfg
  else
    let n = List.length w in
    let matrix = Array.make_matrix n n (Set.empty) in
    for j = 0 to (n-1) do
      let vars = Set.filter(fun x -> List.length x.body = 1 && List.nth x.body 0 = List.nth w j) cfg.rules in
      let lhs = Set.map ( fun x -> x.head) vars in
      matrix.(j).(j) <- Set.union matrix.(j).(j) lhs;
      for i = j downto 0 do 
        for k = i to (j-1) do
          let vars = Set.filter(fun x -> prodVars x && Set.belongs (List.nth x.body 0) matrix.(i).(k)
                                          && Set.belongs (List.nth x.body 1) matrix.(k+1).(j)) cfg.rules in
          let lhs = Set.map ( fun x -> x.head) vars in
          matrix.(i).(j) <- Set.union matrix.(i).(j) lhs
        done
      done
    done; 
    Set.belongs cfg.initial matrix.(0).(n-1)

let accept (cfg: cfg) (w: word) =
	cykAccept (chomsky cfg) w

end
# 1 "src/ContextFreeGrammar.ml"
(*
 * ContextFreeGrammar.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Improved Learn-OCaml support and error handling.
 * jun/2021 (amd) - Added checks for '~' in the #validate method.
 * may/2021 (amd) - Added support for an extern representation.
 * jan/2021 (amd) - Module in an independent file and some cleanup.
 * feb/2020 (jg) - Main functionalities.
 * dec/2019 (amd) - Initial skeleton, inside the big file "OCamlFlat.ml".
 *)

(*
 * Description: Context-free grammar functionality.
 *
 * TODO: More cleanup.
 *)

open BasicTypes

module type ContextFreeGrammarSig =
sig
	open CFGTypes

	val modelDesignation : string
	
	val first : word -> bool -> t -> symbol Set.t
	val follow : symbol -> bool -> t -> symbol Set.t
	val lookahead : rule -> bool -> t -> symbol Set.t
	
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation: t
				method representationx : tx
				method validate : unit

				method tracing: unit
				method isRegular: bool
				method first: word -> symbol Set.t
			  method follow: symbol -> symbol Set.t
			  method lookahead: rule -> symbol Set.t
				method accept: word -> bool
				method acceptWithTracing: word -> unit
				method generate: int -> words

				method checkProperty : string -> bool
				method checkExercise: Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties
											
			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t
			end
end




module ContextFreeGrammar : ContextFreeGrammarSig =
struct
	open CFGTypes

	let modelDesignation = "context free grammar"

	(*------Auxiliary functions---------*)

	(* given a head, returns the set of all its bodies according to the cfg's rules *)
	let bodiesOfHead h rl =
		let rls = Set.filter (fun r -> r.head = h) rl in
			Set.map (fun r -> r.body) rls


	(* given 2 sets of words, to each word of the left set, appends each word of the right set *)
	let concatWords lws rws =
		if lws = Set.empty then rws
		else if rws = Set.empty then lws
		else
			let pairs = Set.combinations lws rws in
				Set.map (fun (x,y) -> x@y) pairs

	(* tests if the number of symbols in the given word exceeds the given lenght *)
	let exceedsMaxLen w l alph =
		let cleanWord = List.filter (fun c -> Set.belongs c alph) w in
			(List.length cleanWord) > l



	let subX h rws rl =
		let bs = bodiesOfHead h rl in
			concatWords bs rws


	(* applies the cfg's rules to the given word *)
	let rec subVar w vs rs =
		match w with
			| [] -> Set.make [[]]
			| x::xs -> if (Set.belongs x vs) then subX x (subVar xs vs rs) rs
				else concatWords (Set.make [[x]]) (subVar xs vs rs)


	(* removes the empty symbol from all non-empty words *)
	let removeEpsi w = List.filter (fun c -> c <> epsilon) w


	(* filters out all words that have variables and cleans any unnecessary epsilon *)
	let cleanNonWords ws vs =
		let hasVar w = List.exists (fun c -> Set.belongs c vs) w in
		let ws = Set.filter (fun w -> not (hasVar w)) ws in
			Set.map (fun w -> removeEpsi w) ws

  let removeEpsilonFromWord w =
    List.filter (fun c -> c <> epsilon) w

  let removeDollarFromWord w =
    List.filter (fun c -> c <> dollar) w

  let rec doWordGenerateEmptyX w seen (rep:t) =
    let rec doGenerateEmpty x =
      if List.mem x seen
      then false
      else(
		    let bodies = bodiesOfHead x rep.rules in
		    Set.exists (fun b -> doWordGenerateEmptyX b (x::seen) rep) bodies 
		  )
		in      
      List.for_all doGenerateEmpty w

  let doWordGenerateEmpty w (rep:t) =
    doWordGenerateEmptyX (removeDollarFromWord w) [] rep

  let rec firstX testWord seen simple (rep:t) =
    match testWord with
		  | [] -> Set.empty
			| [x] when Set.belongs x rep.variables -> 
					let bodies = bodiesOfHead x rep.rules in
					if Set.belongs x seen 
					  then Set.empty
					  else let result = Set.flatMap ( fun b ->
					          let result = firstX b (Set.add x seen) simple rep in
                    let empty = if b = []
                                then Set.make [epsilon]
                                else Set.empty in
					          Set.union empty result
					        ) bodies
					        in
                  if Set.exists (fun b -> doWordGenerateEmpty b rep) bodies 
                    then Set.union result (Set.make [epsilon])
                    else Set.make (removeEpsilonFromWord (Set.toList result))
			| x::xs when Set.belongs x rep.alphabet -> 
					Set.make [x]
			| x::xs -> Set.union 
		  						(firstX [x] seen simple rep) 
									(if doWordGenerateEmpty [x] rep then firstX xs seen simple rep else Set.empty)  

  let first2 (testWord:word) simple (rep:t) =
    firstX testWord Set.empty simple rep

  let rec first (testWord:word) simple (rep:t) =
    let first = first2 testWord simple rep in
    if simple then Set.filter (fun c -> c <> epsilon) first else first

	let getFollowRules (testSymbol:symbol) (rep:t) =
	  Set.filter (fun r -> Set.belongs testSymbol (Set.make r.body) ) rep.rules

  let rec getFollowInfo2 testSymbol h b =
    match b with
      | [] -> []
      | x::xs when x = testSymbol -> (h, xs) :: getFollowInfo2 testSymbol h xs
      | x::xs -> getFollowInfo2 testSymbol h xs

  (* given a variable X, returns the pairs (Y,w2) *)
  let getFollowInfo testSymbol rep =
    let rules = Set.toList (getFollowRules testSymbol rep) in
    List.flatten (List.map (fun r -> getFollowInfo2 testSymbol r.head r.body) rules )

    
  let rec followX (testSymbol:symbol) seen simple (rep:t) =
    let pairs = Set.make (getFollowInfo testSymbol rep) in
    let dollar = if testSymbol = rep.initial
                  then Set.make [dollar]
                  else Set.empty
    in
    let set = Set.flatMap (fun (y,w) -> 
          Set.union 
            (Set.filter (fun s -> s <> epsilon) (first w simple rep))
            (if (not (Set.belongs y seen) && doWordGenerateEmpty w rep) 
              then followX y (Set.add testSymbol seen) simple rep
              else Set.empty
            )
    ) pairs 
    in
    Set.union set dollar
    
  let follow2 testSymbol simple rep =
    followX testSymbol (Set.make []) simple rep
  
  let follow testSymbol simple rep =
    let follow = follow2 testSymbol simple rep in
    if simple then Set.filter (fun c -> c <> dollar) follow else follow


  let lookahead rule simple (rep:t) =
    let x = rule.head in
    let w = rule.body in
      Set.filter (
        fun c -> c <> epsilon 
      ) (Set.union (first2 w simple rep) (if doWordGenerateEmpty w rep then follow2 x simple rep else Set.empty))

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super

      val mutable simplified = false

			val representation: t =
				match arg with
					| Arg.Representation r -> r
					| Arg.RepresentationX r -> CFGConversions.internalize r
					| _ -> CFGConversions.fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation =
				representation

			method representationx: tx =
				CFGConversions.externalize representation

			method toJSon: JSon.t =
				CFGConversions.toJSon (super#toJSon) representation

			method validate: unit = (

				(* the alphabet must not contain epsilon ('~') *)
				let isValidAlphabet = not (Set.belongs epsilon representation.alphabet) in

				(* the variables must not contain epsilon ('~') *)
				let isValidVariables = not (Set.belongs epsilon representation.variables) in

				let isIntersectionValid = (Set.inter representation.variables representation.alphabet) = Set.empty in

				let isInitialValid = Set.belongs representation.initial representation.variables in

				let areRuleHeadsValid =
					let hs = Set.map (fun r -> r.head) representation.rules in
						Set.subset hs representation.variables
				in

				let areRuleBodiesValid =
					let bs = Set.map (fun r -> Set.make r.body) representation.rules in
					let allValidSymbs = Set.add epsilon (Set.union representation.alphabet representation.variables) in
					let res = Set.exists (fun b -> not (Set.subset b allValidSymbs)) bs in
						not res
				in
				
				if not isValidAlphabet then
					Error.error self#id.Entity.name
						"The alphabet contains epsilon '~', and it should not" ()
				;

				if not isValidVariables then
					Error.error self#id.Entity.name
						"The variables contain epsilon '~', and it should not" ()
				;

				if not isIntersectionValid then
					Error.error self#id.Entity.name
						"The intersection between the alphabet and the variables is not empty" ()
				;

				if not isInitialValid then
					Error.error (symb2str representation.initial)
						"Symbol initial does not belong to the set of all variables" ()
				;

				if not areRuleHeadsValid then
					Error.error self#id.Entity.name
						"Some rule heads do not belong to the set of all variables" ()
				;

				if not areRuleBodiesValid then
					Error.error self#id.Entity.name
						"Some rule bodies have invalid characters" ()
				)

			method tracing: unit = ()

			(* This method checks if the grammar is regular
			*
			* @returns bool -> true if regular, false otherwise
			*)
			method isRegular : bool =

				let vs = representation.variables in
				let alp = representation.alphabet in

				let bs = Set.map (fun r -> r.body) representation.rules in

				let isRightLinear bs =
					let isRightLinearX b =
						match b with
							| [a] -> (Set.belongs a alp) || a = epsilon
							| [a; v] -> (Set.belongs a alp) && (Set.belongs v vs)
							| _ -> false
					in
						Set.for_all (fun b -> isRightLinearX b) bs
				in

				let isLeftLinear bs =
					let rec isLeftLinearX b =
						match b with
							| [a] -> (Set.belongs a alp) || a = epsilon
							| [v; a] -> (Set.belongs v vs) && (Set.belongs a alp)
							| _ -> false
					in
						Set.for_all (fun b -> isLeftLinearX b) bs
				in
					isRightLinear bs || isLeftLinear bs


      method first testWord = first testWord simplified self#representation
      method follow testSymbol = follow testSymbol simplified self#representation
      method lookahead rule = lookahead rule simplified self#representation

			(* This method checks if the given word is accepted by the grammar
			*
			* @param testWord -> word to be tested
			*
			* @returns bool -> true if it accepts the word, false otherwise
			*)


			method accept (testWord:word) : bool =
				ChomskyNormalForm.accept (self#representation) testWord

			method private acceptXXX (testWord:word) : bool =

				(* any word with a symbol not from the cfg alphabet will not be accepted
				if not (Set.subset (Set.make testWord) representation.alphabet) then false else
				*)

				let vs = representation.variables in


				(* for word wa, get subword to the left of its first variable *)
				let rec getPrefix wa =
					match wa with
						| [] -> []
						| x::xs -> if Set.belongs x vs then [] else x::(getPrefix xs)
				in

				(* for word wa, get subword to the rigth of its last variable *)
				let getSuffix wa =
					let rec getSuffixX wa sfx =
						match wa with
							| [] -> sfx
							| x::xs -> let auxSfx = sfx@[x] in
										if Set.belongs x vs then getSuffixX xs []
											else getSuffixX xs auxSfx
					in
						getSuffixX wa []
				in

				let rec firstNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n > 0 then x::(firstNElements xs (n-1)) else []
				in

				let rec lastNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n < (List.length w) then lastNElements xs n else w
				in

				(* a word can be discarded if its prefix does not match the leftmostmost part of word w *)
				let keepByPrefix genW testW =
					let pgw = getPrefix genW in
					let ptw = firstNElements testW (List.length pgw) in
						pgw = [] || pgw = ptw
				in


				(* a word can be discarded if its suffix does not match the rightmost part of word w *)
				let keepBySufix genW testW =
					let sgw = getSuffix genW in
					let stw = lastNElements testW (List.length sgw) in
						sgw = [] || sgw = stw
				in

				(* the word inst discarded only if it cant be discarded by neither its prefix nor its suffix *)
				let toKeep w tw = (w = [] && tw = []) || (keepByPrefix w tw && keepBySufix w tw) in


				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in
				let l = List.length testWord in

				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
					let rws = Set.filter (fun w -> not (exceedsMaxLen w l alph)) subsWs in
					let rws = Set.map (fun w -> removeEpsi w) rws in
						Set.filter (fun w -> toKeep w testWord ) rws

				in

				let start = Set.make [[representation.initial]] in

				let res = Set.historicalFixedPoint nextGeneration start in
					Set.exists (fun x -> x = testWord ) res




			method acceptWithTracing (testWord:word) =



				let vs = representation.variables in


				(* for word wa, get subword to the left of its first variable *)
				let rec getPrefix wa =
					match wa with
						| [] -> []
						| x::xs -> if Set.belongs x vs then [] else x::(getPrefix xs)
				in

				(* for word wa, get subword to the rigth of its last variable *)
				let getSuffix wa =
					let rec getSuffixX wa sfx =
						match wa with
							| [] -> sfx
							| x::xs -> let auxSfx = sfx@[x] in
										if Set.belongs x vs then getSuffixX xs []
											else getSuffixX xs auxSfx
					in
						getSuffixX wa []
				in

				let rec firstNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n > 0 then x::(firstNElements xs (n-1)) else []
				in

				let rec lastNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n < (List.length w) then lastNElements xs n else w
				in

				(* a word can be discarded if its prefix does not match the leftmostmost part of word w *)
				let keepByPrefix genW testW =
					let pgw = getPrefix genW in
					let ptw = firstNElements testW (List.length pgw) in
						pgw = [] || pgw = ptw
				in


				(* a word can be discarded if its suffix does not match the rightmost part of word w *)
				let keepBySufix genW testW =
					let sgw = getSuffix genW in
					let stw = lastNElements testW (List.length sgw) in
						sgw = [] || sgw = stw
				in

				(* the word inst discarded only if it cant be discarded by neither its prefix nor its suffix *)
				let toKeep w tw = (w = [] && tw = []) || (keepByPrefix w tw && keepBySufix w tw) in


				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in
				let l = List.length testWord in

				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
					let rws = Set.filter (fun w -> not (exceedsMaxLen w l alph)) subsWs in
					let rws = Set.map (fun w -> removeEpsi w) rws in
						Set.filter (fun w -> toKeep w testWord ) rws

				in

				let start = Set.make [[representation.initial]] in

				let res = Set.historicalFixedPointTracing nextGeneration start in


				let trimRes l =
					match l with
					| [] -> []
					| x::xs -> if Set.belongs testWord x then xs
								else l
				in

				let res2 = List.rev (trimRes (List.rev res)) in


				let printWset ws =
					Util.print ["["];
					Set.iter (fun w -> Util.print [word2str w; ";"]) ws;
					Util.println ["]"];
				in

					List.iter (fun ws -> printWset ws) res2




			(* This method generates all words up the the given lenght that belong to the grammars language
			*
			* @ param lenght -> the max lenght of generated words
			*
			* @returns words -> the set of generated words
			*)
			method generate (length:int) : words =

				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in


				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
						Set.filter (fun w -> not (exceedsMaxLen w length alph)) subsWs
				in

				let start = Set.make [[representation.initial]] in

				let res = Set.historicalFixedPoint nextGeneration start in

					cleanNonWords res vs

			method checkProperty (prop: string) =
				match prop with
					| "regular" -> self#isRegular
					| "context free grammar" -> true
					| _ -> super#checkProperty prop

		(* Learn-OCaml support *)
			method moduleName = CFGForLearnOCaml.moduleName
			method xTypeName = CFGForLearnOCaml.xTypeName
			method xTypeDeclString : string = CFGForLearnOCaml.prelude
			method toDisplayString (name: string): string =
				CFGForLearnOCaml.solution name self#representationx
			method example : JSon.t = CFGForLearnOCaml.example
		end
end

module ContextFreeGrammarTests: sig end =
struct
	let active = false
	
	let test0 () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let j = m#toJSon in
			JSon.show j

	let test1 () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_balanced") in
		let e = new Exercise.exercise (Arg.Predef "exer_balanced") in
		let result = m#checkExercise e in
			if result then Util.println ["it works"] else Util.println ["it does not work"]

	let testRegular () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let ws = m#isRegular in
			if ws then Util.println ["is regular"] else Util.println ["is not regular"]


	let testAcc () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let ws = m#accept [] in
			if ws then Util.println ["Word was accepted"]
			else Util.println ["Word was not accepted"]


	let testTrace () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
			m#acceptWithTracing (word "01")

	let testGen () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_simple") in
		let ws = m#generate 4 in
			Util.printWords ws

	let show m =
		let j = m#toJSon in
			JSon.show j
	
	let showB b =
		if b then print_string "YES\n"
		else print_string "NO\n"	
	
	let testChomsky () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_balanced") in
			showB (m#accept (word ""));			
			showB (m#accept (word "[[[]]]"));			
			showB (m#accept (word "[[][][]][]"));		
			showB (m#accept (word "[[][]]][]"))			

	let testExercice () =
		let e = new Exercise.exercise (Arg.Predef "exer_balanced") in
		let g = new ContextFreeGrammar.model (Arg.Predef "cfg_balanced") in
		let b = g#checkExercise e in
			showB b

	let runAll =
		if Util.testing active "ContextFreeGrammar" then begin
		(*
			test0 ();
			test1 ();
			testRegular ();
			testAcc ();
			testTrace ();
			testGen ()*)
			testExercice()
		end
end


# 3 "src/RDParser.ml"
open BasicTypes
open CFGTypes

module RDParser =
struct

  (* given a head, returns the set of all its bodies according to the cfg's rules *)
	let bodiesOfHead h rl =
	  let open CFGTypes in
		let rls = Set.filter (fun r -> r.head = h) rl in
			Set.map (fun r -> r.body) rls

  let rec tabCreator tabLevel =
    if tabLevel > 0 then "\t"^ tabCreator (tabLevel - 1) else ""

  class virtual parser =
    object(self)

    val arrayVar = "word"
    val currentCharVar = "currentChar"
    val currentIndexVar = "wordIndex"
      
    val getCharFun = "getChar"
    val parseErrorFun = "parseError"
    val matchCharFun = "matchChar"

    val virtual equality : string
    val virtual orOp : string
    val virtual functionArgsOpen : string
    val virtual functionArgsClose : string
    val virtual ifOpen : string 
    val virtual ifClose : string
    val virtual ifElseGuardOpen : string
    val virtual ifElseGuardClose : string
    val virtual expressionTermination : string
    val virtual return : string

    (**Method that will print includes/imports needed for the parser**)
    method virtual setupIncludes : string
    
    
    (**Method used to setup the variables for the parser.**)
  	(**Use values arrayVar, currentCharVar and currentIndexVar**)
  	(**to get correct variable names.**)
    method virtual setupVariables : string
    
    
    (**Method that prints the getCharFunction.**)
    (**This function consumes a symbol while parsing.**)
    method virtual getCharFunction : string
    
    
    (**Method that prints the match function.**)
    (**This function verifies if symbols match.**)
    (**Consumes the symbol if there is a match.**)
    (**Otherwise, calls error function.**)
    method virtual matchFunction : string
    
    
    (**Method that prints the error function.**)
    (**This function exists the program when called symbols match.**)
    method virtual errorFunction : string
    

    method printFunctions vars rep =
      if vars = Set.empty then ""
      else let (x,xs) = Set.cut vars in
        self#symbolFunction x rep ^
        (self#printFunctions xs rep)
 
    (**Method that prints the programs main function.**)
    method virtual mainFunction : symbol -> string
    

    method virtual createFun : string -> string -> string


    (*TODO Move to lower level*)
    method createFunCalls funs (rep: t) =
      match funs with
      | [] -> []
      | x::xs when Set.belongs x rep.alphabet -> [matchCharFun ^ functionArgsOpen ^ "\'" ^ symb2str x ^ "\'" ^ functionArgsClose ^ expressionTermination] @ self#createFunCalls xs rep
      | x::xs when Set.belongs x rep.variables -> [symb2str x ^ functionArgsOpen ^ functionArgsClose ^ expressionTermination] @ self#createFunCalls xs rep
      | x::xs -> []

    (*TODO Move to lower level*)
    method createIfConds conditions =
      let p = String.make 1 '\'' in
      match conditions with
      | [] -> ""
      | [x] -> currentCharVar ^ equality ^ p ^ symb2str x ^ p
      | x::xs -> currentCharVar ^ equality ^ p ^ symb2str x ^ p ^ " " ^ orOp ^ " " ^ self#createIfConds xs
      
    (*TODO Move to lower level*)
    method createIf ifList tabLevel = 
      let rec createIf2 first ifList tabLevel =
        let rec createExpr exprList tabLevel =
          match exprList with 
          | [] -> ""
          | x::xs -> (tabCreator tabLevel) ^ x ^ "\n" ^ createExpr xs tabLevel
        in
        let tab = (tabCreator tabLevel) in
        match ifList with
        | [] -> ""
        | [(c,e)] -> tab ^ "else" ^ ifElseGuardOpen ^ "\n" ^ (createExpr e (tabLevel+1)) ^ tab ^ ifElseGuardClose
        | (c,e)::xs -> tab ^ (if first then "if" else "else if") ^ ifOpen ^ c ^ ifClose ^ ifElseGuardOpen ^ "\n" ^ 
                       (createExpr e (tabLevel+1)) ^
                       tab ^ (ifElseGuardClose) ^ "\n" ^ createIf2 false xs tabLevel
      in
      createIf2 true ifList tabLevel

  
    method symbolFunction s (rep: t) =
      let open CFGTypes in
      let rec getNextTerminals rule = 
(*        Printf.printf "\tGetting lookahead for rules";*)
(*        List.iter (fun r -> Printf.printf " %s " r) (CFGSyntax.toStringList (Set.make [rule]));*)
(*        Printf.printf "\n";*)
(*        Set.iter (fun r -> Printf.printf "\tGot result %s\n" (symb2str r)) (ContextFreeGrammar.lookahead rule false rep);*)
        ContextFreeGrammar.lookahead rule false rep
      in

      let rules = Set.filter (fun {head;body} -> head = s) rep.rules in
      let funCalls = (List.map (fun {head;body} -> self#createFunCalls body rep) (Set.toList rules)) in
      let lookaheads = Set.map (getNextTerminals) rules in
      let mergedMap = List.map2 (fun a b -> (self#createIfConds (Set.toList a),b)) (Set.toList lookaheads) funCalls in
(*      Printf.printf "Current Var: %s\n" (symb2str s);*)
(*      List.iter (fun (a,b) -> Printf.printf "\t%s" a;*)
(*                              Printf.printf "\n";*)
(*                              List.iter (fun c -> Printf.printf "\t\t%s\n" c) b*)
(*      ) mergedMap;*)
      let mergedMap = mergedMap @ [("",[(parseErrorFun ^ functionArgsOpen ^ functionArgsClose ^  expressionTermination)])] in
      self#createFun (symb2str s) (self#createIf mergedMap 1)


    method virtual build : t -> string

  end
end


module RDParserWithDeclarations =
struct

  class virtual parser =
    object(self) inherit RDParser.parser as super
    
  end

end


module RDParserWithoutDeclarations =
struct

  class virtual parser =
    object(self) inherit RDParser.parser as super
    
      method build (rep: t) =
        self#setupIncludes ^
        self#setupVariables ^
        self#getCharFunction ^
        self#errorFunction ^
        self#matchFunction ^
        (self#printFunctions rep.variables rep) ^
        (self#mainFunction rep.initial)
    
  end
end


module RDParserNeedFunDeclaration =
struct

  class virtual parser =
    object(self) inherit RDParserWithDeclarations.parser as super
    
      (***Method that prints function declarations*)
      method functionDeclarations vars =
        let rec printDecl vars =
          if vars = Set.empty then ""
          else let (x,xs) = Set.cut vars in
            Printf.sprintf {|void %s();|} (symb2str x) ^ "\n" ^ printDecl xs
          in
      printDecl vars ^ "\n"
    
      method build (rep: t) =
        Util.stripHead (self#setupIncludes ^
        self#setupVariables ^
        self#functionDeclarations rep.variables ^
        self#getCharFunction ^
        self#errorFunction ^
        self#matchFunction ^
        (self#printFunctions rep.variables rep) ^
        (self#mainFunction rep.initial))
    
  end

end


module RDParserNeedRecursiveFunDeclaration =
struct

  class virtual parser =
    object(self) inherit RDParserWithDeclarations.parser as super
    
      method printFunctions vars rep =
        let first = true in
        let rec printFunctionsX vars rep first =
          if vars = Set.empty then ""
          else let (x,xs) = Set.cut vars in
            (if first then "let rec " else "and ") ^
            self#symbolFunction x rep ^
            (printFunctionsX xs rep false)
        in
        printFunctionsX vars rep first
    
      method build (rep: t) =
        Util.stripHead (self#setupIncludes ^
        self#setupVariables ^
        self#getCharFunction ^
        self#errorFunction ^
        self#matchFunction ^
        (self#printFunctions rep.variables rep) ^
        (self#mainFunction rep.initial))
    
  end

end


module RDParserC =
struct

  class parser =
    object(self) inherit RDParserNeedFunDeclaration.parser as super

    val equality = "=="
    val orOp = "||"
    val functionArgsOpen = "("
    val functionArgsClose = ")"
    val ifOpen = "("
    val ifClose = ")"
    val ifElseGuardOpen = "{"
    val ifElseGuardClose = "}"
    val expressionTermination = ";"
    val return = "return"


    method setupIncludes =
  	  Printf.sprintf {|
		 #include <stdio.h>
		 #include <stdlib.h>
		 #include <string.h>
      |}
    
    
    method setupVariables =
      Printf.sprintf {|
		char* %s;
		char %s;
		int %s;
		|} arrayVar currentCharVar currentIndexVar
    
    
    method getCharFunction =
      Printf.sprintf {|
		char %s(){
			return %s[%s++];
		}
      |} (getCharFun) (arrayVar) (currentIndexVar)
    
    
    method matchFunction =
      Printf.sprintf {|
		void %s(char t) {
			if(%s == t) {
				%s = %s();
			}
			else {
				%s();
			}
		}
      |} (matchCharFun) (currentCharVar) (currentCharVar) (getCharFun) (parseErrorFun)
    
    
    method errorFunction = 
      Printf.sprintf {|
		void %s() {
			printf("Error parsing! %s = %%c\n", %s);
			exit(1);
		}
      |} (parseErrorFun) (currentCharVar) (currentCharVar)
    
      
    method mainFunction initialVar = 
      Printf.sprintf {|
		int main(int argc, char* argv[]){
		  char termChar = '$'; 
		  char* tmp = strcat( argv[1], &termChar);
			%s = tmp;
			%s = %s();
			%s();
			if(%s == '$'){
				printf("Parsing OK!\n");
			} else {
				%s();
			}
			return 0;
		}
    |} (arrayVar) (currentCharVar) (getCharFun) (symb2str initialVar) (currentCharVar) (parseErrorFun)
    

    method createFun name contents =
      Printf.sprintf {|
        void %s() {
        %s
        }
      |} name contents

  end
end


module RDParserOCaml =
struct

	let tabCreator = RDParser.tabCreator

  class parser =
    object(self) inherit RDParserNeedRecursiveFunDeclaration.parser as super

    val equality = "="
    val orOp = "||"
    val functionArgsOpen = "("
    val functionArgsClose = ")"
    val ifOpen = "("
    val ifClose = ")"
    val ifElseGuardOpen = "("
    val ifElseGuardClose = ")"
    val expressionTermination = ";"
    val return = "()"
    
    method setupIncludes = ""


    method setupVariables = 
      Printf.sprintf {|
        let %s = ref [||]
        let %s = ref 'l'
        let %s = ref 0
      |} arrayVar currentCharVar currentIndexVar
      
    
    method getCharFunction = 
      Printf.sprintf {|
        let %s() =
          if ( !%s < (Array.length !%s) - 1 )
          then %s := !%s + 1;
          %s := !%s.(!%s)
              |} (getCharFun)
                (currentIndexVar) (arrayVar)
                (currentIndexVar) (currentIndexVar)
                currentCharVar (arrayVar) (currentIndexVar)
        
        
    method createFunCalls funs (rep: t) =
      match funs with
      | [] -> []
      | x::xs when Set.belongs x rep.alphabet -> [matchCharFun ^ functionArgsOpen ^ "\'" ^ (symb2str x) ^ "\'" ^ functionArgsClose ^ expressionTermination] @ self#createFunCalls xs rep
      | x::xs when Set.belongs x rep.variables -> [String.lowercase_ascii (symb2str x) ^ functionArgsOpen ^ functionArgsClose ^ expressionTermination] @ self#createFunCalls xs rep
      | x::xs -> []


    method matchFunction = 
      Printf.sprintf {|
		let %s t =
			if (!%s = t)
			then (%s())
			else (%s())
      |} matchCharFun currentCharVar getCharFun parseErrorFun

      
    method errorFunction = 
      Printf.sprintf {|
		let %s() =
			Printf.printf "Error parsing! %s = %%c\n" !%s;
			exit 1
      |} parseErrorFun currentCharVar currentCharVar


    method createIfConds conditions =
      let p = String.make 1 '\'' in
      match conditions with
      | [] -> ""
      | [x] -> "!" ^ currentCharVar ^ equality ^ p ^ symb2str x ^ p
      | x::xs -> "!" ^ currentCharVar ^ equality ^ p ^ symb2str x ^ p ^ " " ^ orOp ^ " " ^ self#createIfConds xs
      

    method createIf ifList tabLevel = 
      let rec createIf2 first ifList tabLevel =
        let rec createExpr exprList tabLevel =
          match exprList with 
          | [] -> ""
          | x::xs -> (tabCreator tabLevel) ^ x ^ "\n" ^ createExpr xs tabLevel
        in
        let tab = (tabCreator tabLevel) in
        match ifList with
        | [] -> ""
        | [(c,e)] -> tab ^ "else" ^ ifElseGuardOpen ^ "\n" ^ (createExpr e (tabLevel+1)) ^ tab ^ ifElseGuardClose
        | (c,e)::xs -> tab ^ (if first then "if" else "else if") ^ ifOpen ^ c ^ ifClose ^ " then " ^ ifElseGuardOpen ^ "\n" ^ 
                       (createExpr e (tabLevel+1)) ^
                       tab ^ (ifElseGuardClose) ^ "\n" ^ createIf2 false xs tabLevel
      in
      createIf2 true ifList tabLevel
    
    
    method mainFunction i = 
      Printf.sprintf {|
		let explode s = List.init (String.length s) (String.get s)

		let main () =
			%s := Array.of_list (explode Sys.argv.(1));
			%s := Array.append (!%s) ([|'$'|]);
			%s := !%s.(0);
			%s();
			if !%s = '$'
			then Printf.printf "Parsing OK!"
			else %s();
			exit 0

		let _ = main()
	  |} arrayVar arrayVar arrayVar
                  currentCharVar arrayVar
                  (String.lowercase_ascii (symb2str i))
                  currentCharVar
                  parseErrorFun

    method createFun name contents =
      (*Does not need let, inherited class deals with it*)
      Printf.sprintf {|
		%s () =
			%s
      |} (String.lowercase_ascii name) contents

  end
end

module RDParserJava =
struct

  class parser =
    object(self) inherit RDParserWithoutDeclarations.parser as super

    val equality = "=="
    val orOp = "||"
    val functionArgsOpen = "("
    val functionArgsClose = ")"
    val ifOpen = "("
    val ifClose = ")"
    val ifElseGuardOpen = "{"
    val ifElseGuardClose = "}"
    val expressionTermination = ";"
    val return = "return"

    method setupIncludes = ""
    
    method setupVariables =
  	  Printf.sprintf {|
		static char[] %s;
		static char %s;
		static int %s;
      |} arrayVar currentCharVar currentIndexVar
 

    method getCharFunction =
      Printf.sprintf {|
		static char %s() {
			return %s[%s++];
		}
      |} (getCharFun) (arrayVar) (currentIndexVar)
    
    method matchFunction =
      Printf.sprintf {|
		static void %s(char t) {
			if(%s == t)
				%s = %s();
			else %s();
		}
      |} (matchCharFun) (currentCharVar) (currentCharVar) (getCharFun) (parseErrorFun)
    
    
    method errorFunction = 
      Printf.sprintf {|
		static void %s() {
			System.out.println("Error parsing! %s = " + %s);
			System.exit(1);
		}
      |} (parseErrorFun) (currentCharVar) (currentCharVar)


    method mainFunction initialVar = 
      Printf.sprintf {|
		public static void main(String[] args) {
			%s = 0;
			%s = (args[0].toString()+'$').toCharArray(); //Add terminal symbol
			%s = %s();
			%s();
			if(%s == '$')
				System.out.println("Parsing OK!");
			else %s();
		}
    |} currentIndexVar arrayVar (currentCharVar) (getCharFun) (symb2str initialVar) (currentCharVar) (parseErrorFun)


    method createFun name contents =
      Printf.sprintf {|
		static void %s () {
			%s
		}
      |} name contents

	method build (rep: t) =
		"public class Main {" ^
			super#build rep ^
		"}"

  end
end


# 3 "src/LL1Grammar.ml"
open BasicTypes
open CFGTypes  

module type LL1GrammarSig =
sig  
  type syntaxTable = { term : symbol option; var : symbol option; rBody : word option }
  type acceptTable = { input : string; stack: string; production: string }
  type recognized = { recog : string; left : string }
	type acceptStep = {
    syntaxTable : syntaxTable;
    acceptedString: string;
    acceptTable : acceptTable;
    recognized : recognized;
    accepted: bool option;
    nodes: cfgTree list
  }
  
  val leftRecursionRemovalTransform : string
  val leftFactoringTransform : string
  val cleanProductiveTransform : string
  val cleanAccessibleTransform : string
  val unitRemovalTransform : string
  val epsilonRemovalTransform :  string
  val ll1Transform : string
  
  type transformation = { tType : string; grammar : ContextFreeGrammar.model }
	
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method validate : unit
				method toJSon: JSon.t
				method representation: t
				method representationx: tx

				method tracing: unit
				method isRegular: bool
				method accept: word -> bool
				method acceptWithTracing: word -> unit
				method generate: int -> words
				
				method isSimplified : bool
				method rdparserOpts : string list
				method toggleSimplified : unit
				
				method first: word -> symbol Set.t
			  method follow: symbol -> symbol Set.t
			  method lookahead: rule -> symbol Set.t
			  method isLL1: bool
			  method isLeftRecursive: bool
			  method createParsingTable: ((variable * symbol) * word) Set.t
			  method hasParsingTableConflict : bool 
			  method acceptZ: word -> acceptStep list
 			  method productiveSymbols: symbol Set.t
 			  method accessibleSymbols: symbol Set.t
        method productiveRewrite: ContextFreeGrammar.model
        method accessibleRewrite: ContextFreeGrammar.model
        method clean: transformation list
        method isFullyProductive: bool
        method isFullyAccessible: bool
        method isClean: bool
        method removeLeftRecursion: transformation
        method removeDirectLeftRecursion: ContextFreeGrammar.model
        method leftFactoring: transformation
        method isLeftFactoring: bool
        method leftCorner: symbol -> symbol Set.t
        method hasEmptyProductions: bool
        method removeEmptyProductions: transformation
        method hasUnitProductions: bool
        method removeUnitProductions: transformation
        method generateRecursiveDescendentParser: string -> string
        method transformToLL1: transformation list

				method checkProperty : string -> bool
				method checkExercise: Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties
				
				(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t
			end
end

module LL1Grammar : LL1GrammarSig =
struct
  open ContextFreeGrammar  
  open RDParserC
  open RDParserOCaml
  open RDParserJava
   
  type syntaxTable = { term : symbol option; var : symbol option; rBody : word option }
  type acceptTable = { input : string; stack: string; production: string }
  type recognized = { recog : string; left : string }
	type acceptStep = {
    syntaxTable : syntaxTable;
    acceptedString: string;
    acceptTable : acceptTable;
    recognized : recognized;
    accepted: bool option;
    nodes: cfgTree list
  }
  
  let bodiesOfHead = RDParser.bodiesOfHead
  
  let leftRecursionRemovalTransform = "Remove left recursion"
  let leftFactoringTransform = "Left factoring"
  let cleanProductiveTransform = "Clean unproductive symbols"
  let cleanAccessibleTransform = "Clean inaccessible symbols"
  let unitRemovalTransform = "Unit productions removal"
  let epsilonRemovalTransform = "Epsilon productions removal"
  let ll1Transform = "LL1 transformation"
  
  type transformation = { tType : string; grammar : ContextFreeGrammar.model }
  
  let newStep ?(term = None) ?(var = None) ?(rBody = None)
              ?(acceptedString = "") 
              ?(input = "") ?(stack = "") ?(production = "") 
              ?(recog = "") ?(left = "") 
              ?(accepted = None) ?(nodes = []) simple =
    (* let dollar = String.make 1 dollar in
    let input = if simple then input else input ^ dollar in
    let stack = if simple then stack else stack ^ dollar in *)
    {
      syntaxTable = {term; var; rBody};
      acceptedString = acceptedString;
      acceptTable = {input; stack; production};
      recognized = {recog; left};
      accepted = accepted;
      nodes = nodes
    }
    
  
  (*type rule = CFGSyntax.rule*)
  

  let rec doWordGenerateEmptyX w seen (rep:t) =
    let rec doGenerateEmpty x =
      if List.mem x seen
      then false
      else(
		    let bodies = bodiesOfHead x rep.rules in
		    Set.exists (fun b -> doWordGenerateEmptyX b (x::seen) rep) bodies 
		  )
		in      
      List.for_all doGenerateEmpty w

  let removeEpsilonFromWord w =
    List.filter (fun c -> c <> epsilon) w

  let removeDollarFromWord w =
    List.filter (fun c -> c <> dollar) w

  let doWordGenerateEmpty w (rep:t) =
    doWordGenerateEmptyX (removeDollarFromWord w) [] rep
    
  let printRepresentation (rep:t) =
    Printf.printf "Alphabet = "; Util.printAlphabet rep.alphabet;
    Printf.printf "Variables = "; Util.printAlphabet rep.variables;
    Printf.printf "Initial = %s\n" (symb2str rep.initial);
    Printf.printf "Rules {\n"; Set.iter (fun {head=h; body=b} -> Printf.printf "\t%s -> %s\n" (symb2str h) (word2str b)) rep.rules;
    Printf.printf "}\n\n"

  let rec print_tuples = (*TEST*)
    function
    | [] -> ()
    | (a, b) :: rest ->
      Printf.printf "%c -> " a;
      Util.printAlphabet b;
      print_tuples rest
      
  let rec print_list = (*TEST*)
    function
    | [] -> Printf.printf "";
    | x::xs ->
      Printf.printf "%c" x;
      print_list xs  
  
  (*Given a variable, returns all rules with variable as head*)
  let sameHeadRules (testSymbol:symbol) (rep:t) =
	  Set.toList (Set.filter (fun r -> testSymbol = r.head) rep.rules)

  let rec pairs l =
    match l with
      | [] -> []
      | x::xs -> List.map (fun v -> (x,v)) xs :: pairs xs

  (*Given a word and variable, returns the word behind the variable*)
  let rec behindSymbol (word:word) (var:variable) = 
    match word with
    | [] -> []
    | x::xs -> if x <> var then x::behindSymbol xs var else []


  let rec leftRecursionTest initial (seen:variable Set.t) (rep:t) =
    if Set.belongs initial rep.alphabet then false else (*rule starting with a terminal symbol can't be left recursive*)
      let ruleBodies = Set.toList (bodiesOfHead initial rep.rules) in (* example: rulesBodies = [['B';'a']; ['b']*)
(*        Printf.printf "initial = %c\n" initial;*)
      let rec leftRecursionTest2 head body seen (rep:t) =
        let wordBehind = behindSymbol body head in
        let behindGenerateEmpty = doWordGenerateEmpty wordBehind rep in
        let body = if behindGenerateEmpty 
                     then List.filter (fun x -> not (List.mem x wordBehind)) body 
                     else body
        in
        
        match body with
        | [] -> false
        | x::xs when x = head || Set.belongs x seen -> true
        | x::xs -> leftRecursionTest x (Set.cons x seen) rep in
        
      List.exists (fun x -> x = true) (List.map (fun x -> leftRecursionTest2 initial x seen rep) ruleBodies)

      
  let isLeftRecursive (rep:t) = 
    Set.exists (fun x -> x = true) (Set.map (fun v -> leftRecursionTest v Set.empty rep) rep.variables)

  let isLL1Deterministic simple (rep:t) =
    let variables = rep.variables in
    let pairsSet = Set.map (fun v -> Set.make (List.flatten (pairs (sameHeadRules v rep)))) variables in
    let lookaheadInterSet = Set.flatMap (fun v -> Set.map (fun (p1,p2) -> Set.inter (lookahead p1 simple rep) (lookahead p2 simple rep)) v) pairsSet in
      Set.for_all (fun x -> Set.size x = 0) lookaheadInterSet

  let isLL1 simple (rep:t) = 
    isLL1Deterministic simple rep
  
  (*given a production X->a, does lookahead(X->a), b, and returns pair ((X,b),a)*)
  let lookahead2Tuple rule simple (rep:t) =
    let lookahead = lookahead rule simple rep in 
      Set.map (fun l -> ((rule.head, l), rule.body)) lookahead
  
  let createParsingTable simple (rep:t) = 
    let lookaheadSet = Set.flatMap (fun r -> lookahead2Tuple r simple rep) rep.rules in
      lookaheadSet
  
  let hasParsingTableConflict simple (rep:t) =
    let parsingTable = createParsingTable simple rep in
    let repeatsTbl = Hashtbl.create (Set.size parsingTable) in
    let getRepeatNum c repeatsTbl =
      let repeat = Hashtbl.find_opt repeatsTbl c in
      match repeat with
      | None -> Hashtbl.add repeatsTbl c 1; false
      | Some a -> true
    in
    let boolResults = Set.map (fun ( (v,t), _ ) -> getRepeatNum (v,t) repeatsTbl ) parsingTable in
    Set.exists (fun r -> r) boolResults
    
  
  (*accept*)
  
(*  let printParsingInfo entry stack sub isSub =*)
(*    Printf.printf "\t"; print_list entry;*)
(*    Printf.printf "\t"; print_list stack;*)
(*    if isSub*)
(*      then (Printf.printf "\t%c->" (List.nth stack 0); print_list sub;)*)
(*      else Printf.printf "\t";*)
(*    Printf.printf "\n"*)
(*  *)
(*  (*given the entry, stack and parsingTable, rewrites the leftmost*)*)
(*  (*variable on the stack with its respective parsingTable rule*)*)
(*  let ruleRewrite (entry:word) (stack:word) parsingTable =*)
(*    let entryChar = List.nth entry 0 in*)
(*    let stackChar = List.nth stack 0 in*)
(*    let parsingTableList = Set.toList parsingTable in*)
(*    let substitution = List.assoc (stackChar, entryChar) parsingTableList in*)
(*      match stack with*)
(*      | [] -> []*)
(*      | x::xs ->*)
(*                printParsingInfo entry stack substitution true;*)
(*                substitution@xs*)
(*  *)
(*  let rec acceptX entry stack parsingTable (rep:t) =*)
(*    match entry with*)
(*    | [] -> if doWordGenerateEmpty stack rep then true else false*)
(*    | x::xs -> match stack with*)
(*                | [] -> false*)
(*                | x2::xs2 -> if Set.belongs x2 rep.variables*)
(*                             then*)
(*                                let newStack = ruleRewrite entry stack parsingTable in*)
(*                                acceptX entry newStack parsingTable rep*)
(*                             else if x=x2 *)
(*                                  then (printParsingInfo entry stack [] false;*)
(*                                       acceptX xs xs2 parsingTable rep )*)
(*                                  else false*)
(*  *)
(*  let acceptZ word rep = *)
(*    Printf.printf "\t"; Printf.printf "Entry: ";*)
(*    Printf.printf "\t"; Printf.printf "Stack: ";*)
(*    Printf.printf "\t"; Printf.printf "Rule: \n";*)
(*    let parsingTable = createParsingTable rep in*)
(*      try (acceptX word [rep.initial] parsingTable rep) *)
(*        with Not_found -> Printf.printf "\t\t\tApplicable rule not found!\n"; false*)

  let word2tree w (rep:t) =
    let rec word2tree2 w =
    match w with
    | [] -> []
    | x::xs -> (if Set.belongs x rep.alphabet
                then Leaf x
                else Root(x,[]))
                :: word2tree2 xs
    in
    
    if List.length w = 0
    then [Leaf epsilon]
    else word2tree2 w

  let rec acceptX entry stack parsingTable (currPerm:symbol list) simple (rep:t) =
    match entry with
    | [] -> [] (*Not supposed to happen*)
    | x::xs when x = dollar ->
          (match stack with
          | [] -> [] (*Not supposed to happen*)
          | x::xs -> if doWordGenerateEmpty [x] rep
                      then
                        (
                          if x = dollar
                          then [newStep ~acceptedString:(word2str currPerm)
                               ~input:(word2str entry)
                               ~stack:(word2str stack)
                               ~recog:(word2str (currPerm))
                               ~accepted:(Some true)
                                simple]
                          else (newStep ~var:(Some (List.hd stack))
                              ~term:(Some dollar)
                              ~rBody:(Some [])
                              ~acceptedString:(word2str currPerm)
                              ~input:(word2str entry)
                              ~stack:(word2str stack) 
                              ~production:(symb2str (List.hd stack) ^ " -> " ^ "") 
                              ~recog:(word2str currPerm)
                              ~nodes:(word2tree [] rep)
                              simple) :: acceptX entry xs parsingTable currPerm simple rep
                        )
                      else [newStep ~var:(Some (List.hd stack))
                      ~term:(Some dollar)
                      ~rBody:(Some [])
                      ~acceptedString:(word2str currPerm) 
                      ~input:(word2str entry)
                      ~stack:(word2str stack)
                      ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                      ~accepted:(Some false)
                      simple]
             )
    | x::xs -> match stack with
                | [] -> [] (*Not supposed to happen*)
                | [epsilon] -> [newStep ~acceptedString:(word2str currPerm)
                                ~input:(word2str entry)
                                ~stack:(word2str stack)
                                ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                                ~accepted:(Some false)
                                simple]
                | x2::xs2 -> if Set.belongs x2 rep.variables
                             then
                                let entryChar = List.nth entry 0 in
                                let stackChar = List.nth stack 0 in
                                let parsingTableList = Set.toList parsingTable in
                                let substitution = List.assoc_opt (stackChar, entryChar) parsingTableList in
                                match substitution with
                                  | None -> [newStep ~term:(Some entryChar) ~var:(Some stackChar)
                                                     ~acceptedString:(word2str currPerm)
                                                     ~input:(word2str entry) ~stack:(word2str stack)
                                                     ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                                                     ~accepted:(Some false)
                                                     simple]
                                  | Some s -> let newStack = 
                                                match stack with
                                                | [] -> []
                                                | x::xs -> s@xs 
                                              in
                                              (newStep ~term:(Some entryChar) ~var:(Some stackChar) ~rBody:(Some s)
                                                       ~acceptedString:(word2str currPerm)
                                                       ~input:(word2str entry) ~stack:(word2str stack) ~production:(symb2str (List.nth stack 0) ^ " -> " ^ word2str s)
                                                       ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                                                       ~nodes:(word2tree s rep)
                                                       simple) :: acceptX entry newStack parsingTable currPerm simple rep
                              else if x=x2 
                                  then
                                    let newCurrPerm = currPerm @ [x] in
                                    (newStep ~acceptedString:(word2str newCurrPerm)
                                             ~input:(word2str entry) ~stack:(word2str stack)
                                             ~recog:(word2str newCurrPerm) ~left:(word2str (List.tl (removeDollarFromWord stack)))
                                             simple) :: acceptX xs xs2 parsingTable newCurrPerm simple rep 
                                  else [newStep ~acceptedString:(word2str currPerm)
                                                ~input:(word2str entry) ~stack:(word2str stack) 
                                                ~recog:(word2str currPerm) ~left:(word2str (removeDollarFromWord stack))
                                                ~accepted:(Some false)
                                                simple]
  
  let acceptZ word simple (rep:t) = 
    let word = word @ [dollar] in
    let initial = [rep.initial] @ [dollar] in
    let parsingTable = createParsingTable simple rep in
      (newStep ~input:(word2str word) ~stack:(word2str initial) ~nodes:[Root(rep.initial,[])] simple)
      ::acceptX word initial parsingTable [] simple rep

  let rec acumFixedPoint (f: 'a Set.t -> 'a Set.t) (x: 'a Set.t): 'a Set.t =
    let next = Set.union x (f x) in
      if x = next then x
      else acumFixedPoint f next
  
  (*productive symbols*)
  
  (*given a rule and a set of productive variables, verifies if given*)
  (*rule is productive*)
  let isRuleProductive r prodVars (rep:t) =
(*    Printf.printf "\t\t\tisRuleProductive - Prod = %s   prodVars = %s\n" (word2str r) (word2str (Set.toList prodVars));*)
    List.for_all (fun c -> (*Printf.printf "\t\t\t\tc = %c %b\n" c (Set.belongs c rep.alphabet || Set.belongs c prodVars);*) Set.belongs c rep.alphabet || Set.belongs c prodVars) r
      
  (*given a variable and a set of productive variables, verifies if given *)
  (*variable is productive*)
  let isSymbolProductive h prodVars (rep:t) =
    let rules = bodiesOfHead h rep.rules in
      Set.exists (fun r -> 
(*                          Printf.printf "\t\tProduction = %c -> %s\n" h (word2str r);*)
                          isRuleProductive r prodVars rep
                  ) rules
      
        
  let rec productiveSymbolsFP (rep:t) varP =
    Set.filter (fun v -> (*Printf.printf "\tVar %c\n" v;*) isSymbolProductive v varP rep) (Set.diff rep.variables varP)
  
  (*show the productive symbols of the current grammar*)
  let productiveSymbols (rep:t) =
    acumFixedPoint (productiveSymbolsFP rep) Set.empty
  
  (*show the simplified grammar with only productive symbols*)
  (*TODO Confirm correct new model*)
  let productiveGrammarRewrite (rep:t) =
    let prodSyms = productiveSymbols rep in
(*    Printf.printf "Productive Symbols:\n";*)
(*    Set.iter (fun s -> Printf.printf "\t%c\n" s) prodSyms;*)
(*    Printf.printf "\n";*)
    let unprodSyms = Set.diff rep.variables prodSyms in
(*    Printf.printf "Unproductive Symbols:\n";*)
(*    Set.iter (fun s -> Printf.printf "\t%c\n" s) unprodSyms;*)
(*    Printf.printf "\n";*)
    let newRules = Set.filter (fun r -> Set.belongs r.head prodSyms && List.for_all (fun c -> not (Set.belongs c unprodSyms)) r.body) rep.rules in
(*    Printf.printf "New productions:\n";*)
(*    Set.iter (fun {head=h;body=b} -> Printf.printf "\t%c -> %s\n" h (word2str b)) newRules;*)
      new ContextFreeGrammar.model (Arg.Representation {
								alphabet = rep.alphabet; (*TODO Get productive alphabet*)
								variables = prodSyms;
								initial = rep.initial;
								rules = newRules
						} )

  
  (*accessible symbols*)
  
  (*given a rule and a set of accessible symbols, adds all symbols from the*)
  (*rule to the set*)
  let ruleAccessibleSymbols r aSymbols =
    Set.flatten (Set.make (List.map (fun s -> Set.cons s aSymbols) r))

  let rulesAccessibleSymbols h aSymbols (rep:t) =
    let rules = bodiesOfHead h rep.rules in
      Set.flatMap (fun r -> ruleAccessibleSymbols r aSymbols) rules
  
  let rec accessibleSymbolsX (rep:t) aSymbols =
    let vars = Set.filter (fun v -> Set.belongs v rep.variables) aSymbols in (*Remove terminals*)
      Set.flatMap (fun v -> rulesAccessibleSymbols v aSymbols rep) vars
  
  (*show the accessible symbols of the current grammar*)
  let accessibleSymbols (rep:t) = 
    Util.fixedPoint (accessibleSymbolsX rep) (Set.make [rep.initial])
  
  (*TODO Confirm correct new model*)
  let accessibleGrammarRewrite (rep:t) =
    let accessSymbs = accessibleSymbols rep in
    let accessTerms = Set.filter (fun s -> Set.belongs s rep.alphabet) accessSymbs in
    let accessVars = Set.filter (fun s -> Set.belongs s rep.variables) accessSymbs in
    let rules = Set.filter (fun r -> Set.belongs r.head accessVars) rep.rules in
      new ContextFreeGrammar.model (Arg.Representation {
								alphabet = accessTerms;
								variables = accessVars;
								initial = rep.initial;
								rules = rules
						} )

  let clean (rep:t) =
    let prodRewrite = {tType = cleanProductiveTransform; grammar = productiveGrammarRewrite rep} in
    let accessRewrite = {tType = cleanAccessibleTransform; grammar = accessibleGrammarRewrite prodRewrite.grammar#representation} in
    [prodRewrite; accessRewrite]
(*    accessibleGrammarRewrite (productiveGrammarRewrite rep)#representation*)

  let isCFGFullyProductive (rep:t) =
    Set.equals (productiveSymbols rep) (rep.variables)

  let isCFGFullyAccessible (rep:t) =
    Set.equals (accessibleSymbols rep) (Set.union rep.variables rep.alphabet)
    
  let isClean (rep:t) =
    isCFGFullyProductive rep && isCFGFullyAccessible rep
  
  let getNewVar vs =
    let chars = Set.make ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'] in
    let symbs = Set.map char2symb chars in
    let acceptableVars = Set.diff symbs vs in
      Set.nth acceptableVars 0


  let rec leftCorner2 symbol seen (rep: t) =
    match symbol with
    | [] -> Set.empty
    | x::xs ->
      if Set.belongs x seen 
      then Set.make [x]
      else 
        if Set.belongs x rep.alphabet
        then Set.make [x]
        else Set.union 
              (Set.make [x])
              (Set.flatMap (fun b -> (leftCorner2 b (Set.cons x seen) rep)) (bodiesOfHead x rep.rules))

  let leftCorner symbol rep =
    leftCorner2 [symbol] Set.empty rep

    
  (*left recursion removal*)

  let sortLeftCorner l =
    let sortFun e1 e2 =
      let (_, e1) = e1 in
      let (_, e2) = e2 in
      if List.length e1 > List.length e2 then -1
      else (if List.length e1 < List.length e2 then 1 else 0)
    in
    List.sort sortFun l

  let addToMap map varL =
    let rec addToMap2 map varL value =
      match varL with
      | [] -> ()
      | x::xs -> (*Printf.printf "Adding var %c with value %d\n" x value;*)
                  Hashtbl.add map x value; addToMap2 map xs (value+1)
    in
    addToMap2 map varL 0

(* TODO Util.printWord does not exist anymore *)
(*  let rec print_rules r =*)
(*    match r with*)
(*    | [] -> Printf.printf "\n"*)
(*    | x::xs -> Printf.printf " - %c ->" x.head; Util.printWord x.body; print_rules xs*)


  let removeDirectLeftRecursion (rep:t) = 
    let hasRuleDirectLeftRecursion r =
      match r.body with
      | [] -> false
      | x::xs when x = r.head -> true
      | x::xs -> false
    in
    
    let recursiveRuleRewrite r nV =
      let body =
        match r.body with
        | [] -> [epsilon] (*Not reacheable*)
        | x::xs -> xs@[nV]
      in
      { head = nV; body = body }
    in
      
    let nRecursiveRuleRewrite r nV =
      let body = r.body@[nV] in
        { head = r.head; body = body }
    in
    
    let rec removeDLRFromVar v drs ndrs nV =
      if v = Set.empty then Set.empty
      else let (x,xs) = Set.cut v in
(*        Printf.printf "\tRemoving direct left recursion from variable %c\n" x;*)
        let recursiveRs = Set.filter (fun r -> r.head = x) drs in
        let nRecursiveRs = Set.filter (fun r -> r.head = x) ndrs in
        let newVar = getNewVar nV in
(*        Printf.printf "\tNew variable is %c\n" newVar;*)
        let recRulesRewriteTmp = Set.map (fun r -> recursiveRuleRewrite r newVar) recursiveRs in
        let recRulesRewrite = Set.cons ( {head = newVar; body = []} ) recRulesRewriteTmp in
        let nRecRulesRewrite = Set.map (fun r -> nRecursiveRuleRewrite r newVar) nRecursiveRs in
        let newRules = Set.union recRulesRewrite nRecRulesRewrite in
(*        print_rules (Set.toList newRules);*)
          Set.union newRules (removeDLRFromVar xs drs ndrs (Set.cons newVar nV))
    in
    
    let leftRecursiveRules = Set.filter (fun r -> hasRuleDirectLeftRecursion r) rep.rules in
(*        print_rules (Set.toList leftRecursiveRules);*)
    let leftRecursiveVars = Set.map (fun r -> r.head) leftRecursiveRules in
    let nonLeftRecursiveRules = Set.diff rep.rules leftRecursiveRules in
(*        print_rules (Set.toList nonLeftRecursiveRules);*)
    let nonLeftRecursiveRulesClean = Set.filter (fun {head = h; body = _} -> not (Set.belongs h leftRecursiveVars)) nonLeftRecursiveRules in
    let newRules = Set.union nonLeftRecursiveRulesClean (removeDLRFromVar leftRecursiveVars leftRecursiveRules nonLeftRecursiveRules rep.variables) in
(*      print_rules (Set.toList newRules);*)
    let newVars = Set.union rep.variables (Set.map (fun r -> r.head) newRules) in
      new ContextFreeGrammar.model (Arg.Representation {
		    alphabet = rep.alphabet;
		    variables = newVars;
			  initial = rep.initial;
			  rules = newRules
		  } )


  let rec removeIndirectLeftRecursion map varL (rep:t) = 
    match varL with
    | [] -> new ContextFreeGrammar.model (Arg.Representation {
		          alphabet = rep.alphabet;
		          variables = rep.variables;
			        initial = rep.initial;
			        rules = rep.rules
		        } )
    | var::xs -> 
      let perVarIndirectRemoval map var (rep:t) =
        let perVarProdIndirectRemoval prodHead iVal prodBody rhsValues (rep:t) =
          let results = Set.flatMap (
            fun (jVal, rhsBody) ->
              match jVal with
              | None -> Set.make [{head = prodHead; body = rhsBody}]
              | Some jVal -> 
                if iVal > jVal
                then (
                  let rhsVar = (List.hd rhsBody) in
                  let rhsVarBodies = bodiesOfHead rhsVar rep.rules in
                  let replaceRules = Set.flatMap (fun rhsBody ->
                    if List.length prodBody >= 1
                    then (
                      if List.hd prodBody = rhsVar
                      then Set.make [{head = prodHead; body = rhsBody@(if List.length prodBody >= 1 then List.tl prodBody else prodBody)}]
                      else Set.make []
                    )
                    else Set.make [] 
                  ) rhsVarBodies 
                  in
                  replaceRules
                )
                else  Set.make [{head = prodHead; body = rhsBody}]
          ) rhsValues in
          results
        in
        let iVal = Hashtbl.find_opt map var in
        match iVal with
        | None -> Set.filter (fun {head=h;body=_} -> h=var) rep.rules
        | Some iVal -> (
          let varRules = bodiesOfHead var rep.rules in
          let rhsValues = Set.map (
            fun b -> 
              if List.length b >= 1 
              then (Hashtbl.find_opt map (List.hd b), b)
              else (None, b)
          ) varRules
          in
          Set.flatMap (fun b ->
            let r = perVarProdIndirectRemoval var iVal b rhsValues rep in
            r
          ) varRules)
      in
      let newProds = Set.flatMap (fun v -> perVarIndirectRemoval map v rep) rep.variables in
      let newGrammar = new ContextFreeGrammar.model (Arg.Representation {
         alphabet = rep.alphabet;
         variables = rep.variables;
         initial = rep.initial;
         rules = newProds
       } ) in
      let newGrammar = removeDirectLeftRecursion newGrammar#representation in
      removeIndirectLeftRecursion map xs newGrammar#representation


  let removeLeftRecursion (rep:t) =
    let map = Hashtbl.create (Set.size rep.variables) in
    let leftCornerTest = List.map (fun v -> (v, (Set.toList (leftCorner v rep))) ) (Set.toList rep.variables) in
    let sortedLeftCornerTest = sortLeftCorner leftCornerTest in
    addToMap map (List.map (fun (v,_) -> v) sortedLeftCornerTest);
    let sortedVars = List.map (fun (s,_) -> s) sortedLeftCornerTest in
    let result = removeIndirectLeftRecursion map sortedVars rep in
      {tType = leftRecursionRemovalTransform; grammar = result}
      
  (*left factoring*)
  
  let rec lcp l1 l2 =
    match l1 with
    | [] -> []
    | x1::xs1 -> match l2 with
                | [] -> []
                | x2::xs2 -> if x1=x2 then [x1]@(lcp xs1 xs2) else []
  
  let perVarLCP v rs =
    let rules = Set.filter (fun r -> r.head = v) rs in
    let combos = List.flatten (pairs (Set.toList rules)) in
    let lcpList = List.map ( fun (r1,r2) -> lcp r1.body r2.body) combos in
    let lcpList = List.filter (fun l -> l <> []) lcpList in
      Set.toList (Set.make lcpList) (*Remove repeats*)
  
  let rec sameRuleFactoring nV p rb =
    match p with
    | [] -> [nV]
    | x::xs -> match rb with
              | [] -> []
              | x2::xs2 -> [x2]@sameRuleFactoring nV xs xs2
      
  let rec newRuleFactoring rb p =
    match rb with
    | [] -> []
    | x::xs -> match p with
              | [] -> [x]@newRuleFactoring xs p
              | x2::xs2 -> []@newRuleFactoring xs xs2
      
  let rec ruleHasPrefix r p rb =
    match p with
    | [] ->true
    | x::xs -> match rb with
              |[] -> false
              |x2::xs2 -> if x = x2 then ruleHasPrefix r xs xs2 else false
     
  let rec getSmallestLCP l currSmallest =
    match l with
    | [] -> currSmallest
    | x::xs -> if (x <> [] && List.length x < List.length currSmallest)
               then getSmallestLCP xs x
               else getSmallestLCP xs currSmallest
      
  let rec getBiggestList ll currBiggest =
    match ll with
    | [] -> currBiggest
    | x::xs -> let length = List.length x in
                if length > currBiggest
                then getBiggestList xs length
                else getBiggestList xs currBiggest
      
  let rec createLargeList size =
    match size with
    | 0 -> []
    | _ -> [symb "a"] @ createLargeList (size-1)
      
  let rec perVarFactoring pair allVars (rep:t) = (* pair = ('A', ['a']) *)
    if pair = Set.empty then Set.empty
    else let (x,xs) = Set.cut pair in
      let var = fst x in
      let prefix = snd x in
(*     Printf.printf "prefix = "; print_list prefix; Printf.printf "\n";*)
      let varRules = Set.filter (fun r -> r.head = var) rep.rules in
      let prefixedRules = Set.filter (fun r -> ruleHasPrefix r prefix r.body) varRules in
(*     Printf.printf "prefixedRules = "; Util.println (CFGSyntax.toStringList prefixedRules);*)
      let nonPrefixedRules = Set.filter (fun r -> not (ruleHasPrefix r prefix r.body)) varRules in
(*     Printf.printf "nonPrefixedRules = "; Util.println (CFGSyntax.toStringList nonPrefixedRules);*)
      let newVar = getNewVar allVars in
(*     Printf.printf "newVar = %c\n" newVar;*)
      let newSameHeadRulesSet = Set.map (fun r -> { head = var; body = sameRuleFactoring newVar prefix r.body } ) prefixedRules in
      let newHeadRulesSet = Set.map (fun r -> { head = newVar; body = newRuleFactoring r.body prefix } ) prefixedRules in
      let rules = Set.union nonPrefixedRules (Set.union newSameHeadRulesSet newHeadRulesSet) in
(*     print_rules (Set.toList rules);*)
        Set.union rules (perVarFactoring xs (Set.cons newVar allVars) rep)
  
  let getPerVarLCPResult (rep:t) = 
    let perVarLCPResult = Set.map (fun v -> (v, perVarLCP v rep.rules)) rep.variables in
    let perVarLCPResult = Set.filter (fun (_,l) -> l <> []) perVarLCPResult in
      Set.map ( fun (v,l) -> (v, getSmallestLCP l (createLargeList ((getBiggestList l 0)+1))) ) perVarLCPResult

  let isLeftFactoring (rep:t) =
    Set.map (fun (v,l) -> v) (getPerVarLCPResult rep) <> Set.empty

  let rec leftFactoring (rep:t) =
    let perVarLCPResult = getPerVarLCPResult rep in
(*    Printf.printf "perVarLCPResult = "; Set.iter (fun (v,l) -> Printf.printf "%c, " v; print_list l) perVarLCPResult; Printf.printf "\n";*)
    let variablesToFactorize = Set.map (fun (v,l) -> v) perVarLCPResult in
(*    Printf.printf "Variables to factorize = "; print_list (Set.toList variablesToFactorize); Printf.printf "\n";*)
    let unchangedVariables = Set.diff rep.variables variablesToFactorize in
(*    Printf.printf "Unchanged variables = "; print_list (Set.toList unchangedVariables); Printf.printf "\n";*)
    let unchangedRules = Set.filter (fun {head = h; body = _} -> Set.belongs h unchangedVariables) rep.rules in
    let newRules = perVarFactoring perVarLCPResult rep.variables rep in
    let newVars = Set.map (fun ({head=v;body=_}) -> v ) newRules in
    let newGrammar = new ContextFreeGrammar.model (Arg.Representation {
	      alphabet = rep.alphabet;
	      variables = Set.union rep.variables newVars;
	      initial = rep.initial;
	      rules = Set.union newRules unchangedRules
	    } ) in
    if isLeftFactoring newGrammar#representation 
    then leftFactoring newGrammar#representation 
    else {tType = leftFactoringTransform; grammar = newGrammar}

  let hasEmptyProductions (rep:t) =
    let nullableVars = Set.filter (fun v -> doWordGenerateEmpty [v] rep) rep.variables in
    Set.size nullableVars <> 0

  let removeEmptyProductions2 (rep:t) = 
    let rec combi vars body =
      match body with
      | [] -> Set.make [[]]
      | x::xs -> let res = combi vars xs in
                  (*Printf.printf "Current body symbol is %c\n" x;
                  Printf.printf "res = \n";
                  Set.iter (fun l -> Printf.printf "\t%s\n" (word2str l)) res;*)
                  Set.flatMap (fun v ->
                                (*(if x = v
                                then (
                                  Printf.printf "\tx = v (%c = %c)\n" x v;
                                  Set.iter (fun p -> Printf.printf "\t\t{%s}\n" (word2str p)) (Set.union res (Set.map (fun l -> v::l) res))
                                )
                                else (
                                  Printf.printf "\tx =/= v (%c =/= %c)\n" x v;
                                  Set.iter (fun p -> Printf.printf "\t\t{%s}\n" (word2str p)) (Set.map (fun l -> x::l) res)
                                ));*)
                                if x = v
                                then Set.union res (Set.map (fun l -> v::l) res)
                                else Set.map (fun l -> x::l) res
                  ) vars
    in
    let changeProds vars prod = 
      let {head=h; body=b} = prod in
      if List.length b = 0 then Set.empty
      else (
        let prodBodiesSet = Set.filter (fun p -> List.length p <> 0) (combi vars b) in
        Set.map (fun b -> {head = h; body = b} ) prodBodiesSet
      )
    in
    let nullableVars = Set.filter (fun v -> doWordGenerateEmpty [v] rep) rep.variables in
    if Set.size nullableVars = 0 
    then (
      new ContextFreeGrammar.model (Arg.Representation {
	        alphabet = rep.alphabet;
	        variables = rep.variables;
	        initial = rep.initial;
	        rules = rep.rules
	      })
    )
    else (
      let toChangeProds = Set.filter (fun {head=h;body=b} -> 
                                        Set.exists (
                                          fun v -> List.length b >= 1 && List.mem v b
                                        ) nullableVars
                           ) rep.rules 
      in
      let unchangedProds = Set.filter (
                            fun p -> List.length p.body >= 1
                           ) (Set.diff rep.rules toChangeProds) in
      let newProds = Set.flatMap (changeProds nullableVars) toChangeProds in
(*      Set.iter (fun p -> Printf.printf "{%c;%s}\n" p.head (word2str p.body) ) newProds;*)
(*      if Set.belongs rep.initial nullableVars
      then (
        let newInitial = getNewVar rep.variables in
        let newInitialProds = Set.make [ { head = newInitial; body = []}; { head = newInitial; body = [rep.initial]} ] in
        let newProds = Set.union newInitialProds newProds in
        new ContextFreeGrammar.model (Arg.Representation {
	        alphabet = rep.alphabet;
	        variables = Set.cons newInitial rep.variables;
	        initial = newInitial;
	        rules = Set.union newProds unchangedProds
	      } )
      ) else ( *)
        new ContextFreeGrammar.model (Arg.Representation {
	        alphabet = rep.alphabet;
	        variables = rep.variables;
	        initial = rep.initial;
	        rules = Set.union newProds unchangedProds
	      } (* ) *)
      )
    )
    
  
  let removeEmptyProductions (rep:t) =
    { tType = epsilonRemovalTransform; grammar = removeEmptyProductions2 rep }
  

  let isUnitProd body (rep:t) =
    let rec isUnitProd2 cS cB p =
      match cB with
      | [] -> false
      | x::xs -> if doWordGenerateEmpty cB rep && doWordGenerateEmpty p rep
                  then true
                  else isUnitProd2 x xs (p@[cS])
    in
    let isUnitProdAux r (rep:t) =
      match r with
      | [] -> false
      | x::xs -> isUnitProd2 x xs []
    in
    if (List.length body = 1 && Set.belongs (List.hd body) rep.variables) 
    then true 
    else (
      if List.length body > 1 && List.for_all ( fun c -> Set.belongs c rep.variables ) body
        then isUnitProdAux body rep 
        else false
      )

  let hasUnitProductions (rep:t) =
    Set.size (Set.filter (fun {head = _; body = b} -> isUnitProd b rep ) rep.rules) <> 0


  let rec findUnitPair2 cS cB p (rep:t) =
    match cB with
    | [] -> []
    | x::xs -> if doWordGenerateEmpty cB rep && doWordGenerateEmpty p rep
                then [cS]
                else findUnitPair2 x xs (p@[cS]) rep

  let findUnitPairAux r (rep:t) =
    match r with
    | [] -> []
    | x::xs -> findUnitPair2 x xs [] rep
             
  let rec findUnitPairX origVar var seen (rep:t) =
    if Set.belongs var seen then [] else (
      let rules = bodiesOfHead var rep.rules in
      let results = List.flatten (
                      List.map (fun r -> 
                          if List.length r = 1 && Set.belongs (List.hd r) rep.variables
                          then (
                            if Set.belongs (List.hd r) seen
                            then []@findUnitPairX origVar (List.hd r) (Set.cons var seen) rep
                            else r@findUnitPairX origVar (List.hd r) (Set.cons var seen) rep
                          )
                          else  findUnitPairAux r rep 
                      ) (Set.toList rules)
                    ) 
      in
      results
    )
    
  let findUnitPair var (rep:t) =
    let results = List.map (fun r -> (var, r)) (findUnitPairX var var Set.empty rep) in
    [(var, var)] @ results
(*    (var,(findUnitPairX var Set.empty rep))*)

  (*Used to sort unit pair lists by biggest length to lowest length*)
  let compareUnitPairList l1 l2 =
    if List.length l1 > List.length l2 then -1
    else (if List.length l1 < List.length l2 then 1
    else 0)  
   
  let getNonUnitProductions var (rep:t) = 
    let prods = bodiesOfHead var rep.rules in
(*    Printf.printf "var = %c\n" var;*)
(*    Set.iter (fun p -> Printf.printf "\tIs %c -> %s unit? %b\n" var (word2str p) (isUnitProd p rep)) prods;*)
(*    Printf.printf "\n";*)
    Set.filter (fun p -> not (isUnitProd p rep)) prods

  let removeUnitProductions (rep:t) = 
    let perVarPair pair (rep:t) =
      let (h,b) = pair in
      let nUnitProds = getNonUnitProductions b rep in
(*      Set.iter (fun p -> Printf.printf "%c -> %s\n" h (word2str p)) nUnitProds;*)
      Set.toList (Set.map (fun p -> {head = h; body = p}) nUnitProds)
    in
    let perVar pairs (rep:t) =
      List.flatten (List.map (fun p -> perVarPair p rep) pairs)
    in
    let unitPairs = List.map (fun v -> findUnitPair v rep) (Set.toList rep.variables) in
    (*let unitPairs = List.sort compareUnitPairList unitPairs in*)
    let newProds = List.flatten (
                    List.map (fun l ->
                      perVar l rep
                     ) unitPairs 
                   ) in
    let result = new ContextFreeGrammar.model (Arg.Representation {
	      alphabet = rep.alphabet;
	      variables = rep.variables;
	      initial = rep.initial;
	      rules = Set.make newProds
	    } )
	  in
	    {tType = unitRemovalTransform; grammar = result}

    
    
  let generateRecursiveDescendentParser lang (rep:t) =
    match String.lowercase_ascii lang with
      | "c" -> let parser = new RDParserC.parser in parser#build rep
      | "ocaml" -> let parser = new RDParserOCaml.parser in parser#build rep
      | "java" -> let parser = new RDParserJava.parser in parser#build rep
      | _ -> "Language " ^ lang ^ " is not supported.\n"


  let transformToLL1 (rep:t) =
    let transform1 = {tType = epsilonRemovalTransform; grammar = (removeEmptyProductions rep).grammar} in
    let transform2 = {tType = unitRemovalTransform; grammar = (removeUnitProductions transform1.grammar#representation).grammar} in
    let cleanResult = clean transform2.grammar#representation in
    let transform3 = {tType = cleanProductiveTransform; grammar = (List.nth cleanResult 0).grammar} in
    let transform4 = {tType = cleanAccessibleTransform; grammar = (List.nth cleanResult 1).grammar} in
    let transform5 = {tType = leftRecursionRemovalTransform; grammar = (removeLeftRecursion transform4.grammar#representation).grammar} in
    let transform6 = {tType = leftFactoringTransform; grammar = (leftFactoring transform5.grammar#representation).grammar} in
    [transform1; transform2; transform3; transform4; transform5; transform6]
  

  class model (arg: (t,tx) Arg.alternatives) =
    object(self) inherit ContextFreeGrammar.model arg as super
    
    val mutable simplified = false
    
    method isSimplified = simplified
    method rdparserOpts = [ "OCaml"; "C"; "Java"; "Rust" ]
    method toggleSimplified = Printf.printf "simplified is %b toggling to %b\n" simplified (not simplified);
                              simplified <- not simplified
    
    method follow testSymbol = follow testSymbol simplified self#representation
    method lookahead rule = lookahead rule simplified self#representation
    method isLL1 = isLL1 simplified self#representation
    method isLeftRecursive = isLeftRecursive self#representation
    method createParsingTable = createParsingTable simplified self#representation
    method hasParsingTableConflict = hasParsingTableConflict simplified self#representation
    method acceptZ w = acceptZ w simplified self#representation
    method productiveSymbols = productiveSymbols self#representation
    method accessibleSymbols = accessibleSymbols self#representation
    method productiveRewrite = productiveGrammarRewrite self#representation
    method accessibleRewrite = accessibleGrammarRewrite self#representation
    method clean = clean self#representation
    method isFullyProductive = isCFGFullyProductive self#representation
    method isFullyAccessible = isCFGFullyAccessible self#representation
    method isClean = isClean self#representation
    method removeLeftRecursion = removeLeftRecursion self#representation
    method removeDirectLeftRecursion = removeDirectLeftRecursion self#representation
    method leftFactoring = leftFactoring self#representation
    method isLeftFactoring = isLeftFactoring self#representation
    method leftCorner s = leftCorner s self#representation
    method hasEmptyProductions = hasEmptyProductions self#representation
    method removeEmptyProductions = removeEmptyProductions self#representation
    method hasUnitProductions = hasUnitProductions self#representation
    method removeUnitProductions = removeUnitProductions self#representation
    method generateRecursiveDescendentParser pLang = generateRecursiveDescendentParser pLang self#representation
    method transformToLL1 = transformToLL1 self#representation
  end
end

module LL1GrammarTests: sig end =
struct
	let active = false

  let example1 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 1",
	  name : "G1",
	  alphabet : ["a", "b", "c", "d", "e"],
	  variables : ["S", "A", "B", "C", "D", "E"],
	  initial : "S",
    rules : ["S -> ABCDE", "A -> a | ", "B -> b | ", "C -> c", "D -> d | ", "E -> e | "]
  } |}

  let example2 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 2",
	  name : "G2",
	  alphabet : ["a", "b", "c", "d"],
	  variables : ["S", "B", "C"],
	  initial : "S",
    rules : ["S -> Bb | Cd", "B -> aB | ", "C -> cC | "]
  } |}
  
  let example3 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 3",
	  name : "G3",
	  alphabet : ["+", "*", "(", ")", "i"],
	  variables : ["E", "D", "T", "U", "F"],
	  initial : "E",
    rules : ["E -> TD", "D -> +TD | ", "T -> FU", "U -> *FU | ", "F -> i | (E)"]
  } |}
  
  let example4 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 4",
	  name : "G4",
	  alphabet : ["a", "b", "c", "f", "g", "h"],
	  variables : ["S", "B", "C", "D", "E", "F"],
	  initial : "S",
    rules : ["S -> aBDh", "B -> cC", "C -> bC | ", "D -> EF", "E -> g | ", "F -> f | "]
  } |}

  let example5 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 5",
	  name : "G5",
	  alphabet : ["n", "+", "*"],
	  variables : ["E", "A", "B"],
	  initial : "E",
    rules : ["E -> nA", "A -> EB | ", "B -> +A | *A"]
  } |}
  
  let example6 = {| {
	  kind : "context free grammar", 
	  description : "Exemple 6",
	  name : "G6",
	  alphabet : ["a", "b"],
	  variables : ["N", "A", "B", "C"],
	  initial : "N",
    rules : ["N -> AB | BA", "A -> a | CAC", "B -> b | CBC", "C -> a | b"]
  } |}
  
  let cfg_dissertation = {| {
	  kind : "context free grammar", 
	  description : "dissertation example",
	  name : "G2",
	  alphabet : ["a", "b", "c"],
	  variables : ["S", "A", "B", "C", "D", "E"],
	  initial : "S",
    rules : ["S -> ABC", "A -> aD", "B -> CE", "C -> c", "D -> AB | ", "E -> bE | "]
  } |}

  let non_deterministic_grammar = {| {
	  kind : "context free grammar", 
	  description : "Non deterministic grammar",
	  name : "N",
	  alphabet : ["a", "b", "d", "g", "h"],
	  variables : ["S", "A", "B", "C"],
	  initial : "S",
    rules : ["S -> ACB | CbB | Ba", "A -> da | BC", "B -> g | ", "C -> h | "]
  } |}

  let accessible_symbols1 = {| {
	  kind : "context free grammar", 
	  description : "Accessible symbols example",
	  name : "AS1",
	  alphabet : ["a", "b"],
	  variables : ["A", "B", "C", "D", "E"],
	  initial : "A",
    rules : ["A -> aBb | bBa", "B -> Cb | bC", "C -> a | aC", "D -> E | Db", "E -> aE | Da"]
  } |}
  
  let accessible_symbols2 = {| {
	  kind : "context free grammar", 
	  description : "Accessible symbols example",
	  name : "AS2",
	  alphabet : ["a", "b"],
	  variables : ["S", "B"],
	  initial : "S",
    rules : ["S -> a", "B -> b"]
  } |}
  
  let productive_symbols1 = {| {
	  kind : "context free grammar", 
	  description : "Productive symbols example",
	  name : "PS1",
	  alphabet : ["a", "b"],
	  variables : ["A", "B", "C", "D", "E"],
	  initial : "A",
    rules : ["A -> aBb | bBa", "B -> CD | aC | Ab", "C -> a | aC", "D -> E | DA", "E -> aE | Da"]
  } |}

  let productive_symbols2 = {| {
	  kind : "context free grammar", 
	  description : "Productive symbols example",
	  name : "PS2",
	  alphabet : ["a", "b"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> a | A", "A -> AB", "B -> b"]
  } |}
  
  let clean_grammar1 = {| {
	  kind : "context free grammar", 
	  description : "Clean example from https://www.cs.scranton.edu/~mccloske/courses/cmps260/cfg_remove_useless.html",
	  name : "Clean1",
	  alphabet : ["a", "b", "c", "d"],
	  variables : ["S", "A", "B", "C", "D"],
	  initial : "S",
    rules : ["S -> aSa | bB | bAA", "A -> a | SbA | aB", "B -> AB | CaB", "C -> cC | Sa | bD", "D -> dD | "]
  } |}

  let direct_left_recursion_grammar1 = {| {
	  kind : "context free grammar", 
	  description : "Direct Left-recursion example 1",
	  name : "DR1",
	  alphabet : ["a", "b"],
	  variables : ["A"],
	  initial : "A",
    rules : ["A -> Aa | b"]
  }  |}
  
  let direct_left_recursion_grammar2 = {| {
	  kind : "context free grammar", 
	  description : "Direct Left-recursion example 2",
	  name : "DR2",
	  alphabet : ["a", "b"],
	  variables : ["B"],
	  initial : "B",
    rules : ["B -> a | Bb"]
  } |}
  
  let indirect_left_recursion_grammar1 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example 1",
	  name : "IR1",
	  alphabet : ["a"],
	  variables : ["S", "A"],
	  initial : "S",
    rules : ["S -> A | a", "A -> S"]
  }  |}

  let indirect_left_recursion_grammar2 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example 2",
	  name : "IR2",
	  alphabet : ["a"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> A | B", "A -> a", "B -> S"]
  }  |}
  
  let indirect_left_recursion_grammar3 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example 2",
	  name : "IR3",
	  alphabet : ["a", "b", "c"],
	  variables : ["S", "A", "B", "C"],
	  initial : "S",
    rules : ["S -> ABCS", "A -> a | ", "B -> b | ", "C -> c | "]
  }  |} 
  
  let indirect_left_recursion_grammar4 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example",
	  name : "IR4",
	  alphabet : ["a", "b", "d", "e", "f", "g"],
	  variables : ["A", "B", "C", "D"],
	  initial : "A",
    rules : ["A -> Ba | b", "B -> Cd | e", "C-> Df | g", "D -> Df | Aa | Cg"]
  }  |}

  let indirect_left_recursion_grammar5 = {| {
	  kind : "context free grammar", 
	  description : "Indirect Left-recursion example 2",
	  name : "IR3",
	  alphabet : ["a"],
	  variables : ["S", "A"],
	  initial : "S",
    rules : ["S -> AS", "A -> a | "]
  }  |}

  let left_factoring_example = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF1",
	  alphabet : ["a", "b", "e", "i", "t"],
	  variables : ["S", "E"],
	  initial : "S",
    rules : ["S -> iEtS | iEtSeS | a", "E -> b"]
  } |}

  let left_factoring_example2 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF2",
	  alphabet : ["a", "c"],
	  variables : ["A", "B"],
	  initial : "A",
    rules : ["A -> aAB | aBc | aAc", "B ->"]
  } |}

  let left_factoring_example3 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF3",
	  alphabet : ["a", "b"],
	  variables : ["S"],
	  initial : "S",
    rules : ["S -> bSSaaS | bSSaSb | bSb | a"]
  } |}

  let left_factoring_example4 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF4",
	  alphabet : ["a", "b"],
	  variables : ["S"],
	  initial : "S",
    rules : ["S -> aSSbS | aSaSb | abb | b"]
  } |}
  
  let left_factoring_example5 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF5",
	  alphabet : ["a", "b", "c", "d"],
	  variables : ["S"],
	  initial : "S",
    rules : ["S -> a | ab | abc | abcd"]
  } |}
  
  let left_factoring_example6 = {| {
	  kind : "context free grammar", 
	  description : "Left Factoring example",
	  name : "LF6",
	  alphabet : ["a", "b", "c", "d"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> aAd | aB", "A -> a | ab", "B -> ccd | ddc"]
  } |}
  
  let unit_removal_example1 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example",
	  name : "UR1",
	  alphabet : ["a", "b"],
	  variables : ["E", "T", "F", "I"],
	  initial : "E",
    rules : ["E -> T", "T -> F", "F -> I", "I -> a | b | Ia | Ib"]
  } |}

  let unit_removal_example2 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example 2",
	  name : "UR2",
	  alphabet : ["a", "b", "c"],
	  variables : ["A", "B", "C"],
	  initial : "A",
    rules : ["A -> B | a", "B -> C | b", "C -> A | c"]
  } |}
  
  let unit_removal_example3 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example 3",
	  name : "UR3",
	  alphabet : ["a", "b", "c"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> Aa | B | c", "A -> a | bc | B", "B -> A | bb"]
  } |}
  
  let unit_removal_example4 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example 4",
	  name : "UR4",
	  alphabet : ["a", "b", "d"],
	  variables : ["S", "A", "B", "C", "D", "E"],
	  initial : "S",
    rules : ["S -> AC", "A -> a", "B -> D", "C -> B | d", "D -> E", "E -> b"]
  } |}
  
  let unit_removal_example5 = {| {
	  kind : "context free grammar", 
	  description : "unit removal example 5",
	  name : "UR5",
	  alphabet : ["a", "b", "0", "1", "(", ")", "+", "*"],
	  variables : ["I", "F", "T", "E"],
	  initial : "E",
    rules : ["E -> T | E+T", "T -> F | T*F", "F -> I | (E)", "I -> a | b | Ia | Ib | I0 | I1"]
  } |}

  let epsilon_removal_example1 = {| {
	  kind : "context free grammar", 
	  description : "epsilon removal example",
	  name : "ER1",
	  alphabet : ["a", "b", "d"],
	  variables : ["S", "A", "B", "C", "D"],
	  initial : "S",
    rules : ["S -> ABaC", "A -> BC", "B -> b | ", "C -> D | ", "D -> d"]
  } |}
  
  let epsilon_removal_example2 = {| {
	  kind : "context free grammar", 
	  description : "epsilon removal example 2",
	  name : "ER2",
	  alphabet : ["a", "b"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> AB", "A -> AaA | ", "B -> BbB | "]
  } |}
  
  let epsilon_removal_example3 = {| {
	  kind : "context free grammar", 
	  description : "epsilon removal example 3",
	  name : "ER3",
	  alphabet : ["a", "b"],
	  variables : ["S", "A", "B"],
	  initial : "S",
    rules : ["S -> AB", "A -> aAA | ", "B -> bBB | "]
  } |}

  let firstPairConversion_old l = Set.make (List.map (fun (a,b) -> (a, Set.make b)) l)
  let followPairConversion_old l = Set.make (List.map (fun (a,b) -> (a, Set.make b)) l)
  let lookaheadPairConversion_old l = Set.make (List.map (fun (a,b) -> CFGSyntax.parseLine a, Set.make b) l)

  let firstPairConversion l = Set.make (List.map (fun (a,b) -> (symb a, Set.make (List.map char2symb b))) l)
  let followPairConversion l = Set.make (List.map (fun (a,b) -> (char2symb a, Set.make (List.map char2symb b))) l)
  let lookaheadPairConversion l = Set.make (List.map (fun (a,b) -> (Set.nth (CFGSyntax.parseLine a) 0), Set.make (List.map char2symb b)) l)

  let printRepresentation (rep: t) =
    Printf.printf "Alphabet = "; Util.printAlphabet rep.alphabet;
    Printf.printf "Variables = "; Util.printAlphabet rep.variables;
    Printf.printf "Initial = %s\n" (symb2str rep.initial);
    Printf.printf "Rules {\n"; Set.iter (fun {head=h; body=b} -> Printf.printf "\t%s -> %s\n" (symb2str h) (word2str b)) rep.rules;
    Printf.printf "}\n\n"

  let rec testFunction2 f l c =
    if l = Set.empty then ()
    else let ((t,r),xs) = Set.cut l in
      if (f t = r) then () else Printf.printf "\t\tTest %i fails!\n" c;
      testFunction2 f xs (c+1)


	let colorRed = "\027[31m"
	let colorGreen = "\027[32m"
	let colorOff = "\027[0m"

(*	let colorRed = ""*)
(*	let colorGreen = ""*)
(*	let colorOff = ""*)

  let failPrint str =
    Printf.printf "%s" (colorRed ^ str ^ colorOff)
    
  let okPrint str =
    Printf.printf "%s" (colorGreen ^ str ^ colorOff)

  let printResult r =
    if r
    then okPrint "O"
    else failPrint "X"
  
  let printFirstTest t =
    Set.iter (fun (v,s) -> 
      Printf.printf "(%s, [" v;
      Set.iter (fun v -> Printf.printf "%c " v) s;
      Printf.printf "%s" "]) "
    ) t
    
  let compareTheseSets s1 s2 =
    Set.for_all (fun (h1,r1) ->
      Set.exists (fun (h2,r2) -> h1 = h2 && Set.equals r1 r2) s2
    ) s1 && Set.size s1 = Set.size s2

  let testFirst g r =
    let allResults = Set.map (fun v -> (v, g#first [v])) (g#representation : t).variables in
(*    Printf.printf "\n\tComparing:";*)
(*    Set.iter (fun (v,b) -> Printf.printf "\n\t\t%s->\t" (symb2str v); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) b) r;*)
(*    Printf.printf "\n\twith:";*)
(*    Set.iter (fun (v,b) -> Printf.printf "\n\t\t%s->\t" (symb2str v); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) b) allResults;*)
(*    Printf.printf "\n";*)
(*    r = allResults*)
    compareTheseSets r allResults

  let testFollow g r =
    let allResults = Set.map (fun v -> (v, g#follow v)) (g#representation : t).variables in
(*    Printf.printf "\n\tComparing:";*)
(*    Set.iter (fun (v,b) -> Printf.printf "\n\t\t%s->\t" (symb2str v); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) b) r;*)
(*    Printf.printf "\n\twith:";*)
(*    Set.iter (fun (v,b) -> Printf.printf "\n\t\t%s->\t" (symb2str v); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) b) allResults;*)
(*    Printf.printf "\n";*)
(*    r = allResults*)
    compareTheseSets r allResults
    
  let testLookahead g r =
    let rep = (g#representation : t) in
    let allResults = 
      Set.flatMap (fun v -> 
        let rules = Set.filter (fun {head=h; _} -> h = v ) rep.rules in
        Set.map (fun r -> 
          (r, g#lookahead r)
        ) rules
      ) rep.variables 
    in
(*    Printf.printf "\n\tComparing:";*)
(*    Set.iter (fun ({head=h;body=b},r) -> Printf.printf "\n\t\t%s->%s\t" (symb2str h) (word2str b); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) r) r;*)
(*    Printf.printf "\n\twith:";*)
(*    Set.iter (fun ({head=h;body=b},r) -> Printf.printf "\n\t\t%s->%s\t" (symb2str h) (word2str b); Set.iter (fun s ->  Printf.printf " %s " (symb2str s)) r) allResults;*)
(*    Printf.printf "\n";*)
(*    r = allResults*)
    compareTheseSets r allResults

  let testFunction1 f r =
    f = r



(*	let test0 () =*)
(*		let m = new LL1Grammar.model (Arg.Text cfg_simple) in*)
(*		let j = m#toJSon in*)
(*			JSon.show j*)

	let dollar = '$'
	let epsilon = '~'

  let testExample1 () =
    Printf.printf "Example1 test: [";
    let first = [ ("S", ['a'; 'b'; 'c']); ("A", ['a'; '~']); ("B", ['b'; '~']); ("C", ['c']); ("D", ['d'; '~']); ("E", ['e'; '~']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', ['$']); ('A', ['b'; 'c']); ('B', ['c']); ('C', ['d'; 'e'; '$']); ('D', ['e'; '$']); ('E', ['$']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ABCDE", ['a'; 'b'; 'c']); 
                      ("A->a", ['a']); ("A->", ['b'; 'c']);
                      ("B->b", ['b']); ("B->", ['c']); 
                      ("C->c", ['c']); 
                      ("D->d", ['d']); ("D->", ['e'; dollar]); 
                      ("E->e", ['e']); ("E->", [dollar]) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample2 () =
    Printf.printf "Example2 test: [";
    let first = [ ("S", ['a'; 'b'; 'c'; 'd']); ("B", ['a'; epsilon]); ("C", ['c'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('B', ['b']); ('C', ['d']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->Bb", ['a'; 'b']); ("S->Cd", ['c'; 'd']);
                      ("B->aB", ['a']); ("B->", ['b']);
                      ("C->cC", ['c']); ("C->", ['d']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample3 () =
    Printf.printf "Example3 test: [";
    let first = [ ("U", ['*'; epsilon]); ("D", ['+'; epsilon]); ("E", ['('; 'i']); ("F", ['('; 'i']); ("T", ['('; 'i']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('U', [')'; '+'; dollar]); ('D', [')'; dollar]); ('E', [')'; dollar]); ('F', [')'; '*'; '+'; dollar]); ('T', [')'; '+'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("U->*FU", ['*']); ("U->", [')'; '+'; dollar]); 
                      ("D->+TD", ['+']); ("D->", [')'; dollar]); 
                      ("E->TD", ['('; 'i']); 
                      ("F->i", ['i']); ("F->(E)", ['(']);
                      ("T->FU", ['('; 'i']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example3) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample4 () =
    Printf.printf "Example4 test: [";
    let first = [ ("S", ['a']); ("B", ['c']); ("C", ['b'; epsilon]); ("D", ['f'; 'g'; epsilon]); ("E", ['g'; epsilon]); ("F", ['f'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('B', ['f'; 'g'; 'h']); ('C', ['f'; 'g'; 'h']); ('D', ['h']); ('E', ['f'; 'h']); ('F', ['h']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->aBDh", ['a']);
                      ("B->cC", ['c']);
                      ("C->bC", ['b']); ("C->", ['f'; 'g'; 'h']); 
                      ("D->EF", ['f'; 'g'; 'h']);
                      ("E->g", ['g']); ("E->", ['f'; 'h']);
                      ("F->f", ['f']); ("F->", ['h']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example4) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample5 () =
    Printf.printf "Example5 test: [";
    let first = [ ("E", ['n']); ("A", ['n'; epsilon]); ("B", ['*'; '+']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('E', ['*'; '+'; dollar]); ('A', ['*'; '+'; dollar]); ('B', ['*'; '+'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("E->nA", ['n']);
                      ("A->EB", ['n']); ("A->", ['*'; '+'; dollar]);
                      ("B->+A", ['+']); ("B->*A", ['*']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example5) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testExample6 () =
    Printf.printf "Example6 test: [";
    let first = [ ("N", ['a'; 'b']); ("A", ['a'; 'b']); ("B", ['a'; 'b']); ("C", ['a'; 'b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('N', [dollar]); ('A', ['a'; 'b'; dollar]); ('B', ['a'; 'b'; dollar]); ('C', ['a'; 'b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("N->AB", ['a'; 'b']); ("N->BA", ['a'; 'b']);
                      ("A->a", ['a']); ("A->CAC", ['a'; 'b']);
                      ("B->b", ['b']); ("B->CBC", ['a'; 'b']); 
                      ("C->a", ['a']); ("C->b", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text example6) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"

  let testDissertation () =
    Printf.printf "%s" (Printf.sprintf "Dissertation test: [");
    let first = [ ("S", ['a']); ("A", ['a']); ("B", ['c']); ("C", ['c']); ("D", ['a'; epsilon]); ("E", ['b'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['c']); ('B', ['c']); ('C', ['b'; 'c'; dollar]); ('D', ['c']); ('E', ['c']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ABC", ['a']); 
                      ("A->aD", ['a']); 
                      ("B->CE", ['c']);
                      ("C->c", ['c']); 
                      ("D->AB", ['a']); ("D->", ['c']);
                      ("E->bE", ['b']); ("E->", ['c']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text cfg_dissertation) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"
    
  let testNFGrammar () =
    Printf.printf "Non deterministic grammar test: [";
    let first = [ ("S", ['a'; 'b'; 'd'; 'g'; 'h'; epsilon]); ("A", ['d'; 'g'; 'h'; epsilon]); ("B", ['g'; epsilon]); ("C", ['h'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['g'; 'h'; dollar]); ('B', ['a'; 'g'; 'h'; dollar]); ('C', ['b'; 'g'; 'h'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ACB", ['d'; 'g'; 'h'; dollar]); ("S->CbB", ['b'; 'h']); ("S->Ba", ['a'; 'g']);
                      ("A->da", ['d']); ("A->BC", ['g'; 'h'; dollar]); 
                      ("B->g", ['g']); ("B->", ['a'; 'g'; 'h'; dollar]); 
                      ("C->h", ['h']); ("C->", ['b'; 'g'; 'h'; dollar]); ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text non_deterministic_grammar) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isLeftRecursive false);
    Printf.printf "]\n"

  let testAccessible1 () =
    Printf.printf "%s" (Printf.sprintf "Remove inaccessible symbols test 1: [");
    let first = [ ("A", ['a'; 'b']); ("B", ['a'; 'b']); ("C", ['a']); ("D", ['a']); ("E", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', [dollar]); ('B', ['a'; 'b']); ('C', ['a'; 'b']); ('D', ['a'; 'b']); ('E', ['a'; 'b']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->aBb", ['a']); ("A->bBa", ['b']); 
                      ("B->Cb", ['a']); ("B->bC", ['b']);
                      ("C->a", ['a']); ("C->aC", ['a']); 
                      ("D->E", ['a']); ("D->Db", ['a']);
                      ("E->aE", ['a']); ("E->Da", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text accessible_symbols1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isFullyAccessible false);
    let transformed = new LL1Grammar.model (Arg.Representation m#accessibleRewrite#representation) in
(*      printRepresentation m#representation;*)
(*      printRepresentation transformed#representation;*)
      printResult (testFunction1 transformed#isFullyAccessible true);
    Printf.printf "]\n"
    
  let testAccessible2 () =
    Printf.printf "%s" (Printf.sprintf "Remove inaccessible symbols test 2: [");
    let first = [ ("S", ['a']); ("B", ['b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('B', []) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->a", ['a']); 
                      ("B->b", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text accessible_symbols2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive false);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isFullyAccessible false);
      printResult (testFunction1 m#isFullyProductive true);
    let transformed = new LL1Grammar.model (Arg.Representation m#accessibleRewrite#representation) in
      printResult (testFunction1 transformed#isFullyAccessible true);
      printResult (testFunction1 transformed#isFullyProductive true);
    Printf.printf "]\n"

  let testProductive1 () =
    Printf.printf "%s" (Printf.sprintf "Remove unproductive symbols test 1: [");
    let first = [ ("A", ['a'; 'b']); ("B", ['a'; 'b']); ("C", ['a']); ("D", ['a']); ("E", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', ['a'; 'b'; dollar]); ('B', ['a'; 'b']); ('C', ['a'; 'b']); ('D', ['a'; 'b']); ('E', ['a'; 'b']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->aBb", ['a']); ("A->bBa", ['b']); 
                      ("B->CD", ['a']); ("B->aC", ['a']); ("B->Ab", ['a'; 'b']);
                      ("C->a", ['a']); ("C->aC", ['a']); 
                      ("D->E", ['a']); ("D->DA", ['a']);
                      ("E->aE", ['a']); ("E->Da", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text productive_symbols1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring true);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isFullyProductive false);
    let transformed = new LL1Grammar.model (Arg.Representation m#productiveRewrite#representation) in
      printResult (testFunction1 transformed#isFullyProductive true);
    Printf.printf "]\n"

  let testProductive2 () =
    Printf.printf "%s" (Printf.sprintf "Remove unproductive symbols test 2: [");
    let first = [ ("S", ['a']); ("A", []); ("B", ['b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['b'; dollar]); ('B', ['b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->a", ['a']); ("S->A", []); 
                      ("A->AB", []);
                      ("B->b", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text productive_symbols2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 true);
      printResult (testFunction1 m#isFullyAccessible true);
      printResult (testFunction1 m#isFullyProductive false);
    let transformed = new LL1Grammar.model (Arg.Representation m#productiveRewrite#representation) in
      printResult (testFunction1 transformed#isFullyAccessible false);
      printResult (testFunction1 transformed#isFullyProductive true);
    let fullyTransformed =  new LL1Grammar.model (Arg.Representation (List.nth m#clean 1).grammar#representation) in
      printResult (testFunction1 fullyTransformed#isFullyAccessible true);
      printResult (testFunction1 fullyTransformed#isFullyProductive true);
    Printf.printf "]\n"

  let cleanGrammar1 () = 
    Printf.printf "%s" (Printf.sprintf "Clean grammar test 1: [");
    let first = [ ("S", ['a'; 'b']); ("A", ['a'; 'b']); ("B", ['a'; 'b'; 'c']); ("C", ['a'; 'b'; 'c']); ("D", ['d'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', ['a'; 'b'; dollar]); ('A', ['a'; 'b'; 'c'; dollar]); ('B', ['a'; 'b'; 'c'; dollar]); ('C', ['a']); ('D', ['a']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->aSa", ['a']); ("S->bB", ['b']); ("S->bAA", ['b']);
                      ("A->a", ['a']); ("A->SbA", ['a'; 'b']); ("A->aB", ['a']);
                      ("B->AB", ['a'; 'b']); ("B->CaB", ['a'; 'b'; 'c']);
                      ("C->cC", ['c']); ("C->Sa", ['a'; 'b']); ("C->bD", ['b']);
                      ("D->dD", ['d']); ("D->", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text clean_grammar1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive false);
      printResult (testFunction1 m#isLeftFactoring true);
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 m#isFullyAccessible true);
      printResult (testFunction1 m#isFullyProductive false);
    let fullyTransformed =  new LL1Grammar.model (Arg.Representation (List.nth m#clean 1).grammar#representation) in
      printResult (testFunction1 fullyTransformed#isFullyAccessible true);
      printResult (testFunction1 fullyTransformed#isFullyProductive true);
    Printf.printf "]\n"
    
  let testDirectRecursion1 () =
    Printf.printf "Direct recursion test 1: [";
    let first = [ ("A", ['b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', ['a'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->Aa", ['b']); ("A->b", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text direct_left_recursion_grammar1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeLeftRecursion.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
    Printf.printf "]\n"

  let testDirectRecursion2 () =
    Printf.printf "Direct recursion test 2: [";
    let first = [ ("B", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('B', ['b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("B->a", ['a']); ("B->Bb", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text direct_left_recursion_grammar2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeLeftRecursion.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
    Printf.printf "]\n"

  let testIndirectRecursion1 () =
    Printf.printf "Indirect recursion test 1: [";
    let first = [ ("A", ['a']); ("S", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', [dollar]); ('S', [dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->A", ['a']); ("S->a", ['a']); ("A->S", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are unit productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
      printResult (testFunction1 transformed#isClean false);
    (*Grammar can be cleaned yet.*)
    let transformed = new LL1Grammar.model (Arg.Representation (List.nth transformed#clean 1).grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
      printResult (testFunction1 transformed#isClean true);
    Printf.printf "]\n"
    
  let testIndirectRecursion2 () = (*FIXME Trying to clean grammar right away fails*)
    Printf.printf "Indirect recursion test 2: [";
    let first = [ ("S", ['a']); ("A", ['a']); ("B", ['a']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', [dollar]); ('B', [dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->A", ['a']); ("S->B", ['a']);
                      ("A->a", ['a']);
                      ("B->S", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are epsilon productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive true);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are unit productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation transformed#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
    (*Grammar can be cleaned yet.*)
      printResult (testFunction1 transformed#isClean false);
    let transformed = new LL1Grammar.model (Arg.Representation (List.nth transformed#clean 1).grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
      printResult (testFunction1 transformed#isClean true);
    Printf.printf "]\n"

  let testIndirectRecursion3 () =
    Printf.printf "Indirect recursion test 3: [";
    let first = [ ("S", ['a'; 'b'; 'c']); ("A", ['a'; epsilon]); ("B", ['b'; epsilon]); ("C", ['c'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; 'b'; 'c']); ('B', ['a'; 'b'; 'c']); ('C', ['a'; 'b'; 'c']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ABCS", ['a'; 'b'; 'c']);
                      ("A->a", ['a']); ("A->", ['a'; 'b'; 'c']); 
                      ("B->b", ['b']); ("B->", ['a'; 'b'; 'c']); 
                      ("C->c", ['c']); ("C->", ['a'; 'b'; 'c']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar3) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are epsilon productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive true);
      printResult (testFunction1 transformed#isLeftFactoring true);
      printResult (testFunction1 transformed#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are unit productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation transformed#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring true);
      printResult (testFunction1 transformed#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation transformed#leftFactoring.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 false); (*This grammar is not LL(1)*)
    Printf.printf "]\n"
    
  let testIndirectRecursion4 () =
    Printf.printf "Indirect recursion test 4: [";
    let first = [ ("A", ['b'; 'e'; 'g']); ("B", ['b'; 'e'; 'g']); ("C", ['b'; 'e'; 'g']); ("D", ['b'; 'e'; 'g']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', ['a'; dollar]); ('B', ['a']); ('C', ['d'; 'g']); ('D', ['f']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->Ba", ['b'; 'e'; 'g']); ("A->b", ['b']);
                      ("B->Cd", ['b'; 'e'; 'g']); ("B->e", ['e']);
                      ("C->Df", ['b'; 'e'; 'g']); ("C->g", ['g']);
                      ("D->Df", ['b'; 'e'; 'g']); ("D->Aa", ['b'; 'e'; 'g']); ("D->Cg", ['b'; 'e'; 'g']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar4) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeLeftRecursion.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring true);
      printResult (testFunction1 transformed#isLL1 false);
    let transformed = new LL1Grammar.model (Arg.Representation transformed#leftFactoring.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 false);
    Printf.printf "]\n"

  let testIndirectRecursion5 () =
    Printf.printf "Indirect recursion test 5: [";
    let first = [ ("S", ['a']); ("A", ['a'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a']) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->AS", ['a']);
                      ("A->a", ['a']); ("A->", ['a']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text indirect_left_recursion_grammar5) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#isLeftRecursive true);
      printResult (testFunction1 m#isLeftFactoring false);
      printResult (testFunction1 m#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are epsilon productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive true);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 false);
    (*Left recursion cannot be solved with #removeLeftRecursion. There are unit productions that need to be removed first.*)
    let transformed = new LL1Grammar.model (Arg.Representation transformed#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#isLeftRecursive false);
      printResult (testFunction1 transformed#isLeftFactoring false);
      printResult (testFunction1 transformed#isLL1 true);
    Printf.printf "]\n"

  let testLeftFactoring1 () =
    Printf.printf "Left factoring test 1: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 false);
    Printf.printf "]\n"
    
  let testLeftFactoring2 () =
    Printf.printf "Left factoring test 2: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example2) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 false);
    Printf.printf "]\n"
    
  let testLeftFactoring3 () =
    Printf.printf "Left factoring test 3: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example3) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 false);
    Printf.printf "]\n"
    
  let testLeftFactoring4 () =
    Printf.printf "Left factoring test 4: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example4) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 false);
    Printf.printf "]\n"
    
  let testLeftFactoring5 () =
    Printf.printf "Left factoring test 5: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example5) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 true);
    Printf.printf "]\n"
    
  let testLeftFactoring6 () =
    Printf.printf "Left factoring test 6: [";
    let m = new LL1Grammar.model (Arg.Text left_factoring_example6) in
    let transformedM = new LL1Grammar.model (Arg.Representation m#leftFactoring.grammar#representation) in
      printResult (testFunction1 m#isLL1 false);
      printResult (testFunction1 transformedM#isLeftFactoring false);
      printResult (testFunction1 transformedM#isLL1 true);
    Printf.printf "]\n"

  let testUnitRemoval1 () =
    Printf.printf "Unit production removal test 1: [";
    let first = [ ("E", ['a'; 'b']); ("T", ['a'; 'b']); ("F", ['a'; 'b']); ("I", ['a'; 'b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('E', [dollar]); ('T', [dollar]); ('F', [dollar]); ('I', ['a'; 'b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("E->T", ['a'; 'b']);
                      ("T->F", ['a'; 'b']); 
                      ("F->I", ['a'; 'b']);
                      ("I->a", ['a']); ("I->b", ['b']); ("I->Ia", ['a'; 'b']); ("I->Ib", ['a'; 'b'])] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"

  let testUnitRemoval2 () =
    Printf.printf "Unit production removal test 2: [";
    let first = [ ("A", ['a'; 'b'; 'c']); ("B", ['a'; 'b'; 'c']); ("C", ['a'; 'b'; 'c']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('A', [dollar]); ('B', [dollar]); ('C', [dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("A->B", ['a'; 'b'; 'c']); ("A->a", ['a']);
                      ("B->C", ['a'; 'b'; 'c']); ("B->b", ['b']);
                      ("C->A", ['a'; 'b'; 'c']); ("C->c", ['c'])] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"

  let testUnitRemoval3 () =
    Printf.printf "Unit production removal test 3: [";
    let first = [ ("S", ['a'; 'b'; 'c']); ("A", ['a'; 'b']); ("B", ['a'; 'b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; dollar]); ('B', ['a'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->Aa", ['a'; 'b']); ("S->B", ['a'; 'b']); ("S->c", ['c']);
                      ("A->a", ['a']); ("A->bc", ['b']); ("A->B", ['a'; 'b']); 
                      ("B->A", ['a'; 'b';]); ("B->bb", ['b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example3) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"

  let testUnitRemoval4 () =
    Printf.printf "Unit production removal test 4: [";
    let first = [ ("S", ['a']); ("A", ['a']); ("B", ['b']); ("C", ['b'; 'd']); ("D", ['b']); ("E", ['b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['b'; 'd']); ('B', [dollar]); ('C', [dollar]); ('D', [dollar]); ('E', [dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->AC", ['a']);
                      ("A->a", ['a']); 
                      ("B->D", ['b']);
                      ("C->B", ['b']); ("C->d", ['d']);
                      ("D->E", ['b']);
                      ("E->b", ['b'])] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example4) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"
    
  let testUnitRemoval5 () =
    Printf.printf "Unit production removal test 5: [";
    let first = [ ("E", ['a'; 'b'; '(']); ("T", ['a'; 'b'; '(']); ("F", ['a'; 'b'; '(']); ("I", ['a'; 'b']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('E', [')'; '+'; dollar]); ('T', [')'; '*'; '+'; dollar]); ('F', [')'; '*'; '+'; dollar]); ('I', ['0'; 'a'; '1'; 'b'; ')'; '*'; '+'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("E->T", ['a'; 'b'; '(']); ("E->E+T", ['a'; 'b'; '(']);
                      ("T->F", ['a'; 'b'; '(']); ("T->T*F", ['a'; 'b'; '(']); 
                      ("F->I", ['a'; 'b']); ("F->(E)", ['(']);
                      ("I->a", ['a']); ("I->b", ['b']); ("I->Ia", ['a'; 'b']); ("I->Ib", ['a'; 'b']); ("I->I0", ['a'; 'b']); ("I->I1", ['a'; 'b']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text unit_removal_example5) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasUnitProductions true);
    let transformed = new LL1Grammar.model (Arg.Representation m#removeUnitProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasUnitProductions false);
    Printf.printf "]\n"

  let testEmptyRemoval1 () =
    Printf.printf "Empty production removal test 1: [";
    let first = [ ("S", ['a'; 'b'; 'd']); ("A", ['b'; 'd'; epsilon]); ("B", ['b'; epsilon]); ("C", ['d'; epsilon]); ("D", ['d']) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; 'b']); ('B', ['a'; 'b'; 'd']); ('C', ['a'; 'b'; dollar]); ('D', ['a'; 'b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->ABaC", ['a'; 'b'; 'd']);
                      ("A->BC", ['a'; 'b'; 'd']); 
                      ("B->b", ['b']); ("B->", ['a'; 'b'; 'd']);
                      ("C->D", ['d']); ("C->", ['a'; 'b'; dollar]); 
                      ("D->d", ['d']) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text epsilon_removal_example1) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasEmptyProductions true); 
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasEmptyProductions false); 
    Printf.printf "]\n"
    
  let testEmptyRemoval2 () =
    Printf.printf "Empty production removal test 2: [";
    let first = [ ("S", ['a'; 'b'; epsilon]); ("A", ['a'; epsilon]); ("B", ['b'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; 'b'; dollar]); ('B', ['b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->AB", ['a'; 'b'; dollar]);
                      ("A->AaA", ['a']); ("A->", ['a'; 'b'; dollar]); 
                      ("B->BbB", ['b']); ("B->", ['b'; dollar]) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text epsilon_removal_example2) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasEmptyProductions true); 
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasEmptyProductions false); 
    Printf.printf "]\n"
    
  let testEmptyRemoval3 () =
    Printf.printf "Empty production removal test 3: [";
    let first = [ ("S", ['a'; 'b'; epsilon]); ("A", ['a'; epsilon]); ("B", ['b'; epsilon]) ] in
    let first2 = firstPairConversion first in
    let follow = [ ('S', [dollar]); ('A', ['a'; 'b'; dollar]); ('B', ['b'; dollar]) ] in
    let follow2 = followPairConversion follow in
    let lookahead = [ ("S->AB", ['a'; 'b'; dollar]);
                      ("A->aAA", ['a']); ("A->", ['a'; 'b'; dollar]); 
                      ("B->bBB", ['b']); ("B->", ['b'; dollar]) ] in
    let lookahead2 = lookaheadPairConversion lookahead in
    let m = new LL1Grammar.model (Arg.Text epsilon_removal_example3) in
      printResult (testFirst m first2);
      printResult (testFollow m follow2);
      printResult (testLookahead m lookahead2);
      printResult (testFunction1 m#hasEmptyProductions true); 
    let transformed = new LL1Grammar.model (Arg.Representation m#removeEmptyProductions.grammar#representation) in
      printResult (testFunction1 transformed#hasEmptyProductions false);
    Printf.printf "]\n"
    ;;
    
	let runAll =
		if Util.testing active "LL1Grammar" then begin
			testExample1 ();
			testExample2 ();
			testExample3 ();
			testExample4 ();
			testExample5 ();
			testExample6 ();
			testDissertation ();
			testNFGrammar ();
			testAccessible1 ();
			testAccessible2 ();
			testProductive1 ();
			testProductive2 ();
			cleanGrammar1 ();
			testDirectRecursion1 ();
			testDirectRecursion2 ();
			testIndirectRecursion1 ();
			testIndirectRecursion2 ();
			testIndirectRecursion3 ();
			testIndirectRecursion4 ();
			testIndirectRecursion5 ();
			testLeftFactoring1 ();
			testLeftFactoring2 ();
			testLeftFactoring3 ();
			testLeftFactoring4 ();
			testLeftFactoring5 ();
			testLeftFactoring6 ();
			testUnitRemoval1 ();
			testUnitRemoval2 ();
			testUnitRemoval3 ();
			testUnitRemoval4 ();
			testUnitRemoval5 ();
			testEmptyRemoval1 ();
			testEmptyRemoval2 ();
			testEmptyRemoval3 ()
		end
end


# 3 "src/LRGrammar.ml"
(*
 * LRGrammar.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Bernardo Sousa (br)
 *)

(*
 * ChangeLog:
 *
 * may/2022 (br) - Most of the LR theory implemented.
 * mar/2022 (amd) - Skeleton.
 *)

(*
 * Description: A very simple parser for CFG syntax.
 *)
 
open BasicTypes
open CFGTypes  

(*
module type LRGrammarSig =
sig
	open CFGSyntax
	type t = ContextFreeGrammar.t
	type tx = ContextFreeGrammar.tx
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation: t
				method representationx : tx
				method validate : unit

				method tracing: unit
				method isRegular: bool
				method accept: word -> bool
				method acceptWithTracing: word -> unit
				method generate: int -> words

				method checkProperty : string -> bool
				method checkExercise: Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties
			
			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t
								
			(* LR parsing *)
				method isLR1 : bool


			end
end
*)

module LRAux =
struct
	let word s = str2word s
		
	let rec nats n = if n = 0 then [] else nats(n-1) @ [string_of_int (n-1)]
	
	let rec pop n l (*uses reverse stack and removes n heads from stack *)= 
		match l with
		| [] -> []
		| x::xs -> if(n>0) then pop (n-1) xs else x::pop n xs  
	
	
	let getTail l = (*remove head from list *)
		match l with
		| [] -> []
		|_::xs -> xs
	
	
end	

module LR0Grammar =
struct
	open LRAux

	type lr0Item = {head:symbol; body1:symbol list; body2:symbol list}		
	type lr0State = lr0Item set
	type lr0Diagram = lr0State set * (lr0State * symbol * lr0State ) set
	
	type stateName = string
	
	type lr0StateId = stateName * lr0State 
	type lr0DiagramId = lr0StateId set * (lr0StateId * symbol * lr0StateId ) set
	
	type lr0Action = Accept | Shift | Reduce of rule 
	type lr0TableEntry = stateName * (symbol * stateName) set * lr0Action
	type lr0Table = lr0TableEntry set
	
	let rule2Item (rule: rule) : lr0Item = (* converte uma regra num item novo, com o ponto á esquerda do corpo desse item *)
		{head = rule.head; body1 = []; body2 = rule.body} 
		
	
	let kernelAdvanceItem {head=h; body1=b1; body2=b2} = (* função auxiliar para avançar o ponto de um item em um simbolo para o nucleo do proximo estado. Ex: A ->.ab para A -> a.b *)
		match b2 with
		| [] -> failwith "kernelAdvanceItem: Este caso nem deve ser alcançavel"
		| x::xs -> {head = h; body1 = b1 @ [x]; body2 = xs} 
	
	

	
		
	let getDirector {head=_; body1=_; body2=b2} = (* obtem o simbolo diretor de um item *)
		match b2 with 
			| [] -> epsilon (* epsilon, aka no symbol *) 
			| x::_ -> x	
			
			
		
			
	let getDirectors state = (* Aplica a função getDirector a todos os itens de um dado estado*)
		Set.filter (fun d-> d <> epsilon)(Set.map getDirector state)
	
		
		
	let getRulesWithThisHead rules director = (* recebe o conjunto de regras da gramática e filtra esse conjunto para obter as regras cuja cabeça tem aquele simbolo diretor *)
		Set.filter (fun {head = h; body =_} -> h = director) rules 
		
	
	let diagramsJoin2 (s0,t0) (s1,t1) = (Set.union s0 s1, Set.union t0 t1) (* juntar dois diagramas LR0 para obter um diagrama LR0 resultante da união *)	
	
	let rec diagramsJoinList l : lr0Diagram = (* Juntar um conjunto de diagramas para produzir o diagrama LR0, cada diagrama desta lista corresponde a um estado rescrito como diagrama *)
		match l with
		| [] -> (Set.empty , Set.empty) 
		| d::ds -> diagramsJoin2 d (diagramsJoinList ds)
	
	
	let isNextSymbolNotAVariable {head=h; body1=b1; body2=b2} (cfg:t)=
		if(List.length b2 = 0) then true
		else
			if(Set.belongs (List.hd b2) cfg.variables) then false else true
	
	let isCompleteItem {head=h; body1=b1; body2=b2} =
		b2 = []
	
	let isStateInConflict lr0State cfg = 
		let completeItems = Set.filter(isCompleteItem) lr0State in
		if(Set.size completeItems < 1 ) then false
		else if(Set.size completeItems > 1 ) then true
		else
			let itemsProneToConflict = Set.filter(fun it -> isNextSymbolNotAVariable it cfg) lr0State in
			if(Set.size itemsProneToConflict > 1) then true else false
	
	let makeLR0DiagramId diagram : lr0DiagramId (* Cria etiquetas para os estados e os mesmos estados contidos nas transições do diagrama*) =
		let (states,transitions) = diagram in
		let dictionary = List.combine (Set.toList states) (nats (Set.size states)) in
		let statesId = Set.map (fun s -> (List.assoc s dictionary,s) ) states in
		let transitionsId = Set.map (fun (a,b,c) -> ((List.assoc a dictionary, a), b,(List.assoc c dictionary, c))) transitions in
			(statesId, transitionsId)
	
	
	let makeLR0TableEntry (id, lr0State) (cfg:t) transitions = 
		let stateTransitions = Set.filter (fun ((a,_),_,_)-> a = id) transitions in
			if Set.size stateTransitions = 0 then
				let {head = h;body1 = b1;body2 = b2} = List.hd (Set.toList lr0State) in
					if h = cfg.initial then 
						(id,Set.empty,Accept)
					else
						(id,Set.empty,Reduce ({head = h;body = b1}))			
			else  
				let nextShifts = Set.map (fun (a,b,(cId,c)) -> (b,cId)) stateTransitions in
					(id, nextShifts, Shift)
		
	(* falta uma função para aceitar/recusar palavra usando a tabela *)
	
	(*pre: isLR0 cfg *)
	let makeLR0Table (labeledDiagram:lr0DiagramId) cfg : lr0Table = (* recebe um diagrama numerado e constroi a tabela de parsing a partir das transições *) 
		let (statesId, transitionsId) = labeledDiagram in
			Set.map (fun s -> makeLR0TableEntry s cfg transitionsId) statesId
	
	
	let startRules (cfg: t) =
		let initial = cfg.initial in
		let rules = cfg.rules in
			Set.filter (fun {head=h; body=_} -> h = initial) rules
			
		
		
	let lr0StateClosureStep (cfg: t) currentItems = (* Create items for current directors *)
		let directors = getDirectors currentItems in
		let varDirectors = Set.inter directors cfg.variables in
		let newRules = Set.flatMap (fun d -> getRulesWithThisHead cfg.rules d) varDirectors in
		let newItems = Set.map rule2Item newRules in
			Set.union currentItems newItems 
		
		
	let rec lr0StateClosure cfg currentItems : lr0State = (* Create all items for a singular state *)
		let next = lr0StateClosureStep cfg currentItems in
		let next2 = Set.union currentItems next in
			if Set.size next2 = Set.size currentItems then next2
			else lr0StateClosure cfg next2


		
	let makeSingularNextLR0Diagram (cfg:t) prevState symbol : lr0Diagram = (* Creates a diagram containing only 1 state using the previous state and the transition symbol*)
		let items4Kernel = Set.filter (fun it -> getDirector it = symbol) prevState in (* falta avançar o ponto *)
		let kernel = Set.map (kernelAdvanceItem) items4Kernel in
		let closure = lr0StateClosure cfg kernel in
			(Set.make [prevState; closure], Set.make [(prevState ,symbol , closure )])
					

	
	let makeNextLR0Diagram (cfg:t) prevState : lr0Diagram = (* For each director symbol on the previous state, create a diagram and join all obtained diagrams into a single diagram*)
		let dirs = getDirectors prevState in
		let diagrams = Set.map (fun d -> makeSingularNextLR0Diagram cfg prevState d) dirs in
			diagramsJoinList (Set.toList diagrams) 

			
		
	let makeNextLR0DiagramAll (cfg:t) states : lr0Diagram = (* build the diagram using the initial state, then use the makeNextLR0Diagram function to calculate all states obtainable from the initial state*)
		let diagrams = Set.map (fun s -> makeNextLR0Diagram cfg s) states in
			diagramsJoinList (Set.toList diagrams)
			
	
	let makeFirstLR0Diagram (cfg:t) : lr0Diagram = (* O primeiro estado tem um procedimento de criação um pouco differente *) 
		let kernel = Set.map rule2Item (startRules cfg) in
		let closure = lr0StateClosure cfg kernel in
		(Set.make [closure], Set.empty)
		
		(*Set.make [closure] (* apesar de ser criado um par, nesta função só se cria o conjunto de items, o conjunto vazio das transições vazias é criado no makeLR0Diagram *) *)
		

	
	let rec makeLR0DiagramX (cfg:t) diagram = (* função auxiliar que irá produzir o diagrama LR0 *)
		let (states,transitions) : lr0Diagram = diagram in 
		let next = makeNextLR0DiagramAll cfg states in
		let next2 = diagramsJoin2 next (states,transitions) in
		let (states2,transitions2) = next2 in
			if Set.size states = Set.size states2 && Set.size transitions = Set.size transitions2 then next2
			else makeLR0DiagramX cfg next2 
					 

	let makeLR0Diagram (cfg:t) = makeLR0DiagramX cfg (makeFirstLR0Diagram cfg)  (* ponto de partida na construlão do diagrama LR0 *)
	
	
	let rec parseOperation lr0Table word stateStack symbolStack (cfg:t) = 
		let currentState = int_of_string (List.hd stateStack) in 
		let (id,shifts,action) = Set.nth lr0Table currentState in (* get corresponding table entry *)	
		match action with
		| Shift -> 
			begin
				match word with
				| [] -> false
				| s::_ -> 
					if(Set.belongs s cfg.alphabet || Set.belongs s cfg.variables) then
						let targetShifts = Set.filter (fun (a,b) -> a = s) shifts in
							if(Set.size targetShifts = 0) then false
							(* failwith (String.of_seq (List.to_seq ([char_of_int ((int_of_char '0') + currentState)] @ [s]))) *)
							(* para testar failwith (String.of_seq (List.to_seq [s])) (* Casos corretos estão a ir parar aqui por alguma razão, provavelmente após uma redução *) *)
							else
								let (nextSymbol,nextState) = Set.nth targetShifts 0 in
								let nextStateStack = [nextState] @ stateStack in
								let nextSymbolStack = [nextSymbol] @ symbolStack in
									parseOperation lr0Table (getTail word) nextStateStack nextSymbolStack cfg
					else
						failwith "singleParseOperation: este simbolo não pertence ao alfabeto desta gramatica"
			end
		| Accept -> 
			word = []	
		| Reduce({head = h;body = b}) -> 
			let popNumber = List.length b in
			let nextStateStack = (pop popNumber stateStack) in
			let nextSymbolStack = (pop popNumber symbolStack) in
			let wordWithAddedHead = [h] @ word in
				parseOperation lr0Table (wordWithAddedHead) nextStateStack nextSymbolStack cfg (*Add the variable, aka head of the reduction rule, to the word being processed *)
					
	
	(* pre: isLR0 cfg 
	   pre: isWordValid word cfg *)
	let acceptWordLR0 (word:symbol list) cfg : bool = 
		let lr0Table = makeLR0Table (makeLR0DiagramId (makeLR0Diagram cfg)) cfg in
		let stateRevStack = ["0"] in (*char list due to mix of numbers and symbols *) 
		let symbolRevStack : symbol list = [] in
			parseOperation lr0Table word stateRevStack symbolRevStack cfg
			
			
		
	let isLR0 cfg = (* verificar se a gramatica é lr0, ou seja, em todos os estados com items completos, não existem simbolos não terminais á direita de um ponto (item não completo *)
		let (states,transitions) = makeLR0Diagram cfg in
		let conflictItemStates = Set.filter(fun s -> isStateInConflict s cfg) states in
			if(Set.size conflictItemStates > 0) then false else true
		
end		
	(* ----- SLR1 -----*)
module SLR1Grammar =
struct
	open LRAux
	open LR0Grammar
	
(*
	type lr0Item = LR0Grammar.lr0Item		
	type lr0State = LR0Grammar.lr0State	
	type lr0Diagram = LR0Grammar.lr0Diagram	
	
	type stateName = LR0Grammar.stateName
	
	type lr0StateId = LR0Grammar.lr0StateId	
	type lr0DiagramId = LR0Grammar.lr0DiagramId		
*)
	
	type slr1Action = Accept | Shift | Reduce of rule 
	type slr1TableEntry = stateName * (symbol * stateName) set * (symbol * slr1Action set ) set
	type slr1Table = slr1TableEntry set
	
	let kernelAdvanceItem {head=h; body1=b1; body2=b2} = (* função auxiliar para avançar o ponto de um item em um simbolo para o nucleo do proximo estado. Ex: A ->.ab para A -> a.b *)
		match b2 with
		| [] -> failwith "kernelAdvanceItem: Este caso nem deve ser alcançavel"
		| x::xs -> {head = h; body1 = b1 @ [x]; body2 = xs} 
	
	let getNextSymbolForLR0Item (it:lr0Item)  =
		match it.body2 with
		| [] -> epsilon
		| x::xs -> x
	
	let follow w = (* Injected follow to test SLR1 grammars*)
		match w with
		| [] -> Set.make [dollar] (* Não deve acontecer*)
		| x::xs -> 
			if(x = symb "A") then Set.make [symb "c"]
			else if(x = symb "B") then Set.make [symb "d"]
			else if(x = symb "X") then Set.make [dollar]
			else Set.make [dollar]

			
	let followSetForSLR1Item it =
		follow([it.head])
		
	let isCompleteLR0Item (it:lr0Item) =
		it.body2 = []

	let countCompleteLR0Items lr0State = 
		let completeItems = Set.filter(isCompleteLR0Item) lr0State in
			Set.size completeItems	
			
	let rec buildSLR1ReductionActionsForOne completeItems symbol = (* Warning, input must only contain complete items *)
		let reductionItems = Set.filter(fun it -> Set.belongs symbol (followSetForSLR1Item it)) completeItems in
			Set.map (fun it -> Reduce ({head = it.head;body = it.body1}) ) reductionItems	
			


	let buildSLR1ReductionActions completeItems alphabet = 
		Set.map(fun symbol -> (symbol, buildSLR1ReductionActionsForOne completeItems symbol) ) alphabet
	
	
	
	let buildSLR1ShiftActionsForOne items symbol : slr1Action set = 
		let shiftItems = Set.filter(fun it -> (getNextSymbolForLR0Item it) = symbol) items in
		if(Set.size shiftItems > 0) then
			Set.make [Shift]
		else
			Set.empty
			


	let buildSLR1ShiftActions completeItems alphabet = 
		Set.map(fun symbol -> (symbol, buildSLR1ShiftActionsForOne completeItems symbol) ) alphabet
	
			
	let buildSLR1MixedActionsForOne items symbol = 
		let reductionItems = Set.filter(fun it -> (Set.belongs symbol (followSetForSLR1Item it)) && isCompleteLR0Item it) items in
		let shiftItems = Set.filter(fun it -> (getNextSymbolForLR0Item it) = symbol) items in
		let reductionEntries = Set.map (fun it -> Reduce ({head = it.head;body = it.body1}) ) reductionItems in
			if(Set.size shiftItems > 0) then
				Set.union (Set.make [Shift]) reductionEntries
			else
				reductionEntries
				

	let rec buildSLR1MixedActions (items:lr0State) alphabet = (* True build function - prototype *) (* transformar na forma do buildLR1ReductionActions *)
		Set.map(fun symbol -> (symbol, buildSLR1MixedActionsForOne items symbol) ) alphabet
	
			
			
	let makeSLR1TableEntry (id, lr0State) (cfg:t) transitions = 
		let stateTransitions = Set.filter (fun ((a,_),_,_)-> a = id) transitions in
			if Set.size stateTransitions = 0 then
				let {head = h;body1 = b1;body2 = b2} = List.hd (Set.toList lr0State) in
					if h = cfg.initial then 
						let slr1Actions : (symbol * slr1Action set) set = Set.make [dollar,Set.make [Accept]] in
							(id,Set.empty,slr1Actions)
					else
						let completeAlphabet = Set.add dollar cfg.alphabet in (* Se o $, final da palavra, não estiver no alfabeto da gramática *)
						let slr1Actions : (symbol * slr1Action set) set = buildSLR1ReductionActions lr0State completeAlphabet in
							(id,Set.empty, slr1Actions)	
			else  
				let nextShifts = Set.map (fun (a,b,(cId,c)) -> (b,cId)) stateTransitions in
					if(countCompleteLR0Items lr0State = 0) then (* Não existem reducoes *)
						let slr1Actions = buildSLR1ShiftActions lr0State cfg.alphabet in
							(id, nextShifts, slr1Actions)
					else (* Existem reducoes e transferencias *)
						let completeAlphabet = Set.add dollar cfg.alphabet in (* Se o $, final da palavra, não estiver no alfabeto da gramática *)
						let slr1Actions = buildSLR1MixedActions lr0State completeAlphabet in
							(id, nextShifts, slr1Actions)	
		
	
	(*pre: isLR1 cfg *)

	let makeSLR1Table (labeledDiagram:lr0DiagramId) cfg : slr1Table = (* recebe um diagrama numerado e constroi a tabela de parsing a partir das transições *) 
		let (statesId, transitionsId) = labeledDiagram in
			Set.map (fun s -> makeSLR1TableEntry s cfg transitionsId) statesId
			
			
		
		
	let rec parseOperationSLR1 slr1Table word stateStack symbolStack (cfg:t) = 
		let currentState = int_of_string(List.hd stateStack) in 
		let (id,shifts,actionSet) = Set.nth slr1Table currentState in (* get corresponding table entry *)
		let topSymbol = List.nth word 0 in
			if(Set.belongs topSymbol cfg.variables) then (*Fazemos um Shift com uma variavel*)
				let targetShifts = Set.filter (fun (a,b) -> a = topSymbol) shifts in
					if(Set.size targetShifts = 0) then false
					else
						let (nextSymbol,nextState) = Set.nth targetShifts 0 in
						let nextStateStack = [nextState] @ stateStack in
						let nextSymbolStack = [nextSymbol] @ symbolStack in
							parseOperationSLR1 slr1Table (getTail word) nextStateStack nextSymbolStack cfg
			else 
				let peekedSymbol = List.nth word 0 in
				let peekedsymbolAndActions = Set.filter( fun (s,a) -> s = peekedSymbol && Set.size a > 0 ) actionSet in
				let nEntries = Set.size peekedsymbolAndActions in
					if nEntries = 0 then 
						false
					else if nEntries > 1 then
						failwith "ParseOperationLR1: conflito"
					else
						let (symbol,actions) = Set.hd peekedsymbolAndActions in (* atualmente está a falhar aqui, com hd failure *)
						let action = Set.hd actions in
						match action with
						| Shift -> 
							begin
								match word with
								| [] -> false
								| s::_ -> 
									if(Set.belongs s cfg.alphabet || Set.belongs s cfg.variables) then
										let targetShifts = Set.filter (fun (a,b) -> a = s) shifts in
											if(Set.size targetShifts = 0) then false
											(* failwith (String.of_seq (List.to_seq ([char_of_int ((int_of_char '0') + currentState)] @ [s]))) *)
											(* para testar failwith (String.of_seq (List.to_seq [s])) (* Casos corretos estão a ir parar aqui por alguma razão, provavelmente após uma redução *) *)
											else
												let (nextSymbol,nextState) = Set.nth targetShifts 0 in
												let nextStateStack = [nextState] @ stateStack in
												let nextSymbolStack = [nextSymbol] @ symbolStack in
													parseOperationSLR1 slr1Table (getTail word) nextStateStack nextSymbolStack cfg
									else
										failwith "ParseOperationLR1: este simbolo não pertence ao alfabeto desta gramatica"
							end
						| Accept -> 
							word = [dollar]	
						| Reduce({head = h;body = b}) -> 
							let popNumber = List.length b in
							let nextStateStack = (pop popNumber stateStack) in
							let nextSymbolStack = (pop popNumber symbolStack) in
							let wordWithAddedHead = [h] @ word in
								parseOperationSLR1 slr1Table (wordWithAddedHead) nextStateStack nextSymbolStack cfg (*Add the variable, aka head of the reduction rule, to the word being processed *)
			
	(* pre: isSLR1 cfg 
	   pre: isWordValid word cfg *)
	let acceptWordSLR1 (word:symbol list) cfg : bool = 
		let slr1Table = makeSLR1Table (LR0Grammar.makeLR0DiagramId (LR0Grammar.makeLR0Diagram cfg)) cfg in
		let stateRevStack = ["0"] in (*char list due to mix of numbers and symbols *) 
		let symbolRevStack : symbol list = [] in
			parseOperationSLR1 slr1Table (word @ [dollar]) stateRevStack symbolRevStack cfg	
	
	let entryHasConflict slr1TableEntry : bool =
		let (id,shifts,actionSet) = slr1TableEntry in
		let entryConflicts = Set.filter ( fun (_, actions) -> Set.size actions > 1) actionSet in
			not (Set.isEmpty entryConflicts)
	
	let isSLR1 cfg : bool =
		let slr1Table = makeSLR1Table (makeLR0DiagramId (makeLR0Diagram cfg)) cfg in
		let conflicts = Set.filter (entryHasConflict) slr1Table in
			Set.isEmpty conflicts

end
	(* ----- LR1 -----*)
module LR1Grammar =
struct
	open LRAux
	
	type lr1Item = {head:symbol; body1:symbol list; body2:symbol list; lookahead:symbols}	
	type lr1State = lr1Item set
	type lr1Diagram = lr1State set * (lr1State * symbol * lr1State ) set
	
	type stateName = string
	type lr1StateId = stateName * lr1State 
	type lr1DiagramId = lr1StateId set * (lr1StateId * symbol * lr1StateId ) set
	

	type lr1Action = Accept | Shift | Reduce of rule
	type lr1TableEntry = stateName * (symbol * stateName) set * (symbol * lr1Action set ) set (* talvez seja (symbol * lr1Action set ) set *)
	type lr1Table = lr1TableEntry set
	
	let isCompleteLR1Item {head=h; body1=b1; body2=b2;lookahead=l} =
		b2 = []

	let countCompleteLR1Items lr1State = 
		let completeItems = Set.filter(isCompleteLR1Item) lr1State in
			Set.size completeItems
			
			
	let getNextSymbolForLR1Item {head=h; body1=b1; body2=b2;lookahead=l}  =
		match b2 with
		| [] -> epsilon
		| x::xs -> x
		
	let getDirectorLR1 {head=_; body1=_; body2=b2; lookahead=l} = (* obtem o simbolo diretor de um item *)
		match b2 with 
			| [] -> epsilon (* epsilon, aka no symbol *) 
			| x::_ -> x
			
	(*
	let first symbols = (* Injected first to test LR1 grammars lookahead - Luis Monteiro *)
		match symbols with
		| [] -> Set.make ['$'] (* Não deve acontecer*)
		| x::xs -> 
			if(x = 'A') then ['$';'a';'b']
			else if(x = 'B') then ['a';'b']
			else if(x = 'a') then ['a']
			else ['b']
	*)		
			
	let first symbols = (* Injected first to test LALR1 grammars lookahead - Luis Monteiro *)
		match symbols with
		| [] -> Set.make [dollar] (* Não deve acontecer*)
		| x::xs -> 
			if(x = symb "X") then Set.make [symb "c"; symb "d"]
			else if(x = symb "C") then Set.make [symb "c"; symb "d"]
			else if(x = symb "c") then Set.make [symb "c"]
			else Set.make [symb "d"]
	
	let getDirectorWithLookaheadLR1 {head=_; body1=_; body2=b2; lookahead=l} cfg = (* obtem o simbolo diretor de um item *)
		match b2 with 
			| [] -> (epsilon,Set.empty) (* epsilon, aka no symbol *) 
			| x::xs -> if(List.length b2 > 1) then (x,first xs) else (x,l)	
			
			
	let getDirectorsLR1 state = (* Aplica a função getDirector a todos os itens de um dado estado*)
		Set.filter (fun d-> d <> epsilon)(Set.map getDirectorLR1 state)
			
	let kernelAdvanceLR1Item {head=h; body1=b1; body2=b2;lookahead = l} = (* função auxiliar para avançar o ponto de um item em um simbolo para o nucleo do proximo estado. Ex: A ->.ab para A -> a.b *)
		match b2 with
		| [] -> failwith "kernelAdvanceItem: Este caso nem deve ser alcançavel"
		| x::xs -> {head = h; body1 = b1 @ [x]; body2 = xs;lookahead = l} 
	
			
			
			
	let buildLR1Item {head=h; body1=b1; body2=b2} lookahead =
		{head=h; body1=b1; body2=b2; lookahead=lookahead}
	
			
	let getDirectorsWithLookaheadLR1 (state:lr1State) cfg = (* Aplica a função getDirectorWithLookaheadLR1 a todos os itens de um dado estado*)
		Set.filter (fun (d,l)-> d <> epsilon)(Set.map (fun it -> getDirectorWithLookaheadLR1 it cfg) state) 
	
	
	let hasSameCore {head=h1; body1=b1; body2=b2; lookahead=l1} {head=h2; body1=b21; body2=b22; lookahead=l2} = 
		(h1 = h2 && b1 = b21 && b2 = b22)
		
	let mergeTwoItemsWithSameCore {head=h1; body1=b1; body2=b2; lookahead=l1} {head=h2; body1=b21; body2=b22; lookahead=l2} =
		let combinedLookahead = Set.union l1 l2 in
		{head=h1; body1=b1;body2=b2;lookahead=combinedLookahead}
	
		
	let mergeOneItem item currentItems = (* careful with the args order*)
		let (a,b) = Set.partition (fun i -> hasSameCore item i ) currentItems in
			if Set.size a = 0 then Set.add item currentItems
			else Set.add (mergeTwoItemsWithSameCore (Set.hd a) item) b	
			
	(*
	let mergeItems2 currentItems newItems =
		let rec process currentItems newItems =
			match newItems with
			| [] -> currentItems 
			| i::is -> process (mergeOneItem i currentItems) is 
		in
			process currentItems (Set.toList newItems) 		
	*)
	
			
	let rec mergeItems currentItems newItems =
		if Set.isEmpty newItems then
			currentItems
		else
			let (i,is) = Set.cut newItems in
				mergeItems (mergeOneItem i currentItems) is 
	
	(*			
	let rec mergeItems currentItems newItems =
		Set.match_ newItems 
			(fun () -> currentItems)
			(fun i is -> mergeItems (mergeOneItem i currentItems) is)
	*)
	
	let rule2ItemLR1 (rule: rule) lookahead =
		{head = rule.head; body1 = []; body2 = rule.body; lookahead = lookahead} 
	
	let generateItemsForVarDirectorWithLookahead director rules lookahead = 
		let itemRules = Set.filter (fun {head = h; body =_} -> h = director) rules in 
		let items = Set.map (fun r -> rule2ItemLR1 r lookahead) itemRules in	
			items
			
	let diagramsJoin2LR1 (s0,t0) (s1,t1) = (Set.union s0 s1, Set.union t0 t1) (* juntar dois diagramas LR0 para obter um diagrama LR0 resultante da união *)	
	
	let rec diagramsJoinListLR1 l : lr1Diagram = (* Juntar um conjunto de diagramas para produzir o diagrama LR0, cada diagrama desta lista corresponde a um estado rescrito como diagrama *)
		match l with
		| [] -> (Set.empty , Set.empty) 
		| d::ds -> diagramsJoin2LR1 d (diagramsJoinListLR1 ds)
	
	
	
	let makeLR1DiagramId diagram : lr1DiagramId (* Cria etiquetas para os estados e os mesmos estados contidos nas transições do diagrama*) =
		let (states,transitions) = diagram in
		let dictionary = List.combine (Set.toList states) (nats (Set.size states)) in
		let statesId = Set.map (fun s -> (List.assoc s dictionary,s) ) states in
		let transitionsId = Set.map (fun (a,b,c) -> ((List.assoc a dictionary, a), b,(List.assoc c dictionary, c))) transitions in
			(statesId, transitionsId)
	



	let rec buildLR1ReductionActionsForOne completeItems symbol = (* Warning, input must only contain complete items *)
		let reductionItems = Set.filter(fun it -> Set.belongs symbol it.lookahead) completeItems in
			Set.map (fun it -> Reduce ({head = it.head;body = it.body1}) ) reductionItems	
			


	let buildLR1ReductionActions completeItems alphabet = 
		Set.map(fun symbol -> (symbol, buildLR1ReductionActionsForOne completeItems symbol) ) alphabet
	
	
	let buildLR1ShiftActionsForOne items symbol : lr1Action set = 
		let shiftItems = Set.filter(fun it -> (getNextSymbolForLR1Item it) = symbol) items in
		if(Set.size shiftItems > 0) then
			Set.make [Shift]
		else
			Set.empty
			


	let buildLR1ShiftActions completeItems alphabet = 
		Set.map(fun symbol -> (symbol, buildLR1ShiftActionsForOne completeItems symbol) ) alphabet
	
	

	let buildLR1MixedActionsForOne items symbol = 
	let reductionItems = Set.filter(fun it -> (Set.belongs symbol it.lookahead) && isCompleteLR1Item it) items in
		let shiftItems = Set.filter(fun it -> (getNextSymbolForLR1Item it) = symbol) items in
		let reductionEntries = Set.map (fun it -> Reduce ({head = it.head;body = it.body1}) ) reductionItems in
			if(Set.size shiftItems > 0) then
				Set.union (Set.make [Shift]) reductionEntries
			else
				reductionEntries
				

	let rec buildLR1MixedActions (items:lr1State) alphabet = (* True build function - prototype *) (* transformar na forma do buildLR1ReductionActions *)
		Set.map(fun symbol -> (symbol, buildLR1MixedActionsForOne items symbol) ) alphabet
		
			
	let makeLR1TableEntry (id, lr1State) (cfg:t) transitions = (* possivelmente dar merge aos buildLR1Actions?*)
		let stateTransitions = Set.filter (fun ((a,_),_,_)-> a = id) transitions in
			if Set.size stateTransitions = 0 then (* this part seems fine *)
				let {head = h;body1 = b1;body2 = b2;lookahead = l} = List.hd (Set.toList lr1State) in
					if h = cfg.initial then 
						let lr1Actions : (symbol * lr1Action set) set = Set.make [dollar,Set.make [Accept]] in
							(id,Set.empty,lr1Actions)
					else
						let completeAlphabet = Set.add dollar cfg.alphabet in (* Se o $, final da palavra, não estiver no alfabeto da gramática *)
						let lr1Actions : (symbol * lr1Action set) set = buildLR1ReductionActions lr1State completeAlphabet in
							(id,Set.empty, lr1Actions)		
			else  (* Existem Shifts e possivelmente tambem reducoes *)
				let nextShifts = Set.map (fun (a,b,(cId,c)) -> (b,cId)) stateTransitions in
					if(countCompleteLR1Items lr1State = 0) then (* Não existem reducoes *)
						let lr1Actions = buildLR1ShiftActions lr1State cfg.alphabet in
							(id, nextShifts, lr1Actions)
					else (* Existem reducoes e transferencias *)
						let completeAlphabet = Set.add dollar cfg.alphabet in (* Se o $, final da palavra, não estiver no alfabeto da gramática *)
						let lr1Actions = buildLR1MixedActions lr1State completeAlphabet in
							(id, nextShifts, lr1Actions)
					
	
	(*pre: isLR1 cfg *)
	let makeLR1Table (labeledDiagram:lr1DiagramId) cfg : lr1Table = (* recebe um diagrama numerado e constroi a tabela de parsing a partir das transições *) 
		let (statesId, transitionsId) = labeledDiagram in
			Set.map (fun s -> makeLR1TableEntry s cfg transitionsId) statesId


	
		
	let lr1StateClosureStep (cfg: t) currentItems = (* Create items for current directors *)
		let directorsWithLookahead : (symbol * symbols) set = getDirectorsWithLookaheadLR1 currentItems cfg in
		
		let varDirectorsWithLookahead = Set.filter (fun (d,_) -> Set.belongs d cfg.variables) directorsWithLookahead in
		let newItems = Set.flatMap (fun (d,l) -> generateItemsForVarDirectorWithLookahead d cfg.rules l) varDirectorsWithLookahead in
		let mergedItems = mergeItems currentItems newItems in
			mergedItems
		
		
	let rec lr1StateClosure cfg currentItems : lr1State = (* Create all items for a singular state *)
		let next = lr1StateClosureStep cfg currentItems in
			if Set.subset next currentItems then next
			else lr1StateClosure cfg next
			
			
	
	let makeSingularNextLR1Diagram (cfg:t) prevState symbol : lr1Diagram = (* Creates a diagram containing only 1 state using the previous state and the transition symbol*)
		let items4Kernel = Set.filter (fun it -> getDirectorLR1 it = symbol) prevState in (* falta avançar o ponto *)
		let kernel = Set.map (kernelAdvanceLR1Item) items4Kernel in
		let closure = lr1StateClosure cfg kernel in
			(Set.make [prevState; closure], Set.make [(prevState ,symbol , closure )])
					

	
	let makeNextLR1Diagram (cfg:t) prevState : lr1Diagram = (* For each director symbol on the previous state, create a diagram and join all obtained diagrams into a single diagram*)
		let dirs = getDirectorsLR1 prevState in
		let diagrams = Set.map (fun d -> makeSingularNextLR1Diagram cfg prevState d) dirs in
			diagramsJoinListLR1 (Set.toList diagrams) 

			
		
	let makeNextLR1DiagramAll (cfg:t) states : lr1Diagram = (* build the diagram using the initial state, then use the makeNextLR0Diagram function to calculate all states obtainable from the initial state*)
		let diagrams = Set.map (fun s -> makeNextLR1Diagram cfg s) states in
			diagramsJoinListLR1 (Set.toList diagrams)

		
		
	let rec makeLR1DiagramX (cfg:t) diagram = (* função auxiliar que irá produzir o diagrama LR1 *)
		let (states,transitions) : lr1Diagram = diagram in 
		let next = makeNextLR1DiagramAll cfg states in
		let next2 = diagramsJoin2LR1 next (states,transitions) in
		let (states2,transitions2) = next2 in
			if Set.size states = Set.size states2 && Set.size transitions = Set.size transitions2 then next2
			else makeLR1DiagramX cfg next2 
			
			
	let makeFirstLR1Diagram (cfg:t) : lr1Diagram = (* O primeiro estado tem um procedimento de criação um pouco differente *) 
		let kernel = Set.map (fun r -> rule2ItemLR1 r (Set.make [dollar])) (LR0Grammar.startRules cfg) in	
		(*let kernelWithLookahead : lr1Item = buildLR1KernelItems kernel '$' in *)
		let closure = lr1StateClosure cfg kernel in
			(Set.make [closure], Set.empty)	
	
	let makeLR1Diagram (cfg:t) = makeLR1DiagramX cfg (makeFirstLR1Diagram cfg)  (* ponto de partida na construção do diagrama LR1 *)
	
	
	let rec parseOperationLR1 lr1Table word stateStack symbolStack (cfg:t) = 
		let currentState = int_of_string(List.hd stateStack) in 
		let (id,shifts,actionSet) = Set.nth lr1Table currentState in (* get corresponding table entry *)
		let topSymbol = List.nth word 0 in
			if(Set.belongs topSymbol cfg.variables) then (*Fazemos um Shift com uma variavel*)
				let targetShifts = Set.filter (fun (a,b) -> a = topSymbol) shifts in
					if(Set.size targetShifts = 0) then false
					else
						let (nextSymbol,nextState) = Set.nth targetShifts 0 in
						let nextStateStack = [nextState] @ stateStack in
						let nextSymbolStack = [nextSymbol] @ symbolStack in
							parseOperationLR1 lr1Table (getTail word) nextStateStack nextSymbolStack cfg
			else 
				let peekedSymbol = List.nth word 0 in
				let peekedsymbolAndActions = Set.filter( fun (s,a) -> s = peekedSymbol && Set.size a > 0 ) actionSet in
				let nEntries = Set.size peekedsymbolAndActions in
					if nEntries = 0 then 
						false
					else if nEntries > 1 then
						failwith "ParseOperationLR1: conflito"
					else
						let (symbol,actions) = Set.hd peekedsymbolAndActions in (* atualmente está a falhar aqui, com hd failure *)
						let action = Set.hd actions in
						match action with
						| Shift -> 
							begin
								match word with
								| [] -> false
								| s::_ -> 
									if(Set.belongs s cfg.alphabet || Set.belongs s cfg.variables) then
										let targetShifts = Set.filter (fun (a,b) -> a = s) shifts in
											if(Set.size targetShifts = 0) then false
											(* failwith (String.of_seq (List.to_seq ([char_of_int ((int_of_char '0') + currentState)] @ [s]))) *)
											(* para testar failwith (String.of_seq (List.to_seq [s])) (* Casos corretos estão a ir parar aqui por alguma razão, provavelmente após uma redução *) *)
											else
												let (nextSymbol,nextState) = Set.nth targetShifts 0 in
												let nextStateStack = [nextState] @ stateStack in
												let nextSymbolStack = [nextSymbol] @ symbolStack in
													parseOperationLR1 lr1Table (getTail word) nextStateStack nextSymbolStack cfg
									else
										failwith "ParseOperationLR1: este simbolo não pertence ao alfabeto desta gramatica"
							end
						| Accept -> 
							word = [dollar]	
						| Reduce({head = h;body = b}) -> 
							let popNumber = List.length b in
							let nextStateStack = (pop popNumber stateStack) in
							let nextSymbolStack = (pop popNumber symbolStack) in
							let wordWithAddedHead = [h] @ word in
								parseOperationLR1 lr1Table (wordWithAddedHead) nextStateStack nextSymbolStack cfg (*Add the variable, aka head of the reduction rule, to the word being processed *)
								
	
	(* pre: isLR1 cfg 
	   pre: isWordValid word cfg *)
	let acceptWordLR1 (word:symbol list) cfg : bool = 
		let lr1Table = makeLR1Table (makeLR1DiagramId (makeLR1Diagram cfg)) cfg in
		let stateRevStack = ["0"] in (*char list due to mix of numbers and symbols *) 
		let symbolRevStack : symbol list = [] in
			parseOperationLR1 lr1Table (word @ [dollar]) stateRevStack symbolRevStack cfg
	
	let entryHasConflict slr1TableEntry : bool =
		let (id,shifts,actionSet) = slr1TableEntry in
		let entryConflicts = Set.filter ( fun (_, actions) -> Set.size actions > 1) actionSet in
			Set.size entryConflicts > 0
	
	let isLR1 cfg : bool =
		let slr1Table = makeLR1Table (makeLR1DiagramId (makeLR1Diagram cfg)) cfg in
		let conflicts = Set.filter (entryHasConflict) slr1Table in
			Set.size conflicts = 0
	
end	
	(* ----- LALR1 -----*)
module LALR1Grammar =
	struct
	open LRAux
	open LR0Grammar	
	open LR1Grammar	
		
		
	let itemsSameCores it1 it2 =
		it1.head = it2.head && it1.body1 = it2.body1 && it1.body2 = it2.body2
		
	let itemsJoinLookahead it1 it2 =
		{head = it1.head; body1 = it1.body1; body2 = it1.body2; lookahead = (Set.union it1.lookahead it2.lookahead)}
	
	
	let getStateCore (state:lr1State) =
		Set.map (fun it -> {head = it.head; body1 = it.body1; body2 = it.body2}) state
	
	
	let haveSameCores lr1state1 lr1state2 =
		let state1Core = getStateCore lr1state1 in
		let state2Core = getStateCore lr1state2 in
			Set.equals state1Core state2Core
				
	(*pre: hasSameCores state1 state2 *)
	let rec mergeLR1States state1 state2 =
		Set.map (fun it -> 
			let fit = Set.find (fun it2 -> itemsSameCores it it2) state2 in itemsJoinLookahead it fit) state1 
	
	
	
	type lr1StateId = stateName * lr1State 
	type lr1DiagramId = lr1StateId set * (lr1StateId * symbol * lr1StateId ) set
		
	let rec lr1StateFusionId statesId  =
		match statesId with
		| [] -> []
		| (id,x)::xs -> 
			let ss = lr1StateFusionId xs in
			let (a,b) = List.partition (fun (_,y)-> haveSameCores x y) ss in 
				match a with
				| [] -> (id,x)::ss 
				| [(id2,y)] -> (id^","^id2,mergeLR1States x y)::b(* fundir x com y*)
				| _ -> failwith "lr1StateFusionFail"	
		
				
	
	let rec lr1StateFusion states  =
		match states with
		| [] -> []
		| x::xs -> 
			let ss = lr1StateFusion xs in
			let (a,b) = List.partition (haveSameCores x) ss in 
				match a with
				| [] -> x::ss 
				| [y] -> mergeLR1States x y::b(* fundir x com y*)
				| _ -> failwith "lr1StateFusionFail"
		
	let translate state fstates =
		Set.find (fun s -> haveSameCores state s) fstates
		
				
	let lr1TransFusion trans fstates =
		Set.map (fun (s1,sym,s2) -> (translate s1 fstates,sym,translate s2 fstates)) trans
				
	
	let makeLALR1FromLR1 diagram =
		let (states,transitions) : lr1Diagram = diagram in 
		let fstates = lr1StateFusion (Set.toList states) in
		let ftrans = lr1TransFusion transitions (Set.make fstates) in
		let lalr1Diagram : lr1Diagram = ((Set.make fstates),ftrans) in
			lalr1Diagram
			
			
	(* pre: isLR1 cfg 
	   pre: isWordValid word cfg *)
	let acceptWordLALR1 (word: word) cfg : bool = 
		let lr1Table = makeLR1Table (makeLR1DiagramId (makeLALR1FromLR1 (makeLR1Diagram cfg))) cfg in
		let stateRevStack = ["0"] in (*char list due to mix of numbers and symbols *) 
		let symbolRevStack : symbol list = [] in
			parseOperationLR1 lr1Table (word @ [dollar]) stateRevStack symbolRevStack cfg
			
			
	let entryHasConflict slr1TableEntry : bool =
		let (id,shifts,actionSet) = slr1TableEntry in
		let entryConflicts = Set.filter ( fun (_, actions) -> Set.size actions > 1) actionSet in
			not (Set.isEmpty entryConflicts)
	
	let isLALR1 cfg : bool =
		let slr1Table = makeLR1Table (makeLR1DiagramId (makeLALR1FromLR1 (makeLR1Diagram cfg))) cfg in
		let conflicts = Set.filter (entryHasConflict) slr1Table in
			Set.isEmpty conflicts
	
end	
(*----- Test functions LR1-----*)
module LRTests =
	struct
	open LR0Grammar
	open SLR1Grammar
	open LR1Grammar
	open LALR1Grammar

let (lr1grammar:t) = (* basic LR1 with no First usage *)
{alphabet = symbols "01";
variables = symbols "SXA" ;
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> A"; "A -> 0" ; "A -> 1"])
} ;;

let (lr1grammar2:t) = (* basic LR1 with basic First usage *)
{alphabet = symbols "01";
variables = symbols "SXA" ;
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> A01"; "A -> 0" ; "A -> 1"])
} ;;

let ttLR1 () = (* Full grammar *)
	makeLR1Diagram lr1grammar;;

let (lr1grammarX:t) = (* exemplo do professor Luis Monteiro *) 
{alphabet = symbols "ab";
variables = symbols "SAB";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> A"; "A -> BA"; "A -> ~" ; "B -> aB"; "B -> b"; ])
} ;; (* Resumindo esta gramatica: (B)^n, onde B = (a)^m b *)
(* (a*b)* *)

let ttLR1X () = (* Full grammar *)
	makeLR1Diagram lr1grammarX;;
	
let ttLR1Id () = 
	makeLR1DiagramId (makeLR1Diagram lr1grammarX) ;;
	
let ttLR1Table () =
	makeLR1Table (makeLR1DiagramId (makeLR1Diagram lr1grammarX)) lr1grammarX ;; 
	
let ttLR1Word () = (* simple test *)
	acceptWordLR1 (word "ab") lr1grammarX ;;
	
	
	
let ttLR1Word2 () = (* long simple test *)
	acceptWordLR1 (word "bbbbbbbb") lr1grammarX ;;
	
let ttLR1Word3 () = (* long complex test *)
	acceptWordLR1 (word "aaaaaaaaabbbbbbbb") lr1grammarX ;;
	
let ttLR1Word4 () = (* empty test *)
	acceptWordLR1 [] lr1grammarX ;;
	
let ttLR1Word5 () = (* combination test *)
	acceptWordLR1 (word "ababababababababaaaaaaaaabbbbbbbb") lr1grammarX ;;
	
let ttLR1WordFail () = (* falha mas da erro em vez de false *)
	acceptWordLR1 (word "bbbbbbbba") lr1grammarX ;;
	
	
(*----- Test functions LALR1-----*)


let (lalr1grammar:t) = (* basic SLR1 with Follow usage *)
	{alphabet = symbols "cd";
	variables = symbols "SXC";
	initial = symb "S";
	rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> CC"; "X -> ~"; "C -> cC" ; "C -> d"])
} ;;

let ttLALR1 () = (* Full grammar *)
	makeLALR1FromLR1 (makeLR1Diagram lalr1grammar);;	
	
let ttLR1Table () =
	makeLR1Table (makeLR1DiagramId (makeLR1Diagram lalr1grammar)) lalr1grammar ;; 
	
let ttLALR1Table () =
	makeLR1Table (makeLR1DiagramId (makeLALR1FromLR1 (makeLR1Diagram lalr1grammar))) lalr1grammar ;; 	

let ttLALR1Word () = (* simple test *)
	acceptWordLALR1 [] lalr1grammar ;;
	
let ttLALR1Word2 () = (* simple test *)
	acceptWordLALR1 (word "dd" ) lalr1grammar ;;
	
	
let ttLALR1WordFail () = (* simple test *)
	acceptWordLALR1 (word "cd" )  lalr1grammar ;;
	
let ttIsLALR1 () = isLALR1 lalr1grammar ;;

let ttIsLR1 () = isLR1 lalr1grammar ;;

	
	
(*----- Test functions SLR1-----*)

let (slr1grammar:t) = (* basic SLR1 with Follow usage *)
	{alphabet = symbols "acdz";
	variables = symbols "SXAB";
	initial = symb "S";
	rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> aAc"; "X -> aBd"; "A -> z" ; "B -> z"])
} ;;


let (slr1grammarFail:t) = (* basic SLR1 with Follow usage *)
	{alphabet = symbols "acdz";
	variables = symbols "SXAB";
	initial = symb "S";
	rules = CFGSyntax.parse (Set.make ["S -> X"; "X -> aAd"; "X -> aBd"; "A -> z" ; "B -> z"])
} ;;

let ttSLR1Table () =
	makeSLR1Table (makeLR0DiagramId (makeLR0Diagram slr1grammar)) slr1grammar ;; 

let ttSLR1Word() =
	acceptWordSLR1 (word "azc") slr1grammar ;;
	
let ttSLR1Word2() =
	acceptWordSLR1 (word "azd") slr1grammar ;;
	
let ttSLR1WordFail() =
	acceptWordSLR1 (word "") slr1grammar ;;
	
let ttSLR1WordFail2() =
	acceptWordSLR1 (word "azcd") slr1grammar ;;
	
let ttSLR1WordFail3() =
	acceptWordSLR1 (word "az") slr1grammar ;;
	
let ttSLR1WordFail4() =
	acceptWordSLR1 (word "azc$") slr1grammar ;;
	
let ttIsSLR1() = isSLR1 slr1grammar ;;

let ttIsSLR1Fail() = isSLR1 slr1grammarFail ;; (* é preciso alterar o follow para testar fails... dor *)
	


(*----- Test functions LR0-----*)

let showLR0States (cfg:t) =
	let diagram = makeLR0Diagram cfg in
	let (states,transitions) : lr0Diagram = diagram in
	states
	
let showLR0Transitions (cfg:t) =
	let diagram = makeLR0Diagram cfg in
	let (states,transitions) : lr0Diagram = diagram in
	transitions


let (grammar:t) = 
{alphabet = symbols "01";
variables = symbols "SX";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 1S0"; "S -> X"; "X -> 0X1" ; "X -> ~"])
} ;;

let tt () = (* Full grammar *)
	makeLR0Diagram grammar;;


let (grammar2:t) = 
{alphabet = symbols "1";
variables = symbols "S";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 1"])
} ;;

let tt2 () (* Single State Grammar *) =
	makeLR0Diagram grammar2;;
	

let (grammar3:t)  = 
{alphabet = symbols "01";
variables = symbols "S";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 111111"])
} ;;

let tt3 () (* Multiple State/Single Rule Grammar *)=
	makeLR0Diagram grammar3;;


let (grammar4:t)  = 
{alphabet = symbols "01";
variables = symbols "S";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 111111"; "S -> 000000"])
} ;;

let tt4 () (* Multiple State/Multiple Rule Grammar *) =
	makeLR0Diagram grammar4;;


let (grammar5:t)  = 
{alphabet = symbols "01";
variables = symbols "SA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 111111"; "S -> 0X1"; "X -> 01" ])
} ;;

let (grammar5v2:t) (* Copy to test sorting in sets *) = 
{alphabet = symbols "01";
variables = symbols "SA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 111111"; "S -> 0A1"; "A -> 01" ])
} ;;

let tt5 () (* Multiple Variables/Multiple State/Multiple Rule Grammar *) =
	makeLR0Diagram grammar5;;


let (grammar6:t) = 
{alphabet = symbols "01";
variables = symbols "SXA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> 1X0"; "X -> A"; "A -> 0A1"; "A -> 01"])
} ;;

let tt6 () (* Almost Full Grammar\No rule containing only epsilon *) =
	makeLR0Diagram grammar6;;

let (grammar7:t) = 
{alphabet = symbols "ab$";
variables = symbols "SXA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X$"; "X -> XA"; "X -> A"; "A -> aXb"; "A -> ab"])
} ;;

let tt7 () (* Gramática LR0 do exemplo de Luis Monteiro *) =
	makeLR0Diagram grammar7;;

	
let tt7Count () : bool (* Gramática LR0 do exemplo de Luis Monteiro *) =
	let (a,b) :lr0Diagram = makeLR0Diagram grammar7 in
	if (Set.size a = 9 && Set.size b = 13) then true else false
	
	
let (grammar7f:t) = 
{alphabet = symbols "abc$";
variables = symbols "SXA";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X$"; "X -> XA"; "X -> A"; "A -> aXb"; "A -> ab";"A -> abc"])
} ;;

let (grammar7alt:t) = 
{alphabet = symbols "ab$";
variables = symbols "SXAF";
initial = symb "S";
rules = CFGSyntax.parse (Set.make ["S -> X$"; "X -> XA"; "X -> A"; "A -> aXb"; "A -> ab";"A -> abF"; "F -> FA" ])
} ;;
	
let tt7LR0 () (* Gramática LR0 do exemplo de Luis Monteiro *) =
	isLR0 grammar7;;
	
let ttLR0Fail () (* Deve dar falso devido a dois items completos *) =
	isLR0 grammar7f;;
	
let ttLR0Alt () (* Devia dar verdadeiro com um item completo e um item incompleto que espera uma variavel, mas na prática isto é impossivel, porque é preciso calcular o fecho para essa variavel, esse fecho novo causa um conflito, a menos que a variavel não tenha uma regra respectiva, sendo nesse caso uma variavel inutil *) =
	isLR0 grammar7alt;;

let ttIncon1 () = makeLR0Diagram grammar5;;

let ttIncon2 () = makeLR0Diagram grammar5v2;;


let ttId () = makeLR0DiagramId (makeLR0Diagram grammar7) ;;
		
let ttx () =
	makeLR0Table (makeLR0DiagramId (makeLR0Diagram grammar7)) grammar7 ;; 
	
let ttx2 () =
	makeLR0Table (makeLR0DiagramId (makeLR0Diagram grammar5)) grammar5 ;; 


let ttWord () = 
	acceptWordLR0 (word "1") grammar2 ;;
	
let ttWordFail () = 
	acceptWordLR0 (word "10") grammar2 ;;
	
let ttWord2 () = 
	acceptWordLR0 (word "111111") grammar5 ;;
	
let ttWord2Fail () = 
	acceptWordLR0 (word "1111111") grammar5 ;;	
	
let ttWord3 () = 
	acceptWordLR0 (word "0011") grammar5 ;;
	
let ttWord3Fail () = 
	acceptWordLR0 (word "00111") grammar5 ;;
	
let ttWord4 () = 
	acceptWordLR0 (word "100110") grammar6 ;;
	
let ttWord4Fail () = 
	acceptWordLR0 (word "10011") grammar6 ;;
	
let ttWordX () =
	acceptWordLR0 (word "aaabbb$") grammar7 ;;
	
let ttWordXFail () =
	acceptWordLR0 (word "aaabbb") grammar7 ;;
	
end	
(*----- End of Testing -----*)
	
module LRGrammar =
struct
	open LR1Grammar
	
	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit LL1Grammar.model arg as super
		
	

	(* only a random example *)
			method isLR1 : bool =
				isLR1 (self#representation)
	end
end


open LRTests ;;
(*
module LRGrammarTests: sig end =
struct
	let active = false
	
	
	
	let test0 () =
		let m = new LR0Grammar.model (Arg.Predef "cfg_simple") in
		let j = m#toJSon in
			JSon.show j

	let test1 () =
		let m = new LR0Grammar.model (Arg.Predef "cfg_simple") in
		let r = m#startRules in
			Util.println ["*** Rules for the start symbol:"];
			CFGSyntax.show r

	let runAll =
		if Util.testing(active) then (
			Util.header "LRGrammarTests";
			test0 ();
			test1 ()	
		)
end
*)

 
# 3 "src/ContextFreeGrammarFull.ml"
(*
 * ContextFreeGrammarFull.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
????
 *)

(*
 * Description: Context-free grammar functionality.
 *
 * TODO: More cleanup.
 *)

open BasicTypes
open CFGTypes  

module type ContextFreeGrammarFullSig =
sig
	type syntaxTable = LL1Grammar.syntaxTable
	type acceptTable = LL1Grammar.acceptTable
	type recognized = LL1Grammar.recognized
	type acceptStep = LL1Grammar.acceptStep
	type transformation = LL1Grammar.transformation

	class model :
		(t,tx) Arg.alternatives ->
			object
			(*ContextFreeGrammar Basic*)
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation: t
				method representationx : tx
				method validate : unit

				method tracing: unit
				method isRegular: bool
				method first: word -> symbol Set.t
				method follow: symbol -> symbol Set.t
				method lookahead: rule -> symbol Set.t
				method accept: word -> bool
				method acceptWithTracing: word -> unit
				method generate: int -> words

			(* LL1Grammar *)
				method isSimplified : bool
				method rdparserOpts : string list
				method toggleSimplified : unit

				method isLL1: bool
				method isLeftRecursive: bool
				method createParsingTable: ((variable * symbol) * word) Set.t
				method hasParsingTableConflict : bool 
				method acceptZ: word -> acceptStep list
				method productiveSymbols: symbol Set.t
				method accessibleSymbols: symbol Set.t
				method productiveRewrite: ContextFreeGrammar.model
				method accessibleRewrite: ContextFreeGrammar.model
				method clean: transformation list
				method isFullyProductive: bool
				method isFullyAccessible: bool
				method isClean: bool
				method removeLeftRecursion: transformation
				method removeDirectLeftRecursion: ContextFreeGrammar.model
				method leftFactoring: transformation
				method isLeftFactoring: bool
				method leftCorner: symbol -> symbol Set.t
				method hasEmptyProductions: bool
				method removeEmptyProductions: transformation
				method hasUnitProductions: bool
				method removeUnitProductions: transformation
				method generateRecursiveDescendentParser: string -> string
				method transformToLL1: transformation list

			(* LRGrammar *)
				method isLR1 : bool

			(* Exercices support *)
				method checkProperty : string -> bool
				method checkExercise: Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties
											
			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t
			end
end

module ContextFreeGrammarFull : ContextFreeGrammarFullSig =
struct
	type syntaxTable = LL1Grammar.syntaxTable
	type acceptTable = LL1Grammar.acceptTable
	type recognized = LL1Grammar.recognized
	type acceptStep = LL1Grammar.acceptStep
	type transformation = LL1Grammar.transformation

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit LRGrammar.model arg as super

     end


end


# 3 "src/TuringMachine.ml"
(*
 * TuringMachine.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Miguel Lourenço (ml)
 *)

(*
 * ChangeLog:
 *
 * ???/2022 (ml) - ???.
 * jun/2022 (amd) - Initial skeleton.
 *)

(*
 * Description: Turing machine functionality.
 *)

open BasicTypes

module TurMachTypes =
struct
	type transition = state * symbol * state * symbol * direction
	type transitions = transition set

	type tx = {
		entryAlphabet: symbol list;
		tapeAlphabet: symbol list;
		empty: symbol;
		states: state list;
		initialState: state;
		transitions: transition list;
		acceptStates: state list;
		criteria: bool;
		markers: symbol list
	}
	
	type t = {
		entryAlphabet: symbols;
		tapeAlphabet: symbols;
		empty: symbol;
		states: states;
		initialState: state;
		transitions: transitions;
		acceptStates: states;
		criteria: bool; (* true = acceptStates | false = stop *)
		markers: symbols
	}
	type configuration = state * symbol list * symbol list
	type configurations = configuration set

	type path = configuration list
	type paths = path list

end

open TurMachTypes

module type TuringMachineSig = sig
	val modelDesignation: string

	val getDefaultMachine: t

	val transitionGet1: transition set -> state set
	val transitionGet2: transition set -> symbol set
	val transitionGet3: transition set -> state set
	val transitionGet4: transition set -> symbol set

	val configurationGet1: configurations -> state set
	val configurationGet2: configurations -> symbol list set
	val configurationGet3: configurations -> symbol list set

	val reverse: symbol list -> symbol list

	val printConfiguration: configuration -> unit
	val printConfigurations: configurations -> unit

	class model:
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors: string list
				method handleErrors: unit
				method toJSon: JSon.t
				method representation: t
				method representationx: tx
				method validate: unit
				
				method tracing: unit

				method acceptOld: word -> bool
				method accept: word -> bool
				method acceptFull: word -> bool * configuration list * configuration set list
				method generate: int -> words	
				method downgradeModelToFiniteAutomaton: FiniteAutomaton.model
				method reachable: state -> states
				method productive: states
				method getUsefulStates: states
				method getUselessStates: states
				method isDeterministic: bool
				method areAllStatesUseful: bool
				method cleanUselessStates: t
				method acceptLB: word -> bool
				method acceptFullLB: word -> bool * configuration list * configuration set list
				method isLB: bool
				method convertToStopCriteria: model
				method hasState: state -> bool
				method hasTransition: transition -> bool
				method isFinal: state -> bool
				method isInitial: state -> bool
				method addState: state -> t
				method addInitialState: state -> t
				method addFinalState: state -> t
				method removeState: state -> t
				method changeStateToInitial: state -> t
				method changeStateToFinal: state -> t
				method changeStateFromFinal: state -> t
				method addTransition: transition -> t
				method removeTransition: transition -> t
				method renameState: state -> state -> t
			
			(* Exercices support *)
				method checkProperty : string -> bool
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties

			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t			
	end
end

module TuringMachine : TuringMachineSig =
struct

		(*-----------------------Types--------------------------------*)

	type configuration = state * symbol list * symbol list
	type configurations = configuration set

	type path = configuration list
	type paths = path list

		(*-----------------------Export functions--------------------------------*)

	let modelDesignation = "turing machine"

	let getDefaultMachine = {
		entryAlphabet = Set.empty;
		tapeAlphabet = Set.add empty Set.empty;	
		empty = empty;
		states = Set.add "START" Set.empty;
		initialState = "START";
		transitions = Set.empty;
		acceptStates = Set.empty;
		criteria = false;
		markers = Set.empty
	}
	
	let transitionGet1 trns = Set.map ( fun (a,_,_,_,_) -> a ) trns
	let transitionGet2 trns = Set.map ( fun (_,b,_,_,_) -> b ) trns
	let transitionGet3 trns = Set.map ( fun (_,_,c,_,_) -> c ) trns
  let transitionGet4 trns = Set.map ( fun (_,_,_,d,_) -> d ) trns
	let configurationGet1 configs =  Set.map ( fun (a,_,_) -> a ) configs
	let configurationGet2 configs =  Set.map ( fun (_,b,_) -> b ) configs
	let configurationGet3 configs =  Set.map ( fun (_,_,c) -> c ) configs

	let rec reverse l =
		match l with
		| [] -> []
		| x::xs -> (reverse xs)@[x]

	let printConfiguration config =
		let (state, left, right) = config in
		Util.print ["("; state2str state; ", "];
		List.iter (fun x -> Util.print [symb2str x]) left;
		Util.print [", "];
		List.iter (fun x -> Util.print [symb2str x]) right;
		Util.print [")"]

	let printConfigurations configs = 
		Util.println ["printing configs"];
		Set.iter (fun x -> printConfiguration x) configs;
		Util.println [string_of_int (Set.size configs)]

	(*-----------------------Auxiliary functions--------------------------------*)

	let internalize (tm: tx): t = {
		entryAlphabet = Set.make tm.entryAlphabet;
		tapeAlphabet = Set.make tm.tapeAlphabet;	
		empty = tm.empty;	
		states = Set.make tm.states;
		initialState = tm.initialState;
		transitions = Set.make tm.transitions;
		acceptStates = Set.make tm.acceptStates;
		criteria = tm.criteria;
		markers = Set.make tm.markers
	}

	let externalize (tm: t): tx = {
		entryAlphabet = Set.toList tm.entryAlphabet;
		tapeAlphabet = Set.toList tm.tapeAlphabet;	
		empty = tm.empty;
		states = Set.toList tm.states;
		initialState = tm.initialState;
		transitions = Set.toList tm.transitions;
		acceptStates = Set.toList tm.acceptStates;
		criteria = tm.criteria;
		markers = Set.toList tm.markers
	}

	let fromJSon (j: JSon.t): t =
		if JSon.isNull j || not (JSon.hasField j "kind") then {
			entryAlphabet = Set.empty;
			tapeAlphabet = Set.make [empty];	
			empty = empty;
			states = Set.make [draftState];
			initialState = draftState;
			transitions = Set.empty;
			acceptStates = Set.empty;
			criteria = false;
			markers = Set.empty
		}
		else {
			entryAlphabet = JSon.fieldSymbolSet j "entryAlphabet";
			tapeAlphabet = JSon.fieldSymbolSet j "tapeAlphabet";
			empty = JSon.fieldSymbol j "empty";
			states = JSon.fieldStateSet j "states";
			initialState = JSon.fieldState j "initialState";
			transitions = JSon.fieldTMTransitionSet j "transitions";
			acceptStates = JSon.fieldStateSet j "acceptStates";
			criteria = JSon.fieldBool j "criteria";
			markers = JSon.fieldSymbolSet j "markers"
		}

	let toJSon (rep: t): JSon.t =
		JSon.makeAssoc [
			("entryAlphabet", JSon.makeSymbolSet rep.entryAlphabet);
			("tapeAlphabet", JSon.makeSymbolSet rep.tapeAlphabet);
			("empty", JSon.makeSymbol rep.empty);
			("states", JSon.makeStateSet rep.states);
			("initialState", JSon.makeState rep.initialState);
			("transitions", JSon.makeTMTransitionsSet rep.transitions);
			("acceptStates", JSon.makeStateSet rep.acceptStates);
			("criteria", JSon.makeBool rep.criteria);
			("markers", JSon.makeSymbolSet rep.markers)
		]	
	
	let getMarkers rep = (Set.nth rep.markers 0, Set.nth rep.markers 1)
	let transitionsTm2Fa trns = Set.map ( fun (a,b,c,_,_) -> (a,b,c) ) trns
	let transitionGetSymbs trns = Set.union (Set.map (fun (_,b,_,_,_) -> b) trns) (Set.map (fun (_,_,_,d,_) -> d) trns)

	let rec acceptX left right st trs seen limit = 
		if limit = 0 then 
			state "~"
		else
			let newLimit = limit - 1 in
			let config = (reverse left)@[symb (state2str st)]@right in
				if Set.belongs config seen then state "~"
				else
					let newSeen = Set.add config seen in
					match left, right with
					| [], [] -> st
					| [], x::xs -> acceptX [empty] right st trs newSeen newLimit
					| x::xs, [] -> acceptX left [empty] st trs newSeen newLimit
					| x::xs, y::ys -> let getTransi = Set.filter (fun (a,b,_,_,_) -> st = a && y = b) trs in
														if Set.isEmpty getTransi then st
														else 
															let (_,_,nst,nsymb,dir) = Set.nth getTransi 0 in
																if dir = R then
																	let newLeft = nsymb::left in
																		acceptX newLeft ys nst trs newSeen newLimit;
																else
																	let newRight = x::nsymb::right in
																		acceptX xs newRight nst trs	newSeen newLimit

	let acceptOld w rp =
		let bWord = w@[empty] in
			let lastST: state = acceptX [empty] bWord rp.initialState rp.transitions Set.empty 100 in
				if rp.criteria then Set.exists (fun x -> x = lastST) rp.acceptStates
				else if lastST = state "~" then false
				else true

	(* Config de erro *)
	let stopConfig = (state "~", [], [])

	let initialConfigs tm w: configuration set =
		Set.make [(tm.initialState, [empty], w@[empty])]

	let isAcceptingConfig tm (s,l,r): bool =
		if tm.criteria then Set.belongs s tm.acceptStates
		else 
			let nextSymb = if not ((List.length r) = 0) then List.nth r 0 else empty in
			let stateTransitions = Set.filter (fun (a,b,_,_,_) -> a = s && b = nextSymb) tm.transitions in
				Set.isEmpty stateTransitions

	let next (a,b,c,d,e) (s,l,r) = 
		let l = if l = [] then [empty] else l in
		let r = if r = [] then [empty] else r in
		match l, r with
		| l::ls, r::rs -> if a = s && b = r then 
												if e = R then Set.make [(c,d::l::ls,rs)]
												else Set.make [(c,ls,l::d::rs)]
											else Set.empty
		| _, _ -> failwith "next"
	
	let getWord (s,l,r) = word "_"

	let nextConfigs tm (s,l,r): configuration set = 
		Set.flatMap (fun (a,b,c,d,e) -> next (a,b,c,d,e) (s,l,r)) tm.transitions 
	
	let accept w rp = 
		Model.accept rp w initialConfigs nextConfigs isAcceptingConfig

	let acceptFull w rp = 
		Model.acceptFull rp w initialConfigs nextConfigs isAcceptingConfig

	let generate length rp = 
		Model.generateDumb rp rp.entryAlphabet length initialConfigs nextConfigs isAcceptingConfig

	let isLB rep = 

		let hasMarkers = (Set.size rep.markers) = 2 in

		if hasMarkers then
			let (leftMarker, rightMarker) = getMarkers rep in
			let boundedDirection mark rev = Set.exists (fun (_,b,_,d,e) -> b = mark && (d != mark || e != rev)) rep.transitions in

			let bounded = (boundedDirection leftMarker R) && (boundedDirection rightMarker L) in
				bounded
		else
			hasMarkers

	let validate rep id = (

		let validInitSt = Set.belongs rep.initialState rep.states in

		let validAccSts = Set.subset rep.acceptStates rep.states in

		let currentSt = transitionGet1 rep.transitions in
		let readSy = transitionGet2 rep.transitions in
		let newSt = transitionGet3 rep.transitions in
		let writeSy = transitionGet4 rep.transitions in

		let alpha = Set.union (Set.make [empty]) rep.tapeAlphabet in
	
		let validTrns = (Set.subset currentSt rep.states) &&
								(Set.subset newSt rep.states) && 
								(Set.subset readSy alpha) &&
								(Set.subset writeSy alpha) in

		let emptyInAlph = Set.belongs empty rep.tapeAlphabet in

		let emptyIsEmpty = rep.empty = empty in

		let markersSize = ((Set.size rep.markers) = 2 || (Set.size rep.markers) = 0) in 

		if not validInitSt then
			Error.error id.Entity.name
				"The initial state does not belong to the set of all states" ()
		;

		if not validAccSts then
			Error.error id.Entity.name
				"Some accept states do not belong to the set of all states" ()
		;

		if not validTrns then
			Error.error id.Entity.name
				"Some transitions are invalid" ()
		;

		if not emptyInAlph then
			Error.error id.Entity.name
				"Empty symbol isn't in the tape alphabet" ()
		;

		if not emptyIsEmpty then
			Error.error id.Entity.name
				"The empty symbol is not correct, change it to 'B'" ()
		;
		if not markersSize then
			Error.error id.Entity.name
				"Too little or too many markers given" ()
		)

	let downgradeModelToFiniteAutomaton rep = 
		let alphaB = Set.union (Set.make [empty]) rep.tapeAlphabet in
		let fa: FinAutTypes.t = {
				alphabet = alphaB;
				states = rep.states;
				initialState = rep.initialState;
				transitions = transitionsTm2Fa rep.transitions;
				acceptStates = rep.acceptStates
			} in
		new FiniteAutomaton.model (Arg.Representation fa)

	let reachable s rep = 
		let fa = downgradeModelToFiniteAutomaton rep in
			fa#reachable s

	let productive rep =
		if rep.criteria then
			let fa = downgradeModelToFiniteAutomaton rep in
				fa#productive
		else rep.states
		
	let getUsefulStates rep =
		Set.inter (productive rep) (reachable rep.initialState rep)

	let getUselessStates rep =
		Set.diff rep.states (getUsefulStates rep)

	let isDeterministic rep =
		let fa = downgradeModelToFiniteAutomaton rep in
			fa#isDeterministic

	let cleanUselessStates rep =
		let usfSts = getUsefulStates rep in
		let usfTrs = Set.filter (fun (a,_,c,_,_) -> Set.belongs a usfSts && Set.belongs c usfSts) rep.transitions in
		let tapeAlf = Set.add empty (transitionGetSymbs usfTrs) in
		let entryAlf = Set.inter tapeAlf rep.entryAlphabet in
		let newAccSts = Set.inter rep.acceptStates usfSts in
			{
				entryAlphabet = entryAlf;
				tapeAlphabet = tapeAlf;
				empty = rep.empty;
				states = usfSts;
				initialState = rep.initialState;
				transitions = usfTrs;
				acceptStates = newAccSts;
				criteria = rep.criteria;
				markers = rep.markers
			} 

	let areAllStatesUseful rep =
		let fa = downgradeModelToFiniteAutomaton rep in
			fa#areAllStatesUseful

	let acceptLB w rep = 
		if isLB rep then
			let (leftMarker, rightMarker) = getMarkers rep in
			let newWord = [leftMarker]@w@[rightMarker] in
				accept newWord rep
		else
			false

	let acceptFullLB w rep =
		if isLB rep then
			let (leftMarker, rightMarker) = getMarkers rep in
			let newWord = [leftMarker]@w@[rightMarker] in
				acceptFull newWord rep
		else
			(false,[],[])

	let convertToStopCriteriaOld rep =
		let stEnd = state "END" in
		let endState = stEnd in
		let completeStates = Set.union (rep.states) (Set.make [endState]) in

		let newAlph = Set.union (rep.tapeAlphabet) (Set.make [empty]) in
		let nonAcceptStates =  Set.filter (fun x -> not (Set.exists (fun y -> y = x) rep.acceptStates)) rep.states in

		let missingSymbols st = Set.filter (fun x -> not (Set.exists (fun (a,b,_,_,_) -> a = st && b = x) rep.transitions)) newAlph in
		let createTransitions st = Set.map (fun x -> (st,x,endState,x,R)) (missingSymbols st) in
		let newTransList = Set.flatten (Set.map (fun x -> createTransitions x) nonAcceptStates) in
		let fullTransitions = Set.union (rep.transitions) (newTransList) in
			{
				entryAlphabet = rep.entryAlphabet;
				tapeAlphabet = rep.tapeAlphabet;
				empty = rep.empty;
				states = completeStates;
				initialState = rep.initialState;
				transitions = fullTransitions;
				acceptStates = Set.empty;
				criteria = false;
				markers = rep.markers
			}

	let convertToStopCriteria rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = Set.empty;
			criteria = false;
			markers = rep.markers
		}

	let hasState st rep = 
		Set.belongs st rep.states

	let hasTransition trs rep =
		Set.belongs trs rep.transitions

	let isFinal st rep = 
		Set.belongs st rep.acceptStates

	let isInitial st rep = 
		st = rep.initialState
	
	let addState s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = Set.add s rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}
			
	let addInitialState s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = Set.add s rep.states;
			initialState = s;
			transitions = rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}

	let addFinalState s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = Set.add s rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = Set.add s rep.acceptStates;
			criteria = true;
			markers = rep.markers
		}

	let removeState s rep =
		if s != rep.initialState then
			{
				entryAlphabet = rep.entryAlphabet;
				tapeAlphabet = rep.tapeAlphabet;
				empty = rep.empty;
				states = Set.remove s rep.states;
				initialState = rep.initialState;
				transitions = Set.filter (fun (a,_,c,_,_) -> a = s || c = s) rep.transitions;
				acceptStates = Set.remove s rep.acceptStates;
				criteria = rep.criteria;
				markers = rep.markers
			}
		else 
			rep

	let changeStateToInitial s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = s;
			transitions = rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}

	let changeStateFromFinal s rep =
		let newAcceptSts = Set.remove s rep.acceptStates in
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = newAcceptSts;
			criteria = if (Set.size newAcceptSts) = 0 then false else true;
			markers = rep.markers
		}

	let changeStateToFinal s rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = rep.transitions;
			acceptStates = Set.add s rep.acceptStates;
			criteria = true;
			markers = rep.markers
		}

	let renameState st name rep =
		let initial = if st = rep.initialState then name else rep.initialState in
		let newStates = Set.remove st (Set.add name rep.states) in
		let newTransitions = Set.map (fun (s,a,t,b,c) -> 
			if s = st && t = st then (name,a,name,b,c) 
			else if s = st then (name,a,t,b,c) 
			else if t = st then (s,a,name,b,c) 
			else (s,a,t,b,c)
		) rep.transitions in
		let newAcceptStates = Set.map (fun s -> if s = st then name else s) rep.acceptStates in
			{
				entryAlphabet = rep.entryAlphabet;
				tapeAlphabet = rep.tapeAlphabet;
				empty = rep.empty;
				states = newStates;
				initialState = initial;
				transitions = newTransitions;
				acceptStates = newAcceptStates;
				criteria = true;
				markers = rep.markers
			}

	let addTransition trs rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = Set.add trs rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}

	let removeTransition trs rep =
		{
			entryAlphabet = rep.entryAlphabet;
			tapeAlphabet = rep.tapeAlphabet;
			empty = rep.empty;
			states = rep.states;
			initialState = rep.initialState;
			transitions = Set.remove trs rep.transitions;
			acceptStates = rep.acceptStates;
			criteria = rep.criteria;
			markers = rep.markers
		}

	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super
			val representation: t =
				match arg with
				| Arg.Representation r -> r
				| Arg.RepresentationX r -> internalize r
				| _ -> fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors

			method representation: t =
				representation

			method representationx: tx =
				externalize representation

			method toJSon: JSon.t =
				JSon.append (super#toJSon) (toJSon representation)

			method validate: unit = 
				validate representation self#id
				
			method tracing : unit = 
				()
						
			method acceptOld(w: word): bool =
				acceptOld w representation

			method accept (w: word): bool =
				accept w representation

			method acceptFull (w:word): bool * 'c list * 'c set list =
				acceptFull w representation

			method generate (length: int): words =
				generate length representation

			method reachable (s:state): states =
				reachable s representation

			method productive : states =
				productive representation
				
			method getUsefulStates: states =
				getUsefulStates representation

			method getUselessStates: states =
				getUselessStates representation

			method isDeterministic: bool =
				isDeterministic representation

			method cleanUselessStates: t =
				cleanUselessStates representation

			method areAllStatesUseful: bool =
				areAllStatesUseful representation

			method acceptLB (w: word) : bool =
				acceptLB w representation

			method acceptFullLB (w: word) : bool * 'c list * 'c set list =
				acceptFullLB w representation

			method isLB : bool = 	
				isLB representation

			method convertToStopCriteria: model =
				let tm = convertToStopCriteria representation in
					new model (Arg.Representation tm)

			method hasState(s: state): bool =
				hasState s representation

			method hasTransition (trs: transition): bool =
				hasTransition trs representation

			method isFinal (st: state): bool =
				isFinal st representation

			method isInitial (st: state): bool =
				isInitial st representation

			method addState (s: state) : t =
				addState s representation

			method addInitialState (s: state) : t =
				addInitialState s representation

			method addFinalState (s: state) : t =
				addFinalState s representation
		
			method removeState (s: state) : t =
				removeState s representation

			method changeStateToInitial (s: state) : t =
				changeStateToInitial s representation

			method changeStateToFinal (s: state) : t =
				changeStateToFinal s representation

			method changeStateFromFinal (s: state) : t =
				changeStateFromFinal s representation
			
			method renameState (s:state) (newS:state): t =
				renameState s newS representation

			method addTransition (trs:transition) : t =
				addTransition trs representation

			method removeTransition (trs:transition) : t =
				removeTransition trs representation

			method downgradeModelToFiniteAutomaton: FiniteAutomaton.model =
				downgradeModelToFiniteAutomaton representation
			
			method checkProperty prop =
				match prop with
					| "deterministic" -> self#isDeterministic
					| "linear bounded" -> self#isLB
					| "acceptance by states" -> representation.criteria
					| "acceptance by stop" -> not representation.criteria
					| "turing machine" -> true
					| _ -> super#checkProperty prop
		
		(* Learn-OCaml support *)
		(* incomplete *)
			method moduleName =
				"TuringMachine"

			method xTypeName =
				"turingMachine"
				
			method xTypeDeclString : string = ""

			method toDisplayString (name: string): string = ""

			method example : JSon.t =
				JSon.parse {|
				{
					kind : "turing machine"
				}
			|}
	end
end
	
module TuringMachineTests : sig end =
struct
	let active = true

	(* usar exemplos 1,2,4,10 *)

	(*
	Primeiro exemplo dos slides do professor - Troca os a's pelos b's
	Este exemplo e:
		- determinista
		- nao entra em loop de configuracao
		- nao corre infinitamente sem repetir configuracao
		- nao tem estados useless
		- termina por paragem
	}
	*)
	let tm_astar1 = {| {
		kind: "turing machine",
		description: "this is an example",
		name: "tm_astar1",
		entryAlphabet: ["a", "b"],
		tapeAlphabet: ["a", "b","B"],
		empty: "B",
		states: ["q1", "q2"],
		initialState: "q1",
		transitions: [
			["q1", "B", "q2", "B", "L"],
			["q1", "a", "q1", "b", "R"],
			["q1", "b", "q1", "a", "R"],
			["q2", "a", "q2", "a", "L"],
			["q2", "b", "q2", "b", "L"]
		],
		acceptStates: [],
		criteria: "false",
		markers: [],
		} |}


	(*
	Segundo exemplo dos slides do professor - Transforma a fita BuB em BuBuB
	Este exemplo e:
		- determinista
		- nao entra em loop de configuracao
		- nao corre infinitamente sem repetir configuracao
		- nao tem estados useless
		- termina por paragem
	*)
	let tm_astar2 = {| {
		kind: "turing machine",
		description: "this is an example",
		name: "tm_astar2",
		entryAlphabet: ["a", "b"],
		tapeAlphabet: ["a", "b", "X", "Y","B"],
		empty: "B",
		states: ["q1", "q2", "q3", "q4", "q5", "q6", "q7"],
		initialState: "q1",
		transitions: [
			["q1", "a", "q2", "X", "R"],
			["q1", "b", "q5", "Y", "R"],
			["q1", "B", "q7", "B", "L"],

			["q2", "a", "q2", "a", "R"],
			["q2", "b", "q2", "b", "R"],
			["q2", "B", "q3", "B", "R"],

			["q3", "a", "q3", "a", "R"],
			["q3", "b", "q3", "b", "R"],
			["q3", "B", "q4", "a", "L"],

			["q4", "a", "q4", "a", "L"],
			["q4", "b", "q4", "b", "L"],
			["q4", "B", "q4", "B", "L"],
			["q4", "X", "q1", "X", "R"],
			["q4", "Y", "q1", "Y", "R"],

			["q5", "a", "q5", "a", "R"],
			["q5", "b", "q5", "b", "R"],
			["q5", "B", "q6", "B", "R"],

			["q6", "a", "q6", "a", "R"],
			["q6", "b", "q6", "b", "R"],
			["q6", "B", "q4", "b", "L"],

			["q7", "X", "q7", "a", "L"],
			["q7", "Y", "q7", "b", "L"]
		],
		acceptStates: [],
		criteria: "false",
		markers: []
		} |}

	(*
		Terceiro exemplo dos slides do stor - Aceita a palavra (a + b)*aa(a + b)*
		Este exemplo e:
			- determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por estados de aceitacao
	*)
	let tm_astar3 = {| {
			kind: "turing machine",
			description: "this is an example changed",
			name: "tm_astar3",
			entryAlphabet: ["a", "b"],
			tapeAlphabet: ["a", "b","B"],
			empty: "B",
			states: ["q1", "q2", "q3"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q2", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q2", "a", "q3", "a", "R"],
				["q2", "b", "q1", "b", "R"]
			],
			acceptStates: ["q3"],
			criteria: "true",
			markers: []
			} |}

	(*
		Quarto exemplo dos slides do professor - Aceita a palavra a(i)b(i)c(i) para i >= 0
		Este exemplo e:
			- determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless	
			- termina por estados de aceitacao
	*)
	let tm_astar4 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar4",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "X", "Y", "Z", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4", "q5", "q6"],
			initialState: "q1",
			transitions: [
				["q1", "B", "q6", "B", "R"],
				["q1", "Y", "q5", "Y", "R"],
				["q1", "a", "q2", "X", "R"],

				["q2", "a", "q2", "a", "R"],
				["q2", "Y", "q2", "Y", "R"],
				["q2", "b", "q3", "Y", "R"],

				["q3", "b", "q3", "b", "R"],
				["q3", "Z", "q3", "Z", "R"],
				["q3", "c", "q4", "Z", "L"],

				["q4", "Z", "q4", "Z", "L"],
				["q4", "Y", "q4", "Y", "L"],
				["q4", "b", "q4", "b", "L"],
				["q4", "a", "q4", "a", "L"],

				["q5", "Y", "q5", "Y", "R"],
				["q5", "Z", "q5", "Z", "R"],
				["q5", "B", "q6", "B", "R"]
			],
			acceptStates: ["q6"],
			criteria: "true",
			markers: []
			} |}

	(*
		Quinto exemplo dos slides do professor - Aceita a palavra (a + b)*aa(a + b)* por paragem (Semelhante ao exemplo 3)
		Este exemplo e:
			- nao determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por paragem
	*)
	let tm_astar5 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar5",
			entryAlphabet: ["a", "b"],
			tapeAlphabet: ["a", "b", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q2", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q1", "B", "q4", "B", "R"],

				["q2", "a", "q3", "a", "R"],
				["q2", "b", "q1", "b", "R"],
				["q2", "B", "q4", "B", "R"],

				["q4", "a", "q4", "a", "R"],
				["q4", "b", "q4", "b", "R"],
				["q4", "B", "q4", "B", "R"]
			],
			acceptStates: [],
			criteria: "false",
			markers: []
			} |}

	(*
		Exemplo nao determinista dos slides - Aceita todas as palavras contendo um c precedido ou seguido de ab
		Este exemplo e:
			- nao determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por estados de aceitacao
	*)
	let tm_astar6 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar6",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4", "q5", "q6", "q7"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q1", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q1", "c", "q1", "c", "R"],

				["q1", "c", "q2", "c", "R"],
				["q1", "c", "q5", "c", "L"],

				["q2", "a", "q3", "a", "R"],

				["q3", "b", "q4", "b", "R"],

				["q5", "b", "q6", "b", "L"],

				["q6", "a", "q7", "a", "L"]
			],
			acceptStates: ["q4", "q7"],
			criteria: "true",
			markers: []
			} |}

	(*
		Primeiro exemplo original
		Este exemplo e:
			- nao determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por estado de aceitacao
	*)
	let tm_astar7 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar7",
			entryAlphabet: ["a", "b", "c", "d", "e"],
			tapeAlphabet: ["a", "b", "c", "d", "e", "B"],
			empty: "B",
			states: ["q1", "q2", "q3"],
			initialState: "q1",
			transitions: [
				["q1", "a", "q2", "a", "R"],

				["q1", "a", "q1", "a", "R"],
				["q1", "b", "q1", "b", "R"],
				["q1", "c", "q1", "c", "R"],
				["q1", "d", "q1", "d", "R"],
				["q1", "e", "q1", "e", "R"],

				["q2", "c", "q3", "c", "R"]
			],
			acceptStates: ["q3"],
			criteria: "true",
			markers: []
			} |}

	(*
		Segundo exemplo original
		Este exemplo e:
			- determinista
			- entra em loop de configuracao quando a palavra e vazia -> ""
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por estados de aceitacao
	*)
	let tm_astar8 = {| {
			kind: "turing machine",
			description: "this is an example",
			name: "tm_astar8",
			entryAlphabet: ["a"],
			tapeAlphabet: ["a", "B"],
			empty: "B",
			states: ["q1", "q2", "q3"],
			initialState: "q1",
			transitions: [
				["q1", "B", "q2", "B", "R"],
				["q2", "B", "q1", "B", "L"],

				["q2", "a", "q3", "a", "R"]
			],
			acceptStates: ["q3"],
			criteria: "true",
			markers: []
			} |}

	(*
		Variante do exemplo 4 com estados useless
		Este exemplo e:
			- determinista
			- nao entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- tem estados useless:
				- o estado q7 e unreachable e productive
				- o estado q8 e unreachable e unproductive
				- o estado q9 e reachable e unproductive
				(Os restantes sao todos reachable e productive)
			- termina por estados de aceitacao				
	*)
	let tm_astar9 = {| {
			kind: "turing machine",
			description : "this is an example",
			name: "tm_astar9",
			entryAlphabet: ["a", "b", "c"],
			tapeAlphabet: ["a", "b", "c", "X", "Y", "Z", "B"],
			empty: "B",
			states: ["q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"],
			initialState: "q1",
			transitions: [

				["q1", "B", "q6", "B", "R"],
				["q1", "Y", "q5", "Y", "R"],
				["q1", "a", "q2", "X", "R"],

				["q2", "a", "q2", "a", "R"],
				["q2", "Y", "q2", "Y", "R"],
				["q2", "b", "q3", "Y", "R"],

				["q3", "b", "q3", "b", "R"],
				["q3", "Z", "q3", "Z", "R"],
				["q3", "c", "q4", "Z", "L"],

				["q4", "Z", "q4", "Z", "L"],
				["q4", "Y", "q4", "Y", "L"],
				["q4", "b", "q4", "b", "L"],
				["q4", "a", "q4", "a", "L"],
				
				["q4", "X", "q1", "X", "R"],

				["q5", "Y", "q5", "Y", "R"],
				["q5", "Z", "q5", "Z", "R"],
				["q5", "B", "q6", "B", "R"],

				["q5", "b", "q9", "c", "R"],

				["q7", "b", "q8", "c", "R"],
				["q7", "B", "q6", "B", "R"]

			],
			acceptStates: ["q6"],
			criteria: "true"
			} |}

	(*
		Este exemplo e:
			- determinista
			- nao entra em loop de configuracao
			- corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por paragem
	*)
	let tm_astar10 = {| {
		kind: "turing machine",
		description: "this is an example",
		name: "tm_astar10",
		entryAlphabet: ["a", "b", "c"],
		tapeAlphabet: ["a", "b", "c", "B"],
		empty: "B",
		states: ["q1"],
		initialState: "q1",
		transitions: [
			["q1", "B", "q1", "c", "R"],
			["q1", "a", "q1", "a", "R"],
			["q1", "b", "q1", "b", "R"],
			["q1", "c", "q1", "c", "R"]
		],
		acceptStates: [],
		criteria: "false",
		markers: []
		} |}

	(*
		Este exemplo e:
			- determinista
			- entra em loop de configuracao
			- nao corre infinitamente sem repetir configuracao
			- nao tem estados useless
			- termina por paragem
	*)
	let tm_astar11 = {| {
		kind : "turing machine",
		description : "this is an example",
		name : "tm_astar11",
		entryAlphabet: ["a", "b", "c"],
		tapeAlphabet: ["a", "b", "c", "B"],
		empty: "B",
		states : ["q1", "q2", "q3"],
		initialState : "q1",
		transitions : [
			["q1", "a", "q2", "c", "R"],
			["q1", "b", "q1", "b", "R"],
			["q1", "c", "q1", "a", "R"],
			["q2", "b", "q1", "b", "L"],
			["q2", "c", "q3", "c", "R"]
		],
		acceptStates : [],
		criteria : "false",
		markers: []
		} |}
	(*
		Testar:

		method representation: t
		method representationx: tx
		method toJSon: JSon.t
		method validate: unit 
		method tracing : unit
		method acceptOld(w: word): bool
		method accept (w: word): bool
		method acceptFull (w:word): bool * 'c list * 'c set list
		method generate (length: int): words 
		method reachable (s:state): states
		method productive : states
		method getUsefulStates: states
		method getUselessStates: states
		method isDeterministic: bool
		method cleanUselessStates: t
		method areAllStatesUseful: bool
		method acceptLB (w: word) : bool
		method acceptFullLB (w: word) : bool * 'c list * 'c set list
		method isLB : bool
		method convertToStopCriteria: model
		method addState (s: state) : t
		method addInitialState (s: state) : t
		method addFinalState (s: state) : t
		method removeState (s: state) : t
		method changeStateToInitial (s: state) : t
		method changeStateToFinal (s: state) : t
		method changeStateFromFinal (s: state) : t
		method renameState (s:state) (newS:state): t 
		method addTransition (trs:transition) : t
		method removeTransition (trs:transition) : t
		method downgradeModelToFiniteAutomaton: FiniteAutomaton.model

		Tipos de maquina:
		 - Determinista ou Nao
		 - Entra em loop de configuracoes ou Nao
		 - Tem estados useless ou nao
		 - Corre infinitamente sem repetir configuracao ou nao
	*)
	let print_init_msg b =
		String.concat " " ["current function:"; b]

	let check_accept f w =
		let msg = if f w then "true" else "false" in 
			Util.println [msg]

	let check_ret_bool f =
		let msg = if f then "true" else "false" in 
			Util.println [msg]

	let check_reachable f st =
		let sts : states = f st in
			Util.printStates sts

	let check_ret_states f =
		let sts : states = f in
			Util.printStates sts

	
	let test0 () =
		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
			Util.println ["++++++++++++++++++"];
			check_accept tm3#acceptOld (word "baaa");
			check_accept tm3#accept (word "baaa");
			check_accept tm3#acceptOld (word "bab");
			check_accept tm3#accept (word "bab")
(*
	let test_representation () =
	let test_representationx () =
	let test_toJSon () =
	let test_validate () =
	let test_tracing () +
	let test_acceptOld () =
	let test_accept () =
	let test_acceptFull () =
	let test_generate () =
	let test_reachable () =
	let test_productive () =
	let test_getUsefulStates () =
	let test_getUselessStates () =
	let test_isDeterministic () =
	let test_cleanUselessStates () =
	let test_areAllStatesUseful () =
	let test_acceptLB () =
	let test_acceptFullLB () =
	let test_isLB () =
	let test_convertToStopCriteria () =
	let test_addState () =
	let test_addInitialState () =
	let test_addFinalState () =
	let test_removeState () =
	let test_changeStateToInitial () =
	let test_changeStateToFinal () =
	let test_changeStateFromFinal () =
	let test_renameState () =
	let test_addTransition () =
	let test_removeTransition () =
	let test_downgradeModelToFiniteAutomaton () =
*)

(*
	For the accept, we will test if it accepts a word,
	if an automata gets stuck repeating the same configuration
	or simply keeps going infinetly
*)
	
	let testAccept () =
		Util.println [print_init_msg "accept"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm5 = new TuringMachine.model (Arg.Text tm_astar5) in
		let tm10 = new TuringMachine.model (Arg.Text tm_astar10) in
		let tm11 = new TuringMachine.model (Arg.Text tm_astar11) in

			(* 
				Test accepts word / stop by acceptence
				Expeted: q3, true
			*)
			check_accept tm3#acceptOld (word "baaa");
			(* 
				Test NOT accepts word
				Expected: q1, false
			*)
			check_accept tm3#acceptOld (word "bbb");

			(* 
				Test accepts word / no accept states
				Expected: q3, true
			*)
			check_accept tm5#accept (word "aaa");
			(*
				Test NOT accepts word / no replace states (expected to be impossible)
				Expected: q1, true
			*)
			check_accept tm5#acceptOld (word "abbcc");
			(* 
				Test runs infinitely
				Expected: ~, false
			*)
			check_accept tm10#acceptOld (word "a");
			(* 
				Test get stuck in configuration loop 
				Expected: ~, false
			*)
			check_accept tm11#acceptOld (word "ab");
			(*
				Test does NOT get stuck in configuration loop
				Expected: true
			*)
			check_accept tm11#acceptOld (word "ac")

			

	let testReachable () =
		Util.println [print_init_msg "reachable"];

		let tm4 = new TuringMachine.model (Arg.Text tm_astar4) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in

		 	(*
				Expected: q2, q3, q4	 
			*)
			check_reachable tm4#reachable (state "q2");
			(*
				Expected: q2, q3, q4, q5, q6, q9	 
			*)
			check_reachable tm9#reachable (state "q1");
			(*
				Expected: q7, q8, q6	 
			*)
			check_reachable tm9#reachable (state "q7")
			

	let testProductive () =
		Util.println [print_init_msg "productive"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm6 = new TuringMachine.model (Arg.Text tm_astar6) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in
			(*
				Expected:	 q1,q2
			*)
			check_ret_states tm3#productive;
			(*
				Expected:	 q1,q2,q3,q4,q5
			*)
		  check_ret_states tm6#productive;
			(*
				Expected:	 q1,q2,q3,q4,q5,q7
			*)
			check_ret_states tm9#productive

	let testGetUsefulStates () =
		Util.println [print_init_msg "getUsefulStates"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm6 = new TuringMachine.model (Arg.Text tm_astar6) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in

			(*
				Expected:	 q1,q2,q3
			*)
			check_ret_states tm3#getUsefulStates;
			(*
				Expected:	 q1,q2,q3,q4,q5,q6,q7
			*)
		  check_ret_states tm6#getUsefulStates;
			(*
				Expected:	 q1,q2,q3,q4,q5,q6
			*)
			check_ret_states tm9#getUsefulStates

	let testGetUselessStates () =
		Util.println [print_init_msg "getUselessStates"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm6 = new TuringMachine.model (Arg.Text tm_astar6) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in

			(*
				Expected:	
			*)
			check_ret_states tm3#getUselessStates;
			(*
				Expected:	 
			*)
		  check_ret_states tm6#getUselessStates;
			(*
				Expected:	 q1,q2,q3,q4,q5,q6
			*)
			check_ret_states tm9#getUselessStates

	let testIsDeterministic () =
		Util.println [print_init_msg "isDeterministic"];

		let tmND = new TuringMachine.model (Arg.Text tm_astar6) in
		let tmD = new TuringMachine.model (Arg.Text tm_astar4) in

			(*
				Expected:	 false
			*)
			check_ret_bool tmND#isDeterministic;
			(*
				Expected:	 true
			*)
			check_ret_bool tmD#isDeterministic

	let testAreAllStatesUseful () =
		Util.println [print_init_msg "areAllStatesUseful"];

		let tm3 = new TuringMachine.model (Arg.Text tm_astar3) in
		let tm6 = new TuringMachine.model (Arg.Text tm_astar6) in
		let tm9 = new TuringMachine.model (Arg.Text tm_astar9) in

			(*
				Expected:	 true
			*)
			check_ret_bool tm3#areAllStatesUseful;
			(*
				Expected:	 true
			*)
			check_ret_bool tm6#areAllStatesUseful;
			(*
				Expected:	 false
			*)
			check_ret_bool tm9#areAllStatesUseful
		
	let testCleanUselessStates () =
		let tm = new TuringMachine.model (Arg.Text tm_astar9) in
			Util.println [print_init_msg "areAllStatesUseful"];
			let ntm = new TuringMachine.model (Arg.Representation tm#cleanUselessStates) in
				let j = ntm#toJSon in
					JSon.show j;
					Util.println []

	let runAll =
		if Util.testing active "TuringMachine" then begin
			Util.header "TuringMachineTests starting...";
			test0 ();
			(*
				testCleanUselessStates ();
				testAccept ();
				testReachable ();
				testProductive ();
				testGetUsefulStates ();
				testGetUselessStates ();
				testIsDeterministic ();
				testAreAllStatesUseful ();
				testCleanUselessStates ();
			*)
		end
end


# 3 "src/PushdownAutomaton.ml"
(*
 * PushdownAutomaton.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Carlos Freitas (cf)
 *)

(*
 * ChangeLog:
 *
 * ???/2022 (cf) - ???.
 * may/2022 (amd) - Initial skeleton.
 *)

(*
 * Description: Pushdown automata functionality.
 *)

open BasicTypes
open Util

module type PushdownAutomatonSig = sig

	type transition = state * symbol * symbol * state * symbol list
	type transitions = transition set
	type tx = {
		inputAlphabet: symbol list;
		stackAlphabet: symbol list;
		states: state list;
		initialState: state;
		initialStackSymbol: symbol;
		transitions: transition list;
		acceptStates: state list;
		criteria: bool
	}
	type t = {
		inputAlphabet : symbols;
		stackAlphabet : symbols;
		states : states;
		initialState : state;
		initialStackSymbol : symbol;
		transitions : transitions;
		acceptStates : states;
		criteria: bool; (* true = acceptStates | false = emptyStack *) 
	}
	val modelDesignation : string
	class model :
		(t,tx) Arg.alternatives ->
			object
				method id: Entity.t
				method errors : string list
				method handleErrors : unit
				method toJSon: JSon.t
				method representation : t
				method representationx : tx
				method validate : unit
				
				method tracing : unit

				method accept : word -> bool
				method generate : int -> words	
			
			(* Exercices support *)
				method checkProperty : string -> bool
				method checkExercise : Exercise.exercise -> bool
				method checkExerciseFailures : Exercise.exercise
											-> words * words * properties

			(* Learn-OCaml support *)
				method moduleName : string
				method xTypeName : string
				method xTypeDeclString : string
				method toDisplayString : string -> string
				method example : JSon.t			end
end

module PushdownAutomaton : PushdownAutomatonSig =
struct

	type transition =
		  state			(* state *)	
		* symbol		(* current symbol on top of the stack *)
		* symbol		(* consumed input symbol *)
		* state			(* next state *)
		* symbol list	(* new top of stack*)

	type transitions = transition set

	type tx = {
		inputAlphabet: symbol list;
		stackAlphabet: symbol list;
		states: state list;
		initialState: state;
		initialStackSymbol: symbol;
		transitions: transition list;
		acceptStates: state list;
		criteria: bool
	}
	
	type t = {
		inputAlphabet: symbols;		(* Input Alphabet *)
		stackAlphabet: symbols;		(* Stack Alphabet *)
		states: states;				(* States *)
		initialState: state;		(* Initial state *)
		initialStackSymbol: symbol;	(* Initial Symbol on the Stack *)
		transitions: transitions;	(* Transition relation *)
		acceptStates: states;		(* Accept states *)
		criteria: bool				(* true = acceptStates | false = emptyStack *)
	}

	let modelDesignation = "pushdown automaton"

	let internalize (pda: tx): t = {
		inputAlphabet = Set.make pda.inputAlphabet;
		stackAlphabet = Set.make pda.stackAlphabet;
		states = Set.make pda.states;
		initialState = pda.initialState;
		initialStackSymbol = pda.initialStackSymbol;
		transitions = Set.make pda.transitions;
		acceptStates = Set.make pda.acceptStates;
		criteria = pda.criteria
	}

	let externalize (pda: t): tx = {
		inputAlphabet = Set.toList pda.inputAlphabet;
		stackAlphabet = Set.toList pda.stackAlphabet;
		states = Set.toList pda.states;
		initialState = pda.initialState;
		initialStackSymbol = pda.initialStackSymbol;
		transitions = Set.toList pda.transitions;
		acceptStates = Set.toList pda.acceptStates;
		criteria = pda.criteria
	}

	let fromJSon (j: JSon.t): t =
		if JSon.isNull j || not (JSon.hasField j "kind") then {			
			inputAlphabet = Set.empty;
			stackAlphabet = Set.make [draftVar];
			states = Set.make [draftState];
			initialState = draftState;
			initialStackSymbol = draftVar;
			transitions = Set.empty;
			acceptStates = Set.empty;
			criteria = false
		}
		else {
			inputAlphabet = JSon.fieldSymbolSet j "inputAlphabet";
			stackAlphabet = JSon.fieldSymbolSet j "stackAlphabet";
			states = JSon.fieldStateSet j "states";
			initialState = JSon.fieldState j "initialState";
			initialStackSymbol = JSon.fieldSymbol j "initialStackSymbol";
			transitions = JSon.fieldQuintupletsSet j "transitions";
			acceptStates = JSon.fieldStateSet j "acceptStates";
			criteria = JSon.fieldBool j "criteria"
		}

	let toJSon (rep: t): JSon.t =
		JSon.makeAssoc [
			("inputAlphabet", JSon.makeSymbolSet rep.inputAlphabet);
			("stackAlphabet", JSon.makeSymbolSet rep.stackAlphabet);
			("states", JSon.makeStateSet rep.states);
			("initialState", JSon.makeState rep.initialState);
			("initialStackSymbol", JSon.makeSymbol rep.initialStackSymbol);
			("transitions", JSon.makeQuintupletsSet rep.transitions);
			("acceptStates", JSon.makeStateSet rep.acceptStates);
			("criteria", JSon.makeBool rep.criteria)
		]
	
			
	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit Model.model arg modelDesignation as super

			val representation: t =
				match arg with
				| Arg.Representation r -> r
				| Arg.RepresentationX r -> internalize r
				| _ -> fromJSon (Arg.fromAlternatives arg)

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation: t =
				representation

			method representationx: tx =
				externalize representation

			method toJSon: JSon.t =
				JSon.append (super#toJSon) (toJSon representation)

			method validate: unit = ()

			method tracing : unit = ()
						
			method accept(w: word): bool =
				false

			method generate (length: int): words =
				Set.empty	
		
		(* Learn-OCaml support *)
		(* incomplete *)
			method moduleName =
				"PushdownAutomaton"

			method xTypeName =
				"pushdownAutomaton"
				
			method xTypeDeclString : string = ""

			method toDisplayString (name: string): string = ""

			method example : JSon.t =
				JSon.parse {|
				{
					kind : "pushdown automaton"
				}
			|}
	end
end
	
module PushdownAutomatonTests : sig end =
struct
	let active = false

	let pda_astar = {| {
			kind : "pushdown automaton",
			description : "this is an example",
			name : "dfa_astar",
			alphabet: ["a"],
			states : ["START"],
			initialState : "START",
			transitions : [
				["START", "a", "START"]
			],
			acceptStates : ["START"]
			} |}

	let aNbNExample = {| {
		kind : "pushdown automaton",
		description : "this is an example for anbn",
		name : "anbn_astar",
		inputAlphabet: ["a", "b", "_"],
		stackAlphabet: ["a", "z"],
		states: ["p", "q"],
		initialState: "p",
		initialStackSymbol: "z",
		transitions: [
			["p", "a", "a", "p", "aa"],
			["p", "z", "a", "p", "za"],
			["p", "a", "b", "q", "B"],
			["q", "a", "b", "q", "B"],
			["q", "z", "_", "q", "B"]
		],
		acceptStates: [],
		criteria: "false"
		} |}

	let generateTransitionsToPD st alphEntr alphPD =
		let allAlph = Set.add dollar (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,symb,st,symb,R)) allAlph 

	let generateTransitionsFromPD st alphEntr alphPD =
		let allAlph = Set.add dollar (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,symb,st,symb,L)) allAlph 

	let insertSymbolsPD alphEntr (rep: PushdownAutomaton.t) =
		let alphPD = rep.stackAlphabet in
		let st1 = state (IdGenerator.gen("q")) in
		let st2 = state (IdGenerator.gen("q")) in
		let st3 = state (IdGenerator.gen("q")) in
		let newSts = Set.add st1 ( Set.add st2 ( Set.add st3 Set.empty)) in
		let newTrs1 = Set.union (generateTransitionsToPD st1 alphEntr alphPD) (generateTransitionsFromPD st3 alphEntr alphPD) in
		let newTrs2 = Set.add (st1,empty,st2,symb "$",R) (Set.add (st2,empty,st3,rep.initialStackSymbol,R) ( Set.add (st3,empty,rep.initialState,empty,R)  newTrs1 )) in
			(Set.union rep.states newSts) , newTrs2

	let rec fillStackTransition lastSt prevSt trs wordL =
		match wordL with
		| [] -> trs
		| x::y ->	let newState = if (Set.isEmpty (Set.make y)) then lastSt else IdGenerator.gen("q") in
							let dir = if (Set.isEmpty (Set.make y)) then L else R in
								fillStackTransition lastSt newState (Set.add (prevSt,empty, newState, x, dir) trs) y 

	let convertNormalTransition trs alphEntr alphPD =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = trs in

		let st1 = state (IdGenerator.gen("q")) in
		let st2 = state (IdGenerator.gen("q")) in
		let st3 = state (IdGenerator.gen("q")) in

		let ftrs = (startState,readSymbol,st1,empty,R) in
		let trsTPD = Set.add ftrs (generateTransitionsToPD st1 alphEntr alphPD) in
		let trsRTOP = Set.add (st1,empty,st2,empty,L) trsTPD in

		let firstDirection = if ((List.length writeSymbolL) = 1) then L else R in
		let lastSt = if ((List.length writeSymbolL) = 1) then st3 else state (IdGenerator.gen("q")) in

		let replaceTop = Set.add (st2,unstackedSymbol,st3, (List.hd writeSymbolL), firstDirection) trsRTOP in
		let additionalSymbolTrs = Set.union replaceTop (fillStackTransition lastSt st3 Set.empty (List.tl writeSymbolL)) in
		let trsFPD = Set.union additionalSymbolTrs (generateTransitionsFromPD lastSt alphEntr alphPD) in
		let trsLast = Set.add (lastSt,empty,nextState,empty,R) trsFPD in
			Set.add lastSt (Set.add st3 (Set.add st2 (Set.add st1 Set.empty))), trsLast

	let convertAcceptTransition trs alphEntr alphPD initialStackSymb =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = trs in

		let st1 = state (IdGenerator.gen("q")) in
		let st2 = state (IdGenerator.gen("q")) in
		let st3 = state (IdGenerator.gen("q")) in
		
		let ftrs = Set.add (startState,dollar,st1,dollar,R) Set.empty in
		let checkInitSS = Set.add (st1,initialStackSymb,st2,empty,R) ftrs in
		let lastCheck = Set.add (st2,empty,st3,empty,R) checkInitSS in
			Set.add st3 (Set.add st2 (Set.add st1 Set.empty)), lastCheck

	let convertTransitionX trs alphEntr alphPD initialStackSymb = 
		let (_,_,readSymbol,_,_) = trs in
			if readSymbol == draftVar then convertAcceptTransition trs alphEntr alphPD initialStackSymb
			else convertNormalTransition trs alphEntr alphPD

	let rec convertTransitions newSts newTrs alphEntr (rep: PushdownAutomaton.t) trs = 
		let alphPD = rep.stackAlphabet in
		let initialStackSymb = rep.initialStackSymbol in
		if (Set.size trs) = 0 then newSts, newTrs
		else 
			let (nSts,nTrs) = convertTransitionX (Set.hd trs) alphEntr alphPD initialStackSymb in
				convertTransitions (Set.union nSts newSts) (Set.union nTrs newTrs) alphEntr rep (Set.tl trs)


(*Se parar por pilha vazia 'e ncess'ario criar um estado final*)

	let getFinalStates trs =
		Set.map (fun (_,_,_,d,_) -> d) (Set.filter (fun (_,_,c,_,_) -> c = dollar) trs)

	let pda2tm (pda: PushdownAutomaton.model) =
		let rep: PushdownAutomaton.t = pda#representation in
		let pdaAlphabet = Set.remove draftVar rep.inputAlphabet in
		let (initialStates, initialTransitions) = insertSymbolsPD pdaAlphabet rep in
		let (convertedTransitionStates,convertedTransitions) = convertTransitions Set.empty Set.empty pdaAlphabet rep rep.transitions in
		let allAlphabet = Set.add dollar ( Set.union pdaAlphabet rep.stackAlphabet) in
		let allStates = Set.union initialStates convertedTransitionStates in
		let allTransitions = Set.union initialTransitions convertedTransitions in
		let allAcceptStates = Set.union rep.acceptStates (getFinalStates rep.transitions) in
		let tm: TurMachTypes.t = {
						entryAlphabet = rep.inputAlphabet;
						tapeAlphabet = allAlphabet;
						empty = empty;
						states = allStates;
						initialState = state "q00";
						transitions = allTransitions;
						acceptStates = allAcceptStates;
						criteria = true;
						markers = Set.empty
					}	in
		new TuringMachine.model (Arg.Representation tm)
	
	let test0 () = (* not a pushdown automaton - will change*)
		let pda = new PushdownAutomaton.model (Arg.Text pda_astar) in
			let j = pda#toJSon in
				JSon.show j

	let test1 () =
		let pda = new PushdownAutomaton.model (Arg.Text aNbNExample) in
		let tm = pda2tm pda in
		let j = tm#toJSon in
			JSon.show j

	let runAll =
		if Util.testing active "PushdownAutomaton" then begin
			Util.header "PushdownAutomatonTests starting...";
			test1 ()
		end
end

# 1 "src/PolyModel.ml"
(*
 * PolyModel.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by João Gonçalves (jg)
 *)

(*
 * ChangeLog:
 *
 * apr/2021 (amd) - Several new build functions.
 * jan/2021 (amd) - Created this module, collecting all the operation.
                    involving two or more kinds of models.
                    This allows to got rid of the mutual recursion between
                    modules, allowing storing each module in a different file.
 * dec/2019 (jg) - Initial code, across several modules in file "OCamlFlat.ml".
 *)

(*
 * Description: Poly-model operations.
 *
 * TODO: Cleanup.
 *)
 
open BasicTypes
open Util

module type PolyModelSig =
sig
	val json2model : JSon.t -> Model.model
	val text2model : string -> Model.model
	val file2model : string -> Model.model
	val example2model : string -> Model.model
	val re2fa : RegularExpression.model -> FiniteAutomaton.model
	val fa2re : FiniteAutomaton.model -> RegularExpression.model
	val fa2tm : FiniteAutomaton.model -> TuringMachine.model
	val re2tm : RegularExpression.model -> TuringMachine.model
	val re2cfg : RegularExpression.model -> ContextFreeGrammar.model
	val fa2cfg : FiniteAutomaton.model -> ContextFreeGrammar.model
	val cfg2fa : ContextFreeGrammar.model -> FiniteAutomaton.model
	val cfg2re : ContextFreeGrammar.model -> RegularExpression.model
	val cfg2tm : ContextFreeGrammar.model -> TuringMachine.model

	
end

module PolyModel : PolyModelSig =
struct

	let json2model (j: JSon.t): Model.model =	(* will build any model *)
		let kind = JSon.fieldString j "kind" in
			if FiniteAutomaton.modelDesignation = kind then
				(new FiniteAutomaton.model (Arg.JSon j) :> Model.model)
			else if RegularExpression.modelDesignation = kind then
				(new RegularExpression.model (Arg.JSon j) :> Model.model)
			else if ContextFreeGrammar.modelDesignation = kind then
				(new ContextFreeGrammar.model (Arg.JSon j) :> Model.model)
			else if FiniteEnumeration.modelDesignation = kind then
				(new FiniteEnumeration.model (Arg.JSon j) :> Model.model)
			else
				(new FiniteAutomaton.model (Arg.JSon j) :> Model.model)

	let text2model (text: string): Model.model =	(* will build any model *)
		json2model (JSon.parse text)

	let file2model (filename: string): Model.model =	(* will load any model *)
		json2model (JSon.fromFile filename)

	let example2model (name: string): Model.model =	(* will load any model *)
		text2model (Examples.example name)

(**
	* This method converts the automaton into a regular expression that accepts its language, by
	* using the transitive closure algorithm
	*
	* @returns RegularExpression.model -> the resulting regular expression
	*
	* Desc: The resulting expression is not minimal
	*)

		(* This method converts the regular expression to its equivalent finite automaton
	*
	* @returns FiniteAutomaton.model -> the resulting finite automaton
	*)
	let re2fa re =
		let open FiniteAutomaton in
		let open RegExpSyntax in

		(*auxiliary var for genName function*)
		let k = ref 0 in

		(*for each new state, generates a name that will distinguish it from all the other generated states *)
		let genName () =
			let n = !k in
			let () = k:= n + 1 in
				(*easy way of having all single digit state names have a zero before their actual number*)
				let name = if n > 9 then "new_St" ^ (string_of_int n)
							else "new_St0" ^ (string_of_int n) in
					str2state name in


		let rec compile (rep: RegExpTypes.t) : FinAutTypes.t =
			match rep with
				| Plus(l, r) ->
						let fa1 = compile l in
						let fa2 = compile r in
						let newStart = genName () in
						let newSts = Set.add newStart (Set.union fa1.states fa2.states) in
						let newAccSts = Set.union fa1.acceptStates fa2.acceptStates in
						let newTran1 = (newStart, epsilon, fa1.initialState) in
						let newTran2 = (newStart, epsilon, fa2.initialState) in
						let newTrans = Set.add newTran1 (Set.add newTran2
							(Set.union fa1.transitions fa2.transitions)) in
						let newAlf = Set.union fa1.alphabet fa2.alphabet in

							{alphabet = newAlf; states = newSts; initialState = newStart;
								transitions = newTrans; acceptStates = newAccSts}

				| Seq(l, r) ->
						let fa1 = compile l in
						let fa2 = compile r in
						let ist = fa1.initialState in
						let sts = Set.union fa1.states fa2.states in
						let asts = fa2.acceptStates in
						let newTrns = Set.map (fun x -> (x, epsilon, fa2.initialState) ) fa1.acceptStates in
						let trns = Set.union newTrns (Set.union fa1.transitions fa2.transitions) in
						let alf = Set.union fa1.alphabet fa2.alphabet in

							{alphabet = alf; states = sts; initialState = ist;
								transitions = trns; acceptStates = asts}

				| Star(r) ->
						let fa = compile r in
						let newStart = genName () in
						let newSts = Set.add newStart fa.states in
						let newTrns = Set.map (fun st -> (st, epsilon, newStart)) fa.acceptStates in
						let allNewTrns = Set.add (newStart, epsilon, fa.initialState) (Set.union newTrns fa.transitions) in

							{alphabet = fa.alphabet; states = newSts; initialState = newStart;
								transitions = allNewTrns; acceptStates = Set.make [newStart]}

				| Symb(c) ->
						let newStart = genName () in
						let newAcc = genName () in
						let newSts = Set.make [newStart; newAcc] in
						let newTrn = Set.make [(newStart, c, newAcc)] in

							{alphabet = Set.make [c]; states = newSts; initialState = newStart;
								transitions = newTrn; acceptStates = Set.make [newAcc]}

				| Empty ->
						let newStart = genName () in

								{alphabet = Set.empty; states = Set.make [newStart]; initialState = newStart;
									transitions = Set.empty; acceptStates = Set.make [newStart]}

				| Zero ->
						let newStart = genName () in

							{alphabet = Set.empty; states = Set.make [newStart]; initialState = newStart;
									transitions = Set.empty; acceptStates = Set.empty}
		in

			new FiniteAutomaton.model (Arg.Representation (compile re#representation))

	let fa2tm (fa : FiniteAutomaton.model) = 
		let rep: FinAutTypes.t = fa#representation in
		let transitionsFa2Tm trns = Set.map (fun (a,b,c) -> (a,b,c,b,R)) trns in
		let tm: TurMachTypes.t = {
						entryAlphabet = rep.alphabet;
						tapeAlphabet = rep.alphabet;
						empty = empty;
						states = rep.states;
						initialState = rep.initialState;
						transitions = transitionsFa2Tm rep.transitions;
						acceptStates = rep.acceptStates;
						criteria = true;
						markers = Set.empty
					}	in
		new TuringMachine.model (Arg.Representation tm)

	let re2tm (re: RegularExpression.model) =
		let reFA = re2fa re in
			fa2tm reFA

	(* Fazer conversao de TM para FA, quando as condicoes se reunem*)
 (*
	let generateTransitionsToPD st alphEntr alphPD =
		let allAlph = Set.add "$" (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,symb,st,symb,R)) allAlph 

	let generateTransitionsFromPD st alphEntr alphPD =
		let allAlph = Set.add "$" (Set.union alphEntr alphPD) in
			Set.map (fun symb -> (st,symb,st,symb,L)) allAlph 

	let insertSymbolsPD alphEntr alphPD initSymbPD initState sts trs =
		let newSts = ["q0", "q1","q2"] in
		let newTrs = Set.union (Set.union (generateTransitionsToPD "q0" alphEntr alphPD) [("q0","B","q1","$",R); ("q1","B","q2",initSymbPD,R); ("q2","B",initState,"B",R)]) (generateTransitionsFromPD "q2" alphEntr alphPD) in
			(Set.union sts newSts) , (Set.union trs newTrs)

	let fillStackTransition stLast prevSt trs wordL = 
		match wordL with
		| [] -> trs
		| x::y ->	let newState = if (Set.isEmpty y) then stLast else IdGenerator.gen("q") in
							let dir = if (Set.isEmpty y) then L else R in
								fillStackTransition newState (Set.union trs (prevSt, "B", newState, x, dir)) y

	let convertTransitionX trs alphEntr alphPD initialStackSymb = 
		let (_,readSymbol,_,_,_) = trs in
			if readSymbol == dollar then convertAcceptTransition trs alphEntr alphPD initialStackSymb
			else convertNormalTransition trs alphEntr alphPD 

	let convertNormalTransition trs alphEntr alphPD =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = trs in

		let st1 = IdGenerator.gen("q") in
		let st2 = IdGenerator.gen("q") in
		let st3 = IdGenerator.gen("q") in
		let st4 = IdGenerator.gen("q") in

		let ftrs = (startState,readSymbol,st1,"B",R) in
		let trsTPD = Set.add ftrs (generateTransitionsToPD st1 alphEntr alphPD) in
		let trsRTOP = Set.add (st1,"B",st2,"B",L) trsTPD in

		let firstDirection = if (writeSymbolL.length == 1) then L else R in
		let lastSt = if (writeSymbolL.length == 1) then st3 else st4 in

		let replaceTop = Set.add (st2,unstackedSymbol,st3,writeSymbolL.hd, firstDirection) trsRTOP in
		let additionalSymbolTrs = Set.union replaceTop (fillStackTransition lastSt st3 Set.empty writeSymbolL.tl) in
		let trsFPD = Set.union additionalSymbolTrs (generateTransitionsFromPD lastSt alphEntr alphPD) in
		let trsLast = Set.add (stLast,"B",nextState,"B",R) trsFPD in
			Set.add lastSt (Set.add st3 (Set.add st2 (Set.add st1 Set.empty))), trsLast

	let convertAcceptTransition trs alphEntr alphPD initialStackSymb =
		let (startState,unstackedSymbol,readSymbol,nextState,writeSymbolL) = trs in

		let st1 = IdGenerator.gen("q") in
		let st2 = IdGenerator.gen("q") in
		let st3 = IdGenerator.gen("q") in
		
		let ftrs = Set.union (startState,dollar,st1,dollar,R) Set.empty in
		let checkInitSS = Set.union (st1,initialStackSymb,st2,"B",R) ftrs in
		let lastCheck = Set.union (st2,"B",st3,"B",R) checkInitSS in
			Set.add st3 (Set.add st2 (Set.add st1 Set.empty)), lastCheck

	let convertTransitions newSts newTrs trs alphEntr alphPD initialStackSymb = 
		match trs with
		| [] -> newSts, newTrs
		| x::y -> let (nSts,nTrs) = convertTransitionX x alphEntr alphPD initialStackSymb in
								convertTransitions (Set.union nSts newSts) (Set.union nTrs newTrs) trs alphEntr alphPD initialStackSymb

(*Se parar por pilha vazia 'e ncess'ario criar um estado final*)

	let getFinalStates fsts trs =
		Set.map (fun (_,_,_,d,_) -> d) (Set.filter (fun (_,_,c,_,_) -> b == dollar) trs)

	let pda2tm (pda: PushdownAutomaton.model) =
		IdGenerator.reset();;
		let rep: FinAutTypes.t = pda#representation in
		let (initialStates, initialTransitions) = insertSymbolsPD rep.inputAlphabet rep.stackAlphabet rep.initialStackSymbol rep.initialState rep.states rep.transitions in
		let (convertedTransitionStates,convertedTransitions) = convertTransitions Set.empty Set.empty rep.transitions rep.inputAlphabet rep.stackAlphabet rep.initialStackSymbol in
		let allAlphabet = Set.union rep.inputAlphabet rep.stackAlphabet in
		let allStates = Set.union initialStates convertedTransitionStates in
		let allTransitions = Set.union initialTransitions convertedTransitions in
		let allAcceptStates = Set.union rep.acceptStates (getFinalStates rep.transitions) in
		let tm: TurMachTypes.t = {
						alphabet = allAlphabet;
						states = allStates;
						initialState = rep.initialState;
						transitions = allTransitions;
						acceptStates = rep.acceptStates;
						criteria = true
					}	in
		new TuringMachine.model (Arg.Representation tm)
	*)
	(*
		module IdGenerator =
		struct
			let current = ref 0;;

			let reset () =
				current := 0

			let gen (s: string) =
				let res = Printf.sprintf "%s%02d" s (!current) in
					current := !current+1;
					res
		end
	*)

	let fa2reMake fa =
		let open FinAutTypes in
		let open RegExpTypes in
		(* Since the algorithm only works for deterministic automaton, we first convert it
			to its deterministic equivalent *)
		let fa = fa#toDeterministic in

		let rep = fa#representation in

		let sts = rep.states in
		let trns = rep.transitions in

		(* transforms the set of expressions into the regex: plus of all expressions of the set *)
		let rec plusSet reSet =
			let open RegExpTypes in
			let rec pls l =
				match l with
					[] -> Zero
					| x::xs -> if xs = [] then x else Plus (x, pls xs)
			in
				pls (Set.toList reSet)
		in

		(* For the given i and j, returns the value of R when k is zero.
			Note that k will always be 0 when called inside this method *)
		let calczerok k i j =
			let ts = Set.filter (fun (a,_,b) -> i = a && j = b) trns in
			if ts <> Set.empty then
				if i <> j then
					let res = Set.map (fun (_,c,_) -> Symb c) ts in
						(k,i,j,plusSet res)
				else
					let res = Set.map (fun (_,c,_) -> Symb c) ts in
					let re = Plus(Empty, (plusSet res)) in
						(k,i,j,re)

			else (k,i,j,Zero)
		in


		(* For the given i and j, returns the value of R when k is not zero. *)
		let calck k i j prvK =
			let getRij i j =
				let r = Set.nth (Set.filter (fun (_,x,y,_) -> x = i && y = j) prvK) 0 in
					(fun (_,_,_,re) -> re) r
			in
			let assembleRe st i j =
				let rik = getRij i st in
				let rkk = Star (getRij st st) in
				let rkj = getRij st j in
					Seq(rik, Seq(rkk,rkj))
			in

			let rij = getRij i j in
			let rikjs = Set.map (fun st -> assembleRe st i j) sts in
			let rikj = plusSet rikjs in
				(k,i,j,Plus(rij,rikj))

		in

		(* Main function that applies previous 2 functions to all possible i and j pairs *)
		let rec rkij k =
			if k < 1 then
				Set.map (fun (i,j) -> calczerok k i j) (Set.combinations sts sts)
			else
				let prvK = rkij (k-1) in
					Set.map (fun(i,j) -> calck k i j prvK) (Set.combinations sts sts)
		in

		let allRks = rkij (Set.size sts) in
		let result = Set.filter (fun (_,i,j,_) -> i = rep.initialState && Set.belongs j rep.acceptStates ) allRks in
		let res = Set.map (fun (_,_,_,re) -> re) result in
		let newRe = plusSet res in
			newRe

	let fa2re fa =
		let re = fa2reMake fa in
			new RegularExpression.model (Arg.Representation re)





		(* This function converts a regular expression to its equivalent regular grammar
		*
		* @returns FiniteAutomaton.model -> the resulting regular grammar
		*)


	let re2cfg re =
		let open ContextFreeGrammar in
		let open CFGSyntax in

		(*auxiliary var for genVar function*)
		let k = ref 0 in

		(* generates new unused variable name for the cfg *)
		let genVar () =
			let n = !k in
			let () = k:= n + 1 in
			let ascii = 65 + n in
			if ascii < 65 || ascii > 90
			then char2symb 'A'
			else char2symb (Char.chr ascii)
		in

		(*
		let convertPlsRules rl i1 i2 newInit =
			(* swaps the initial variables of both old cfgs for the new initial var *)
			let swapInits c = if c = i1 || c = i2 then newInit else c in

			let newBody b = List.map (fun c -> swapInits c) b in
			let newRule r = {head = swapInits r.head; body = newBody r.body} in

				Set.map (fun r -> newRule r) rl

		in
		*)

		(* create gcf rules for plus expression *)
		let convertPlsRules rl i1 i2 newInit =
			let open CFGTypes in
			let newRule1 = {head = newInit; body = [i1]} in
			let newRule2 = {head = newInit; body = [i2]} in

				Set.add newRule1 (Set.add newRule2 rl)

		in

		(* create gcf rules for seq expression *)
		let convertSeqRules lcfg rcfg =
			let open CFGTypes in
			let rl1 = lcfg.rules in
			let rl2 = rcfg.rules in
			let alp1 = lcfg.alphabet in
			let rl = Set.union rl1 rl2 in

			let newBody r =
				let b = r.body in
					match b with
						| [c] when Set.belongs r rl1 && not (Set.belongs c alp1) && c <> epsilon -> b
						| [c] when Set.belongs r rl1 && Set.belongs c alp1 -> [c; rcfg.initial]
						| [epsilon] when Set.belongs r rl1 -> [epsilon; rcfg.initial]
						| b when Set.belongs r rl2 -> b
						| _ -> b
			in
			let newRule r = {head = r.head; body = newBody r} in
				Set.map (fun r -> newRule r) rl
		in

		(* create gcf rules for star expression *)
		let convertStrRules cfg =
			let open CFGTypes in

			let newBody b =
				match b with
					| [c] when Set.belongs c cfg.alphabet -> [c; cfg.initial]
					| _ -> b
			in
			let r0 = {head = cfg.initial; body = [epsilon]} in

			let newRule r = {head = r.head; body = newBody r.body} in
			let newRules = Set.map (fun r -> newRule r) cfg.rules in
				Set.add r0 newRules
		in



		let rec compile rep =
			let open RegExpTypes in
			let open CFGTypes in
			match rep with

				| Plus(l, r) ->
						let cl = compile l in
						let cr = compile r in
						let alp = Set.union cl.alphabet cr.alphabet in
						let init = genVar () in
						let vs = Set.add init (Set.union cl.variables cr.variables) in
						let rl = Set.union cl.rules cr.rules in
						let rl = convertPlsRules rl cl.initial cr.initial init in

							{alphabet = alp; variables = vs;
								initial = init; rules = rl}

				| Seq(l, r) ->
						let cl = compile l in
						let cr = compile r in
						let alp = Set.union cl.alphabet cr.alphabet in
						let init = cl.initial in
						let vs = Set.union cl.variables cr.variables in
						let rl = convertSeqRules cl cr in

							{alphabet = alp; variables = vs;
								initial = init; rules = rl}

				| Star(re) ->
						let cre = compile re in
						let alp = cre.alphabet in
						let init = cre.initial in
						let vs = cre.variables in
						let rl = convertStrRules cre in

							{alphabet = alp; variables = vs;
								initial = init; rules = rl}

				| Symb(c) ->
						let alp = Set.make [c] in
						let init = genVar () in
						let vars = Set.make [init] in
						let rules = Set.make [{head = init; body = [c]}] in

							{alphabet = alp; variables = vars;
								initial = init; rules = rules}

				| Empty ->
						let alp = Set.empty in
						let init = genVar () in
						let vars = Set.make [init] in
						let rules = Set.make [{head = init; body = [epsilon]}] in
							{alphabet = alp; variables = vars;
								initial = init; rules = rules}

				| Zero ->
						let alp = Set.empty in
						let init = genVar () in
						let var2 = genVar () in
						let vars = Set.make [init; var2] in
						let r1 = {head = init; body = [var2]} in
						let r2 = {head = var2; body = [init]} in
						let rules = Set.make [r1; r2] in

							{alphabet = alp; variables = vars;
								initial = init; rules = rules}
		in


		let cfg = compile re#representation in

			new ContextFreeGrammar.model (Arg.Representation (cfg))


	(**
		* This method converts the automaton into its equivalent regular grammar
		*
		* @returns ContextFreeGrammar.model -> the resulting regular grammar
		*)

	let fa2cfg fa =
			let re = fa2re fa in
				re2cfg re





	(* This method converts the right-linear grammar to its automaton equivalent
	*
	* @pre - the grammar needs to be regular
	*
	* @returns FiniteAutomaton.model -> the equivalent finite automaton
	*)
	let cfg2fa cfg =
		let open CFGTypes in

		let rep = cfg#representation in

		let alp = rep.alphabet in
		let vrs = rep.variables in
		let toState sy = state (symb2str sy) in

		(* This name will always be unique in the generated automaton *)
		let accSt = state "AccSt" in

		let alphabet = alp in
		let states = Set.map (fun v -> toState v) rep.variables in
		let states = Set.add accSt states in
		let initialState = toState rep.initial in
		let acceptStates = Set.make [accSt] in



		let ruleToTrans rh rb =
			match rb with
				| [s;v] when Set.belongs s alp && Set.belongs v vrs	-> Set.make [(toState rh, s, toState v)]

				| [v] when Set.belongs v vrs -> Set.make [(toState rh, epsilon, toState v)]

				| [s] when Set.belongs s alp -> Set.make [(toState rh, s, accSt)]

				| [e] when e = epsilon -> Set.make [(toState rh, epsilon, accSt)]

				| _ -> Set.empty
		in

		let transitions = Set.flatMap (fun r -> ruleToTrans r.head r.body) rep.rules in

		let open FinAutTypes in
		let fa = {
			alphabet = alphabet;
			states = states;
			initialState = initialState;
			transitions = transitions;
			acceptStates = acceptStates
		} in
			new FiniteAutomaton.model (Arg.Representation (fa))


	(* This method converts the right-linear grammar to its equivalent regular expression
	*
	* @pre - the grammar needs to be regular
	*
	* @returns FiniteAutomaton.model -> the equivalent regular expression
	*)

	let cfg2re cfg =
		let fa = cfg2fa cfg in
			fa2re fa

	let cfg2tm (cfg: ContextFreeGrammar.model) =
		let cfgFA = cfg2fa cfg in
			fa2tm cfgFA
end

module PolyModelTests: sig end =
struct
	open PolyModel

	let active = false

	let testToFA () =
		let re = new RegularExpression.model (Arg.Predef "re_abc") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let testToFA2 () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let testToFA3 () =
		let re = new RegularExpression.model (Arg.Predef "re_complex") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let testToFA4 () =
		let re = new RegularExpression.model (Arg.Predef "re_convoluted") in
		let fa = re2fa re in
			JSon.show fa#toJSon

	let fa_toRe = {| {
		kind : "finite automaton",
		description : "this is an example",
		name : "fa_toRe",
		alphabet : ["a","b"],
		states : ["1", "2"],
		initialState : "1",
		transitions : [
				["1","a","2"],["2","b","2"]
			],
		acceptStates : ["2"]
	} |}

	let testSimplify () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_toRe) in
		let r = fa2re fa in
		JSon.show r#toJSon;
		let rs = r#simplify in
		let j = rs#toJSon in
			JSon.show j

	let testToRe () =
		let fa = new FiniteAutomaton.model (Arg.Text fa_toRe) in
		let r = fa2re fa in
		let j = r#toJSon in
			JSon.show j

	let testToGrammar () =
		let re = new RegularExpression.model (Arg.Predef "re_simple") in
		let res = re2cfg re in
			JSon.show res#toJSon

	let testToAutomaton () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_abc") in
		let res = cfg2fa m in
			JSon.show res#toJSon

	let testToRe () =
		let m = new ContextFreeGrammar.model (Arg.Predef "cfg_abc") in
		let res = cfg2re m in
			JSon.show res#toJSon

	let runAll =
		if Util.testing active "PolyModel" then begin
			testSimplify ()
		end
end
# 1 "src/LearnOCaml.ml"
(*
 * LearnOCaml.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias, Rita Macedo (amd, rm)
 *)

(*
 * ChangeLog:
 *
 * jul/2021 (amd) - Added semantic validation of the student's solution.
 * jun/2021 (amd) - Added support for typed student answers in the file
 *                  "template.ml". Many improvements in the implementation,
 *                  and managed to get rid of many technical
 *                  complexities in the file "template.ml".
 * mar/2021 (amd, rm) - Initial version
 *)

(*
 * Description: This module helps in the use of the OCamlFLAT library
 * inside Learn-OCaml (the platform for learning the OCaml language). The goal
 * is to support FLAT related exercises inside Learn-OCaml. 
 *  
 * As for FLAT exercises developed for current text-based interaction of
 * Learn-OCaml, our current approach is to avoid changing Learn-OCaml
 * itself.
 * 
 * The current solution comprehends:
 *
 * - A technique that allows the OCamlFLAT library to be available inside the
 *   exercise environment of Learn-OCaml (via the file "prepare.ml").
 *   
 * - The idea of reducing FLAT model analysis to OCaml function analysis. This
 *   allows us to represent FLAT exercises using what is available in
 *   Learn-OCaml, without changing anything in Learn-OCaml.
 *   
 * - This module, which supplies some functions that helps in
 *   the creation by hand of FLAT exercises following the conventions
 *   of Learn-OCaml.
 *   
 * - A translator from the OCamlFLAT exercise format to the Learn-OCaml
 *   exercise format. The translator generates a directory containing all
 *   the usual files: "template.ml", "solution.ml", "meta.json", etc.
 *)

open BasicTypes

module type LearnOCamlSig =
sig
	val setOCamlFlatDir : string -> unit
	val setExercicesDir : string -> unit
	val setExerciceName : string -> unit
	val processAnswer : Model.model
					-> Exercise.exercise -> (string * int) list
	val decl2json : string -> JSon.t
	val generateExerciseDir : JSon.t -> JSon.t -> bool -> unit
end

module LearnOCaml : LearnOCamlSig =
struct
	(* ----- Dir/File management ----- *)
	let oCamlFlatDir = ref ""
	let exercicesDir = ref ""
	let exerciceName = ref ""		

	let setOCamlFlatDir dirname =
		oCamlFlatDir := Util.handleHomeDir dirname

	let setExercicesDir dirname =
		exercicesDir := Util.handleHomeDir dirname

	let setExerciceName filename =
		exerciceName := filename

	let initialize () =
		if !oCamlFlatDir = "" then
			begin
				setOCamlFlatDir
					"~/work/OCamlFlat";
				setExercicesDir
					"~/work/learn-test/my-learn-ocaml-repository/exercises";
				setExerciceName
					"default"
			end

	let libFile () =
		!oCamlFlatDir ^ "/lib/OCamlFlat.ml"

	let targetDir () =
		!exercicesDir ^ "/" ^ !exerciceName
	
	let targetFile fileName =
		targetDir () ^ "/" ^ fileName
	
	let adjust txt =
		Util.stripHead txt
 
	let createTargetDir () =
		ignore (Sys.command ("mkdir -p " ^ targetDir ()) )

	let createTargetFile fileName text =
		let co = open_out (targetFile fileName) in
		let text = adjust text in
		begin
			output_string co text;
			close_out co
		end

	let getExerciceDirContents () =
		Array.to_list (Sys.readdir !exercicesDir)
	
	(* ----- Utility functions ----- *)
	let processUnitTest m expected w =
			(word2str w, expected, m#accept w = expected)

	let semanticValidation (m: Model.model) =
		m#errors

	let convertSemanticValidationResult mesg =
			(mesg, 0)

	let processUnitTests (m: Model.model) (e: Exercise.exercise) =
		let open Exercise in
		let rep = e#representation in
			List.map (processUnitTest m true) (Set.toList rep.inside)
			@
			List.map (processUnitTest m false) (Set.toList rep.outside)
			
	let convertUnitTestResult (word, acceptance, passed) =
		let ar = if acceptance then "Acceptance" else "Rejection" in
		let pf = if passed then "passed" else "failed" in
		let points = if passed then 1 else 0 in
			(ar ^ " of word \"" ^ word ^ "\" " ^ pf, points)

	let processProperty m p =
		(p, m#checkProperty p)
	
	let processProperties (m: Model.model) (e: Exercise.exercise) =
		let open Exercise in
		let rep = e#representation in
		let props = Set.toList rep.properties in
			List.map (processProperty m) props	
		
	let convertPropertyResult (property, passed) =
		let pf = if passed then "passed" else "failed" in
		let points = if passed then 3 else 0 in
			("Property \"" ^ property ^ "\" " ^ pf, points)

	let finalResult0 res =
		[("###  Checking for static semantic errors...", -999)]
		@ res
		@ [("### Errors found", -999)]

	let finalResult1 res1 res2 =
		[]
		@ [("###  Checking for static semantic errors...", -999)]
		@ [("### Checking unit tests...", -999)]
		@ res1
		@ [("### Checking properties...", -999)]
		@ res2
		@ [("### Done", -999)]

	let processAnswer (m: Model.model) (e: Exercise.exercise) =
		let semanticValidationResults = semanticValidation m in
		let res0 = List.map convertSemanticValidationResult semanticValidationResults in
		if res0 <> [] then
			finalResult0 res0
		else
			let unitTestsResults = processUnitTests m e in
			let res1 = List.map convertUnitTestResult unitTestsResults in
			let propertyResults = processProperties m e in
			let res2 = List.map convertPropertyResult propertyResults in
				finalResult1 res1 res2

	let kind2designation kind =
		match kind with
		| "finiteAutomaton" | "FiniteAutomaton.tx" ->
			FiniteAutomaton.modelDesignation
		| "regularExpression" | "RegularExpression.tx" ->
			RegularExpression.modelDesignation
		| "contextFreeGrammar" | "ContextFreeGrammar.tx" ->
			ContextFreeGrammar.modelDesignation
		| "finiteEnumeration" | "FiniteEnumeration.tx" ->
			FiniteEnumeration.modelDesignation
		| _ ->
			"*** invalid ***"

	let completeJSon kind j  =
		match kind with
		| "regularExpression" | "RegularExpression.tx" ->
			JSon.makeAssoc [("re", j)]
		| "finiteEnumeration" | "FiniteEnumeration.tx" ->
			JSon.makeAssoc [("words", j)]
		| _ ->
			j

	let decl2json s =
		try
			let a = String.index_from s 0 ':' in
			let b = String.index_from s a '=' in
			let kind = String.trim (String.sub s (a+1) (b-a-1)) in
			let ocamlExp = String.sub s (b+1) (String.length s -b-1) in
			let jExp = JSon.parseOon ocamlExp in
			let mainJSon = completeJSon kind jExp in
			let jHead = Entity.toJSon (Entity.dummyId (kind2designation kind)) in
				JSon.append jHead mainJSon
		with _ ->
			JSon.JNull
	
	(* ----- FILE descr.html ----- *)
	let fileName =
		"descr.html"
	
	let contents (exercise: JSon.t) =
		Printf.sprintf
{ooo_descr_html_ooo|
		<h3> %s </h3>
		<p> %s </p>
|ooo_descr_html_ooo}
		(JSon.fieldString exercise "description")
		(JSon.fieldString exercise "problem")
	
	let generateFile_Descr (exercise: JSon.t) =
		let text = contents exercise in
			createTargetFile fileName text

	(* ----- FILE meta.json ----- *)
	let fileName =
		"meta.json"
		
	let contents (exercise: JSon.t) =
		Printf.sprintf
	{ooo_meta_json_ooo|
		{
		  "learnocaml_version" : "1",
		  "kind" : "exercise",
		  "stars" : 0,
		  "title" : "%s"
		}
	|ooo_meta_json_ooo}
		(JSon.fieldString exercise "description")
	
	let generateFile_Meta (exercise: JSon.t) =
		let text = contents exercise in
			createTargetFile fileName text

	(* ----- FILE prelude.ml ----- *)
	let fileName =
		"prelude.ml"

	let generateFile_Prelude (solution: Model.model) =
		let text = solution#xTypeDeclString in
			createTargetFile fileName text
	
	(* ----- FILE prepare.ml ----- *)
	let fileName =
		"prepare.ml"

	let generateFile_Prepare () =
		let cmd = "cp -a " ^ libFile () ^ " " ^ targetFile fileName in
			ignore (Sys.command cmd)

	(* ----- FILE solution.ml ----- *)
	let fileName =
		"solution.ml"

	let contentsJSon (solution: Model.model) =
		Printf.sprintf
	{ooo_solution_ml_ooo|
		let solution = {| %s |}
	|ooo_solution_ml_ooo}
		(JSon.toStringN 2 solution#toJSon)

	let contents (solution: Model.model) =
		let signature = "\n\t\t(* OCamlFlat exercise *)" in
		let body = solution#toDisplayString "solution" in
			signature ^ body
	
	let generateFile_Solution (solution: Model.model) useJSon =
		let text =
			if useJSon then contentsJSon solution
			else contents solution
		in
			createTargetFile fileName text

	(* ----- FILE template.ml ----- *)
	let fileName =
		"template.ml"

	let contentsJSon (solution: Model.model) =
		Printf.sprintf
	{ooo_template_ml_json_ooo|
		(* Write your solution below, by using the provided example as a template *)

		let solution = {| %s |}
	|ooo_template_ml_json_ooo}
		(JSon.toStringN 2 solution#example)

	let contents (solution: Model.model) =
		Printf.sprintf
	{ooo_template_ml_ooo|
		(* Write your solution below, by using the provided example as a template *)
		%s|ooo_template_ml_ooo}	(* please, do not change this *)
		((PolyModel.json2model solution#example)#toDisplayString "solution")

	let generateFile_Template (solution: Model.model) useJSon =
		let text =
			if useJSon then contentsJSon solution
						else contents solution
		in
			createTargetFile fileName text

	(* ----- FILE test.ml ----- *)
	let fileName =
		"test.ml"
	
	let exercisePart (exercise: JSon.t) =
		Printf.sprintf
	{ooo_exercise_ooo|
		let exercise = {| %s |}
	|ooo_exercise_ooo}
		(JSon.toStringN 2 exercise)

	let handleAnswerPartJSon =
		Printf.sprintf
	{ooo_handle_answer_json_ooo|
		let handleAnswer (): Learnocaml_report.t =
			test_variable_property
				[%%ty: string]
				"solution"
				(fun solution ->
					checkAnswer
						(PolyModel.text2model solution)
						(new Exercise.exercise (Arg.Text exercise))
				)
	|ooo_handle_answer_json_ooo}
	
	let handleAnswerPart (solution: Model.model) =
		Printf.sprintf
	{ooo_handle_answer_ooo|
		let handleAnswer (): Learnocaml_report.t =
			test_variable_property
				[%%ty: %s]
				"solution"
				(fun solution ->
					checkAnswer
						((new %s.model (Arg.RepresentationX solution)):
										%s.model :> Model.model)
						(new Exercise.exercise (Arg.Text exercise))
				)
	|ooo_handle_answer_ooo}
	solution#xTypeName
	solution#moduleName
	solution#moduleName
		
	let contents exerciseText handleAnswerText =
		Printf.sprintf
	{ooo_test_ml_ooo|
		open Test_lib
		open Report
		%s
		let convertResult (diagnostic, points) =
			match points with
			| _ when points > 0 ->
				Message ([Text diagnostic], Success points)
			| -999 ->
				Message ([Break; Text diagnostic], Informative)
			| _ ->
				Message ([Text diagnostic], Failure)
		
		let checkAnswer (m: Model.model) (e: Exercise.exercise) =
			let res = LearnOCaml.processAnswer m e in
				List.map convertResult res
		%s
		let () =
			set_result @@
			ast_sanity_check code_ast @@
			handleAnswer
	|ooo_test_ml_ooo}
		exerciseText
		handleAnswerText

	let generateFile_Test (exercise: JSon.t) (solution: Model.model) useJSon =
		let ex = exercisePart exercise in
		let hs =
			if useJSon then handleAnswerPartJSon
			else handleAnswerPart solution
		in
		let text = contents ex hs
		in
			createTargetFile fileName text	

	(* ----- FILE index.json ----- *)
	let fileName =
		"../index.json"
		
	let contents (l: string list) =
		Printf.sprintf
	{ooo_index_json_ooo|
		{
		  "learnocaml_version" : "1",
		  "groups" : { "OCamlFLAT": {
		    "title": "OCamlFLAT exercise pack for Learn-OCaml",
		    "exercises": %s
		  } }
		}
	|ooo_index_json_ooo}
		(strings2display l)
	
	let generateFile_Index () =
		let l = getExerciceDirContents () in
		let l = List.filter (fun x -> x <> "index.json") l in
		let text = contents l in
		let text = String.map (fun x -> if x = ';' then ',' else x) text in
			createTargetFile fileName text

	(* ----- generateExerciseDir ----- *)
	
	let generateExerciseDir exercise solution useJSon =
		let solution: Model.model = PolyModel.json2model solution in
			initialize ();
			createTargetDir ();
			generateFile_Descr exercise;
			generateFile_Meta exercise;
			generateFile_Prelude solution;
			generateFile_Prepare ();
			generateFile_Solution solution useJSon;
			generateFile_Template solution useJSon;
			generateFile_Test exercise solution useJSon;
			generateFile_Index ()
end


module LearnOCamlTests =
struct
	let active = true

	let prepare target =
		print_string ("Generate: " ^ target ^ "\n");
		LearnOCaml.setOCamlFlatDir "~/work/OCamlFlat";
		LearnOCaml.setExercicesDir "~/work/OCamlFlat/exercises";
		LearnOCaml.setExerciceName target

	let prepare0 () =
		LearnOCaml.setOCamlFlatDir "~/work/OCamlFlat";
		LearnOCaml.setExercicesDir "~/work/learn/my-learn-ocaml-repository/exercises";
		LearnOCaml.setExerciceName "default"

	let make exercise model =
		prepare exercise;
		let exercise = Examples.jsonExample exercise in
		let solution = Examples.jsonExample model in
			LearnOCaml.generateExerciseDir exercise solution false
	
	let test0 () =
		make "exer_astar_fa" "dfa_astar"
			
	let test1 () =
		make "exer_astar_re" "re_astar"
			
	let test2 () =
		make "exer_balanced_cfg" "cfg_balanced"
	
	let fe_colors = {| {
		kind : "finite enumeration",
		description : "this is an example",
		name : "colors",
		words : ["Red", "Yellow", "Blue"]
	} |}
			
	let test3 () =
		prepare "exer_astar";
		let exercise = Examples.jsonExample "exer_astar" in
		let solution = JSon.parse fe_colors in
			LearnOCaml.generateExerciseDir exercise solution false

	let decl1 = {|
		let solution: finiteAutomaton =
		{
			alphabet = ['a'];
			states = ["START"];
			initialState = "START";
			transitions = [("START", 'a', "START")];
			acceptStates = ["START"]
		} |}
		
	let decl2 = {|
		let solution: RegularExpression.tx =
			"z*"
	|}
		
	let decl3 = {|
		let solution: ContextFreeGrammar.tx =
		{
			alphabet = ['0'; '1'];
			variables = ['S'; 'P'];
			initial = 'S';
			rules = [	"S -> 1S0 | P";
						"P -> 0P1 | ~" ]
		}
	|}
		
	let decl4 = {|
		let solution: FiniteEnumeration.tx =
			["A"; "B"; "C"; "D"; "E"]
	|}

	let test4 () =
		let j = LearnOCaml.decl2json decl1  in
			JSon.show j
 
	let runAll =
		if Util.testing active "LearnOCaml" then begin
			test0 ();
			test1 ();
			test2 ()
		end
end

(*
Learnocaml_report.t:

	type report = item list

	and item =
	  | Section of text * report
	  (** A titled block that groups subreports *)
	  | Message of text * status
	  (** Basic report block *)

	and status =
	  | Success of int (** With given points *)
	  | Failure (** With missed points *)
	  | Warning (** A student error without influence on the grade *)
	  | Informative (** A message for the student *)
	  | Important (** An important message *)

	and text = inline list

	and inline =
	  | Text of string (** A word *)
	  | Break (** Line separator *)
	  | Code of string (** For expressions *)
	  | Output of string (** For output *)
*)
# 1 "src/Tests.ml"
(*
 * Tests.ml
 *
 * This file is part of the OCamlFLAT library
 *
 * LEAFS project (partially supported by the OCaml Software Foundation) [2020/21]
 * FACTOR project (partially supported by the Tezos Foundation) [2019/20]
 *
 * NOVA LINCS - NOVA Laboratory for Computer Science and Informatics
 * Dept. de Informatica, FCT, Universidade Nova de Lisboa.
 *
 * This software is distributed under the terms of the GPLv3 license.
 * See the included LICENSE file for details.
 *
 *  Written by Artur Miguel Dias (amd)
 *)

(*
 * ChangeLog:
 *
 * jan/2021 (amd) - Initial version.
 *)

(*
 * Description: Set of unit tests for checking the library from time to time.
 *
 * TODO: This is only a starting point. There are already many (disabled) unit
 * tests in several modules and this stuff needs to be reviewed.
 *)

module Tests : sig end =
struct
	open Examples

	let active = false

(*
	open TopLevel



	let test1 () =
		let a = fa_predef "dfa_1" in
			Util.println [if fa_accept a "ab" then "OK" else "ERROR"] *)

	let runAll =
		if Util.testing active "Tests" then begin

		end
end
# 1 "src/PreOpen.ml"
open Examples


type symbol = char
type finiteAutomaton = FinAutTypes.finiteAutomaton
type regularExpression = RegExpTypes.regularExpression
type contextFreeGrammar = CFGTypes.contextFreeGrammar
type finiteEnumeration = FinEnuTypes.finiteEnumeration
