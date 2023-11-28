open OCamlFlat
open BasicTypes
open CFGSyntax
open JS
open Js_of_ocaml
open Js.Opt
open Lang
open ContextFreeGrammarLL1Graphics
open ContextFreeGrammarLRGraphics
open CFGTypes

module rec ContextFreeGrammarGraphics :
sig
	type t = CFGTypes.t
	type tx = CFGTypes.tx
	type cfgTree2 = Leaf of string * symbol | Root of string * symbol * cfgTree2 list
	type syntaxTable = ContextFreeGrammarFull.syntaxTable
	type acceptTable = ContextFreeGrammarFull.acceptTable
	type recognized = ContextFreeGrammarFull.recognized
	type acceptStep = {
			syntaxTable : syntaxTable;
			acceptedString: string;
			acceptTable : acceptTable;
			recognized : recognized;
			accepted: bool option;
			nodes: cfgTree2 list;
			cyId: string option
	}
    
	type transformation = ContextFreeGrammarLL1Graphics.transformation

	val modelDesignation : unit -> string
	val firstFollowTableId : unit -> string
	val parsingTableId : unit -> string
	val parsingGuideTableId : unit -> string
	val productionsTableId : unit -> string
	val productionsTableId2 : unit -> string

	class model :
		(t,tx) Arg.alternatives ->
			object
			(*ContextFreeGrammar Basic*)
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

			(* LL1Grammar *)
				method isSimplified : bool
				method rdparserOpts : string list
				method toggleSimplified: unit 

				method nSteps: int
				method first: word -> symbol Set.t
				method first1: string
				method follow: symbol -> symbol Set.t
				method follow1: string
				method lookahead: CFGTypes.rule -> symbol Set.t
				method lookahead1: CFGTypes.rule -> string
				method isLL1: bool
				method isLeftRecursive: bool
				method createParsingTable: ((variable * symbol) * word) Set.t
				method hasParsingTableConflict : bool
				method acceptZ: word -> OCamlFlat.LL1Grammar.acceptStep list
				method productiveSymbols: symbol Set.t
				method accessibleSymbols: symbol Set.t
				method productiveRewrite: ContextFreeGrammar.model
				method accessibleRewrite: ContextFreeGrammar.model
				method clean: LL1Grammar.transformation list
				method isFullyProductive: bool
				method isFullyAccessible: bool
				method isClean: bool
				method removeLeftRecursion: LL1Grammar.transformation
				method removeDirectLeftRecursion: ContextFreeGrammar.model
				method leftFactoring: LL1Grammar.transformation
				method isLeftFactoring: bool
				method leftCorner: symbol -> symbol Set.t
				method hasEmptyProductions: bool
				method removeEmptyProductions: LL1Grammar.transformation
				method hasUnitProductions: bool
				method removeUnitProductions: LL1Grammar.transformation
				method generateRecursiveDescendentParser: string -> string
				method transformToLL1: LL1Grammar.transformation list

			(* LRGrammar *)
				method isLR1: bool

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

			(**New public methods**)
				method accept1: word -> unit
				method startAccept: Cytoscape.cytoscape Js_of_ocaml.Js.t -> word -> unit
				method next: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
				method back: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
				method createGrammarTableHtml: string -> unit
				method createParsingTableGuideHtml: unit
				method createFirstAndFollowTableHtml: unit
				method createParsingTableHtml: unit
				(* 			  method paintParsingTableHtml: symbol option -> symbol option -> unit*)
				(*			  method paintParsingTableHtmlGuide: unit*)
				method selectStep: Cytoscape.cytoscape Js_of_ocaml.Js.t -> int -> unit
				method clearCy: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
				method clean1: ContextFreeGrammarLL1Graphics.transformation
				method removeLeftRecursion1: ContextFreeGrammarLL1Graphics.transformation
				method leftFactoring1: ContextFreeGrammarLL1Graphics.transformation
				method removeEmptyProductions1: ContextFreeGrammarLL1Graphics.transformation
				method removeUnitProductions1: ContextFreeGrammarLL1Graphics.transformation
				method getPreviousTransformed: ContextFreeGrammarLL1Graphics.transformation
				method getNextTransformed: ContextFreeGrammarLL1Graphics.transformation
				method transformToLL1X: ContextFreeGrammarLL1Graphics.transformation
			end
end			
=
struct
	open CFGSyntax

	type t = CFGTypes.t
	type tx = CFGTypes.tx
	type cfgTree2 = Leaf of string * symbol | Root of string * symbol * cfgTree2 list
	type syntaxTable = LL1Grammar.syntaxTable
	type acceptTable = LL1Grammar.acceptTable
	type recognized = LL1Grammar.recognized
	type acceptStep = {
		syntaxTable : syntaxTable;
		acceptedString: string;
		acceptTable : acceptTable;
		recognized : recognized;
		accepted: bool option;
		nodes: cfgTree2 list;
		cyId: string option
	}

	type transformation = ContextFreeGrammarLL1Graphics.transformation
	
	let modelDesignation = ContextFreeGrammarLL1Graphics.modelDesignation
	let firstFollowTableId = ContextFreeGrammarLL1Graphics.firstFollowTableId
	let parsingTableId = ContextFreeGrammarLL1Graphics.parsingTableId
	let parsingGuideTableId = ContextFreeGrammarLL1Graphics.parsingGuideTableId
	let productionsTableId = ContextFreeGrammarLL1Graphics.productionsTableId
	let productionsTableId2 = ContextFreeGrammarLL1Graphics.productionsTableId2

	
	class model (arg: (t,tx) Arg.alternatives) =
		object(self) inherit ContextFreeGrammarLRGraphics.model arg as super
	end
end	
