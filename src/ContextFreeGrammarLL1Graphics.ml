open OCamlFlat
open BasicTypes
open CFGSyntax
open JS
open Js_of_ocaml
open Js.Opt
open Lang
open ContextFreeGrammarBasicGraphics
open LL1Grammar
open CFGTypes

module rec ContextFreeGrammarLL1Graphics :
sig
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
    
	type transformation = { tType : string; grammar : ContextFreeGrammarLL1Graphics.model }

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

	let leftRecursionRemovalTransform = LL1Grammar.leftRecursionRemovalTransform
	let leftFactoringTransform = LL1Grammar.leftFactoringTransform
	let cleanProductiveTransform = LL1Grammar.cleanProductiveTransform
	let cleanAccessibleTransform = LL1Grammar.cleanAccessibleTransform
	let unitRemovalTransform = LL1Grammar.unitRemovalTransform
	let epsilonRemovalTransform = LL1Grammar.epsilonRemovalTransform
	let ll1Transform = LL1Grammar.ll1Transform

	type transformation = { tType : string; grammar : ContextFreeGrammarLL1Graphics.model }

	let modelDesignation () = "context free grammar"
	
  let removeDollarFromWord w =
    word2str (List.filter (fun c -> c <> dollar) (str2word w) )

	(**HTML entities**)
  let htmlEpsilon = "ε" (*"&epsilon;"*)
  let htmlArrow = " → " (*" &rarr; "*)
  let htmlDollar = "$"
  
  (**IDs**)
  let firstFollowTableId() = "cfgFirstFollowTable"
  let parsingTableId() = "cfgParsingTable"
  let parsingGuideTableId() = "cfgGuideTable"
  let productionsTableId() = "cfgProductionsTable"
  let productionsTableId2() = "cfgProductionsTable2"
  
  (**Classes**)
  let bgSymbolColorClass = "bgSymbolColor"
  let bgSymbolColorFail = "bgSymbolColorFail"
  let bgRuleColor = "bgRuleColor"
  let bgRuleColorFail = "bgRuleColorFail"
  let monospaceClass = "monospaceClass"
  let currentRow = "currentRow"
  let stringSymbolRemove = "stringSymbolRemove"
  let stringSymbolAdd = "stringSymbolAdd"
  let wordAccepted = "wordAccepted"
  let wordRejected = "wordRejected"

  (**HTML helpers**)
  let doc = Dom_html.document


  let createHtmlTable id =
    let t = Dom_html.createTable doc in
    t##.id := Js.string id;
    t
  
  
  (**Misc helper functions**)
  let bodiesOfHead h rl =
		let rls = Set.filter (fun r -> r.head = h) rl in
			Set.map (fun r -> r.body) rls


  (*Shows symbols in a set*)
  let setToHtmlString s =
    let rec insideBracesString cs =
      if cs = Set.empty then ""
      else let (x,xs) = Set.cut cs in
        let x = if x = epsilon then htmlEpsilon else symb2str x in
        x ^ (if xs = Set.empty then "" else ",") ^ insideBracesString xs
    in
    "{ " ^ insideBracesString s ^ " }"

  
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
    doWordGenerateEmptyX w [] rep


  let first f (rep:t) =
    let rec setString vars =
      if vars = Set.empty then ""
      else let (x,xs) = Set.cut vars in
        let firstSet = f [x] in
          Lang.i18nCFGFirst() ^ "(" ^ (word2str [x]) ^ ") = " ^ setToHtmlString firstSet ^ "<br>" ^ setString xs
        in
      setString rep.variables    


  let follow f (rep:t) =
    let rec setString vars =
      if vars = Set.empty then ""
      else let (x,xs) = Set.cut vars in
        let followSet = f x in
        Lang.i18nCFGFollow() ^ "(" ^ (symb2str x) ^ ") = " ^ setToHtmlString followSet ^ "<br>" ^ setString xs
    in
    setString rep.variables


(**-----------------------------Element creation-----------------------------**)
  let createGrammarTableHtml (rep:t) id =
    let rec fillRule row bodies =
      if bodies = Set.empty then ()
      else let (x,xs) = Set.cut bodies in
        let span = Dom_html.createSpan doc in
          span##.innerHTML := Js.string (if x = [] then htmlEpsilon else word2str x);
          span##.classList##add (Js.string monospaceClass);
          row##appendChild span;
        if xs = Set.empty then fillRule row xs
        else (
          let orSpan = Dom_html.createSpan doc in
            orSpan##.innerHTML := Js.string " | ";
            orSpan##.classList##add (Js.string monospaceClass);
            row##appendChild orSpan;
            fillRule row xs
        )
    in
    let fillRow table head bodies =
      let head = symb2str head in
      let row = table##insertRow (-1) in
      let varCell = row##insertCell (-1) in
        varCell##.id := Js.string (head ^ "Prods");
        varCell##.classList##add (Js.string monospaceClass);
        varCell##.innerHTML := Js.string head;
      let arrowCell = row##insertCell (-1) in
        arrowCell##.innerHTML := Js.string htmlArrow;
        arrowCell##.classList##add (Js.string monospaceClass);
      fillRule row bodies
    in
    let nonInitialRules = Set.map (fun r -> r.head) rep.rules in
    let nonInitialRules = Set.filter (fun h -> h <> rep.initial) nonInitialRules in
    let id = if id = "" then productionsTableId() else id in
    let table = Js.Unsafe.coerce (Dom_html.getElementById id) in
      fillRow table rep.initial (bodiesOfHead rep.initial rep.rules);
      Set.iter (fun head -> fillRow table head (bodiesOfHead head rep.rules)) nonInitialRules


  let createParsingTableGuideHtml () =
    let tableHeader = [
      ("cfgGuideTableRecognized", (Lang.i18nAcceptRecognized()));
      ("cfgGuideTableInput", (Lang.i18nAcceptInput()));
      ("cfgGuideTableStack", (Lang.i18nAcceptStack()));
      ("cfgGuideTableProduction", (Lang.i18nAcceptProduction()));
    ] in
    let table = Js.Unsafe.coerce (Dom_html.getElementById (parsingGuideTableId())) in
    let row = table##insertRow (-1) in
    (**First row**)
    List.iter (fun (id, txt) -> 
      let cell = row##insertCell (-1) in
        cell##.id := Js.string id;
        cell##.innerHTML := Js.string txt;
    ) tableHeader


  let createFirstAndFollowTableHtml (rep:t) firstF followF =
    let table = Js.Unsafe.coerce (Dom_html.getElementById (firstFollowTableId())) in
    let row = table##insertRow (-1) in
      ignore (row##insertCell (-1));
    let td2 = row##insertCell (-1) in
      td2##.id := Js.string "cfgFirstLabel";
      td2##.innerHTML := Js.string (Lang.i18nCFGFirst());
    let td3 = row##insertCell (-1) in
      td3##.id := Js.string "cfgFollowLabel";
      td3##.innerHTML := Js.string (Lang.i18nCFGFollow());
    Set.iter ( fun v ->
      let newRow = table##insertRow (-1) in
      let newCell1 = newRow##insertCell (-1) in
        newCell1##.innerHTML := Js.string (symb2str v);  
        ignore (newCell1##.classList##add (Js.string monospaceClass));
      let newCell2 = newRow##insertCell (-1) in
        newCell2##.innerHTML := Js.string (setToHtmlString (firstF [v]));
        ignore (newCell2##.classList##add (Js.string monospaceClass));
      let newCell3 = newRow##insertCell (-1) in
        newCell3##.innerHTML := Js.string (setToHtmlString (followF v));
        ignore (newCell3##.classList##add (Js.string monospaceClass));
    ) rep.variables


  let createParsingTableHtml (rep:t) cPTF =
    let rec prods2span cell l var =
      if l = Set.empty then ()
      else let (x,xs) = Set.cut l in
        match x with
        | None -> prods2span cell xs var
        | Some x -> 
            let varSpan = Dom_html.createSpan doc in
              varSpan##.innerHTML := Js.string (symb2str var);
              varSpan##.classList##add (Js.string monospaceClass);
            let arrowSpan = Dom_html.createSpan doc in
              arrowSpan##.innerHTML := Js.string htmlArrow;
              arrowSpan##.classList##add (Js.string monospaceClass);
            let ruleSpan = Dom_html.createSpan doc in
            let ruleStr = (
              (if x = [] then htmlEpsilon else word2str x)
            ) in
              ruleSpan##.innerHTML := Js.string (ruleStr ^
              (if xs = Set.empty then "" else "<br>"));
              ruleSpan##.classList##add (Js.string monospaceClass);
            cell##appendChild varSpan;
            cell##appendChild arrowSpan;
            cell##appendChild ruleSpan;
            prods2span cell xs var
    in
    let fillTable2 row var pTable =
      let assocVar var a pTable =
        Set.map (fun ((v,t),l) -> if v = var && t = a then Some l else None ) pTable
      in
        Set.iter ( fun a ->
          let cell = row##insertCell (-1) in
          let assoc = assocVar var a pTable in
            cell##.id :=  Js.string ((symb2str a) ^ (symb2str var));
            prods2span cell assoc var
        ) rep.alphabet;
      let dollarCell = row##insertCell (-1) in
      let dollarAssoc = assocVar var dollar pTable in
        dollarCell##.id := Js.string ((symb2str dollar) ^ (symb2str var));
        prods2span dollarCell dollarAssoc var
    in
    let fillTable table pTable =
      Set.iter ( fun v ->
        let varString = Js.string (symb2str v) in
        let row = table##insertRow (-1) in
        let cell = row##insertCell (-1) in
          cell##.id := varString;
          cell##.innerHTML := varString;
        fillTable2 row v pTable
      ) rep.variables
    in
    let parsingTable = cPTF in
    let table = Js.Unsafe.coerce (Dom_html.getElementById (parsingTableId())) in
    let row = table##insertRow (-1) in
    let emptyCell = row##insertCell (-1) in (*Empty 1st cell *)
      emptyCell##.id := Js.string "empty";
    (**First row with alphabet and $**)
      Set.iter (fun a -> let varString = Js.string (symb2str a) in
                         let cell = row##insertCell (-1) in
                           cell##.id := varString;
                           cell##.className := Js.string monospaceClass;
                           cell##.innerHTML := varString
               ) rep.alphabet;
    let dollarCell = row##insertCell (-1) in
    let dollarString = Js.string htmlDollar in
      dollarCell##.id := dollarString;
      dollarCell##.innerHTML := dollarString;
    (**Rest of rows**)
    fillTable table parsingTable

(**-----------------------------Element paiting------------------------------**)
  let paintGrammarTable step = 
    let clean row =
    let rowElements = row##.childNodes in
    for i = 0 to rowElements##.length - 1 do
      let elem = rowElements##item i in
        elem##.className := Js.string monospaceClass;
    done
    in
    let paint () =
      match step.accepted with
        | Some a -> ()
        | None -> 
          match step.syntaxTable.var with
            | None -> ()
            | Some var -> 
              match step.syntaxTable.rBody with
                | None -> ()
                | Some rule -> 
                  let rule = (if List.length rule = 0 then htmlEpsilon else word2str rule) in
                  let var = Dom_html.getElementById ((symb2str var) ^ "Prods") in
                    var##.classList##add (Js.string "bgSymbolColor");
                  let row = (Js.Unsafe.coerce var)##.parentElement in
                    ignore (row##.classList##add (Js.string "currentProds"));
                  let rowElements = row##.childNodes in
                  for i = 2 to rowElements##.length - 1 do (*Not interested in var and arrow*)
                    let prod = rowElements##item i in
                    let prodString = Js.to_string (prod##.innerHTML) in
                    if prodString = rule
                    then prod##.classList##add (Js.string "bgRuleColor")
                    else ()
                  done
    in
    let currentProds = doc##getElementsByClassName (Js.string "currentProds") in
    for i = 0 to currentProds##.length - 1 do (*Should be only one*)
      let row = currentProds##item i in
      match to_option row with
        | None -> ()
        | Some r -> let r = Js.Unsafe.coerce r in 
                      clean r
    done;
        paint()
    
    
  let paintWord step =
    let elem = Dom_html.getElementById "cfgAcceptWord" in
    let accepted = step.recognized.recog in
    let input = step.acceptTable.input in
    let prod = step.acceptTable.production in
    let toAccept =
      match step.accepted with
      | None -> if String.length accepted > 0 && String.length input > 0 && String.length prod = 0 
                then String.sub input 1 ((String.length input) - 1)
                else input
      | Some a ->
          match a with
          | true -> ""
          | false -> input
    in
      elem##.innerHTML := Js.string (accepted ^ "|" ^ toAccept);
    match step.accepted with
      | None -> elem##.className := (Js.string "")
      | Some a -> if a 
                  then elem##.classList##add (Js.string "wordAccepted")
                  else elem##.classList##add (Js.string "wordRejected")
    
    
  let paintParsingTableHtmlGuide step currPos =
    let splitWord s =
      if String.length s > 0
      then
        let first = String.sub s 0 1 in
        let rest = String.sub s 1 ((String.length s)-1) in
          (first,rest)
        else ("","")
    in
    let splitRule s =
      if String.length s > 0 
      then let var = String.sub s 0 1 in
           let rule = String.split_on_char '>' s in
           let rule = String.trim (List.hd (List.rev rule)) in
           let rule = 
             match rule with
               | "" -> htmlEpsilon
               | s -> s
           in
           (var ^ htmlArrow, rule)
      else ("","")
    in
    let splitRecognize s =
      if String.length s > 1
      then
        let rest = String.sub s 0 ((String.length s)-1) in 
        let last = String.sub s ((String.length s)-1) 1  in
          (rest,last)
      else if String.length s > 0
            then
            let last = String.sub s ((String.length s)-1) 1 in
              ("",last)
            else ("","")
    in
    let clean () =
      let rowsWithClass = doc##getElementsByClassName (Js.string "currentRow") in
      for i = 0 to rowsWithClass##.length - 1 do
        let row = to_option (rowsWithClass##item i) in
        match row with
          | None -> ()
          | Some r -> r##.classList##remove (Js.string "currentRow");
                      let cells = r##.childNodes in
                      for j = 0 to cells##.length - 1 do
                        let cell = to_option (cells##item j) in
                          match cell with
                            | None -> ()
                            | Some c -> (Js.Unsafe.coerce c)##.classList := (Js.string "");
                                        let spans = c##.childNodes in
                                        for k = 0 to spans##.length - 1 do
                                          let span = to_option (spans##item k) in
                                          match span with
                                            | None -> ()
                                            | Some s -> let s = Js.Unsafe.coerce s in
                                                        s##.classList := (Js.string monospaceClass);
                                        done
                      done
      done
    in
    let paint table = 
      let currentRow = table##.rows##item (currPos+1) in (*Ignore table headers*)
      match to_option currentRow with
        | None -> ();
        | Some r ->   r##.classList##add (Js.string "currentRow");
                    let recogCell = r##.cells##item 0 in
                    let inputCell = r##.cells##item 1 in
                    let stackCell = r##.cells##item 2 in
                    let prodCell = r##.cells##item 3 in
                      recogCell##.innerHTML := Js.string "";
                      inputCell##.innerHTML := Js.string "";
                      stackCell##.innerHTML := Js.string "";
                      prodCell##.innerHTML := Js.string "";
                    let term = step.syntaxTable.term in
                    let var = step.syntaxTable.var in
                    let prod = step.acceptTable.production in
                    let (s1,s2) = splitWord step.acceptTable.input in
                    let (s3,s4) = splitWord step.acceptTable.stack in
                    let s1Span = Dom_html.createSpan doc in
                      s1Span##.classList##add (Js.string monospaceClass);
                    let s2Span = Dom_html.createSpan doc in
                      s2Span##.classList##add (Js.string monospaceClass);
                    let s3Span = Dom_html.createSpan doc in
                      s3Span##.classList##add (Js.string monospaceClass);
                    let s4Span = Dom_html.createSpan doc in
                      s4Span##.classList##add (Js.string monospaceClass);
                    let s5Span = Dom_html.createSpan doc in
                      s5Span##.classList##add (Js.string monospaceClass);
                    let s6Span = Dom_html.createSpan doc in
                      s6Span##.classList##add (Js.string monospaceClass);
                    match (term, var) with
                      | (None, None) -> (*When the same symbol is found on input and stack*)
                        let (s5,s6) = splitRecognize step.recognized.recog in
                          s5Span##.innerHTML := Js.string s5;
                          s6Span##.innerHTML := Js.string s6;
                          recogCell##appendChild s5Span;
                          recogCell##appendChild s6Span;
                          s1Span##.innerHTML := Js.string s1;
                          inputCell##appendChild s1Span;
                          s2Span##.innerHTML := Js.string s2;
                          inputCell##appendChild s2Span;
                        (match step.accepted with
                        | None -> 
                          if step.recognized.recog = "" &&
                              step.acceptTable.input <> "" &&
                              step.acceptTable.stack <> "" &&
                              step.acceptTable.production = "" 
                          then (
                            () (*Don't paint first line*)
                          ) else (
                            s6Span##.classList##add (Js.string "stringSymbolAdd");
                            s1Span##.classList##add (Js.string "stringSymbolRemove");
                            s3Span##.innerHTML := Js.string s3;
                            s3Span##.classList##add (Js.string "stringSymbolRemove");
                            stackCell##appendChild s3Span;
                            s4Span##.innerHTML := Js.string s4;
                            stackCell##appendChild s4Span
                          )
                        | Some a -> 
                          stackCell##.innerHTML := Js.string step.acceptTable.stack;
                          (match a with
                          | true -> s5Span##.classList##add (Js.string "stringSymbolAdd");
                                    s6Span##.classList##add (Js.string "stringSymbolAdd")
                          | false -> inputCell##.classList##add (Js.string "bgRuleColorFail");
                                     stackCell##.classList##add (Js.string "bgRuleColorFail")
                          )
                        );
                          prodCell##.innerHTML := Js.string prod
                      | (Some i1, Some i2) -> (*When a rule was found with terminal i1 and veriable i2*) 
                        let (s5,s6) = splitRule prod in
                          recogCell##.innerHTML := Js.string step.recognized.recog;
                            
                          s1Span##.innerHTML := Js.string s1;
                          s1Span##.classList##add (Js.string "bgSymbolColor");
                          inputCell##appendChild s1Span;
                          s2Span##.innerHTML := Js.string s2;
                          inputCell##appendChild s2Span;

                          s3Span##.innerHTML := Js.string s3;
                          s3Span##.classList##add (Js.string "bgSymbolColor");
                          stackCell##appendChild s3Span;
                          s4Span##.innerHTML := Js.string s4;
                          stackCell##appendChild s4Span;
                            
                          s5Span##.innerHTML := Js.string s5;
                          prodCell##appendChild s5Span;
                          s6Span##.innerHTML := Js.string s6;
                          s6Span##.classList##add (Js.string "bgRuleColor");
                          prodCell##appendChild s6Span;
                          match step.accepted with
                          | None -> ()
                          | Some a ->
                            match a with 
                            | true -> ()
                            | false -> prodCell##.classList##add (Js.string "bgRuleColorFail")
    in
    let table = Js.Unsafe.coerce (Dom_html.getElementById (parsingGuideTableId())) in
      clean();
      paint table


  let paintParsingTableHtml term var =
    let paintSpans ?(clean = false) cell =
      let paintSpan spanNode spanClass =
        match to_option spanNode with
        | None -> ()
        | Some s -> s##.className := Js.string spanClass
      in
      let varStr = if clean then monospaceClass else "bgSymbolColor" ^ " " ^ monospaceClass in
      let ruleStr = if clean then monospaceClass else "bgRuleColor" ^ " " ^ monospaceClass in
      let spanNodeList = cell##.childNodes in
        paintSpan (spanNodeList##item 0) varStr;
        paintSpan (spanNodeList##item 2) ruleStr
    in
    let clean cell =
      paintSpans ~clean:true cell
    in
    let term = match term with | None -> "" | Some t -> symb2str t in
    let var = match var with | None -> "" | Some v -> symb2str v in
    let table = Js.Unsafe.coerce (Dom_html.getElementById (parsingTableId())) in
    let rows = table##.rows in
    for i = 0 to rows##.length - 1 do 
      let r = rows##item i in 
      match to_option r with 
       | None -> ()
       | Some a -> let cells = r##.cells in 
                   for i = 0 to (cells##.length - 1) do
                     let c = cells##item i in
                     let b1 = (Js.to_string c##.id) = term || (Js.to_string c##.id) = var in
                     let b2 = (Js.to_string c##.id) = (term ^ var) in
                     if b1 || b2
                     then (
                       if b1 
                       then c##.className := Js.string ("bgSymbolColor" ^ " " ^ monospaceClass)
                       else (if Js.to_string c##.innerHTML = "" 
                              then c##.className := Js.string "bgRuleColorFail"
                              else (paintSpans c; c##.className := Js.string "currentRow")
                            )
                       )
                    else (
                      clean c;
                      c##.className := Js.string monospaceClass
                    )
                    done
    done


  let addNode tree newNode =
    let rec addNodeToTree tree toAdd newNode =
      let rec addNodeToTree2 treeList newNode =
        List.map (fun t -> addNodeToTree t toAdd newNode) treeList
      in
    match tree with
    | Leaf (a,b) -> Leaf (a,b)
    | Root (a,b,c) -> if List.length c <> 0 
                    then Root (a,b,addNodeToTree2 c newNode) 
                    else if !toAdd 
                          then (toAdd := false; Root (a,b,newNode))
                          else Root (a,b,c)
    in
    let x = ref true in
    addNodeToTree tree x newNode
  
    
  let buildTree stepsList =
    let treeRoot = List.hd (List.hd stepsList) in
    let stepsList = List.tl stepsList in
    let rec buildTree2 tree stepsList =
      match stepsList with
      | [] -> tree
      | x::xs -> buildTree2 (addNode tree x) xs
    in
    buildTree2 treeRoot stepsList


  let addNodesToParent cy nodes parent =
    let addNodesAux a nClass b parent =
      Cytoscape.add_node cy a nClass "" b;
      match parent with
       |None -> ()
       |Some n -> Cytoscape.add_edge cy n a ""
    in
    let rec addNodes n parent =
      match n with
        | [] -> ()
        | x::xs -> 
            match x with
              | Leaf (a,b) -> let b = if b = epsilon 
                                      then htmlEpsilon
                                      else (symb2str b) in
                                addNodesAux a "leaf" b parent;
                                addNodes xs parent
              | Root (a,b,c) -> addNodesAux a "root" (symb2str b) parent;
                                addNodes xs parent
    in
    addNodes nodes parent;
    Cytoscape.run_layout (cy##layout Cytoscape.cfglayout)


  let paintNodes cy step =
    let clean cy =
      cy##elements##removeClass (Js.string "selected childrenSelected epsilon");
    in
    let paint cy step =
      match step.cyId with
      | None -> (match step.accepted with
                | None -> 
                  (match Js.Optdef.to_option (Js.array_get (cy##elements) 0) with
                  | None -> ()
                  | Some elem -> 
                    (*Case for only highlighting root node*)
                    elem##addClass (Js.string "childrenSelected"))
                | Some a -> if a 
                            then (
                              let baseElems = 
                                cy##nodes (Js.string "node[nodeType = \'leaf\']")
                              in
                              baseElems##
                              (difference (Js.string ("node[label = \'" ^ htmlEpsilon ^ "\']")))##
                              addClass (Js.string "childrenSelected");
                              baseElems##
                              (difference (Js.string ("node[label != \'" ^ htmlEpsilon ^ "\']")))##
                              addClass (Js.string "epsilon")
                            ) else (
                              ()
                            )
                )
      | Some s -> 
          let node = cy##getElementById (Js.string s) in
            if Js.to_string node##data##.nodeType = "leaf"
            then (
              node##addClass (Js.string "childrenSelected")
            )
            else (
              node##addClass (Js.string "selected");
              node##outgoers##addClass (Js.string "childrenSelected")
            )
    in
    clean cy;
    paint cy step
                                  

  let acceptListNoRepeats (acceptList:LL1Grammar.acceptStep list) =
    let repeatsTbl = Hashtbl.create 10 (**TODO initial table size**) in
    let getRepeatNum c repeatsTbl =
      let repeat = Hashtbl.find_opt repeatsTbl c in
      match repeat with
      | None -> Hashtbl.add repeatsTbl c 1; 0
      | Some a -> Hashtbl.add repeatsTbl c (a+1); a
    in
    List.map ( fun {LL1Grammar.syntaxTable; acceptedString; acceptTable; recognized; accepted; nodes} -> 
        let nodes = List.map (fun f -> match f with
               | CFGTypes.Leaf a -> let id = (symb2str a ^ string_of_int (getRepeatNum a repeatsTbl)) in
                                      Leaf (id,a) 
               | CFGTypes.Root (a,b) -> let id = (symb2str a ^ string_of_int (getRepeatNum a repeatsTbl)) in
                                      Root (id,a,[])
          ) nodes in
        let cyId = None in
      { 
        ContextFreeGrammarLL1Graphics.syntaxTable;
        acceptedString;
        acceptTable;
        recognized;
        accepted; 
        nodes;
        cyId
      }
    ) acceptList
  
  
  let rec sliceTree l p =
    match l with
    | [] -> []
    | t::xs -> if p <= 0 then [t.nodes] else t.nodes::sliceTree xs (p-1)


  let rec sliceList l p =
    match l with
    | [] -> []
    | t::xs -> if p <= 0 then [t] else t::sliceList xs (p-1)
    
    
  let treeNoRepeats (acceptList:ContextFreeGrammarLL1Graphics.acceptStep list) =
    let tree = buildTree (sliceTree acceptList (List.length acceptList)) in
    let rec repeatTree tree =
      match tree with
      | Leaf (a,b) -> [a]
      | Root (a,b,c) -> [a]@List.flatten (List.map (repeatTree) c);
    in
    repeatTree tree


  let insertStep table step evtMethod =
    let row = table##insertRow (-1) in
      (match step.cyId with
        | None -> row##addEventListener 
                    (Js.string "click")
                    (fun evt -> let row = evt##.currentTarget in
                                  evtMethod (row##.rowIndex - 1))
        | Some id -> row##.id := Js.string id;
                     row##addEventListener 
                      (Js.string "click")
                      (fun evt -> let row = evt##.currentTarget in
                                  evtMethod (row##.rowIndex - 1))
      );
    let recognized = step.recognized in
    let acceptTable = step.acceptTable in
    let cell1 = row##insertCell (-1) in
      cell1##.innerHTML := Js.string recognized.recog;
    let cell2 = row##insertCell (-1) in
      cell2##.innerHTML := Js.string acceptTable.input;
    let cell3 = row##insertCell (-1) in
      cell3##.innerHTML := Js.string acceptTable.stack;
    let cell4 = row##insertCell (-1) in
      cell4##.innerHTML := Js.string acceptTable.production


  let test acceptList tree =
    let first = ref true in
    let rec test2 acceptList tree =
      match acceptList with
      | [] -> []
      | x::xs ->
        if !first then (first := false; None::test2 xs tree)
        else ( 
          match tree with
          | Leaf (a,b) -> if b <> epsilon 
                          then [Some a]
                          else []
          | Root (a,b,c) -> Some a :: (List.flatten (List.map (test2 xs) c))
        )
    in
    let steps = (test2 acceptList tree) in
    steps@[None]


  let checkAcceptError step alertError =
    if !alertError
    then (
      match step.accepted with
      | None -> ()
      | Some a -> 
        let input = removeDollarFromWord step.acceptTable.input in
        let stack = removeDollarFromWord step.acceptTable.stack in
        alertError := false;
        match a with
        | true -> ()
        | false -> if input = "" && stack <> ""
                    then JS.alertStr (Lang.i18nAlertCFGAcceptNoInput())
                    else (
                      if input <> "" && stack = ""
                      then JS.alertStr (Lang.i18nAlertCFGAcceptNoStack())
                      else (
                        JS.alertStr (Lang.i18nAlertCFGAcceptNoProduction())
                      )
                    )
    )
    

  class model (arg: (t,tx) Arg.alternatives) =
    object(self) inherit ContextFreeGrammarBasicGraphics.model arg as super

      val mutable acceptList = []
      val mutable currPos = 0 (*For knowing accept step state*)
      val mutable maxPos = 0 (*Draw table until position maxPos*)
      val mutable acceptList2 = []
      val mutable isOver = false
      val alertError = ref true (*For showing accept fail alert only once*)
      val mutable transformationIndex = 0
      val mutable transformations = [||]

      method nSteps = 
        List.length acceptList2

      method first1 =
        first super#first super#representation

      method follow1 =
        follow super#follow super#representation

      method createFirstAndFollowTableHtml =
        createFirstAndFollowTableHtml super#representation (super#first) (super#follow)

      method lookahead1 r =
        let lookaheadSet = super#lookahead r in
          setToHtmlString lookaheadSet

      method createGrammarTableHtml id =
        createGrammarTableHtml super#representation id

      method createParsingTableHtml =
        createParsingTableHtml super#representation self#createParsingTable

      method private paintParsingTableHtml term var =
        paintParsingTableHtml term var 
        
      method createParsingTableGuideHtml =                
        createParsingTableGuideHtml () 
      
      method accept1 w = 
        acceptList <- self#acceptZ w;

      method startAccept cy word =
        let initListeners =
          let epsilon = symb2str epsilon in
          cy##on (Js.string "click") (Js.string "node") (
            fun evt ->
              let id =  
                (match Js.Opt.to_option (evt##.target) with
                | None -> ""
                | Some b -> 
                  let b = Js.Unsafe.coerce b in
                  if (Js.to_string b##data##.label) = epsilon
                    then (
                      (match Js.Optdef.to_option (Js.array_get (b##incomers (Js.string "node")) 0) with
                      | None -> "" (*Should not happen*)
                      | Some elem -> Js.to_string elem##data##.id
                      )
                    )
                  else 
                    Js.to_string b##data##.id
                ) in
                let row = Dom_html.getElementById_opt id in
                  match row with
                  | None -> () (*Row may not exist yet*)
                  | Some r -> 
                    let r = Js.Unsafe.coerce r in
                    self#selectStep cy (r##.rowIndex - 1)
          );
          cy##on (Js.string "mouseover") (Js.string ("node[label = \'" ^ epsilon ^ "\']"))
            (fun evt ->
              let node = Js.Unsafe.coerce (match Js.Opt.to_option evt##.target with | None -> Dom_html.getElementById("") (*Should not happen*) | Some n -> n) in
              Firebug.console##log node
            );
        in
        self#accept1 word;
        acceptList2 <- acceptListNoRepeats acceptList;
        let listWithIds = sliceList (test acceptList2 (buildTree (sliceTree acceptList2 (List.length acceptList2)))) ((List.length acceptList2) - 1) in
        acceptList2 <- List.map2 (
          fun {syntaxTable; acceptedString;
            acceptTable; recognized;
            accepted; nodes;
            cyId} id -> 
              {syntaxTable; acceptedString;
              acceptTable; recognized;
              accepted; nodes;
              cyId = id}
        ) acceptList2 listWithIds;
        currPos <- 0;
        maxPos <- 0;
        isOver <- false;
        alertError := true;
        initListeners;
        let guide = Dom_html.getElementById "cfgGuideTable" in
        guide##.innerHTML := Js.string "";
        self#createParsingTableGuideHtml;
        self#updateElements cy true
      
      method next cy =
        (if currPos = List.length acceptList2 - 1
          then (currPos <- currPos; isOver <- true)
          else currPos <- currPos + 1);
        if maxPos < currPos 
        then (maxPos <- currPos; self#updateElements cy true)
        else self#updateElements cy false
        
      method back cy = 
        (if currPos <= 0
          then currPos <- currPos
          else currPos <- currPos - 1);
        self#updateElements cy false

      method private paintWord step =
        paintWord step
      
      method private paintGrammarTable step =
        paintGrammarTable step                     
      
      method private updateElements (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) insert =
        let step = List.nth acceptList2 currPos in
          if insert && not isOver 
          then 
            (insertStep (Js.Unsafe.coerce (Dom_html.getElementById "cfgGuideTable")) (List.nth acceptList2 currPos) (self#selectStep cy);
            addNodesToParent cy step.nodes step.cyId;
          );
          paintNodes (Js.Unsafe.coerce cy) step;
          paintGrammarTable step;
          paintWord step;
          paintParsingTableHtmlGuide step currPos;
          (*The following tables may not be visible, don't crash*)
          let cy2Tables = Dom_html.getElementById_opt (firstFollowTableId()) in
          (match cy2Tables with
          | None -> ()
          | Some a -> 
            if (Js.Unsafe.coerce a)##.rows##.length = 0 (*if visible but not drawn, draw them*)
            then (
              self#createFirstAndFollowTableHtml;
              self#createParsingTableHtml
            );
            paintParsingTableHtml (step.syntaxTable.term) (step.syntaxTable.var)
          );
        checkAcceptError step alertError


      method clearCy (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) =
        Cytoscape.removeAllElements cy

      method selectStep cy index =
        currPos <- index;
        self#updateElements cy false

      method private newTransformation ( t : LL1Grammar.transformation list ) : unit =
        let rec toGraphics ( l : LL1Grammar.transformation list ) : ContextFreeGrammarLL1Graphics.transformation list =
          match l with
          | [] -> []
          | x::xs -> 
            let newModel = new ContextFreeGrammarLL1Graphics.model (Representation x.grammar#representation) in
            { tType = x.tType; grammar = newModel }
             :: toGraphics xs
        in
        transformationIndex <- List.length t - 1;
        let t = toGraphics t in
        transformations <- Array.of_list t

      method clean1 =
        let newGrammars = super#clean in
        self#newTransformation newGrammars;
        let pos = (List.length newGrammars - 1) in
          transformations.(pos)

      method removeLeftRecursion1 =
        let newGrammar = super#removeLeftRecursion in
        self#newTransformation [newGrammar];
          transformations.(0)

      method leftFactoring1 =
        let newGrammar = super#leftFactoring in
        self#newTransformation [newGrammar];
          transformations.(0)

      method removeEmptyProductions1 =
        let newGrammar = super#removeEmptyProductions in
        self#newTransformation [newGrammar];
          transformations.(0)

      method removeUnitProductions1 =
        let newGrammar = super#removeUnitProductions in
        self#newTransformation [newGrammar];
          transformations.(0)

      method getPreviousTransformed =
        (if transformationIndex <= 0
          then transformationIndex <- transformationIndex
          else transformationIndex <- transformationIndex - 1);
        transformations.(transformationIndex)
      
      method getNextTransformed =
        (if transformationIndex = Array.length transformations - 1
          then transformationIndex <- transformationIndex
          else transformationIndex <- transformationIndex + 1);
        transformations.(transformationIndex)

      method transformToLL1X =
        let newGrammars = super#transformToLL1 in
        self#newTransformation newGrammars;
        let pos = (List.length newGrammars - 1) in
          transformations.(pos)

      method toggleSimplified =
        super#toggleSimplified
    end
  end
