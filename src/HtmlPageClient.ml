(*
 * HtmlPageClient.ml
 *
 * This file is part of the OFLAT app
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
 *  Written by Rita Macedo
 *)

(* 
 * Description: Visualizer component of the application.
 *)

open OCamlFlat
open BasicTypes
open Js_of_ocaml
open Graphics
open Lang
open Listeners
open StateVariables
open JS
open ContextFreeGrammarGraphics

module HtmlPageClient: sig
  open BasicTypes
  val oneBox: unit -> unit
  val twoBoxes: unit -> unit
  val clearBox1: unit -> unit
  val clearBox2: unit -> unit
  val fitBoxRegex: unit -> unit
  val closeBoxRegex: unit -> unit
  val cfgBoxRegex: unit -> unit
  val restoreCy: unit -> unit
  val cfgCyClose: unit -> unit
  val cfgCyOpen: unit -> unit
  val cfgCy2Close: unit -> unit

  val putCyAutomataButtons: unit -> unit
  val closeInfo: unit -> unit
  val putCyButtons: unit -> unit
  val putCy2Buttons: unit -> unit
  val putCyREButtons: unit -> unit
  val putCyCFGButtons: unit -> unit
  val defineCFG: unit -> unit
  val printCFGGrammar: string -> unit
  val printCFG2Grammar: string -> string -> unit
  val putCyTMButtons: unit -> unit
  
  val prepareCFG2Tables: unit -> unit
  val printCFG2AllTables: Js_of_ocaml.Dom_html.tableElement Js_of_ocaml__.Js.t -> Js_of_ocaml.Dom_html.tableElement Js_of_ocaml__.Js.t -> unit
  val printCFGCurrentWord: string -> string -> string -> bool option -> unit
  val printCFGTableGuide: Js_of_ocaml.Dom_html.tableElement Js_of_ocaml__.Js.t -> unit
(*  val printCFG2Accept: string -> string -> string -> string -> string -> string -> bool option -> unit*)
  val printCFGAccept: string -> string -> string -> Js_of_ocaml.Dom_html.tableElement Js_of_ocaml__.Js.t -> Js_of_ocaml.Dom_html.tableElement Js_of_ocaml__.Js.t -> Js_of_ocaml.Dom_html.tableElement Js_of_ocaml__.Js.t -> string -> string -> string -> bool option -> unit
  val prepareCFG2RecursiveDescedentParser: string list -> (string -> string) -> unit
  val printCFG2RecursiveDescedentParser: string -> unit
  val getIsLL1: bool -> Dom_html.divElement Js.t -> unit
  val getIsLeftRecursive: bool -> Dom_html.divElement Js.t -> unit
  val getIsLeftFactoring: bool -> Dom_html.divElement Js.t -> unit
  val getHasParsingTableConflict: bool -> Dom_html.divElement Js.t -> unit
  val getIsCFGClean: bool -> bool -> bool -> Dom_html.divElement Js.t -> unit

  val putButton: String.t -> unit
  val createSpanList: symbol list -> string -> string -> unit
  val createPropertiesList: string -> string -> string -> unit

  val getDeterminim: bool -> Dom_html.divElement Js.t -> unit
  val getMinimism: bool -> Dom_html.divElement Js.t -> unit
  val getHasUselessStates: bool -> states -> Dom_html.divElement Js.t -> unit
  val getNumberStates: int -> Dom_html.divElement Js.t -> unit
  val getNumberTransitions: int -> Dom_html.divElement Js.t -> unit
  val getIsLinearBounded: bool -> Dom_html.divElement Js.t -> unit
  val defineInformationBox: bool -> Dom_html.divElement Js.t
  val defineRE: string -> bool -> unit

  val putWords: words -> unit

  val putEnumButton: unit -> unit
  val addEnumTitle: unit -> unit
  val defineEnumProblem: string -> unit
  val addAcceptedTitle: unit -> unit
  val addNonAcceptTitle: unit -> unit
  val addPropertiesBox: unit -> unit
  val addEnumCheckButton: unit -> unit
  val defineResult: bool -> unit
  val defineTreeButtons: unit -> unit

  val changeLang: unit -> unit 

  val disableButtons: string -> unit
  val enableAllButtons: string -> unit

  val defineMainTitle: string -> unit

  val about: unit -> unit 
  val feedback: unit -> unit

  val showModelInfo: string -> unit
  val putTreeResult: string -> unit
  val putTreeGoodDerivations: string -> unit
  val putTreeBadDerivations: string -> unit
  val putTreeNumbers: string -> unit

  val editModelContent: string -> string -> string -> (unit -> 'a) -> Js_of_ocaml.Dom_html.divElement Js_of_ocaml__.Js.t 
  val setModal: Js_of_ocaml.Dom.node Js_of_ocaml__.Js.t -> unit
  val hideModalWindow: unit -> unit
  val showModalWindow: unit -> unit

  val showModal: unit -> unit

  val disableButton: string -> unit
  val enableButton: string -> unit

end 
= 

struct
  let button_type = Js.string "button"
let div_type = Js.string "div"
let text_type = Js.string "textarea"
let input_type = Js.string "file"
let doc = Dom_html.document


let button txt idtxt action =
let b = Dom_html.createButton ~_type:button_type doc in
b##.id := Js.string idtxt;
b##.innerHTML := Js.string txt;
b##.onclick := Dom_html.handler (fun _ -> action (); Js._true);
b

let button1 txt idtxt classtxt action =
let b = Dom_html.createButton ~_type:button_type doc in
b##.id := Js.string idtxt;
b##.innerHTML := Js.string txt;
b##.className := Js.string classtxt;
b##.onclick := Dom_html.handler (fun _ -> action (); Js._true);
b

let input idtxt action = 
let b = Dom_html.createInput ~_type:input_type doc in
b##.id := Js.string idtxt;
b##.onchange := Dom_html.handler (fun _ -> action (); Js._true);
b

let div idtxt =
let d = Dom_html.createDiv doc in
d##.id := Js.string idtxt;
d

let div1 idtxt txt =
let d = Dom_html.createDiv doc in
  d##.id := Js.string idtxt;
  d##.innerHTML := Js.string txt;
d

let div2 idtxt classtxt txt =
let d = Dom_html.createDiv doc in
d##.id := Js.string idtxt;
d##.className := Js.string classtxt;
d##.innerHTML := Js.string txt;
d

let div3 idtxt classtxt =
  let d = Dom_html.createDiv doc in
  d##.id := Js.string idtxt;
  d##.className := Js.string classtxt;
  d

let textarea idtxt rows cols value =
let t = Dom_html.createTextarea doc in
  t##.id := Js.string idtxt;
  t##.rows := rows;
  t##.cols := cols;
  t##.value := value;
t

let h2 idtxt txt =
let h = Dom_html.createH2 doc in
    h##.id := Js.string idtxt;
    h##.innerHTML := Js.string txt;
  h

let h1 idtxt txt =
let h = Dom_html.createH1 doc in
  h##.id := Js.string idtxt;
  h##.innerHTML := Js.string txt;
h

let span idtxt txt =
let s = Dom_html.createSpan doc in
  s##.id := Js.string idtxt;
  s##.innerHTML := Js.string txt;
s

let span1 idtxt classtxt txt =
  let s = Dom_html.createSpan doc in
    s##.id := Js.string idtxt;
    s##.className := Js.string classtxt;
    s##.innerHTML := Js.string txt;
  s

let p idtxt txt =
let p = Dom_html.createP doc in
  p##.id := Js.string idtxt;
  p##.innerHTML := Js.string txt;
p

let br idtxt =
let b = Dom_html.createBr doc in
    b##.id := Js.string idtxt;
b

let a ref1 txt = 
let c = Dom_html.createA doc in
  c##.href := Js.string ref1;
  c##.innerHTML := Js.string txt;
c

let a1 idtxt ref1 txt = 
let c = Dom_html.createA doc in
  c##.id := Js.string idtxt;
  c##.href := Js.string ref1;
  c##.innerHTML := Js.string txt;
c

let pre idtxt txt =
let p = Dom_html.createPre doc in
  p##.id := Js.string idtxt;
  p##.innerHTML := Js.string txt;
p

let code idtxt txt =
let c = Dom_html.createPre doc in
  c##.id := Js.string idtxt;
  c##.innerHTML := Js.string txt;
c

let table idtxt txt =
let t = Dom_html.createTable doc in
  t##.id := Js.string idtxt;
  t##.innerHTML := Js.string txt;
t

let select id elementsList =
  let select = Dom_html.createSelect doc in
    select##.id := Js.string id;
    List.iter
      (fun l ->
        let option = Dom_html.createOption doc in
        Dom.appendChild option (doc##createTextNode (Js.string l));
        Dom.appendChild select option
      ) elementsList;
  select

let putInnerHtml idtxt txt =
  let element = Dom_html.getElementById idtxt in
  element##.innerHTML := Js.string txt

let putInnerHtmlButtons idtxt txt idtool classTool txt1 =
  let element = Dom_html.getElementById idtxt in
    element##.innerHTML := (Js.string txt);
    let tool = div2 idtool classTool txt1 in
      Dom.appendChild element tool

  let listOnlyAutomataButtons = ["backwards"; "start"; "forward"; "selectRegex"; "selectTuringMachine"]
  let listOnlyTMButtons = ["backwards"; "start"; "forward"]
  let listOnlyExpressionButtons = ["selectAutomaton"; "selectTuringMachine"]
  let listOnlyCFGButtons = [(*"testing";"generate";*)"backwards"; "start"; "forward"]
  let listOtherButtons = ["testing"; "generate"; "fitGraph"; "editModel"]

  let defineMainTitle type1 =
    let title = Dom_html.getElementById "mainTitle" in
      title##.innerHTML := Js.string "";
    if type1 = "finite automaton" then
      title##.innerHTML := Js.string (Lang.i18nMainTitle1 ())
    else if type1 = "regular expression" then
      title##.innerHTML := Js.string (Lang.i18nMainTitle2 ())
    else if type1 = "exercise" then
      title##.innerHTML := Js.string (Lang.i18nMainTitle3 ())
    else if type1 = "context free grammar" then
      title##.innerHTML := Js.string (Lang.i18nMainTitle4 ())
    else if type1 = "turing machine" then
      title##.innerHTML := Js.string (Lang.i18nMainTitleTM ())
    else ()

  let disableButton buttonName =
    let buttonTo = Dom_html.getElementById buttonName in
      buttonTo##setAttribute (Js.string "disabled") (Js.string "disabled")
  
  let enableButton buttonName =
    let buttonTo = Dom_html.getElementById buttonName in
      buttonTo##removeAttribute (Js.string "disabled")

  let disableButtons type1 =
    if type1 = "finite automaton" then
      (List.iter (fun el -> disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> disableButton el) listOnlyTMButtons;
      List.iter (fun el -> enableButton el) listOnlyAutomataButtons)
    else if type1 = "regular expression" then
      (List.iter (fun el -> disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> disableButton el) listOnlyTMButtons;
      List.iter (fun el -> enableButton el) listOnlyExpressionButtons)
    else if type1 = "context free grammar" then
      (List.iter (fun el -> disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> disableButton el) listOnlyTMButtons;
      List.iter (fun el -> enableButton el) listOnlyCFGButtons)
    else if type1= "turing machine" then
      (List.iter (fun el -> disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> enableButton el) listOnlyTMButtons)
    else
      (List.iter (fun el -> disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> disableButton el) listOnlyTMButtons;
      List.iter (fun el -> disableButton el) listOtherButtons)

    let enableAllButtons type1 =
      List.iter (fun el -> enableButton el) listOtherButtons;
      if type1 = "finite automaton" then
        (List.iter (fun el -> enableButton el) listOnlyAutomataButtons)

  let createSpanList word acceptance list =
    let element = Dom_html.getElementById list in
    let string_of_word = String.concat "" (List.map symb2str word) in
      let ac = span acceptance ("' " ^ string_of_word ^ " '") in 
        Dom.appendChild element ac;
      let space = br "br" in 
        Dom.appendChild element space

  let createPropertiesList word acceptance list =
    let element = Dom_html.getElementById list in
      let ac = span acceptance word in 
        Dom.appendChild element ac;
        let space = br "br" in 
          Dom.appendChild element space

  let createBox1 () =
    let box1 = Dom_html.getElementById_opt "Box1" in
    match box1 with
    | None -> ()
    | Some box1 ->
      let buttonBox = div "buttonBox" in
      let regExp = div "regExp" in
      let cy = div "cy" in
      let infoBox = div "infoBox" in
        Dom.appendChild box1 buttonBox;
        Dom.appendChild box1 regExp;
        Dom.appendChild box1 cy;
        Dom.appendChild box1 infoBox
      
  let createBox2 () =
    let box2 = Dom_html.getElementById_opt "Box2" in
    match box2 with
    | None -> ()
    | Some box2 ->
      let buttonBox1 = div "buttonBox1" in
      let textBox = div "textBox" in
      let cy2 = div "cy2" in
      let infoBox2 = div "infoBox2" in
        Dom.appendChild box2 buttonBox1;
        Dom.appendChild box2 textBox;
        Dom.appendChild box2 cy2;
        Dom.appendChild box2 infoBox2

  let clearBox1 () =
    putInnerHtml "Box1" "";
    createBox1 ()
    
  let clearBox2 () =
    putInnerHtml "Box2" "";
    createBox2 ()
    
  
  let closeButton () =
    let c = button1 "X" "closeLeft" "tooltip1" !Listeners.closeLeftListener in             
    let tool = div2 "tooltipCloseLeft" "tooltiptext1" (Lang.i18nTooltipCloseLeft ()) in
      Dom.appendChild c tool;
    c
    
  let putCyButtons () =
    let buttonBox = Dom_html.getElementById "buttonBox" in
      buttonBox##.innerHTML := Js.string "";
    let divButtons2 = div "close" in
      Dom.appendChild buttonBox divButtons2;
    let di = Dom_html.getElementById "close" in
      Dom.appendChild di (closeButton());
    let info = button1 (Lang.i18nFormatting  ()) "formatting" "tooltip2" !Listeners.showModelListener in 
      Dom.appendChild di info;
    let tool = div2 "tooltipSpecification" "tooltiptext2" (Lang.i18nTooltipSpecification ()) in
      Dom.appendChild info tool
      
  let putCy2Buttons () = 
    let buttonBox = Dom_html.getElementById "buttonBox1" in
      let test1 = button1 "X" "closeRight" "tooltip1" !Listeners.closeRightListener in 
        Dom.appendChild buttonBox test1;
      let tool = div2 "tooltipCloseRight" "tooltiptext1" (Lang.i18nTooltipCloseRight ()) in
        Dom.appendChild test1 tool

  let oneBox () = 
    let box1 = Dom_html.getElementById "Box1" in
      box1##.style##.width:= Js.string "99%";
    let box2 = Dom_html.getElementById "Box2" in
      box2##.style##.width:= Js.string "0%";
    clearBox2 ()

  (*Shows 2 boxes side-by-side.*)
  let twoBoxes () =
    clearBox2 ();
    putCy2Buttons();
    let box1 = Dom_html.getElementById "Box1" in
      box1##.style##.width:= Js.string "49.5%";
    let box2 = Dom_html.getElementById "Box2" in
      box2##.style##.width:= Js.string "49.5%"

  let fitBoxRegex () = 
	let regExp = Dom_html.getElementById "regExp" in
		regExp##.style##.height:= Js.string "6.5%";
	let cy = Dom_html.getElementById "cy" in
      cy##.style##.height:= Js.string "50vh"
      
  let closeBoxRegex () =
	let regExp = Dom_html.getElementById "regExp" in
		regExp##.style##.height:= Js.string "0%";
	let cy = Dom_html.getElementById "cy" in
      cy##.style##.height:= Js.string "60vh"
		
  let cfgBoxRegex () =
  let regExp = Dom_html.getElementById "regExp" in
    regExp##.style##.height := Js.string "auto";
    regExp##.style##.overflow := Js.string "unset";
  let cy = Dom_html.getElementById "cy" in
    cy##.style##.height:= Js.string "40vh";
    cy##.style##.textAlign:= Js.string "unset";
    cy##.style##.marginLeft:= Js.string "50%"
(*    cy##.classList##add (Js.string "cyBorder")*)

  let restoreCy () =
  let cy = Dom_html.getElementById "cy" in
    cy##.style##.marginLeft:= Js.string "unset"
(*    cy##.classList##remove (Js.string "cyBorder")*)

  let cfgCyClose() =
    let cy = Dom_html.getElementById "cy" in
    cy##.style##.height:= Js.string "0vh"

  let cfgCyOpen() =
    let cy = Dom_html.getElementById "cy" in
    cy##.style##.height:= Js.string "40vh"
  
  let cfgCy2Close() =
    let cy = Dom_html.getElementById "cy2" in
    cy##.style##.height:= Js.string "0vh"

  let editModelContent titleTxt otherTxt textAreaContent okAction =
    let div = div "modelContentDiv" in
    let title = div1 "editModelTitle" titleTxt in
    let other = div1 "editModelOther" otherTxt in
    let textarea = textarea "modelStringContainer" 5 50 (Js.string textAreaContent) in
    let buttonOk = button (Lang.i18nConfirm()) "modelContentButtonOk" okAction in
      Dom.appendChild div title;
      Dom.appendChild div other;
      Dom.appendChild div textarea;
      Dom.appendChild div buttonOk;
      div
  
  let setModal content = 
    let modalC = Dom_html.getElementById "modal-content" in
    modalC##.innerHTML := Js.string "";
    let break = br "" in
      Dom.appendChild modalC break;
      Dom.appendChild modalC content

  let hideModalWindow () =
    let modal = Dom_html.getElementById "myModal" in
    modal##.style##.display := Js.string "none"

  let showModalWindow () = (*TODO There already exists a showModal, name may change*)
    let modal = Dom_html.getElementById "myModal" in
    modal##.style##.display := Js.string "block"

  let showModelInfo info = 
    JS.log ("Estou a por informacao");
    let textBox = Dom_html.getElementById "textBox" in
      let test = pre "infoAutomata" info in 
        Dom.appendChild textBox test

  let putCyAutomataButtons () =
    putCyButtons();
    let buttonBox = Dom_html.getElementById "buttonBox" in
    let divButtons1 = div "min" in
      Dom.appendChild buttonBox divButtons1;
    let c = button1 (Lang.i18nClean  ()) "clean" "tooltip3" !ListenersFA.cleanUselessListener in 
      Dom.appendChild divButtons1 c;
      let tool = div2 "tooltipClean" "tooltiptext3" (Lang.i18nTooltipClean ()) in
        Dom.appendChild c tool;
    let de = button1 (Lang.i18nDeterministic  ()) "deterministic" "tooltip3" !ListenersFA.getDeterministicListener in 
      Dom.appendChild divButtons1 de;
      let tool = div2 "tooltipDeterministic" "tooltiptext3" (Lang.i18nTooltipDeterministic ()) in
        Dom.appendChild de tool;
    let de = button1 (Lang.i18nMinimize  ()) "minimize" "tooltip3" !ListenersFA.defineMinimizedListener in 
      Dom.appendChild divButtons1 de;
      let tool = div2 "tooltipMinimize" "tooltiptext3" (Lang.i18nTooltipMinimize ()) in
        Dom.appendChild de tool;
    let divButtons = div "prod" in
      Dom.appendChild buttonBox divButtons;
    let b = button1 (Lang.i18nProductive  ()) "productive" "tooltip3" !ListenersFA.paintAllProductivesListener in 
      Dom.appendChild divButtons b;
      let tool = div2 "tooltipProductive" "tooltiptext3" (Lang.i18nTooltipProductive ()) in
        Dom.appendChild b tool;
    let a = button1 (Lang.i18nAccessible ()) "accessible" "tooltip3" !ListenersFA.paintAllReachableListener in
      Dom.appendChild divButtons a;
      let tool = div2 "tooltipAccessible" "tooltiptext3" (Lang.i18nTooltipAccessible ()) in
        Dom.appendChild a tool;
    let u = button1 (Lang.i18nUseful ()) "useful" "tooltip3" !ListenersFA.paintAllUsefulListener in
      Dom.appendChild divButtons u;
      let tool = div2 "tooltipUseful" "tooltiptext3" (Lang.i18nTooltipUseful ()) in
        Dom.appendChild u tool;
    let divButtons3 = div "clear" in
      Dom.appendChild buttonBox divButtons3;
    let clearAuto = button1 (Lang.i18nClearAuto ()) "clearAuto" "tooltip3" !ListenersFA.clearAutoListener in
      Dom.appendChild divButtons3 clearAuto;
      let tool = div2 "tooltipClearAuto" "tooltiptext3" (Lang.i18nTooltipClear ()) in
        Dom.appendChild clearAuto tool

  let showModal () =
    let modalC = Dom_html.getElementById "modal-content" in 
      let blah = p "text" "Isto vai ser o texto do accept das expressões regulares" in
        Dom.appendChild modalC blah

  let closeInfo () =
    let buttonBox = Dom_html.getElementById "buttonBox1" in
      let test = button1 "X" "closeRight" "tooltip1" !Listeners.closeRightListener in 
        Dom.appendChild buttonBox test;
        let tool = div2 "tooltipCloseRight" "tooltiptext1" (Lang.i18nTooltipCloseRight ()) in
          Dom.appendChild test tool
    
  let putCyREButtons() =
    putCyButtons();
    let buttonBox = Dom_html.getElementById "buttonBox" in
    let row1 = div "row1" in
      Dom.appendChild buttonBox row1;
    let direction = button1 (Lang.i18nDirection ()) "changeDirection" "tooltip3" !ListenersRE.changeDirectionListener in 
      Dom.appendChild row1 direction;
    let tool = div2 "tooltipDirection" "tooltiptext3" (Lang.i18nTooltipDirection ()) in
      Dom.appendChild direction tool

  let putCyCFGButtons () =
    putCyButtons();
    let buttonBox = Dom_html.getElementById "buttonBox" in
    let row1 = div "row1" in
      Dom.appendChild buttonBox row1;
    let clean = button1 (Lang.i18nClean ()) "cleanCFG" "tooltip3" !ListenersCFG.cleanCFGListener in
      Dom.appendChild row1 clean;
      let tool = div2 "tooltipCleanCFG" "tooltiptext3" (Lang.i18nTooltipCFGClean ()) in
        Dom.appendChild clean tool;
    let rLRec = button1 (Lang.i18nRemoveLeftRecursion ()) "rLeftRecursion" "tooltip3" !ListenersCFG.removeLeftRecursionListener in
      Dom.appendChild row1 rLRec;
      let tool = div2 "tooltipRemoveLeftRecursion" "tooltiptext3" (Lang.i18nTooltipRemoveLeftRecursion ()) in
        Dom.appendChild rLRec tool;      
    let lf = button1 (Lang.i18nLeftFactoring ()) "leftFactorization" "tooltip3" !ListenersCFG.leftFactoringListener in
      Dom.appendChild row1 lf;
      let tool = div2 "tooltipLeftFactorization" "tooltiptext3" (Lang.i18nTooltipLeftFactoring ()) in
        Dom.appendChild lf tool;      
    let row2 = div "row2" in
      Dom.appendChild buttonBox row2;
      let removeEpsilon = button1 (Lang.i18nRemoveEpsilonProductions ()) "rEpsilon" "tooltip3" !ListenersCFG.removeEpsilonListener in
        Dom.appendChild row2 removeEpsilon;
        let tool = div2 "tooltipRemoveEpsilon" "tooltiptext3" (Lang.i18nTooltipRemoveEpsilonProductions ()) in
          Dom.appendChild removeEpsilon tool;
      let removeUnit = button1 (Lang.i18nRemoveUnitProductions ()) "rUnit" "tooltip3" !ListenersCFG.removeUnitListener in
        Dom.appendChild row2 removeUnit;
        let tool = div2 "tooltipRemoveUnit" "tooltiptext3" (Lang.i18nTooltipRemoveUnitProductions ()) in
          Dom.appendChild removeUnit tool;
      let transformLL1 = button1 (Lang.i18nTransformToLL1 ()) "transformLL1" "tooltip3" !ListenersCFG.transformLL1Listener in
        Dom.appendChild row2 transformLL1;
        let tool = div2 "tooltipTransformLL1" "tooltiptext3" (Lang.i18nTooltipTransformLL1 ()) in
          Dom.appendChild transformLL1 tool;
    let row3 = div "row3" in
      Dom.appendChild buttonBox row3;
      let parsingTable = button1 (Lang.i18nSetTables ()) "setTables" "tooltip3" !ListenersCFG.tablesListener in
        Dom.appendChild row3 parsingTable;
        let tool = div2 "tooltipSetTables" "tooltiptext3" (Lang.i18nTooltipSetTables ()) in
          Dom.appendChild parsingTable tool;
      let rdParser = button1 (Lang.i18nRDParser ()) "rdparserCFG" "tooltip3" !ListenersCFG.recursiveDescedentParserListener in 
        Dom.appendChild row3 rdParser;
        let tool = div2 "tooltipRDParserCFG" "tooltiptext3" (Lang.i18nTooltipRDParser ()) in 
          Dom.appendChild rdParser tool;
    let row4 = div "row4" in
      Dom.appendChild buttonBox row4;
    let simpleToggle = button1 (Lang.i18nToggle ()) "toggle" "tooltip3" !ListenersCFG.simpleToggleListener in 
      Dom.appendChild row4 simpleToggle;
      let tool = div2 "tooltipRDParserCFG" "tooltiptext3" (Lang.i18nTooltipToggleMode ()) in 
        Dom.appendChild simpleToggle tool
        
  let defineCFG () =
    let cfgString = Dom_html.getElementById "regExp" in
      let expr = div1 "cfg" "" in
        Dom.appendChild cfgString expr;
      let cfgTable = table "cfgProductionsTable" "" in
        Dom.appendChild expr cfgTable;
      let break = br "grammarToAcceptBr" in
        Dom.appendChild cfgString break;
      let acceptDiv = div "cfgAccept" in
        Dom.appendChild cfgString acceptDiv;
        let acceptWordDiv = div "cfgAcceptWord" in
        let acceptTableDiv = div "cfgAcceptTable" in
          Dom.appendChild acceptDiv acceptWordDiv;
          Dom.appendChild acceptDiv acceptTableDiv;
          let acceptTable = table "cfgGuideTable" "" in
          Dom.appendChild acceptTableDiv acceptTable;
      putInnerHtml "infoBox" ""

  let putCyTMButtons () =
    putCyButtons();
    let buttonBox = Dom_html.getElementById "buttonBox" in
    let divButtons1 = div "min" in
      Dom.appendChild buttonBox divButtons1;
    let c = button1 (Lang.i18nClean  ()) "clean" "tooltip3" !ListenersTM.cleanUselessListener in 
      Dom.appendChild divButtons1 c;
      let tool = div2 "tooltipClean" "tooltiptext3" (Lang.i18nTooltipClean ()) in
        Dom.appendChild c tool;
    let divButtons = div "prod" in
      Dom.appendChild buttonBox divButtons;
    let b = button1 (Lang.i18nProductive  ()) "productive" "tooltip3" !ListenersTM.paintAllProductivesListener in 
      Dom.appendChild divButtons b;
      let tool = div2 "tooltipProductive" "tooltiptext3" (Lang.i18nTooltipProductive ()) in
        Dom.appendChild b tool;
    let a = button1 (Lang.i18nAccessible ()) "accessible" "tooltip3" !ListenersTM.paintAllReachableListener in
      Dom.appendChild divButtons a;
      let tool = div2 "tooltipAccessible" "tooltiptext3" (Lang.i18nTooltipAccessible ()) in
        Dom.appendChild a tool;
    let u = button1 (Lang.i18nUseful ()) "useful" "tooltip3" !ListenersTM.paintAllUsefulListener in
      Dom.appendChild divButtons u;
      let tool = div2 "tooltipUseful" "tooltiptext3" (Lang.i18nTooltipUseful ()) in
        Dom.appendChild u tool;
    let divButtons3 = div "clear" in
      Dom.appendChild buttonBox divButtons3;
    let clearAuto = button1 (Lang.i18nClearAuto ()) "clearAuto" "tooltip3" !ListenersTM.clearAutoListener in
      Dom.appendChild divButtons3 clearAuto;
      let tool = div2 "tooltipClearAuto" "tooltiptext3" (Lang.i18nTooltipClear ()) in
        Dom.appendChild clearAuto tool
  
  let addBr idtxt addTo =
    let break = br idtxt in
      Dom.appendChild addTo break

  let printCFGGrammar grammarHTML =
    let grammarElement = Dom_html.getElementById "cfg" in
      grammarElement##.innerHTML := Js.string grammarHTML

  let getTransformLang (transformType : string) : string =
    if transformType = LL1Grammar.leftRecursionRemovalTransform
    then Lang.i18nRemoveLeftRecursion()
    else
    if transformType = LL1Grammar.leftFactoringTransform
    then Lang.i18nLeftFactoring()
    else
    if transformType = LL1Grammar.cleanProductiveTransform
    then Lang.i18nRemoveUnproductive()
    else
    if transformType = LL1Grammar.cleanAccessibleTransform
    then Lang.i18nRemoveInaccessible()
    else
    if transformType = LL1Grammar.unitRemovalTransform
    then Lang.i18nRemoveUnitProductions()
    else
    if transformType = LL1Grammar.epsilonRemovalTransform
    then Lang.i18nRemoveEpsilonProductions()
    else
    if transformType = LL1Grammar.ll1Transform
    then Lang.i18nTransformToLL1()
    else transformType

  let printCFG2Grammar transformType tableId = 
    let textBox = Dom_html.getElementById "textBox" in
      textBox##.innerHTML := Js.string "";
    let newGrammarElem = div "cfgGrammar2" in
      Dom.appendChild textBox newGrammarElem;
    let title = h2 "newGrammarTitle" (Lang.i18nNewGrammar()) in
      Dom.appendChild newGrammarElem title;
      let prevButton = button1 (Lang.i18nPrevious()) "previousNewCfg" "tooltip3" !ListenersCFG.previousNewCFGListener in
      let next = button1 (Lang.i18nNext()) "nextNewCfg" "tooltip3" !ListenersCFG.nextNewCFGListener in
      let transformLabel = span "transformLabel" (getTransformLang transformType) in
      Dom.appendChild newGrammarElem transformLabel;
      addBr "infoBoxBr" newGrammarElem;
      Dom.appendChild newGrammarElem prevButton;
      Dom.appendChild newGrammarElem next;
      addBr "infoBoxBr" newGrammarElem;
    let grammarTable = table tableId "" in
      Dom.appendChild newGrammarElem grammarTable;
    let grammarDiv = div "grammarBox" in
      Dom.appendChild newGrammarElem grammarDiv

  let printCFGTableGuide table =
    let textBox = Dom_html.getElementById "cfgAcceptTable" in
      textBox##.innerHTML := Js.string "";
      ignore (textBox##appendChild (Js.Unsafe.coerce @@ table))
  
  let printCFGCurrentWord word1 word2 origWord wordAccepted =
    let textBox = Dom_html.getElementById "cfgAcceptWord" in
      textBox##.innerHTML := Js.string "";
      let word2 = (*ac|bc instead of ac|EC*)
        if String.length word1 > 0
          then String.sub origWord (String.length word1) ((String.length origWord) - (String.length word1))
          else String.sub origWord 0 ((String.length origWord) - (String.length word1))
      in
      let text = div2 "acceptLL1CurrentWord" ( 
        match wordAccepted with
        | None -> ""
        | Some a when a = true -> "wordAccepted"
        | Some a when a = false -> "wordRejected"
        | _ -> "" (*Should not happen*) )
        (word1 ^ "|" ^ word2)
      in
      Dom.appendChild textBox text

  let printTable id title tableString parent =
    let parentElement = Dom_html.getElementById parent in
    let title = h2 (id ^ "Title") title in
      Dom.appendChild parentElement title;
    addBr "printBr" parentElement;
    let tableHTML = table id tableString in
      Dom.appendChild parentElement tableHTML

  let printTable2 id title table parent =
    let parentElement = Dom_html.getElementById parent in
    let title = h2 (id ^ "Title") title in
      Dom.appendChild parentElement title;
    addBr "printBr" parentElement;
      Dom.appendChild parentElement table

  let prepareCFG2Tables () =
    let parent = Dom_html.getElementById "textBox" in
    let id1 = ContextFreeGrammarGraphics.firstFollowTableId() in
    let id2 = ContextFreeGrammarGraphics.parsingTableId() in
    let firstFollowTable = Dom_html.getElementById_opt id1 in
    (match firstFollowTable with
      | None -> let title1 = h2 (id1 ^ "Title") (Lang.i18nFirstAndFollow()) in
                Dom.appendChild parent title1;
                Dom.appendChild parent (table "cfgFirstFollowTable" "")
      | Some a -> ());
    let parsingTable = Dom_html.getElementById_opt id2 in
    (match parsingTable with
      | None -> let title2 = h2 (id2 ^ "Title") (Lang.i18nParsingTable()) in
                Dom.appendChild parent title2;
                Dom.appendChild parent (table "cfgParsingTable" "")
      | Some a -> ())

  let printCFG2AllTables firstFollowTable parsingTable =
    printTable2 "cfgFirstFollowTable" (Lang.i18nFirstAndFollow()) firstFollowTable "textBox";
    printTable2 "cfgParsingTable" (Lang.i18nParsingTable()) parsingTable "textBox"

    
  let printCFGAccept titleAccept titleFirstFollow titleParsingTable firstFollowTable parsingTable parsingGuideTable word1 word2 origWord wordAccepted =
      printCFGCurrentWord word1 word2 origWord wordAccepted;
      printCFGTableGuide parsingGuideTable;
      printCFG2AllTables firstFollowTable parsingTable  

  let printCFG2RecursiveDescedentParser codeString =
    let rdParserCode = Dom_html.getElementById "rdParserCode" in
      rdParserCode##.innerHTML := Js.string "";
      Dom.appendChild rdParserCode (doc##createTextNode (Js.string codeString))

  let prepareCFG2RecursiveDescedentParser optsList changeFun =
    let textBox = Dom_html.getElementById "textBox" in
      let title = h2 "rdParserTitle" (Lang.i18nRDParser()) in
      let div = div "rdParserSelectDiv" in
        Dom.appendChild textBox title;
        Dom.appendChild textBox div;
      let select = select "rdParserSelect" optsList in
        select##.onchange :=
          Dom_html.handler (fun _ -> 
            let v = Js.to_string select##.value in
            printCFG2RecursiveDescedentParser (changeFun v);
            let button = Dom_html.getElementById("copyClipboardBtn") in
            button##.innerHTML := Js.string (Lang.i18nRDParserCopyButtonDefault()); 
            Js._true);
      let copyClipboardBtn = button (Lang.i18nRDParserCopyButtonDefault()) "copyClipboardBtn" 
        (fun _ -> 
          let button = Dom_html.getElementById("copyClipboardBtn") in
          button##.innerHTML := Js.string (Lang.i18nRDParserCopyButtonClick());
          let text : Js.js_string Js.t = (Js.Unsafe.coerce (Dom_html.getElementById("rdParserCode")))##.innerText in
          let clipboard = Js.Unsafe.global##.navigator##.clipboard in
          clipboard##writeText text)
      in
        Dom.appendChild div select;
        Dom.appendChild div copyClipboardBtn;
        addBr "rdParserBr" textBox;
      let rdParserPre = pre "rdParserPre" "" in
      let rdParserCode = code "rdParserCode" "" in
        Dom.appendChild textBox rdParserPre;
        Dom.appendChild rdParserPre rdParserCode;
        (printCFG2RecursiveDescedentParser (changeFun (List.hd optsList)))


  let getIsLL1 ll1 infoBox =
    let text = span "isLL1" 
      (if ll1 
        then (Lang.i18nIsLL1 ()) 
        else (Lang.i18nIsNotLL1 ())
      )
    in
      Dom.appendChild infoBox text
      
  let getIsLeftRecursive lr infoBox =
    let text = span "isLeftRecursive" 
      (if lr
        then (Lang.i18nIsLeftRecursive ())
        else (Lang.i18nIsNotLeftRecursive ())
      )
    in
      Dom.appendChild infoBox text
  
  let getIsLeftFactoring lf infoBox =
    let text = span "isLeftFactoring"
      (if lf
        then (Lang.i18nIsLeftFactoring ())
        else (Lang.i18nIsNotLeftFactoring ())
      )
    in
      Dom.appendChild infoBox text

  let getHasParsingTableConflict pConf infoBox =
    let text = span "hasParsingTableConflict"
      (if pConf
        then (Lang.i18nHasParsingTableConflict())
        else (Lang.i18nHasNotParsingTableConflict())
      )
    in
      Dom.appendChild infoBox text

  let getIsCFGClean c prod access infoBox =
    let text = span "isCFGClean"
    (if c
        then (Lang.i18nIsCFGClean ())
        else Lang.i18nIsNotCFGClean()
            ^ (if not prod then Lang.i18nNotProd () else "")
            ^ (if not access then Lang.i18nNotAccess () else "")
    )
    in
      Dom.appendChild infoBox text

  let putTreeResult text =
    let textBox = Dom_html.getElementById "textBox" in
      let en = div1 "treeResult" text in 
        Dom.appendChild textBox en

  let getDeterminim deter infoBox = 
    if deter then 
      let deterministic = span "isdeterministic" (Lang.i18nIsDeterministic ()) in 
        Dom.appendChild infoBox deterministic
    else 
      let deterministic = span "isdeterministic" (Lang.i18nNotDeterministic ()) in 
        Dom.appendChild infoBox deterministic
    
  let getMinimism min infoBox = 
    if min then 
      let minimal = span "isminimal" (Lang.i18nIsMinimal ()) in 
        Dom.appendChild infoBox minimal 
    else 
      let minimal = span "isminimal" (Lang.i18nNotMinimal ()) in 
        Dom.appendChild infoBox minimal
    
  let getHasUselessStates use uStates infoBox = 
    if use then 
      (let useful = span "areuseful" (Lang.i18nNotUseless ()) in
        Dom.appendChild infoBox useful)
    else 
      (let useless = Set.toList uStates in
        let number = List.length useless in 
          let useful = (Lang.i18nHas ()) ^ (string_of_int number) ^ (Lang.i18nUselessStates ()) in 
            let use = span "areuseful" useful in
              Dom.appendChild infoBox use)

  let getNumberStates nStates infoBox = 
    let number = string_of_int nStates in 
      let sentence = (Lang.i18nNumberStates ()) ^ number ^ ". " in 
        let sentence1 = span "numberstates" sentence in
          Dom.appendChild infoBox sentence1
    
  let getNumberTransitions nTransitions infoBox = 
    let number = string_of_int nTransitions in 
        let sentence = (Lang.i18nNumberTransitions ()) ^ number ^ ". " in 
          let sentence1 = span "numbertransitions" sentence in
            Dom.appendChild infoBox sentence1
  
  (*let getIsLinearBounded is infoBox =
    let lb = string_of_bool is in 
        let sentence = (Lang.i18nisLinearBounded ()) ^ lb ^ ". " in 
          let sentence1 = span "islinearbounded" sentence in
            Dom.appendChild infoBox sentence1*)

  let getIsLinearBounded is infoBox = 
    if is then 
      let lb = span "islinearbounded" (Lang.i18nIsLinearBounded ()) in 
        Dom.appendChild infoBox lb
    else 
      let lb = span "islinearbounded" (Lang.i18nIsNotLinearBounded ()) in 
        Dom.appendChild infoBox lb
    
  let defineInformationBox s =
    let elementStr = if s then "infoBox2" else "infoBox" in
    let element = Dom_html.getElementById elementStr in
    element##.innerHTML := Js.string "";
    element

  let createServerExampleButton name =
      button name "exampleButton" (fun () -> !Listeners.openEntityListener (Examples.example name));;

  let putButton name = 
    let examples = Dom_html.getElementById "examplesServer" in
      let title = name in 
        let example = createServerExampleButton title in 
          Dom.appendChild examples example

  let defineRE def s =
    let elem = if s then Dom_html.getElementById "textBox" else Dom_html.getElementById "regExp" in
      let expr = div1 "reg" def in
        Dom.appendChild elem expr;
      putInnerHtml (if s then "infoBox2" else "infoBox") ""

  let putWords listWords =
    putInnerHtml "textBox" "";
    let textBox = Dom_html.getElementById "textBox" in
    let title = div1 "generateWords" (Lang.i18nGenerateWords ()) in 
      Dom.appendChild textBox title;
    let test = Set.toList listWords in 
      let string_of_word w = "' " ^ String.concat "" (List.map symb2str w) ^ " '" in
        let string_of_words l = String.concat ", " (List.map string_of_word l) in 
          let res = string_of_words test in
            let zz = textarea "textarea" 2 20 (Js.string res) in 
              Dom.appendChild textBox zz

  let putEnumButton () = 
    let buttonBox = Dom_html.getElementById "buttonBox1" in
      let clearButton = button "X" "clearEnum" !ListenersEXER.clearExerciseListener in      
        Dom.appendChild buttonBox clearButton

  let addEnumTitle () =
    let textBox = Dom_html.getElementById "textBox" in
      let en = h2 "enum" (Lang.i18nEnumTitle ()) in 
        Dom.appendChild textBox en

  let defineEnumProblem prob =
    let textBox = Dom_html.getElementById "textBox" in
      let test = (Lang.i18nProblem ()) ^ prob in
        let en = div1 "prob" test in 
          Dom.appendChild textBox en;
    let resultBox = div "resultBox" in 
      Dom.appendChild textBox resultBox

  let addAcceptedTitle () =
    let textBox = Dom_html.getElementById "textBox" in
      let tac = div1 "accept" (Lang.i18nAcceptedWords ()) in 
        Dom.appendChild textBox tac;
      let ac = div "inside" in 
        Dom.appendChild textBox ac    

  let addNonAcceptTitle () =
    let textBox = Dom_html.getElementById "textBox" in
      let twr = div1 "notAccept" (Lang.i18nNonAccepted ()) in 
        Dom.appendChild textBox twr;
      let wr = div "outside" in 
        Dom.appendChild textBox wr

  let addPropertiesBox () =
    let textBox = Dom_html.getElementById "textBox" in
    let twr = div1 "prop" (Lang.i18nProperties ()) in 
        Dom.appendChild textBox twr;
      let ac = div "properties" in 
        Dom.appendChild textBox ac    
    
  let addEnumCheckButton () =
    let textBox = Dom_html.getElementById "textBox" in
      let checkButton = button (Lang.i18nVerify ()) "enumVerify" !ListenersEXER.checkExerciseListener in
        Dom.appendChild textBox checkButton
  
  let defineResult result =
    JS.log ("Entrou?");
    let resultBox = Dom_html.getElementById "resultBox" in
      resultBox##.innerHTML := Js.string "";
    if result then 
      let res = div1 "correct" (Lang.i18nRight ()) in
        Dom.appendChild resultBox res
    else
      (let res = div1 "wrong" (Lang.i18nWrong ()) in 
        Dom.appendChild resultBox res);
      putInnerHtml "properties" "";
      putInnerHtml "inside" "";
      putInnerHtml "outside" ""

  let defineTreeButtons () =
    let textBox = Dom_html.getElementById "textBox" in
      let en = div "treeButtons" in 
        Dom.appendChild textBox en;
      let buttonBox = Dom_html.getElementById "treeButtons" in 
        let previous = button (Lang.i18nPrevious ()) "previousTree" !ListenersRE.previousTreeListener in
          Dom.appendChild buttonBox previous;
        let next = button (Lang.i18nNext ()) "nextTree" !ListenersRE.nextTreeListener in
          Dom.appendChild buttonBox next

  let putTreeGoodDerivations text =
    let textBox = Dom_html.getElementById "textBox" in
      let en = div1 "treeGoodDerivations" text in 
        Dom.appendChild textBox en

  let putTreeBadDerivations text =
    let textBox = Dom_html.getElementById "textBox" in
      let en1 = div1 "treeBadDerivations" text in 
        Dom.appendChild textBox en1

  let putTreeNumbers text =
    let textBox = Dom_html.getElementById "textBox" in
    let treeNumbers = Dom_html.getElementById_opt "treeNumbers" in
    match treeNumbers with
    | None -> let en = div1 "treeNumbers" text in 
                Dom.appendChild textBox en
    | Some e -> e##.innerHTML := Js.string text

  let about () =
    putInnerHtml "regExp" "";
    putInnerHtml "infoBox" "";
    let cy = Dom_html.getElementById "buttonBox" in
      cy##.innerHTML := Js.string "";
        Dom.appendChild cy (closeButton());
    putInnerHtml "mainTitle" (Lang.i18nAboutTitle ());
    let info = div "aboutBox" in 
      Dom.appendChild cy info;
    let aboutBox = Dom_html.getElementById "aboutBox" in
      let subtitle = h2 "aboutSubtitle" (Lang.i18nAboutSubtitle ()) in
        Dom.appendChild aboutBox subtitle;
      let aboutTex = p "aboutText" "" in 
        Dom.appendChild aboutBox aboutTex;
      let text = span "aboutText1" (Lang.i18nAboutText1 ()) in 
        Dom.appendChild aboutTex text;
      let text1 = a "http://nova-lincs.di.fct.unl.pt/" (Lang.i18nNovaLincs ()) in
        Dom.appendChild aboutTex text1;
      let text2 = span "aboutText2" (Lang.i18nAboutText2 ()) in 
        Dom.appendChild aboutTex text2;
      let text3 = a "https://release.di.ubi.pt/factor/index.html" (Lang.i18nFactor ()) in
        Dom.appendChild aboutTex text3;
      let text = span "aaa" (Lang.i18nAboutText16 ()) in
        Dom.appendChild aboutTex text;
      let text = a "https://release.di.ubi.pt/leafs/index.html" (Lang.i18nAboutText17 ()) in
        Dom.appendChild aboutTex text;
      let text = span "bbb" (Lang.i18nAboutText3 ()) in
        Dom.appendChild aboutTex text;
      let text5 = a1 "tezos" "https://tezos.com/" (Lang.i18nFooter ()) in 
        Dom.appendChild aboutTex text5;
      let text6 = span "aboutText16" (Lang.i18nAboutText16 ()) in
        Dom.appendChild aboutTex text6;
      let text7 = a1 "inria" "https://www.inria.fr/" (Lang.i18nFooter1  ()) in
        Dom.appendChild aboutTex text7;
      let text8 = span "aboutText20" ". " in
        Dom.appendChild aboutTex text8;
      let aboutTex1 = p "aboutText17" "" in 
        Dom.appendChild aboutBox aboutTex1;
      let text9 = span "aboutText4" (Lang.i18nAboutText4 ()) in
        Dom.appendChild aboutTex text9;
      let text10 = a "https://gitlab.com/releaselab/leaf/OFLAT" "GitLab" in
        Dom.appendChild aboutTex text10;
      let text11 = span "aboutText18" "." in
        Dom.appendChild aboutTex text11;
      let sub = h2 "aboutSubtitle2" (Lang.i18nAboutSubtitle2 ()) in
        Dom.appendChild aboutBox sub;
      let text12 = p "aboutText5" (Lang.i18nAboutText5 ()) in 
        Dom.appendChild aboutBox text12;
      let text13 = p "aboutText6" (Lang.i18nAboutText6 ()) in 
        Dom.appendChild aboutBox text13;
      let text14 = pre "aboutText7" (Lang.i18nAboutText7 ()) in 
        Dom.appendChild aboutBox text14;
      let text15 = p "aboutText8" (Lang.i18nAboutText8 ()) in 
        Dom.appendChild aboutBox text15;
      let text16 = pre "aboutText9" (Lang.i18nAboutText9 ()) in 
        Dom.appendChild aboutBox text16;
      let text17 = p "aboutText10" (Lang.i18nAboutText10 ()) in 
        Dom.appendChild aboutBox text17;
      let text18 = pre "aboutText11" (Lang.i18nAboutText11 ()) in 
        Dom.appendChild aboutBox text18;
      let text19 = p "aboutText12" (Lang.i18nAboutText12 ()) in 
        Dom.appendChild aboutBox text19;
      let text20 = p "aboutText13" (Lang.i18nAboutText13 ()) in 
        Dom.appendChild aboutBox text20;
      let text21 = p "aboutText14" (Lang.i18nAboutText14 ()) in 
        Dom.appendChild aboutBox text21;
      let text22 = p "aboutText15" (Lang.i18nAboutText15 ()) in 
        Dom.appendChild aboutBox text22

    let feedback () = 
      JS.log ("Start Feedback");
      let cy = Dom_html.getElementById "regExp" in
        cy##.innerHTML := Js.string "";
      let cy = Dom_html.getElementById "infoBox" in
        cy##.innerHTML := Js.string "";
      let cy = Dom_html.getElementById "buttonBox" in
        cy##.innerHTML := Js.string "";
          Dom.appendChild cy (closeButton());
      putInnerHtml "mainTitle" (Lang.i18nFeedback ());
      let test1 = div ("FeedbackDiv") in
        Dom.appendChild cy test1;
      let text = Dom_html.getElementById "FeedbackDiv" in
        let text1 = p "feedbackText" (Lang.i18nFeedbackText ()) in
          Dom.appendChild text text1;
        let spanBox = p "feedbackText1" "" in
          Dom.appendChild text spanBox;
       let spanBox1 = Dom_html.getElementById "feedbackText1" in
        let span1 = span "feedbackText2" (Lang.i18nFeedbackText2 ()) in
          Dom.appendChild spanBox1 span1;
        let link = a "mailto:amd@fct.unl.pt" "Artur Miguel Dias" in
          Dom.appendChild spanBox1 link;
        let span2 = span "feedbackText3" "." in
          Dom.appendChild spanBox1 span2;
        let text3 = p "feedbackThankYou" (Lang.i18nFeedbackThankYou ()) in
          Dom.appendChild text text3
      
    
  let changeLang () =
    Graphics.changeLang !Lang.lang;
    putInnerHtml "title" (Lang.i18nTitle ());
    putInnerHtml "version" (Lang.i18nVersion ());
    
    putInnerHtml "optionNewDefault" (Lang.i18nNewModel ());
    putInnerHtml "optionNewAutomaton" (Lang.i18nMainTitle1());
    putInnerHtml "optionNewRegularExpression" (Lang.i18nMainTitle2());
    putInnerHtml "optionNewContextFreeGrammar" (Lang.i18nMainTitle4());
    putInnerHtml "optionNewTuringMachine" (Lang.i18nMainTitleTM());

    putInnerHtml "editModel" (Lang.i18nEditModel ());
    putInnerHtml "fitGraph" (Lang.i18nFitGraph ());
    putInnerHtml "generate" (Lang.i18nGenerate ());
    putInnerHtml "testing" (Lang.i18nTesting ());
    putInnerHtml "step" (Lang.i18nStep ());
    putInnerHtml "start" (Lang.i18nStart ());
    
     
    putInnerHtml "selectTuringMachine" (Lang.i18nSelectTM ());
    putInnerHtml "selectRegex" (Lang.i18nSelectRegex ());
    putInnerHtml "selectAutomaton" (Lang.i18nSelectAutomaton ());
    putInnerHtml "selectConv" (Lang.i18nSelectConv ());

    putInnerHtml "importModel" (Lang.i18nImportModel ());
    putInnerHtml "exportModel" (Lang.i18nExportModel ());
    putInnerHtml "server" (Lang.i18nServer ());     

    putInnerHtml "selectedL" (Lang.i18nSelectedL ());   
    putInnerHtml "selectPT" (Lang.i18nSelectPT ());
    putInnerHtml "selectEN" (Lang.i18nSelectEN ());
    putInnerHtml "selectFR" (Lang.i18nSelectFR ());

    putInnerHtml "about" (Lang.i18nAbout ());
    putInnerHtml "feedback" (Lang.i18nFeedback ());
        
    putInnerHtml "developed" (Lang.i18nDeveloped ());
    putInnerHtml "footerButton0" (Lang.i18nNovaLincs () );
    putInnerHtml "project" (Lang.i18nProject ());
    putInnerHtml "footerButton3" (Lang.i18nFactor ());
    putInnerHtml "and" (Lang.i18nAnd ());
    (* putInnerHtml "leaf" (Lang.i18nLeafs ()); *)
    putInnerHtml "financing" (Lang.i18nFinancing ());
    putInnerHtml "footerButton1" (Lang.i18nFooter ());
    putInnerHtml "and1" (Lang.i18nAnd ());
    putInnerHtml "footerButton2" (Lang.i18nFooter1 ());

    if (StateVariables.getCy1Type() = StateVariables.getAutomatonType()) then
      (putInnerHtml "tooltipCloseLeft" (Lang.i18nTooltipCloseLeft ());
      putInnerHtmlButtons "formatting" (Lang.i18nFormatting ()) "tooltipSpecification" "tooltiptext2" (Lang.i18nTooltipSpecification ());
      putInnerHtmlButtons "clean" (Lang.i18nClean ()) "tooltipClean" "tooltiptext3" (Lang.i18nTooltipClean ());
      putInnerHtmlButtons "deterministic" (Lang.i18nDeterministic ()) "tooltipDeterministic" "tooltiptext3" (Lang.i18nTooltipDeterministic ());
      putInnerHtmlButtons "minimize" (Lang.i18nMinimize ()) "tooltipMinimize" "tooltiptext3" (Lang.i18nTooltipMinimize ());
      putInnerHtmlButtons "productive" (Lang.i18nProductive ()) "tooltipProductive" "tooltiptext3" (Lang.i18nTooltipProductive ());
      putInnerHtmlButtons "accessible" (Lang.i18nAccessible ()) "tooltipAccessible" "tooltiptext3" (Lang.i18nTooltipAccessible ());
      putInnerHtmlButtons "useful" (Lang.i18nUseful ()) "tooltipUseful" "tooltiptext3" (Lang.i18nTooltipUseful ());
      putInnerHtml "infoBox" "";
      putInnerHtml "mainTitle" (Lang.i18nMainTitle1 ());
      !Listeners.defineInformationBoxListener());

    if (StateVariables.getCy1Type() = StateVariables.getRegexType()) then
      (putInnerHtml "tooltipCloseLeft" (Lang.i18nTooltipCloseLeft ());
      putInnerHtmlButtons "changeDirection" (Lang.i18nDirection ()) "tooltipDirection" "tooltiptext2" (Lang.i18nTooltipDirection ());
      putInnerHtml "mainTitle" (Lang.i18nMainTitle2 ()));

    if (StateVariables.getCy2Type() = StateVariables.getEnumerationType()) then
      (putInnerHtml "enumVerify" (Lang.i18nVerify ());

      let prob = (StateVariables.returnEnum())#representation.problem in
        let prob1 = (Lang.i18nProblem ()) ^ prob in
        putInnerHtml "prob" prob1;
      putInnerHtml "enum" (Lang.i18nEnumTitle ());
      putInnerHtml "accept" (Lang.i18nAcceptedWords ());
      putInnerHtml "notAccept" (Lang.i18nNonAccepted ());
      if Dom_html.getElementById_opt "correct" <> None then
        putInnerHtml "correct" (Lang.i18nRight ());
      if Dom_html.getElementById_opt "wrong" <> None then
        putInnerHtml "wrong" (Lang.i18nWrong ());
      putInnerHtml "mainTitle" (Lang.i18nMainTitle3 ());
      );

    if (StateVariables.getCy2Type() = StateVariables.getInfoType()) then
      (putInnerHtml "generateWords" (Lang.i18nGenerateWords ());
      putInnerHtml "tooltipCloseRight" (Lang.i18nTooltipCloseRight ());
      );

    if (StateVariables.getCy2Type() = StateVariables.getVerifyType()) then
      (putInnerHtml "textBox" "";
      !ListenersRE.resultCountListener ();
      !ListenersRE.defineNumberTreesListener ();
      defineTreeButtons ();
      putInnerHtml "tooltipCloseRight" (Lang.i18nTooltipCloseRight ());
      );

      if (StateVariables.getCy1Type() = StateVariables.getFeedbackType()) then
      (putInnerHtml "mainTitle" "------------";
       putInnerHtml "feedbackText" (Lang.i18nFeedbackText ());
       putInnerHtml "feedbackText2" (Lang.i18nFeedbackText2 ());
       putInnerHtml "feedbackThankYou" (Lang.i18nFeedbackThankYou ());
      );

   if (StateVariables.getCy1Type() = StateVariables.getInfoType ()) then
       (putInnerHtml "mainTitle" (Lang.i18nAboutTitle ());
        putInnerHtml "aboutSubtitle" (Lang.i18nAboutSubtitle ());
        putInnerHtml "aboutSubtitle2" (Lang.i18nAboutSubtitle2 ());
        putInnerHtml "aboutText1" (Lang.i18nAboutText1 ());
        putInnerHtml "aboutText2" (Lang.i18nAboutText2 ());
        putInnerHtml "aaa" (Lang.i18nAboutText16 ());
        putInnerHtml "bbb" (Lang.i18nAboutText3 ());
        putInnerHtml "aboutText4" (Lang.i18nAboutText4 ());
        putInnerHtml "aboutText5" (Lang.i18nAboutText5 ());
        putInnerHtml "aboutText6" (Lang.i18nAboutText6 ());
        putInnerHtml "aboutText7" (Lang.i18nAboutText7 ());
        putInnerHtml "aboutText8" (Lang.i18nAboutText8 ());
        putInnerHtml "aboutText9" (Lang.i18nAboutText9 ());
        putInnerHtml "aboutText10" (Lang.i18nAboutText10 ());
        putInnerHtml "aboutText11" (Lang.i18nAboutText11 ());
        putInnerHtml "aboutText12" (Lang.i18nAboutText12 ());
        putInnerHtml "aboutText13" (Lang.i18nAboutText13 ());
        putInnerHtml "aboutText14" (Lang.i18nAboutText14 ());
        putInnerHtml "aboutText15" (Lang.i18nAboutText15 ());
        putInnerHtml "aboutText16" (Lang.i18nAboutText16 ());
        putInnerHtml "tezos" (Lang.i18nFooter ());
		putInnerHtml "inria" (Lang.i18nFooter1 ());
        
       )

end 
