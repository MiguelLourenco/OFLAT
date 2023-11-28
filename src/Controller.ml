(*
 * Controller.ml
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
 * Description: Controller component of the application.
 *)


open OCamlFlat
open BasicTypes
open Js_of_ocaml
open JS
open Graphics
open FiniteAutomatonGraphics
open RegularExpressionGraphics
open ContextFreeGrammarGraphics
open TuringMachineGraphics
open Lang
open Listeners
open HtmlPageClient
open StateVariables
open String
open Random
open Util

    let changeToTextCtrlRight = ref (fun () -> ())
    let changeToControllerCtrlRight = ref (fun () -> ())
    let changeToControllerCtrlLeft = ref (fun () -> ())

    let oneBox cy = 
      !changeToControllerCtrlRight();
      HtmlPageClient.oneBox();
      Cytoscape.fit cy
      
    let twoBoxes cy =
      HtmlPageClient.twoBoxes();
      Cytoscape.fit cy

    class virtual controller =
      object(self)

      val mutable layoutDir = None
      val mutable updateType = None
      val virtual cy : Cytoscape.cytoscape Js_of_ocaml.Js.t option

      val listOnlyAutomataButtons = ["backwards"; "start"; "forward"; "selectRegex"; "selectTuringMachine"]
      val listOnlyTMButtons = ["backwards"; "start"; "forward"]
      val listOnlyExpressionButtons = ["selectAutomaton"; "selectTuringMachine"]
      val listOnlyCFGButtons = [(*"testing"; "generate";*) "backwards"; "start"; "forward"]
      val listOtherButtons = ["testing"; "generate"; "fitGraph"; "editModel"; "exportModel"]

      method locked : bool = false
	    method addNode (x : int) (y : int) (st: state) : unit = failwith "addNode"
      method eliminateNode (st: state) : unit = failwith "eliminateNode"
      method startGraph : unit = failwith "startGraph"
      method defineExample : unit = failwith "defineExample"
      method defineExample2 : unit = failwith "define Example cy 2"
      method defineInformationBox : unit = failwith "defineInformationBox"
      method automatonToRegExp : RegularExpressionGraphics.model = failwith "automaton to regex"
      method automatonToTM: TuringMachineGraphics.model = failwith "automaton to turing machine"
      method fromExpressionToAutomaton : FiniteAutomatonGraphics.model = failwith "Regex to automaton"
      method fromExpressionToTM : TuringMachineGraphics.model = failwith "Regex to turing machine"
      method createTransition (s1 : state) (s2 : state) : unit = failwith "createTransition"
      method eliminateTransition ((c1: state), (c2: string), (c3: state)): unit = failwith "eliminateTransition"
      method defineMinimize (listColors: string array) (number : int) : unit = failwith "minimize"
      method addFinalNode (x : int) (y : int) (st: state): unit = failwith "addFinalNode"
      method turnFinalNode (st: state): unit = failwith "turnFinalNode"
      method removeFinalNode (st: state): unit = failwith "removeFinalNode"
      method addInitialNode (st: state) : unit = failwith "addInitialNode"
      method accept (st:string) : bool Lwt.t = failwith "accept"
      method getModel : string = ""
      method getWords (number: int): unit = failwith "getWords"
      method getNewSentence = Js.string ("")
      method startStep (word: string): unit = failwith "startStep"
      method nextStep: unit  = failwith "nextStep"  
      method backStep: unit = failwith "backStep"
      method model: Model.model = failwith "model"
      method replicateOnLeft: unit = failwith "replicateOnLeft"
      method setUpdateType (s : string) : unit = failwith "setUpdateType"
      method getUpdateType : string option = updateType
      method updateRight: unit = ()
      method updateScreenSentence: unit = failwith "updateScreenSentence"
      method changeToEditMode: unit = failwith "changeToEditMode"
      
      method getCy: Cytoscape.cytoscape Js_of_ocaml.Js.t = 
        match cy with
          | None -> failwith "getCy"
          | Some cy -> cy
      method getCy_opt = cy

      method feedback = ()
      method about = ()

      method printErrors =
        let errors = [] in
          if errors = [] then 
            ()
          else 
            JS.alertStr (String.concat "\n" errors)

      method clearExerciseAction = 
        HtmlPageClient.oneBox ();
        HtmlPageClient.clearBox2 ();
        let element = Dom_html.getElementById "cy2" in
          element##.innerHTML := Js.string ""

	    method updateButtons = 
        List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
        List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
        List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTMButtons;
        List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;
        List.iter (fun el -> HtmlPageClient.disableButton el) listOtherButtons

      method setTitle: unit = HtmlPageClient.defineMainTitle ("")

      method checkHelper (result: bool) ((insideErrors: word set), (outsideErrors: word set), (properties: property set)) : unit = failwith "checkHelper"

      method changeLayoutDir newLayout =
        layoutDir <- Some (newLayout)

      method getLayoutDir : string = 
        match layoutDir with
          | None -> ""
          | Some layoutDir -> layoutDir
        

      method closeRightAction = 
        HtmlPageClient.oneBox ();
        HtmlPageClient.clearBox2 ();
        !changeToControllerCtrlRight ()

      method resetStyle = ()

      method returnType = ""

      method operation opName modelKind: unit =
        Js.Unsafe.global##logEntry (Js.string opName) (Js.string modelKind)
      

      method getFA : FiniteAutomatonGraphics.model = failwith "get automata"
      method getRE : RegularExpressionGraphics.model = failwith "get RE"
      method getCFG : ContextFreeGrammarGraphics.model = failwith "get CFG"
      method getTM : TuringMachineGraphics.model = failwith "get TM"
      method getExercise : Exercise.exercise = failwith "get RE"
      method getResultTree : bool = failwith "get Result Tree"
      method getWordAsList () : word = failwith "get word as list"

      method editModel : unit = failwith "edit model"
      
      method box2CFGShow (f : ContextFreeGrammarGraphics.transformation) : unit = failwith "show cfg transformation box 2"
      
      method renameState (s : state) : unit = failwith "rename state"
    end

  class textController (s : bool) = 
    object(self) inherit controller as super
    
    val side = s
    
    val cy = 
      let cyString = if s then "cy2" else "cy" in
      let cyElement = Dom_html.getElementById_opt cyString in
      match cyElement with
      | None -> None
      | Some a -> Some (Cytoscape.initCy cyString)

    method returnType = ""
    
    method defineExample = ()
    
    method replicateOnLeft =
      !changeToControllerCtrlLeft ()

    method setUpdateType s =
      updateType <- Some s

    method feedback =
      super#operation "Feedback" "Feedback";
      HtmlPageClient.oneBox ();
      HtmlPageClient.disableButtons (self#returnType);
      HtmlPageClient.feedback()
  
    method about =
    super#operation "About" "About";
      HtmlPageClient.oneBox ();
      StateVariables.changeCy1ToText();
      HtmlPageClient.disableButtons (self#returnType);
      HtmlPageClient.about()

  end

let textCtrl s = new textController s

let ctrlL = ref (textCtrl false)
let ctrlR = ref (textCtrl true)

let changeCtrlL (nc: controller) =
	ctrlL := nc;;

let changeCtrlR (nc: controller) =
  ctrlR := nc;;

let _ = changeCtrlL (textCtrl false);
        changeCtrlR (textCtrl true);;

changeToTextCtrlRight := fun () -> (changeCtrlR (textCtrl true :> controller));;
changeToControllerCtrlRight := fun () -> (changeCtrlR (textCtrl true));;
changeToControllerCtrlLeft := fun () -> (changeCtrlL (textCtrl false));;

class faController (fa: FiniteAutomatonGraphics.model) (s: bool)=
  object(self) inherit controller as super

    val mutable myFA = fa

    val side = s
    val cy = Some (if s then Cytoscape.initFaCy "cy2" else Cytoscape.initFaCy "cy")

    method operationFA opName : unit =
        super#operation opName "FA"

    method model: Model.model = 
      (myFA :> Model.model) 

    method resetStyle = 
      Cytoscape.resetStyle self#getCy Cytoscape.faStyle

    method getFA =
      myFA

    method changeAutomata res =
      myFA <- res

    method getModel = 
      myFA#toDisplayString "solution"

	  method addNode x y st : unit = 
      self#operationFA "add Node";
      JS.log myFA#representation.states;
      if (Set.belongs st myFA#representation.states) then 
        (JS.alertStr (Lang.i18nAlertExists ()))
      else 
        (myFA <- myFA#addNode st false;
        Cytoscape.addNode self#getCy st ~x:x ~y:y false false;
        self#defineInformationBox;)

    method setTitle = 
      oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (FiniteAutomaton.modelDesignation)

    method returnType = FiniteAutomaton.modelDesignation

    method startGraph = self#defineExample (*TODO Remove this call, call defineExample directly*)

    method defineExample =
      self#operationFA "create example";
      self#updateButtons;
      HtmlPageClient.putCyAutomataButtons ();
      HtmlPageClient.closeBoxRegex ();
      myFA#drawExample self#getCy;
      self#defineInformationBox;
      Cytoscape.fit self#getCy_opt;

    method defineExample2 = 
      myFA#drawExample self#getCy;
      self#defineInformationBox;

    method defineInformationBox =
      let infoBox = HtmlPageClient.defineInformationBox side in
      let deter = myFA#isDeterministic in 
        HtmlPageClient.getDeterminim deter infoBox;
      let min = myFA#isMinimized in 
        HtmlPageClient.getMinimism min infoBox;
      let useful = myFA#areAllStatesUseful in
      let uStates = myFA#getUselessStates in 
        HtmlPageClient.getHasUselessStates useful uStates infoBox;
      let nStates = myFA#numberStates in 
        HtmlPageClient.getNumberStates nStates infoBox;
      let nTransitions = myFA#numberTransitions in
        HtmlPageClient.getNumberTransitions nTransitions infoBox
    
    method createTransition source target =
      self#operationFA "add transition";
      let promptResult = (JS.prompt (Lang.i18nTextEnterTransition ()) "c") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v ->
        let v = symb (Js.to_string v) in
        (if v = epsilon
        then myFA <- myFA#newEpsylonTransition (source, v, target)
        else myFA <- myFA#newTransition (source, v, target));
        Cytoscape.addEdge self#getCy (source, v, target);
        self#defineInformationBox;
      
    method addFinalNode x y node =
      self#operationFA "add final node";
      if (Set.belongs node myFA#representation.states) then
        (JS.alertStr (Lang.i18nAlertExists ()))
      else (
        myFA <- myFA#addFinalNode node false false;
        Cytoscape.addNode self#getCy ~x:x ~y:y node false true;
        self#defineInformationBox;
      )

    method addInitialNode node =
      self#operationFA "make node initial";
      let stateExists = Set.belongs node myFA#representation.states in 
          myFA <- (myFA#addInitialNode node false stateExists);
          let cy = self#getCy in
          Cytoscape.resetFaElems cy;
          myFA#drawExample cy;
          self#defineInformationBox;

    method eliminateTransition (v1, v3, v2) =
      self#operationFA "erase transition";
      let c3 = symb v3 in
        if (Set.belongs (v1, c3, v2) myFA#representation.transitions) then
          (
            myFA <- (myFA#eliminateTransition(v1, c3, v2));
            Cytoscape.removeEdge self#getCy v1 (symb2str c3) v2;
            self#defineInformationBox;
          )
        else 
          JS.alertStr ((Lang.i18nAlertTheTransition ()) ^ "(" ^ v1 ^ ", " ^ symb2str c3 ^ ", " ^ v2 ^ ")" ^ (Lang.i18nAlertDoNotExists ()))
    
     method turnFinalNode node =
      self#operationFA "make node final";
      if (Set.belongs node myFA#representation.acceptStates) then
          (JS.alertStr (Lang.i18nAlertAlreadyFinal ()))
      else
        (myFA <- (myFA#changeToFinal node);
        Cytoscape.turnFinal self#getCy node);
      self#defineInformationBox;
    
    method removeFinalNode node =
      self#operationFA "make node not final";
      if (Set.belongs node myFA#representation.acceptStates) then
        (myFA <- (myFA#removeFinal node);
        Cytoscape.removeFinal self#getCy node)
      else
        (JS.alertStr (Lang.i18nAlertNonFinal ())); 
      self#defineInformationBox;
      
    method eliminateNode node =
      self#operationFA "eliminate node";
      let eliminateNodeTransitions (a, b, c) node = 
        if (a = node || c = node) then
          (myFA <- (myFA#eliminateTransition (a, b, c));
      self#defineInformationBox;) in 
        if (node = myFA#representation.initialState )then 
          JS.alertStr (Lang.i18nAlertDelete ()) 
        else 
          if (Set.belongs node myFA#representation.states) then 
            (let isFinal = Set.belongs node myFA#representation.acceptStates in 
            myFA <- myFA#eliminateNode node false isFinal;
            Set.iter (fun el -> (eliminateNodeTransitions el node)) myFA#representation.transitions;
            Cytoscape.removeNode self#getCy node;
            self#defineInformationBox;)
          else 
            JS.alertStr (Lang.i18nAlertUnexistentState ())

    method renameState state =
      self#operationFA "rename node";
      let newName = JS.prompt (Lang.i18nRenameStateQuestion()) state in
      match Js.Opt.to_option newName with
      | None -> ()
      | Some n -> myFA <- myFA#renameState state (Js.to_string n);
                  Cytoscape.resetFaElems self#getCy;
                  self#defineExample
        
    method accept word = 
      self#operationFA "accept";
      self#startStep word;
      let rec tic n =
        match n with
        | true -> Lwt.return()
        | false -> Lwt.bind 
                  (Js_of_ocaml_lwt.Lwt_js.sleep 1.0)
                  (fun () -> self#nextStep; tic myFA#isOver)
      in
      ignore(tic false);
      Lwt.return_true

	  method updateButtons =
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTMButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons
    
    method getWords v = 
      self#operationFA "accepted words";
      let var = myFA#generateUntil v in 
        HtmlPageClient.putWords var

    method getNewSentence = 
      Js.string myFA#newSentence1

    method updateScreenSentence =
      (Dom_html.getElementById "regExp")##.innerHTML := self#getNewSentence

    method startStep word =
      self#operationFA "accept start";
      HtmlPageClient.fitBoxRegex ();
      myFA#changeTheTestingSentence word;
      myFA#startAccept self#getCy;
      self#updateScreenSentence
    
    method nextStep =
      self#operationFA "accept next";
      myFA#next self#getCy;
      self#updateScreenSentence
          
    method backStep = 
      self#operationFA "accept back";
      myFA#back self#getCy;
      self#updateScreenSentence

    method automatonToRegExp =
      self#operationFA "conversion";
      let repFA = myFA#representation in 
      let auto = new FiniteAutomaton.model (Representation (repFA)) in 
      let reg = PolyModel.fa2re (auto) in
        let r = reg#simplify in 
        let repRE = r#representation in 
        new RegularExpressionGraphics.model (Representation (repRE))

    method automatonToTM =
      self#operationFA "conversion";
      let repFA = myFA#representation in 
      let auto = new FiniteAutomaton.model (Representation (repFA)) in 
      let tm = PolyModel.fa2tm auto in
        let repTM = tm#representation in 
        new TuringMachineGraphics.model (Representation (repTM))

    method defineMinimize listColors number =
      myFA#paintMinimization self#getCy listColors;
      myFA#drawMinimize self#getCy listColors number;
      Cytoscape.fit self#getCy_opt
    
    method editModel = 
      !ListenersFA.editModelListener();
    
    method replicateOnLeft =
      let c = new faController self#getFA false in
        ctrlL := (c :> controller);

    method updateRight =
      if !ctrlR#getUpdateType = Some "specification"
      then !Listeners.showModelListener ()
    
    method printErrors =
          let errors = myFA#errors in
            if errors = [] then 
              ()
            else 
              JS.alertStr (String.concat "\n" errors)
  end
      
  class reController (re: RegularExpressionGraphics.model) (s: bool) =
    object(self) inherit controller as super

    val re1 = re

    val side = s
    val cy = Some (if s then Cytoscape.startTree "cy2" else Cytoscape.startTree "cy")

    val mutable step = 0
  
    val mutable re = new RegularExpressionGraphics.model (Representation Empty)
    val mutable resultTree = false
    val mutable wordAsList : word = []

    method model: Model.model = 
      (re1 :> Model.model) 

    method getWordAsList () =
      wordAsList

    method getRE =
      re1

    method getResultTree =
      resultTree

    method setRe newRe = 
      re <- newRe

    method getModel = 
      re1#toDisplayString "solution"

    method setTitle = 
      oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (RegularExpression.modelDesignation)

    method operationRE opName : unit =
      super#operation opName "RE"

    method returnType = RegularExpression.modelDesignation

    method private makeTree cy (re : RegExpTypes.t) =
      let nGetName = ref 0 in
      let genName () = 
        let prefix = "N" in 
        nGetName := !nGetName + 1;
        prefix ^ (string_of_int !nGetName)
      in
      let rec makeTree2 cy (re: RegExpTypes.t) =
      match re with
        | Plus (l, r) ->  let rootName = genName () in 
                          let rootL = makeTree2 cy l in 
                          let rootR = makeTree2 cy r in
                            Cytoscape.makeTreeNode cy rootName "+"; 
                            Cytoscape.makeTreeEdge cy rootName rootL;
                            Cytoscape.makeTreeEdge cy rootName rootR;
                            rootName
        | Seq (l, r) -> let rootName = genName () in 
                        let rootL = makeTree2 cy l in 
                        let rootR = makeTree2 cy r in
                          Cytoscape.makeTreeNode cy rootName "."; 
                          Cytoscape.makeTreeEdge cy rootName rootL;
                          Cytoscape.makeTreeEdge cy rootName rootR;
                          rootName
        | Star (re) -> let rootName = genName () in 
                        let root = makeTree2 cy re  in 
                          Cytoscape.makeTreeNode cy rootName "*"; 
                          Cytoscape.makeTreeEdge cy rootName root;
                          rootName
        | Symb (b) -> let rootName = genName () in 
                        Cytoscape.makeTreeNode cy rootName (symb2str b);
                        rootName
        | Empty -> "Empty"
        | Zero -> "Zero"
      in
      makeTree2 cy re

    method private drawTree cy re text =
      if String.length text >= 120 
      then Cytoscape.makeTreeNode cy "nope" (Lang.i18nAlertRETooBig ())
      else ignore (self#makeTree cy re1#representation)

    method defineExample =
      self#operationRE "create";
      self#updateButtons;
      HtmlPageClient.putCyREButtons();
      HtmlPageClient.fitBoxRegex ();
      Cytoscape.fit self#getCy_opt;
      let test = RegExpSyntax.toString re1#representation in
        self#drawTree self#getCy re1#representation test;
        HtmlPageClient.defineRE test side

    method defineExample2 =
      self#operationRE "create 2";
      let text = RegExpSyntax.toString re1#representation in
        self#drawTree self#getCy re1#representation text;
        HtmlPageClient.defineRE text side

    method accept word = 
      self#operationRE "accept";
      let w = str2word word in
        wordAsList <- w;
        twoBoxes self#getCy_opt;
        re1#startAllTrees w;
        resultTree <- re1#accept w;
        changeCtrlR ((new textController true) :> controller );
        Cytoscape.resetStyle !ctrlR#getCy Cytoscape.reStyle;
        if (resultTree) then
          (!ListenersRE.resultCountListener ();
          let right = re1#getRightTrees in 
          ignore (re1#printTree right (!ctrlR#getCy));
            !ListenersRE.defineNumberTreesListener ();
            HtmlPageClient.defineTreeButtons ();
            Lwt.return_true)
        else 
          (!ListenersRE.resultCountListener();
          let wrong = re1#getWrongTrees in 
          ignore(re1#printTree wrong (!ctrlR#getCy));
            !ListenersRE.defineNumberTreesListener ();
            HtmlPageClient.defineTreeButtons ();
            Lwt.return_false)

    method fromExpressionToAutomaton =
      self#operationRE "conversion";
      let rep = re1#representation in 
      let exp = new RegularExpression.model (Representation rep) in 
      let auto = PolyModel.re2fa exp in 
      let maton = auto#representation in 
        new FiniteAutomatonGraphics.model (Representation (maton))
    
    method fromExpressionToTM = 
      self#operationRE "conversion";
      let rep = re1#representation in 
      let exp = new RegularExpression.model (Representation rep) in 
      let tm = PolyModel.re2tm exp in 
      let mtm = tm#representation in 
        new TuringMachineGraphics.model (Representation (mtm))

	  method updateButtons = 
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTMButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons

    method getWords v = 
      self#operationRE "accepted words";
        let var = re1#generate v in 
          HtmlPageClient.putWords var

    method editModel =
      !ListenersRE.editModelListener(); ()

    method replicateOnLeft =
      let c = new reController self#getRE false in
                ctrlL := (c :> controller)

    method printErrors =
      let errors = re1#errors in
        if errors = [] then 
          ()
        else 
          JS.alertStr (String.concat "\n" errors)
    end

  class cfgController (cfg: ContextFreeGrammarGraphics.model) (s:bool) =
    object(self) inherit controller as super
    
    val mutable myCFG = cfg
    
    val side = s
    val cy = if s then None else Some (Cytoscape.initLL1Cy "cy");
    
    method operationCFG opName: unit =
      super#operation opName "CFG"
    
    method model: Model.model =
      (myCFG :> Model.model)
    
    method getCFG = myCFG
    
    method changeCFG res = myCFG <- res
    
    method getModel =
      myCFG#toDisplayString "solution"

    method setTitle =
      oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (ContextFreeGrammar.modelDesignation)
  
    method returnType = ContextFreeGrammar.modelDesignation
    
    method defineExample = 
      self#operationCFG "create example";
      self#updateButtons;
      HtmlPageClient.putCyCFGButtons();
      HtmlPageClient.cfgBoxRegex();
      HtmlPageClient.cfgCyClose();
      HtmlPageClient.defineCFG();
      myCFG#createGrammarTableHtml "";
      self#defineInformationBox

    method defineInformationBox =
      let infoBox = HtmlPageClient.defineInformationBox side in
      if side then HtmlPageClient.cfgCy2Close();
      let ll1 = myCFG#isLL1 in 
        HtmlPageClient.getIsLL1 ll1 infoBox;
      let lr = myCFG#isLeftRecursive in 
        HtmlPageClient.getIsLeftRecursive lr infoBox;
      let lf = myCFG#isLeftFactoring in 
        HtmlPageClient.getIsLeftFactoring lf infoBox;
      let pConf = myCFG#hasParsingTableConflict in
        HtmlPageClient.getHasParsingTableConflict pConf infoBox;
      let c = myCFG#isClean in
      let prod = myCFG#isFullyProductive in
      let access = myCFG#isFullyAccessible in
        HtmlPageClient.getIsCFGClean c prod access infoBox

    method box2CFGShow (f : ContextFreeGrammarGraphics.transformation) =
      self#operationCFG "create example2";
      twoBoxes self#getCy_opt;
      HtmlPageClient.printCFG2Grammar f.tType (ContextFreeGrammarGraphics.productionsTableId2());
      f.grammar#createGrammarTableHtml (ContextFreeGrammarGraphics.productionsTableId2());
      !ctrlR#defineInformationBox

    method accept word =
      self#operationCFG "accept";
      if myCFG#isLL1
      then (self#acceptCFGLL1 word; Lwt.return_true)
      else (JS.alertStr (Lang.i18nIsNotLL1()); Lwt.return_false)      

    method private acceptCFGLL1 word = 
      self#startStep word;
      let steps = myCFG#nSteps - 1 in
      let rec tic n =
        match n with
        | 0 -> Lwt.return()
        | _ -> Lwt.bind 
                  (Js_of_ocaml_lwt.Lwt_js.sleep 1.0)
                  (fun () -> self#nextStep; tic (n-1))
      in
      ignore (tic steps)

    method getWords v = 
      self#operationCFG "accepted words";
      let var = myCFG#generate v in 
        HtmlPageClient.putWords var


    method startStep word =
      self#operationCFG "accept start";
      twoBoxes self#getCy_opt;
      HtmlPageClient.cfgCyOpen();
      HtmlPageClient.prepareCFG2Tables ();
      match cy with
        | None -> ();
        | Some cy -> Cytoscape.removeAllElements cy;
      myCFG#createFirstAndFollowTableHtml;
      myCFG#createParsingTableHtml;
      if myCFG#isLL1
        then myCFG#startAccept self#getCy (str2word word)
        else JS.alertStr (Lang.i18nIsNotLL1())
    
    method nextStep =
      self#operationCFG "accept next";
      myCFG#next self#getCy
          
    method backStep = 
      self#operationCFG "accept back";
      myCFG#back self#getCy

    method editModel =
      !ListenersCFG.editModelListener(); ()

    method replicateOnLeft =
      let c = new cfgController self#getCFG false in
        ctrlL := (c :> controller);

  	method updateButtons = 
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyTMButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons
  
  end

  class tmController (tm: TuringMachineGraphics.model) (s: bool)=
    object(self) inherit controller as super

    val mutable myTM = tm

    val side = s
    val cy = Some (if s then Cytoscape.initFaCy "cy2" else Cytoscape.initFaCy "cy")

    method operationTM opName : unit =
        super#operation opName "TM"

    method model: Model.model = 
      (myTM :> Model.model) 

    method resetStyle = 
      Cytoscape.resetStyle self#getCy Cytoscape.faStyle

    method getTM =
      myTM

    method changeAutomata res =
      myTM <- res

    method getModel = 
      myTM#toDisplayString "solution" 

    method setTitle = 
      oneBox self#getCy_opt;
      HtmlPageClient.defineMainTitle (TuringMachine.modelDesignation)

    method returnType = TuringMachine.modelDesignation

    method startGraph = self#defineExample (*TODO Remove this call, call defineExample directly*)

    method defineExample =
      self#operationTM "create example";
      self#updateButtons;
      HtmlPageClient.putCyTMButtons ();
      HtmlPageClient.closeBoxRegex ();
      myTM#drawExample self#getCy;
      self#defineInformationBox;
      Cytoscape.fit self#getCy_opt;

    method defineExample2 = 
      myTM#drawExample self#getCy;
      self#defineInformationBox;

    method defineInformationBox =
      let infoBox = HtmlPageClient.defineInformationBox side in
      let deter = myTM#isDeterministic in 
        Util.println ["deter"];
        HtmlPageClient.getDeterminim deter infoBox;
      let useful = myTM#areAllStatesUseful in
      Util.println ["useful"];
      let uStates = myTM#getUselessStates in 
        Util.println ["uStates"];
        HtmlPageClient.getHasUselessStates useful uStates infoBox;
      let nStates = myTM#numberStates in 
        Util.println ["nStates"];
        HtmlPageClient.getNumberStates nStates infoBox;
      let nTransitions = myTM#numberTransitions in
        Util.println ["nTransitions"];
        HtmlPageClient.getNumberTransitions nTransitions infoBox;
      let isLB = myTM#isLB in
        Util.println ["isLB"];
        HtmlPageClient.getIsLinearBounded isLB infoBox;

    (* TODO *)
    (* - Para TM's, como e que posso pedir mais dados como simbolo escrever e a direcao,
       pedir tudo de uma vez ou pedir prompt a prompt?
       - necessario epsilon?
    *)

	  method updateButtons =
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyExpressionButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyCFGButtons;
      List.iter (fun el -> HtmlPageClient.disableButton el) listOnlyAutomataButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOnlyTMButtons;
      List.iter (fun el -> HtmlPageClient.enableButton el) listOtherButtons
    
    method editModel = 
      !ListenersTM.editModelListener();
    
    method replicateOnLeft =
      let c = new tmController self#getTM false in
        ctrlL := (c :> controller);

    method updateRight =
      if !ctrlR#getUpdateType = Some "specification"
      then !Listeners.showModelListener ()
    
    method printErrors =
      let errors = myTM#errors in
        if errors = [] then 
          ()
        else 
          JS.alertStr (String.concat "\n" errors);

    method accept word = 
      self#operationTM "accept";
      self#startStep word;
      let rec tic n =
        match n with
        | true -> Lwt.return()
        | false -> Lwt.bind 
                  (Js_of_ocaml_lwt.Lwt_js.sleep 1.0)
                  (fun () -> self#nextStep; tic myTM#isOver)
      in
      ignore(tic false);
      Lwt.return_true

    method getNewSentence = 
      Js.string myTM#newSentence1

    method updateScreenSentence = 
      (Dom_html.getElementById "regExp")##.innerHTML := self#getNewSentence

    method changeToEditMode =
      self#getTM#changeToEditModelMode self#getCy;
      self#updateScreenSentence

    method startStep word =
      self#operationTM "accept start";
      HtmlPageClient.fitBoxRegex ();
      myTM#changeTheTestingSentence word;
      myTM#startAccept self#getCy;
      self#updateScreenSentence

    method nextStep =
      self#operationTM "accept next";
      myTM#next self#getCy;
      self#updateScreenSentence

    method backStep = 
      self#operationTM "accept back";
      myTM#back self#getCy;
      self#updateScreenSentence

    method checkForSimulation func = 
      if self#getTM#isSimulating then 
        (
          let confResult = (JS.confirm i18nLeaveSimulationToEdit) in
          if confResult then 
            (
              self#changeToEditMode;
              func
            )
          else ()
        )
      else 
        (
          self#changeToEditMode;
          func
        )

    method addNode x y node : unit = 
      self#operationTM "add Node";
      let addNodeLogic x y node =
        if (myTM#hasState node) then 
          (
            JS.alertStr (Lang.i18nAlertExists ())
          )
        else 
          ( 
              myTM <- myTM#addNode node;
              Cytoscape.addNode self#getCy node ~x:x ~y:y false false;
              self#defineInformationBox
          )
      in
      self#checkForSimulation (addNodeLogic x y node)


    method addInitialNode node =
      self#operationTM "make node initial";
      if (myTM#hasState node) then
        (
          JS.alertStr (Lang.i18nAlertExists ())
        )
      else 
        (
          myTM <- (myTM#addInitialNode node);
          let cy = self#getCy in   
          Cytoscape.resetFaElems cy;
          myTM#drawExample cy;
          self#defineInformationBox;
          if self#getTM#isSimulating then self#changeToEditMode else ()
        )

    method addFinalNode x y node =
      self#operationTM "add final node";
      if (myTM#hasState node) then
        (
          JS.alertStr (Lang.i18nAlertExists ())
        )
      else 
        (
          myTM <- myTM#addFinalNode node;
          Cytoscape.addNode self#getCy ~x:x ~y:y node false true;
          self#defineInformationBox;
          if self#getTM#isSimulating then self#changeToEditMode else ()
        )

    method eliminateNode node =
      self#operationTM "eliminate node";
      if (not (myTM#hasState node)) then 
        (
          JS.alertStr (Lang.i18nAlertUnexistentState ())
        )
      else if (myTM#isInitial node) then 
        (
          JS.alertStr (Lang.i18nAlertDelete ()) 
        )
      else
        (
          let eliminateNodeTransitions (a, b, c) node = 
            if (a = node || c = node) then
              ( 
                self#eliminateTransition (a, b, c);
              ) in 
          myTM <- myTM#eliminateNode node;
          Set.iter (fun (a,b,c,d,e) -> (eliminateNodeTransitions (a, myTM#makeLabel b d e,c) node)) myTM#representation.transitions;
          Cytoscape.removeNode self#getCy node;
          self#defineInformationBox;
          if self#getTM#isSimulating then self#changeToEditMode else ()
        )

    method turnFinalNode node =
      self#operationTM "make node final";
      else 
        (
          self#changeToEditMode;
        )
      if (myTM#isFinal node) then
        (
          JS.alertStr (Lang.i18nAlertAlreadyFinal ())
        )
      else
        (
          myTM <- (myTM#changeToFinal node);
          Cytoscape.turnFinal self#getCy node;
          self#defineInformationBox;
          if self#getTM#isSimulating then self#changeToEditMode else ()
        )

    method removeFinalNode node =
      self#operationTM "make node not final";
      if (not (myTM#isFinal node)) then
        (
          JS.alertStr (Lang.i18nAlertNonFinal ())
        )
      else
        (
          myTM <- (myTM#removeFinal node);
          Cytoscape.removeFinal self#getCy node;
          self#defineInformationBox;
          if self#getTM#isSimulating then self#changeToEditMode else ()
        ); 

    method renameState state =
      self#operationTM "rename node";
      if self#getTM#isSimulating then 
        (
          let confResult = (JS.confirm i18nLeaveSimulationToEdit) in
          if confResult then 
            (
              self#changeToEditMode;
            )
          else ()
        )
      else 
        (
          self#changeToEditMode;
        )
      let prompt = JS.prompt (Lang.i18nRenameStateQuestion()) state in
      match Js.Opt.to_option prompt with
      | None -> ()
      | Some n -> 
          let newName = (Js.to_string n) in
            if (myTM#hasState newName) then
              (JS.alertStr (Lang.i18nAlertExists ()))
            else
              (
                myTM <- myTM#renameNode state newName;
                Cytoscape.resetFaElems self#getCy;
                self#defineExample;
                if self#getTM#isSimulating then self#changeToEditMode else ()
              )

    method createTransition source target =
      self#operationTM "add transition";
      let promptResult = (JS.prompt (Lang.i18nTextEnterTransitionTM ()) "a/a/R") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some x ->
          let v = Js.to_string x in
            let (rdSymbol, wrtSymbol, dir) = myTM#dissectTransitionInput v in
            let newTrs = (source, str2symb rdSymbol, target, char2symb wrtSymbol, char2direction dir) in
              if not (stringIsDirection dir) then
                (
                  (JS.alertStr (Lang.i18nAlertDirectionWrong ()))
                )
              else if (String.length v != 5 && (String.length v != 6 && (String.get v 0) != '~')) then
                (
                  (JS.alertStr (Lang.i18nAlertExceededCharacters ()))
                )
              else if (myTM#hasTransition newTrs) then
                (
                  (JS.alertStr (Lang.i18nAlertTransitionExists ()))
                )
              else
                (
                  myTM <- myTM#newTransition newTrs;
                  Cytoscape.addEdgeGeneral self#getCy (source, v, target);
                  self#defineInformationBox;
                  if self#getTM#isSimulating then self#changeToEditMode else ()
                )      

    method eliminateTransition (a, b, c) =
      self#operationTM "erase transition";
      let (d,e,f) = match String.split_on_char '/' (b)  with
        | [d;e;f] -> (d,e,f)
        | _ -> failwith "eliminate transition"
      in
      let trans = (a, str2symb d, c, str2symb e, string2direction f) in
      if (not (myTM#hasTransition trans)) then
        (
          JS.alertStr ((Lang.i18nAlertTheTransition ()) ^ "(" ^ a ^ ", " ^ b ^ ", " ^ c ^ ")" ^ (Lang.i18nAlertDoNotExists ()))
        )
      else 
        (
          myTM <- (myTM#eliminateTransition trans);
          Cytoscape.removeEdge self#getCy a b c;
          self#defineInformationBox;
          if self#getTM#isSimulating then self#changeToEditMode else ()
        )

    method getWords v = 
      self#operationTM "accepted words";
      let var = myTM#generate v in 
        HtmlPageClient.putWords var
  end

  class exerController (exer: Exercise.exercise) (on: bool) (title: string) =
    object(self) inherit controller as super

      val exer1 = exer

      val title1 = title
      
      val cy = Some (Cytoscape.initCy "cy2") (*TODO Check if needed*)
      
      method locked = true (*Exercises always located on the right*)

      method getExercise =
        exer1

      method setTitle = 
        HtmlPageClient.defineMainTitle (title1)

      method operationEXER opName : unit =
        super#operation opName "Exercise"

      method returnType = RegularExpression.modelDesignation

      method defineExample2 =
        self#operationEXER "create";
        twoBoxes !ctrlL#getCy_opt;
        HtmlPageClient.addEnumTitle();
        let prob = exer1#representation.problem in
          HtmlPageClient.defineEnumProblem prob;
        HtmlPageClient.addPropertiesBox ();
        Set.iter (fun el -> HtmlPageClient.createPropertiesList el "nothing" "properties") exer1#representation.properties;
        HtmlPageClient.addAcceptedTitle ();
        Set.iter (fun el -> HtmlPageClient.createSpanList el "nothing" "inside") exer1#representation.inside;
        HtmlPageClient.addNonAcceptTitle ();
        Set.iter (fun el -> HtmlPageClient.createSpanList el "nothing" "outside") exer1#representation.outside;
        HtmlPageClient.addEnumCheckButton ()

     method checkHelper result (insideErrors, outsideErrors, properties) = 
      self#operationEXER "check exercise";
            HtmlPageClient.defineResult result;
              Set.iter (fun el -> 
                if Set.belongs el properties then HtmlPageClient.createPropertiesList el "error" "properties" 
                else HtmlPageClient.createPropertiesList el "right" "properties") exer1#representation.properties;
              Set.iter (fun el -> 
                if Set.belongs el insideErrors then HtmlPageClient.createSpanList el "error" "inside" 
                else HtmlPageClient.createSpanList el "right" "inside") exer1#representation.inside;
              Set.iter (fun el -> 
                if Set.belongs el outsideErrors then HtmlPageClient.createSpanList el "error" "outside" 
                else HtmlPageClient.createSpanList el "right" "outside") exer1#representation.outside

  end

let createFAController fa lr =
  let c = new faController fa lr in
    if lr then begin 
      ctrlR := (c :> controller);
    end
    else begin 
      ctrlL := (c :> controller);
    end

let createREController re lr =
  let c = new reController re lr in
    if lr then begin 
      ctrlR := (c :> controller);
    end
    else begin 
      ctrlL := (c :> controller);
    end

let createCFGController cfg lr =
  let c = new cfgController cfg lr in
    if lr then begin
      ctrlR := (c :> controller);
    end
    else begin
      ctrlL := (c :> controller);
    end

let createTMController tm lr =
  let c = new tmController tm lr in
    if lr then begin 
      ctrlR := (c :> controller);
    end
    else begin 
      ctrlL := (c :> controller);
    end

let createExerController ex lr title =
  let c = new exerController ex lr title in
    if lr then begin 
      ctrlR := (c :> controller);
    end
    else begin 
      ctrlL := (c :> controller);
    end


module Controller
=
  struct
    let listColors = [|"Red"; "Yellow"; "Cyan"; "Green"; "Indigo"; "Blue"; "Magenta"; "Sienna"; "Violet"; "Orange"; "Lime"; "Teal"; "SteelBlue"; "Silver"; "Olive"; "Salmon"; "Crimson"; "Purple"; "DarkKhaki"; "PowderBlue"|]
    let listColorsBig: string array ref = ref [||];;

    let setTitle () =
      if !ctrlR#locked then 
        !ctrlR#setTitle
      else 
        !ctrlL#setTitle

    let closeLeftAction () =
      (match !ctrlR#locked with
      | false -> 
        HtmlPageClient.clearBox1 ();
        !ctrlR#replicateOnLeft;
        !ctrlL#defineExample;
        oneBox !ctrlL#getCy_opt
      | true -> (*apagar apenas esquerda*)
        HtmlPageClient.clearBox1 ();
        !changeToControllerCtrlLeft ());
      !ctrlL#updateButtons;
      setTitle()
      
    let convertFromFA n = 
      if n = 1 then
        let re = !ctrlL#automatonToRegExp in 
          createREController re true;
          !ctrlR#defineExample2
      else if n = 3 then
        let tm = !ctrlL#automatonToTM in
          createTMController tm true;
          !ctrlR#defineExample2;
      Cytoscape.fit !ctrlR#getCy_opt;;

    let convertFromRE n =
      JS.log("in convert RE");
      if n = 2 then
          let fa = !ctrlL#fromExpressionToAutomaton in 
          createFAController fa true;
          !ctrlR#defineExample2;
      else if n = 3 then
          let tm = !ctrlL#fromExpressionToTM in
          createTMController tm true;
          !ctrlR#defineExample2;
      Cytoscape.fit !ctrlR#getCy_opt;;

    let conversionTo n =
      JS.log("in convert");
      twoBoxes !ctrlL#getCy_opt;
      if !ctrlL#returnType = FiniteAutomaton.modelDesignation then
        (convertFromFA n)
      else if !ctrlL#returnType = RegularExpression.modelDesignation then
        (convertFromRE n);;

    let getRandom() = 
      let test = Random.int 16777215 in
      Printf.sprintf "#%06x" test

    let setColor number =
      if (number <= 20) then 
        listColorsBig := listColors
      else 
        (for i=0 to 19 do 
          Array.set !listColorsBig i (Array.get listColors i)
          done;
        for i=20 to number-1 do
          let newColor = getRandom () in 
            Array.set !listColorsBig i newColor
        done);;

    let createModelPrep titleTxt otherTxt textAreaString okAction =
      let modelContent = HtmlPageClient.editModelContent titleTxt otherTxt textAreaString okAction in
        HtmlPageClient.setModal (Js.Unsafe.coerce modelContent);
        HtmlPageClient.showModalWindow ()
    
    let createModelPrepFA textAreaString okAction =
      createModelPrep (Lang.i18nMainTitle1()) (Lang.i18nInstructionsFA()) textAreaString okAction
  
    (* CHECK*)
    let createModelPrepTM textAreaString okAction =
      createModelPrep (Lang.i18nMainTitleTM()) (Lang.i18nInstructionsFA()) textAreaString okAction
    
    let createModelPrepRE textAreaString okAction =
      createModelPrep (Lang.i18nMainTitle2()) "" textAreaString okAction
      
    let createModelPrepCFG textAreaString okAction =
      createModelPrep (Lang.i18nMainTitle4()) (Lang.i18nInstructionsCFG()) textAreaString okAction
    
    let createFA () =
      !ListenersFA.createModelListener()
      
    let createRE () =
      createModelPrepRE "ab" (fun () -> !ListenersRE.createModelListener())
  
    let createCFG () =
      createModelPrepCFG "S -> [ S ] | A\nA -> a" (fun () -> !ListenersCFG.createModelListener())
  
    (* CHECK*)
    let createTM () =
      !ListenersTM.createModelListener()
  
    let extractStringFromTextArea () =
      match Dom_html.getElementById_coerce "modelStringContainer" Dom_html.CoerceTo.textarea with
        | None -> ""
        | Some textarea -> Js.to_string textarea##.value;;
      
    let cfgStr2Model str = (*TODO Where to put this?*)
      let open CFGTypes in
      let splitStr = String.split_on_char '\n' str in
      let initialRule = Set.make [List.hd splitStr] in
      let otherRules = Set.make (List.tl splitStr) in
      let initialParsedRule = CFGSyntax.parse initialRule in
      let initial = (Set.nth initialParsedRule 0).head in
      let parsedRules = CFGSyntax.parse (Set.union initialRule otherRules) in
      let variables = Set.add initial (Set.map (fun {head = h; _} -> h) parsedRules) in
      let alphabet = Set.flatMap (fun {head = h; body = b} -> Set.make (List.filter (fun s -> not (Set.belongs s variables)) b) ) parsedRules in
      new ContextFreeGrammarGraphics.model (Arg.Representation {
        alphabet = alphabet;
        variables = variables;
        initial = initial;
        rules = parsedRules
      } );;
            
    let grammar2Str (rep:ContextFreeGrammarGraphics.t) = (*TODO Where to put this?*)
      let open CFGTypes in
      let initialRules = Set.filter (fun {head = h; _} -> h = rep.initial) rep.rules in
      let nonInitialRules = Set.filter (fun {head = h; _} -> h <> rep.initial) rep.rules in
      let initialRulesStrLst = CFGSyntax.toStringList initialRules in
      let nonInitialRulesStrLst = CFGSyntax.toStringList nonInitialRules in
      let rulesList = initialRulesStrLst @ nonInitialRulesStrLst in
      let rec toString l =
        match l with
        | [] -> ""
        | x::xs -> x ^ "\n" ^ (toString xs)
      in
        toString rulesList;;
      

    (* General listener setup*)        
   
    Listeners.closeRightListener := 
      fun () -> !ctrlR#closeRightAction;
                !ctrlL#resetStyle;
                oneBox (!ctrlL#getCy_opt);
                setTitle();;

    Listeners.defineInformationBoxListener :=
      fun () -> !ctrlL#defineInformationBox;;

    Listeners.openEntityListener :=
    fun (txt) -> (
      let j = JSon.parse txt in
        let kind = JSon.fieldString j "kind" in
        (match kind with
        | k when k = FiniteAutomatonGraphics.modelDesignation() -> 
              HtmlPageClient.clearBox1 ();
              let fa = new FiniteAutomatonGraphics.model (JSon j) in 
              createFAController fa false;
              !ctrlL#defineExample
        | k when k = RegularExpressionGraphics.modelDesignation() -> 
              HtmlPageClient.clearBox1 ();
              let re = new RegularExpressionGraphics.model (JSon j) in 
              createREController re false;
              !ctrlL#defineExample
        | k when k = ContextFreeGrammarGraphics.modelDesignation() -> 
              HtmlPageClient.clearBox1 ();
              let cfg = new ContextFreeGrammarGraphics.model (JSon j) in 
              createCFGController cfg false;
              !ctrlL#defineExample
        | k when k = TuringMachineGraphics.modelDesignation() -> 
              HtmlPageClient.clearBox1 ();
              let tm = new TuringMachineGraphics.model (JSon j) in 
              createTMController tm false;
              !ctrlL#defineExample
        | _ -> HtmlPageClient.clearBox2 ();
              let enu = new Exercise.exercise (JSon j) in 
              createExerController enu true "exercise";
              !ctrlR#defineExample2);
        setTitle();
        !ctrlL#printErrors);;

    Listeners.closeLeftListener := 
    fun () -> 
      (closeLeftAction ();
      setTitle () );;
              
    Listeners.showModelListener := 
      fun () -> twoBoxes !ctrlL#getCy_opt;
                !changeToControllerCtrlRight();
                !ctrlR#setUpdateType "specification";
                let getInfo = JSon.toString ((!ctrlL#model)#toJSon) in
                HtmlPageClient.showModelInfo getInfo;;

    Listeners.createModelListener :=
      fun () -> match Dom_html.getElementById_coerce "selectNewModel" Dom_html.CoerceTo.select with
                | None -> ()
                | Some select -> let value = Js.to_string select##.value in
                                  if value = "optionNewAutomaton" then createFA () else
                                  if value = "optionNewRegularExpression" then createRE () else
                                  if value = "optionNewContextFreeGrammar" then createCFG () else
                                  if value = "optionNewTuringMachine" then createTM ();(* Onde esta esta string definida?*)
                                  select##.selectedIndex := 0;;

    Listeners.editModelListener :=
      fun () -> !ctrlL#editModel;;

    Listeners.updateRightListener :=
      fun () -> !ctrlL#updateRight;;


    (* General Automata listener setup*)

    ListenersAutomata.addNode := fun x y -> 
      let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v -> !ctrlL#addNode x y (Js.to_string v)
      ;;

    ListenersAutomata.removeNode := fun node -> !ctrlL#eliminateNode node;;

    ListenersAutomata.turnFinal := fun node -> !ctrlL#turnFinalNode node;;

    ListenersAutomata.removeTypeFinal := fun node -> !ctrlL#removeFinalNode node;;
  
    ListenersAutomata.turnNodeInitial := fun node -> !ctrlL#addInitialNode node;;
  
    ListenersAutomata.addInitialNode := fun x y ->
      let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v -> !ctrlL#addInitialNode (Js.to_string v)
      ;;
  
    ListenersAutomata.addFinalNode := fun x y ->
      let promptResult = (JS.prompt (Lang.i18nTextEnterState ()) "A") in
      match Js.Opt.to_option promptResult with
      | None -> ()
      | Some v -> !ctrlL#addFinalNode x y (Js.to_string v)
      ;;
      
    ListenersAutomata.addTransition := fun src trg -> !ctrlL#createTransition src trg ;;

    ListenersAutomata.removeTransition := fun srcId trgId label -> !ctrlL#eliminateTransition (srcId, label, trgId);;

    ListenersAutomata.renameNodeListener :=
      fun (state) -> !ctrlL#renameState state;;

  
    (* Finite Automata listener setup*)

    ListenersFA.createModelListener := 
      fun () -> let defaultFA = new FiniteAutomatonGraphics.model (Representation {
                  alphabet = Set.empty;
                  states = Set.make ["START"]; 
                  initialState = "START";
                  transitions = Set.empty;
                  acceptStates = Set.empty
                  }) in
                oneBox !ctrlL#getCy_opt;
                HtmlPageClient.clearBox1();
                createFAController defaultFA false;
                !ctrlL#defineExample;
                setTitle();;

    ListenersFA.editModelListener :=
      fun () -> JS.alert (Lang.i18nModelEditFA());;

    (* ListenersFA.defineInformationBoxListener*)

    ListenersFA.paintAllProductivesListener :=
      fun () -> (!ctrlL#resetStyle;
      (!ctrlL#getFA)#productivePainting !ctrlL#getCy);;

    ListenersFA.paintAllReachableListener := 
      fun () -> !ctrlL#resetStyle;
      (!ctrlL#getFA)#reachablePainting !ctrlL#getCy;;

    ListenersFA.paintAllUsefulListener :=
      fun () -> !ctrlL#resetStyle;
      (!ctrlL#getFA)#usefulPainting !ctrlL#getCy;;

    ListenersFA.cleanUselessListener :=
      fun () -> if ((!ctrlL#getFA)#areAllStatesUseful) then 
                  JS.alertStr (Lang.i18nAlertClean ())
                else 
                  (let auto = (!ctrlL#getFA)#cleanUselessStates1 !ctrlL#getCy in 
                  twoBoxes !ctrlL#getCy_opt;
                  createFAController auto true;
                  !ctrlR#defineExample2;
                  Cytoscape.fit !ctrlR#getCy_opt;);;
  
    ListenersFA.getDeterministicListener :=
      fun () -> if ((!ctrlL#getFA)#isDeterministic) then 
                  JS.alertStr (Lang.i18nAlertDeterministic ())
                else 
                  (let auto = (!ctrlL#getFA)#toDeterministic1 in 
                  twoBoxes !ctrlL#getCy_opt;
                  createFAController auto true;
                  !ctrlR#defineExample2;
                  Cytoscape.fit !ctrlR#getCy_opt);;

    ListenersFA.defineMinimizedListener :=
      fun () -> if ((!ctrlL#getFA)#isDeterministic) then
                  if ((!ctrlL#getFA)#isMinimized) then 
                    JS.alertStr (Lang.i18nAlertMinimum ())
                  else 
                    (let auto = (!ctrlL#getFA)#minimize1 in 
                    twoBoxes !ctrlL#getCy_opt;
                    createFAController auto true;
                    let number = (!ctrlL#getFA)#getColors in
                      setColor number;
                    !ctrlR#defineMinimize !listColorsBig number;)
                else 
                  JS.alertStr (Lang.i18nAlertNeedsDeterministic ());;
      
    ListenersFA.clearAutoListener :=
      fun () -> Cytoscape.resetStyle !ctrlL#getCy Cytoscape.faStyle;;
    

    (* Regular Expression listener setup*)
                  
    ListenersRE.resultCountListener := 
      fun () -> 
        if (!ctrlL#getResultTree)  then 
          HtmlPageClient.putTreeResult (Lang.i18nWordAccepted ())
        else 
          (HtmlPageClient.putTreeResult (Lang.i18nWordNotAccepted ()));
          let blah = !ctrlL#getRE in 
          let blah2 = !ctrlL#getWordAsList() in 
          let (right, wrong) = blah#countRightTrees blah2 in 
            let textt = (Lang.i18nExists ()) ^ (string_of_int (right)) ^ (Lang.i18nGoodDerivations ()) in
              HtmlPageClient.putTreeGoodDerivations textt;
            let textt1 = (Lang.i18nExists ()) ^ (string_of_int (wrong)) ^ (Lang.i18nBadDerivations ()) in
              HtmlPageClient.putTreeBadDerivations textt1;;
        
    ListenersRE.defineNumberTreesListener :=
      fun () -> 
        let pos = (!ctrlL#getRE)#position in 
        let leng = (!ctrlL#getRE)#length in 
        let textt = (string_of_int (pos)) ^ (Lang.i18nBy ()) ^ (string_of_int (leng)) in 
          HtmlPageClient.putTreeNumbers textt;;
    
    ListenersRE.previousTreeListener :=
      fun () -> 
        let back = (!ctrlL#getRE)#back in
        let cy = !ctrlR#getCy in
        Cytoscape.removeAllElements cy;
        ignore ((!ctrlL#getRE)#printTree back cy);
          !ListenersRE.defineNumberTreesListener ();;
    
    ListenersRE.nextTreeListener := 
      fun () ->
        let next = (!ctrlL#getRE)#next in
        let cy = !ctrlR#getCy in
        Cytoscape.removeAllElements cy;
        ignore ((!ctrlL#getRE)#printTree next cy); 
          !ListenersRE.defineNumberTreesListener ();;
        

    ListenersRE.changeDirectionListener :=  fun () -> 
      let newDir = (Cytoscape.changeDirection !ctrlL#getCy !ctrlL#getLayoutDir) in
      !ctrlL#changeLayoutDir newDir;;

    ListenersRE.createModelListener :=
      fun () -> try
                  let str = extractStringFromTextArea() in
                  let reStr = new RegularExpressionGraphics.model (Representation (RegExpSyntax.parse str)) in
                  HtmlPageClient.hideModalWindow();
                  oneBox !ctrlL#getCy_opt;
                  HtmlPageClient.clearBox1();
                  createREController reStr false;
                  !ctrlL#defineExample;
                  setTitle()
                with
                  _ -> JS.alert (Lang.i18nErrorParsing());;

    ListenersRE.editModelListener :=
      fun () -> createModelPrepRE (RegExpSyntax.toString !ctrlL#getRE#representation) (fun () -> !ListenersRE.createModelListener());;


    (* Regular Expression listener setup*)
      
    ListenersCFG.cleanCFGListener :=
      fun () -> let cfg = !ctrlL#getCFG in
                if ( not cfg#isClean ) 
                then ( let newCfg = cfg#clean1 in
                  createCFGController newCfg.grammar true;
                  !ctrlL#box2CFGShow newCfg)
                else JS.alertStr (Lang.i18nAlertIsClean());;
    
    ListenersCFG.previousNewCFGListener :=
      fun () -> let newCFG = (!ctrlL#getCFG)#getPreviousTransformed in
                createCFGController newCFG.grammar true;
                !ctrlL#box2CFGShow newCFG;;

    ListenersCFG.nextNewCFGListener :=
      fun () -> let newCFG = (!ctrlL#getCFG)#getNextTransformed in
                createCFGController newCFG.grammar true;
                !ctrlL#box2CFGShow newCFG;;

    ListenersCFG.removeLeftRecursionListener :=
      fun () -> let cfg = !ctrlL#getCFG in
                if cfg#isLeftRecursive
                then ( let newCfg = cfg#removeLeftRecursion1 in
                  createCFGController newCfg.grammar true;
                  !ctrlL#box2CFGShow newCfg)
                else JS.alertStr (Lang.i18nAlertNotLeftRecursive());;
      
    ListenersCFG.leftFactoringListener :=
      fun () -> let cfg = !ctrlL#getCFG in
                if cfg#isLeftFactoring
                then ( let newCfg = cfg#leftFactoring1 in
                  createCFGController newCfg.grammar true;
                  !ctrlL#box2CFGShow newCfg)
                else JS.alertStr (Lang.i18nAlertNotLeftFactoring());;
    
    ListenersCFG.removeEpsilonListener :=
      fun () -> let cfg = !ctrlL#getCFG in
                if cfg#hasEmptyProductions
                then ( let newCfg = cfg#removeEmptyProductions1 in
                  createCFGController newCfg.grammar true;
                  !ctrlL#box2CFGShow newCfg)
                else JS.alertStr (Lang.i18nAlertNoEmptyProductions());;

    ListenersCFG.removeUnitListener :=
      fun () -> let cfg = !ctrlL#getCFG in
                if cfg#hasUnitProductions
                then ( let newCfg = cfg#removeUnitProductions1 in
                  createCFGController newCfg.grammar true;
                  !ctrlL#box2CFGShow newCfg)
                else JS.alertStr (Lang.i18nAlertNoUnitProductions());;

    ListenersCFG.transformLL1Listener :=
      fun () -> let cfg = !ctrlL#getCFG in
                if not cfg#isLL1
                then ( let newCfg = cfg#transformToLL1X in
                  createCFGController newCfg.grammar true;
                  !ctrlL#box2CFGShow newCfg)
                else JS.alertStr (Lang.i18nAlertIsLL1());;

    ListenersCFG.tablesListener :=
      fun () -> let cfg = !ctrlL#getCFG in
                twoBoxes !ctrlL#getCy_opt;
                HtmlPageClient.prepareCFG2Tables ();
                cfg#createFirstAndFollowTableHtml;
                cfg#createParsingTableHtml;;
    
    ListenersCFG.recursiveDescedentParserListener :=
      fun () -> let cfg = !ctrlL#getCFG in
                if not cfg#isLL1
                then JS.alertStr (Lang.i18nIsNotLL1())
                else  (
                  twoBoxes !ctrlL#getCy_opt;
                  let optsList = cfg#rdparserOpts in
                  HtmlPageClient.prepareCFG2RecursiveDescedentParser optsList (!ctrlL#getCFG#generateRecursiveDescendentParser));;
    
    ListenersCFG.simpleToggleListener :=
      fun () -> let cfg = !ctrlL#getCFG in
                cfg#toggleSimplified;
                let elem = Dom_html.getElementById_opt (ContextFreeGrammarGraphics.firstFollowTableId()) in
                match elem with
                  | None -> ()
                  | Some e -> let otherE = Dom_html.getElementById (ContextFreeGrammarGraphics.parsingTableId()) in
                              e##.innerHTML := Js.string "";
                              otherE##.innerHTML := Js.string "";
                              cfg#createFirstAndFollowTableHtml;
                              cfg#createParsingTableHtml;;
  
    ListenersCFG.createModelListener :=
      fun () -> try 
                  let str = extractStringFromTextArea() in
                  let cfgModel = cfgStr2Model str in
                  HtmlPageClient.hideModalWindow();
                  oneBox !ctrlL#getCy_opt;
                  HtmlPageClient.clearBox1();
                  createCFGController cfgModel false;
                  !ctrlL#defineExample;
                  setTitle()
                with
                  _ -> JS.alert (Lang.i18nErrorParsing());;
  
    ListenersCFG.editModelListener :=
      fun () -> createModelPrepCFG (grammar2Str !ctrlL#getCFG#representation) (fun () -> !ListenersCFG.createModelListener());;


    (* Turing Machine listener setup*)

    ListenersTM.createModelListener := 
      fun () -> let defaultTM = new TuringMachineGraphics.model (Representation TuringMachine.getDefaultMachine) in
                oneBox !ctrlL#getCy_opt;
                HtmlPageClient.clearBox1();
                createTMController defaultTM false;
                !ctrlL#defineExample;
                setTitle();;

    ListenersTM.editModelListener :=
      fun () -> JS.alert (Lang.i18nModelEditTM());
      (!ctrlL#getTM)#changeToEditModelMode !ctrlL#getCy;
      !ctrlL#updateScreenSentence;;

    ListenersTM.paintAllProductivesListener :=
      fun () -> (!ctrlL#resetStyle;
      (!ctrlL#getTM)#productivePainting !ctrlL#getCy);;

    ListenersTM.paintAllReachableListener := 
      fun () -> !ctrlL#resetStyle;
      (!ctrlL#getTM)#reachablePainting !ctrlL#getCy;;

    ListenersTM.paintAllUsefulListener :=
      fun () -> !ctrlL#resetStyle;
      (!ctrlL#getTM)#usefulPainting !ctrlL#getCy;; 

    (* Acho que aqui e indicado usar o estilo do fa dadas as semelhancas entre tm e fa*)
    ListenersTM.clearAutoListener :=
      fun () -> Cytoscape.resetStyle !ctrlL#getCy Cytoscape.faStyle;;

    ListenersTM.cleanUselessListener :=
    fun () -> if ((!ctrlL#getTM)#areAllStatesUseful) then 
                JS.alertStr (Lang.i18nAlertClean ())
              else 
                (let auto = (!ctrlL#getTM)#cleanUselessStatesCy !ctrlL#getCy in 
                twoBoxes !ctrlL#getCy_opt;
                createTMController auto true;
                !ctrlR#defineExample2;
                Cytoscape.fit !ctrlR#getCy_opt;);;


    (* Exercise listener setup*)
                  
    ListenersEXER.checkExerciseListener :=
      fun () -> (
          let model = !ctrlL#model in 
          JS.log (model);
          let result = model#checkExercise (!ctrlR#getExercise) in
          let (insideErrors, outsideErrors, properties) = model#checkExerciseFailures (!ctrlR#getExercise) in 
            !ctrlR#checkHelper result (insideErrors, outsideErrors, properties);
      );;
        
    ListenersEXER.clearExerciseListener :=
      fun () -> (
        HtmlPageClient.oneBox ();
        let element = Dom_html.getElementById "cy2" in
          element##.innerHTML := Js.string "";
        !changeToControllerCtrlRight();
        JS.log ("antes");
        setTitle ();
        JS.log ("depois")
        );;

end
