(*
 * FiniteAutomatonGraphics.ml
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

 open OCamlFlat
open BasicTypes
open JS
open Graphics
open Lang
open Cytoscape
open TurMachTypes

  module rec TuringMachineGraphics : sig
    type transition = state * symbol * state * symbol * direction
    type transitions = transition set
    type t = TurMachTypes.t
    type tx = TurMachTypes.tx
    val modelDesignation: unit -> string (* a funtion required for module recursive call *)
    class model :
      (t, tx) Arg.alternatives ->
        object

        (* Functions inherited by TuringMachine*)

        method id : Entity.t (* Not used *)
        method errors : string list (* Used in Controller*)
        method handleErrors : unit (* Not used *)
        method toJSon : JSon.t (* Used in Controller*)
        method representation : t (* Used in Controller*)
        method representationx : tx (* Not used *)
        method validate : unit (* Not used *)

        method tracing : unit (* Not used *)

        method accept : word -> bool (* Used in controller RE *)
        method generate : int -> words (* Used in controller RE *)
        method generateUntil : int -> words (* Used in Controller e Undefined*)
        method reachable : state -> states (* Used in here e Undefined*)
        method productive : states (* Used in here*)
        method getUsefulStates : states (* Not Used anywhere*)
        method getUselessStates : states (* Used in Controller*)
        method cleanUselessStates: TuringMachine.model (* Used in here*)
        method areAllStatesUseful: bool (* Used in Controller*)
        method isDeterministic : bool (* Used in Controller*)
        method acceptLB : word -> bool  (* Not Used anywhere*)
        method isLB: bool (* Not Used anywhere*)
        method convertToStopCriteria: model (* Not Used anywhere*)

        method checkProperty : string -> bool (* Not Used anywhere*)
        method checkExercise : Exercise.exercise -> bool (* Used in Controller*)
        method checkExerciseFailures : Exercise.exercise -> words * words * properties (* Used in Controller*)

        method moduleName : string (* Not Used anywhere*)
        method xTypeDeclString : string (* Not used *)
        method xTypeName : string (* Not used *)
        method toDisplayString : string -> string(* Used in Controller and Undefined*)
        method example : JSon.t (* Not used *)
        
        (* Native Functions *) 
        val mutable position : int
        val mutable steps : state Set.t array
        val mutable isOver : bool
 
        method startAccept: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit (* Used in Controller*)
        method next: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit (* Used in Controller*)
        method back: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit (* Used in Controller*)
        method changeSentence: unit -> unit (* Used in here*)
        method inputEdges: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit (* Used in here*)
        method inputNodes : Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit (* Used in here*)
        method paint: Cytoscape.cytoscape Js_of_ocaml.Js.t -> state -> int -> bool -> bool -> unit (* Used in here*)
        method paintStates: Cytoscape.cytoscape Js_of_ocaml.Js.t -> int -> state OCamlFlat.Set.t -> bool -> unit (* Used in here*)
        method accept3: Cytoscape.cytoscape Js_of_ocaml.Js.t -> word -> bool Lwt.t (* O que e este accept? *) (* Not used *)
        method drawExample: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit (* Used in Controller*)

        method addInitialNode: state -> bool -> bool -> TuringMachineGraphics.model (* Used in Controller*)
        method addNode: state -> bool -> TuringMachineGraphics.model (* Used in Controller*)
        method addFinalNode: state -> bool -> bool -> TuringMachineGraphics.model (* Used in Controller*)
        method eliminateNode: state -> bool -> bool -> TuringMachineGraphics.model (* Used in Controller*)
        method changeToFinal: state -> TuringMachineGraphics.model (* Used in Controller*)
        method removeFinal: state -> TuringMachineGraphics.model (* Used in Controller*)

        method newTransition: state * symbol * state -> TuringMachineGraphics.model (* Used in Controller*)
        method newEpsylonTransition: state * symbol * state -> TuringMachineGraphics.model (* Necessario para maquina de turing?*) (* Used in Controller*)
        method eliminateTransition: state * symbol * state -> TuringMachineGraphics.model (* Used in Controller*)
        
        method productivePainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit (* Used in Controller*)
        method reachablePainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit (* Used in Controller*)
        method usefulPainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit (* Used in Controller*)
        
        method cleanUselessStatesCy: Cytoscape.cytoscape Js_of_ocaml.Js.t -> TuringMachineGraphics.model
        
        method stringAsList1: string -> char list(* Not Used*)
        method changeTheTestingSentence: string -> unit (* Used in Controllera *)
        method newSentence1: string (* Used in Controller *)
        method numberStates: int (* Used in Controller *)
        method numberTransitions: int (* Used in Controller*)
        method getColors:int (* Used in Controller*)
        method isOver : bool (* Used in Controller*)
        method renameState: state -> string -> TuringMachineGraphics.model (* Used in Controller*)
  end
end
=
struct
  type transition =
    state	(* state *)
    * symbol	(* consumed input symbol *)
    * state	(* next state *)
    * symbol	(* write input symbol *)
    * direction	(* head direction *)

  type transitions = transition set

  type t = TurMachTypes.t
  type tx = TurMachTypes.tx

  let modelDesignation () = "turing machine"	

  (** Auxiliar Methods **)
  let productiveColor = "orange"
  let reachableColor = "yellow"
  let usefulColor = "purple"
  let stepState = "blue"
  let acceptState = "green"
  let wrongFinalState = "red"

  let sentence: char list ref = ref []
 
  let newSentence = ref ""

  let paintProductive (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) state =
    Cytoscape.paintNode cy state productiveColor
        
  let paintReachable (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) state =
    Cytoscape.paintNode cy state reachableColor
        
  let paintUseful (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t)  state =
    Cytoscape.paintNode cy state usefulColor (*= "orange"*)
  
  let nextStates st sy t =
    let n = Set.filter (fun (a,b,c,_,_) -> st = a && sy = b) t in
      Set.map (fun (_,_,d,_,_) -> d) n

  let getNextSteps sts sy t = 
    Set.flatMap (fun st -> nextStates st sy t) sts (*O que acontece quando se faz flat map de um array em que cada celula contem apenas uma string/simbolo*)

  let direction2string direction = if direction = L then "L" else "R"

  let makeLabel rsymb wsymb direction =
    let symbs = String.concat "/" [(symb2str rsymb); (symb2str wsymb)] in
      String.concat ";" [symbs; (direction2string direction)]

  let transformTransitions trs =
    Set.map (fun (rstate, rsymb, nstate, wsymb, direction) -> (rstate, (makeLabel rsymb wsymb direction), nstate)) trs

  
  class model (arg: (t, tx) Arg.alternatives) =
    object(self) inherit TuringMachine.model arg as super

      val mutable steps = [||]

      val mutable position = 0

      val mutable isOver = false

      method startAccept cy =
        steps <- Array.make 1000 Set.empty;
        position <- 0;	
        isOver <- false;
        Array.set steps position (Set.make [self#representation.initialState]);
        self#paintStates cy ((List.length !sentence) - position) (Array.get steps position) true; (* Nao e necessario verificar se tem estado inicial ou nao?*)
        if (position = (List.length !sentence)) then
          (isOver <- true);
        self#changeSentence ()

      method next cy =
        if isOver then
          (JS.alertStr (Lang.i18nAlertNoMoreStates ()))
        else 
        (position <- position + 1;
        let letter = char2symb (List.nth !sentence (position-1)) in 
        let nextSteps = (getNextSteps (Array.get steps (position-1)) letter self#representation.transitions) in
          steps.(position) <- nextSteps;
          if (Set.size nextSteps) = 0 then
            (self#paintStates cy ((List.length !sentence) - position) (Array.get steps (position-1)) false;
            isOver <- true)
          else
            (self#paintStates cy ((List.length !sentence) - position) (Array.get steps position) true;
            if (position = (List.length !sentence)) then
              (isOver <- true;)
            );
        self#changeSentence ())

      method back cy =
        position <- position - 1;
        isOver <- false;
        if position < 0 then
          (position <- 0; JS.alertStr (Lang.i18nAlertArrivedInitial ()))
        else 
          (* Porque nao ter o alph exists aqui a true visto que para ter chegado a pos + 1, era preciso que o symbolo em pos existisse*)
          (self#paintStates cy ((List.length !sentence) - position) (Array.get steps position) (*true*) (Set.belongs (char2symb (List.nth !sentence (position))) self#representation.alphabet); self#changeSentence())

      (* Faz refresh do estado da palavra durante a simulacao de aceitacao da palavra*)
      method changeSentence () = 
        newSentence := "";
        let bar = '|' in 
          if position == 0 then
            (newSentence:= !newSentence ^ String.make 1 bar;
            for i = 0 to (List.length !sentence) - 1 do 
              newSentence:= !newSentence ^ String.make 1 (List.nth !sentence i);
            done)
          else 
            (for i = 0 to position - 1 do
              newSentence:= !newSentence ^ String.make 1 (List.nth !sentence i);
            done;
            newSentence:= !newSentence ^ String.make 1 bar;
            for i = position to (List.length !sentence) - 1 do 
              newSentence := !newSentence ^ String.make 1 (List.nth !sentence i);
            done)
    
      method inputEdges cy = Set.iter (fun el -> (Cytoscape.addEdgeGeneral cy el) ) (transformTransitions self#representation.transitions)
      
      method inputNodes cy = 
        Set.iter (fun el -> (JS.log (el); Cytoscape.addNode cy el (el = self#representation.initialState) (Set.belongs el self#representation.acceptStates)) ) self#representation.states
    
      method paint cy state length final alphExists = 
        if alphExists then
          (if (length != 0) then 
            Cytoscape.paintNode cy state stepState
          else 
              (if (final) then
                Cytoscape.paintNode cy state acceptState
              else 
                Cytoscape.paintNode cy state wrongFinalState))
        else 
          (Cytoscape.paintNode cy state wrongFinalState;
          JS.alertStr (Lang.i18nAlertNoTransitions ()))
    
        (* Usar o faStyle? *)
      method paintStates cy length states alphExists = 
        Cytoscape.resetStyle cy Cytoscape.faStyle;
        Set.iter (fun el -> self#paint cy el length (Set.belongs el self#representation.acceptStates) alphExists) states

      method drawExample cy = 
        self#inputNodes cy;
        self#inputEdges cy

      method addInitialNode node firstNode exists =
        if firstNode then
          (new TuringMachineGraphics.model (Representation {
            alphabet = Set.empty;
            states = Set.make [node]; 
            initialState = node;
            transitions = Set.empty;
            acceptStates = Set.empty;
            criteria = false
          }))  
        else
          if exists then
            (let rep: t = self#representation in 
            new TuringMachineGraphics.model (Representation{
              alphabet = rep.alphabet;
              states = rep.states;
              initialState = node;
              transitions = rep.transitions;
              acceptStates = rep.acceptStates;
              criteria = rep.criteria
            }))
          else
            (let rep: t = self#representation in 
            new TuringMachineGraphics.model (Representation{
              alphabet = rep.alphabet;
              states = Set.add node rep.states;
              initialState = node;
              transitions = rep.transitions;
              acceptStates = rep.acceptStates;
              criteria = rep.criteria
            }))
      
      method addNode node firstNode =
      if firstNode then
        (new TuringMachineGraphics.model (Representation {
          alphabet = Set.empty;
          states = Set.make [node]; 
          initialState = node;
          transitions = Set.empty;
          acceptStates = Set.empty;
          criteria = false
        }))  
      else
        (let rep: t = self#representation in 
          new TuringMachineGraphics.model (Representation{
            alphabet = rep.alphabet;
            states = Set.add node rep.states;
            initialState = rep.initialState;
            transitions = rep.transitions;
            acceptStates = rep.acceptStates;
            criteria = rep.criteria
          }))

      method addFinalNode node firstNode exists = 
        if firstNode then
          (new TuringMachineGraphics.model (Representation {
          alphabet = Set.empty;
          states = Set.make [node]; 
          initialState = node;
          transitions = Set.empty;
          acceptStates = Set.make [node];
          criteria = true
        })) 
        else 
          if exists then
          (let rep: t = self#representation in 
            new TuringMachineGraphics.model (Representation{
              alphabet = rep.alphabet;
              states = rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.add node rep.acceptStates;
              criteria = rep.criteria
            }))
          else 
            (let rep: t = self#representation in 
              new TuringMachineGraphics.model (Representation{
              alphabet = rep.alphabet;
              states = Set.add node rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.add node rep.acceptStates;
              criteria = rep.criteria
            }))

      method changeToFinal node =
        let rep: t = self#representation in 
              new TuringMachineGraphics.model (Representation{
              alphabet = rep.alphabet;
              states = rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.add node rep.acceptStates;
              criteria = rep.criteria
            })

      method removeFinal node =
        let rep: t = self#representation in 
              new TuringMachineGraphics.model (Representation{
              alphabet = rep.alphabet;
              states = rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.remove node rep.acceptStates;
              criteria = rep.criteria
            })

      method eliminateNode node isStart isFinish = 
        let rep: t = self#representation in 
        if (isStart && isFinish) then
          new TuringMachineGraphics.model (Representation{  
            alphabet = rep.alphabet;
            states = Set.remove node rep.states;
            initialState = "";
            transitions = rep.transitions;
            acceptStates = Set.remove node rep.acceptStates;
            criteria = rep.criteria
            })
        else
          if (isStart) then
            new TuringMachineGraphics.model (Representation{  
              alphabet = rep.alphabet;
              states = Set.remove node rep.states;
              initialState = "";
              transitions = rep.transitions;
              acceptStates = rep.acceptStates;
              criteria = rep.criteria
            })
          else 
            if (isFinish) then
              new TuringMachineGraphics.model (Representation{  
                alphabet = rep.alphabet;
                states = Set.remove node rep.states;
                initialState = rep.initialState;
                transitions = rep.transitions;
                acceptStates = Set.remove node rep.acceptStates;
                criteria = rep.criteria
            })
          else
            new TuringMachineGraphics.model (Representation{  
              alphabet = rep.alphabet;
              states = Set.remove node rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = rep.acceptStates;
              criteria = rep.criteria
            })


      method newTransition (a, b, c, d, e) = 
        let rep: t = self#representation in 
          new TuringMachineGraphics.model (Representation{
              alphabet = Set.add b rep.alphabet;
              states = rep.states;
              initialState = rep.initialState;
              transitions = Set.add (a, b, c, d, e) rep.transitions;
              acceptStates = rep.acceptStates;
              criteria = rep.criteria
        })

      method eliminateTransition (a, b, c, d, e) = 
        let rep: t = self#representation in 
        let transitions = Set.remove (a, b, c, d, e) rep.transitions in
        new TuringMachineGraphics.model (Representation{
          alphabet = rep.alphabet;
          states = rep.states;
          initialState = rep.initialState;
          transitions = transitions;
          acceptStates = rep.acceptStates;
          criteria = rep.criteria
      })

      method numberStates: int =
        Set.size self#representation.states

      method numberTransitions: int =
        Set.size self#representation.transitions
  end
end