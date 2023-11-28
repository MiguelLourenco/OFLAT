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
open FinAutTypes

module rec FiniteAutomatonGraphics : sig
	type transition = state * symbol * state
	type transitions = transition set
	type t = FinAutTypes.t
  type tx = FinAutTypes.tx
	val modelDesignation: unit -> string (* a funtion required for module recursive call *)
	class model :
	  (t, tx) Arg.alternatives ->
		  object

      method id: Entity.t
	    method errors : string list
	    method handleErrors : unit
      method toJSon: JSon.t
      method representation : t
      method representationx : tx
      method validate : unit
      method example : JSon.t

      method tracing : unit

      method xTypeDeclString : string
      method xTypeName : string

      method acceptBreadthFirst: word -> bool
      method accept : word -> bool
      method acceptWithTracing : word -> unit
      method generate : int -> words
      method generateUntil : int -> words
      method reachable : state -> states
      method productive : states
      method getUsefulStates : states
      method getUselessStates : states
      method cleanUselessStates: FiniteAutomaton.model
      method areAllStatesUseful: bool

      method toDeterministic : FiniteAutomaton.model
      method isDeterministic : bool
      method equivalencePartition: states set
      method minimize : FiniteAutomaton.model
      method isMinimized : bool

      method checkProperty : string -> bool
      method checkExercise : Exercise.exercise -> bool
      method checkExerciseFailures : Exercise.exercise -> words * words * properties

      method moduleName : string
      method toDisplayString : string -> string

      method toDeterministic1: FiniteAutomatonGraphics.model
      method cleanUselessStates1: Cytoscape.cytoscape Js_of_ocaml.Js.t -> FiniteAutomatonGraphics.model
      method minimize1 : FiniteAutomatonGraphics.model

      val mutable position : int
      val mutable steps : state Set.t array
      val mutable isOver : bool

      method changeSentence: unit -> unit
      method inputEdges: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method inputNodes : Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method next: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method paint: Cytoscape.cytoscape Js_of_ocaml.Js.t -> state -> int -> bool -> bool -> unit
      method paintStates: Cytoscape.cytoscape Js_of_ocaml.Js.t -> int -> state OCamlFlat.Set.t -> bool -> unit
      method accept3: Cytoscape.cytoscape Js_of_ocaml.Js.t -> word -> bool Lwt.t
      method startAccept: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method back: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method drawExample: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit

      method addInitialNode: state -> bool -> bool -> FiniteAutomatonGraphics.model
      method addNode: state -> bool -> FiniteAutomatonGraphics.model
      method addFinalNode: state -> bool -> bool -> FiniteAutomatonGraphics.model
      method eliminateNode: state -> bool -> bool -> FiniteAutomatonGraphics.model
      method changeToFinal: state -> FiniteAutomatonGraphics.model
      method removeFinal: state -> FiniteAutomatonGraphics.model

      method newTransition: state * symbol * state -> FiniteAutomatonGraphics.model
      method newEpsylonTransition: state * symbol * state -> FiniteAutomatonGraphics.model
      method eliminateTransition: state * symbol * state -> FiniteAutomatonGraphics.model
      
      method productivePainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method reachablePainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      method usefulPainting: Cytoscape.cytoscape Js_of_ocaml.Js.t -> unit
      
      method stringAsList1: string -> char list
      method changeTheTestingSentence: string -> unit
      method newSentence1: string
      method paintMinimization: Cytoscape.cytoscape Js_of_ocaml.Js.t -> string array -> unit
      method numberStates: int
      method numberTransitions: int
      method getColors:int
      method drawMinimize: Cytoscape.cytoscape Js_of_ocaml.Js.t -> string array -> int -> unit
      method isOver : bool
      method renameState: state -> string -> FiniteAutomatonGraphics.model
  end
end
= 
struct
  type transition =
		state	(* state *)
	  * symbol	(* consumed input symbol *)
	  * state	(* next state *)

	type transitions = transition set

	type t = FinAutTypes.t
  type tx = FinAutTypes.tx

	let modelDesignation () = "finite automaton"	

  (** Auxiliar Methods **)
  let productiveColor = "orange"
  let reachableColor = "yellow"
  let usefulColor = "purple"
  let stepState = "blue"
  let acceptState = "green"
  let wrongFinalState = "red"

  let paintProductive (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) state =
        Cytoscape.paintNode cy state productiveColor
        
  let paintReachable (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) state =
        Cytoscape.paintNode cy state reachableColor
        
  let paintUseful (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t)  state =
        Cytoscape.paintNode cy state usefulColor
        
  let paintMinimization (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t)  state color = 
        Cytoscape.paintNode cy state color
  
  let paintMinimized (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t)  state color = 
        Cytoscape.paintNode cy state color

  let getMinStates cy list color = 
    Set.iter (fun el -> paintMinimization cy el color; JS.log (el)) list

  let rec intersection l1 l2 =
     match l1 with
        [] -> []
      | x::xs -> (if List.mem x l2 then [x] else []) @ intersection xs l2

  let iterateList meth (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) list =
    List.iter (fun el -> (meth cy el) ) list

  let cut s = (String.get s 0, String.sub s 1 ((String.length s)-1)) ;;
    
  let rec stringAsList s =
        if s = "" then []
        else
            let (x,xs) = cut s in
                x::stringAsList xs
    ;;

  let sentence: char list ref = ref []

  let newSentence = ref ""

  let delay n =
    Js_of_ocaml_lwt.Lwt_js.sleep (float_of_int n *. 0.01)

  let rec delay1 n = if n = 0 then Lwt.return ()
                                  else Lwt.bind (Lwt.pause()) (fun () -> delay1 (n-1))
  
  let transitionGet3 trns = Set.map ( fun (_,_,c) -> c ) trns
  
  let nextEpsilon2 st ts =
        let trns = Set.filter (fun (a,b,c) -> st = a && b = epsilon) ts in
        let nextStates = transitionGet3 trns in	
			Set.add st nextStates 

  let rec closeEmpty sts t = 
		let ns = Set.flatMap (fun st -> nextEpsilon2 st t) sts in
			if (Set.subset ns sts) then ns else closeEmpty (Set.union sts ns) t 

  let nextEpsilon1 st t =
          let n = Set.filter (fun (a,b,c) -> st = a && b = epsilon) t in
                  Set.map ( fun (_,_,d) -> d ) n		
				
	let rec nextEpsilons currsts t = 
		let ns = Set.flatMap (fun nst -> nextEpsilon1 nst t) currsts in
			if (Set.subset ns currsts) then ns else nextEpsilons (Set.union currsts ns) t
			
  let nextStates st sy t =
    let n = Set.filter (fun (a,b,c) -> st = a && sy = b) t in
            Set.map ( fun (_,_,d) -> d) n

  let transition sts sy t = 
        let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
          Set.union nsts (nextEpsilons nsts t)

  class model (arg: (t, tx) Arg.alternatives) =
		object(self) inherit FiniteAutomaton.model arg as super

      val mutable steps = [||]

      val mutable position = 0

      val mutable isOver = false

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

      method inputNodes cy = 
        Set.iter (fun el -> (JS.log (el); Cytoscape.addNode cy el (el = self#representation.initialState) (Set.belongs el self#representation.acceptStates)) ) self#representation.states

      method private inputNodesPainting cy2 colors number = 
        let listStates = Set.toList self#representation.states in 
        for i=0 to number-1 do
          let newState = List.nth listStates i in 
          Cytoscape.addNode cy2 newState (newState = self#representation.initialState) (Set.belongs newState self#representation.acceptStates);
          let color = Array.get colors i in
          paintMinimized cy2 newState color
        done
      

      method inputEdges cy = Set.iter (fun el -> (Cytoscape.addEdge cy el) ) self#representation.transitions

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
            

      method paintStates cy length states alphExists = 
              Cytoscape.resetStyle cy Cytoscape.faStyle;
              Set.iter (fun el -> self#paint cy el length (Set.belongs el self#representation.acceptStates) alphExists) states

      method accept3 (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) (w: word) =
      let transition sts sy t = 
        let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
                  Set.union nsts (nextEpsilons nsts t) in
                
      let rec accept2X sts w t exists =
        match w with
          [] -> Lwt.bind (delay 40) (fun () -> Lwt.bind (Lwt.return (self#paintStates cy (List.length w) sts exists)) (fun () -> Lwt.return ((Set.inter sts self#representation.acceptStates) <> Set.empty)))
          |x::xs -> Lwt.bind (delay 40) (fun () -> Lwt.bind (Lwt.return (self#paintStates cy (List.length w) sts exists)) 
            (fun () -> let nextTrans = transition sts x t in
                          if (Set.size nextTrans) = 0 then
                            accept2X sts [] t false
                          else 
                            accept2X (transition sts x t) xs t true
            )) in
        
      let i = closeEmpty (Set.make [self#representation.initialState]) self#representation.transitions in
              accept2X i w self#representation.transitions true

      method startAccept cy =
        steps <- Array.make 1000 Set.empty;
        position <- 0;	
        isOver <- false;
        let i = closeEmpty (Set.make [self#representation.initialState]) self#representation.transitions in
          Array.set steps position i;
        self#paintStates cy ((List.length !sentence) - position) (Array.get steps position) true; 
        if (position = (List.length !sentence)) then
          (isOver <- true);
        self#changeSentence ()
                  
      method next cy =
        if isOver then
          (JS.alertStr (Lang.i18nAlertNoMoreStates ()))
        else 
        (position <- position + 1;
        let letter = char2symb (List.nth !sentence (position-1)) in 
        let nextSteps = (transition (Array.get steps (position-1)) letter self#representation.transitions) in
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
          (self#paintStates cy ((List.length !sentence) - position) (Array.get steps position) (Set.belongs (char2symb (List.nth !sentence (position))) self#representation.alphabet); self#changeSentence())

      method drawExample cy = 
        self#inputNodes cy;
        self#inputEdges cy
      
      method drawMinimize cy2 colors number =
        self#inputNodesPainting cy2 colors number;
        self#inputEdges cy2

      method addInitialNode node firstNode exists =
        if firstNode then
          (new FiniteAutomatonGraphics.model (Representation {
            alphabet = Set.empty;
	          states = Set.make [node]; 
            initialState = node;
            transitions = Set.empty;
            acceptStates = Set.empty
          }))  
        else
          if exists then
            (let rep: t = self#representation in 
            new FiniteAutomatonGraphics.model (Representation{
              alphabet = rep.alphabet;
	            states = rep.states;
              initialState = node;
              transitions = rep.transitions;
              acceptStates = rep.acceptStates
            }))
          else
            (let rep: t = self#representation in 
            new FiniteAutomatonGraphics.model (Representation{
              alphabet = rep.alphabet;
	            states = Set.add node rep.states;
              initialState = node;
              transitions = rep.transitions;
              acceptStates = rep.acceptStates
            }))
      
      method addNode node firstNode =
      if firstNode then
        (new FiniteAutomatonGraphics.model (Representation {
          alphabet = Set.empty;
	        states = Set.make [node]; 
          initialState = node;
          transitions = Set.empty;
          acceptStates = Set.empty
        }))  
      else
        (let rep: t = self#representation in 
          new FiniteAutomatonGraphics.model (Representation{
            alphabet = rep.alphabet;
	          states = Set.add node rep.states;
            initialState = rep.initialState;
            transitions = rep.transitions;
            acceptStates = rep.acceptStates
          }))

      method addFinalNode node firstNode exists = 
        if firstNode then
          (new FiniteAutomatonGraphics.model (Representation {
          alphabet = Set.empty;
	        states = Set.make [node]; 
          initialState = node;
          transitions = Set.empty;
          acceptStates = Set.make [node]
        })) 
        else 
          if exists then
          (let rep: t = self#representation in 
            new FiniteAutomatonGraphics.model (Representation{
              alphabet = rep.alphabet;
	            states = rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.add node rep.acceptStates
            }))
          else 
            (let rep: t = self#representation in 
              new FiniteAutomatonGraphics.model (Representation{
              alphabet = rep.alphabet;
	            states = Set.add node rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.add node rep.acceptStates
            }))

      method changeToFinal node =
        let rep: t = self#representation in 
              new FiniteAutomatonGraphics.model (Representation{
              alphabet = rep.alphabet;
	            states = rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.add node rep.acceptStates
            })

        method removeFinal node =
        let rep: t = self#representation in 
              new FiniteAutomatonGraphics.model (Representation{
              alphabet = rep.alphabet;
	            states = rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = Set.remove node rep.acceptStates
            })

      method eliminateNode node isStart isFinish = 
        let rep: t = self#representation in 
        if (isStart && isFinish) then
          new FiniteAutomatonGraphics.model (Representation{  
            alphabet = rep.alphabet;
	          states = Set.remove node rep.states;
            initialState = "";
            transitions = rep.transitions;
            acceptStates = Set.remove node rep.acceptStates
            })
        else
          if (isStart) then
            new FiniteAutomatonGraphics.model (Representation{  
              alphabet = rep.alphabet;
	            states = Set.remove node rep.states;
              initialState = "";
              transitions = rep.transitions;
              acceptStates = rep.acceptStates
            })
          else 
            if (isFinish) then
              new FiniteAutomatonGraphics.model (Representation{  
                alphabet = rep.alphabet;
	              states = Set.remove node rep.states;
                initialState = rep.initialState;
                transitions = rep.transitions;
                acceptStates = Set.remove node rep.acceptStates
            })
          else
            new FiniteAutomatonGraphics.model (Representation{  
              alphabet = rep.alphabet;
	            states = Set.remove node rep.states;
              initialState = rep.initialState;
              transitions = rep.transitions;
              acceptStates = rep.acceptStates
            })


      method newTransition (a, b, c) = 
      let rep: t = self#representation in 
        new FiniteAutomatonGraphics.model (Representation{
            alphabet = Set.add b rep.alphabet;
	          states = rep.states;
            initialState = rep.initialState;
            transitions = Set.add (a, b , c) rep.transitions;
            acceptStates = rep.acceptStates
      })

      method newEpsylonTransition (a, b, c) = 
      let rep: t = self#representation in 
        new FiniteAutomatonGraphics.model (Representation{
            alphabet = rep.alphabet;
	          states = rep.states;
            initialState = rep.initialState;
            transitions = Set.add (a, b , c) rep.transitions;
            acceptStates = rep.acceptStates
      })

      method eliminateTransition (a, b, c) = 
        let rep: t = self#representation in 
        let transitions = Set.remove (a, b, c) rep.transitions in
        new FiniteAutomatonGraphics.model (Representation{
          alphabet = rep.alphabet;
          states = rep.states;
          initialState = rep.initialState;
          transitions = transitions;
          acceptStates = rep.acceptStates
      })

      method getColors:int =
        Set.size self#equivalencePartition

      method paintMinimization cy colors: unit = 
          let number = self#getColors in
          let listEquivalence = Set.toList self#equivalencePartition in
          for i=0 to number-1 do 
            getMinStates cy (List.nth listEquivalence i) (Array.get colors i)
          done

      method productivePainting (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t)  =
        let list1 = Set.toList self#productive in
        iterateList paintProductive cy list1

      method reachablePainting cy =
        let list1 = Set.toList (self#reachable (self#representation.initialState)) in
        JS.log (List.length list1);
        iterateList paintReachable cy list1 

      method usefulPainting cy =
        let intre = intersection (Set.toList self#productive) (Set.toList (self#reachable (self#representation.initialState))) in
        iterateList paintUseful cy intre 

      method stringAsList1 s = stringAsList s

      method changeTheTestingSentence word =
        sentence := stringAsList word

      method newSentence1 = !newSentence

      method minimize1: FiniteAutomatonGraphics.model = 			
        let min = super#minimize in
          let rep = min#representation in 
            new FiniteAutomatonGraphics.model (Representation rep) 
 
    method toDeterministic1: FiniteAutomatonGraphics.model = 
      let deter = super#toDeterministic in
        let rep = deter#representation in 
        new FiniteAutomatonGraphics.model (Representation rep) 


      method cleanUselessStates1 (cy:Cytoscape.cytoscape Js_of_ocaml.Js.t) : FiniteAutomatonGraphics.model =
        Cytoscape.resetStyle cy Cytoscape.faStyle;
        let uss = self#getUselessStates in
          Set.iter (fun el -> paintUseful cy el) uss;
        let clean = super#cleanUselessStates in
        let rep = clean#representation in 
        new FiniteAutomatonGraphics.model (Representation rep) 

      method numberStates: int =
        Set.size self#representation.states

      method numberTransitions: int =
        Set.size self#representation.transitions
        
      method renameState state name =
        let rep: t = self#representation in 
        let initial = if state = rep.initialState then name else rep.initialState in
        let newStates = Set.remove state (Set.add name rep.states) in
        let newTransitions = Set.map (fun (s,n,t) -> 
          if s = state && t = state then (name,n,name) else
          if s = state then (name,n,t) else
          if t = state then (s,n,name) else (s,n,t)
        ) rep.transitions in
        let newAcceptStates = Set.map (fun s -> if s = state then name else s) rep.acceptStates in
        new FiniteAutomatonGraphics.model (Representation{
          alphabet = rep.alphabet;
          states = newStates;
          initialState = initial;
          transitions = newTransitions;
          acceptStates = newAcceptStates
      })
      
      method isOver = isOver
      
(*      method renameTransition transition name =*)
end
end
