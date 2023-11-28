(*
 * Listeners.ml
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
 * Description: Part of the controller that is acessible from the
 * Visualizer (HtmlPageClient). Using this module, we avoid the need to use
 * mutually recursive modules.
 *)
open OCamlFlat.BasicTypes

  module Listeners = 
    struct
      let closeLeftListener = ref (fun () -> ())
      let closeRightListener = ref (fun () -> ())
      let openEntityListener = ref (fun (txt : string) -> ())
      let defineInformationBoxListener = ref (fun () -> ())
      let showModelListener = ref (fun () -> ())
      let createModelListener = ref (fun () -> ())
      let editModelListener = ref (fun () -> ())
      let updateRightListener = ref (fun () -> ())
    end
    (* Retirar do controller estas funcoes definidas em fa e tm*)
  module ListenersAutomata = 
    struct
      let addNode = ref (fun (x : int) (y : int) -> ())
      let removeNode = ref (fun (node : state) -> ())
      let turnFinal = ref (fun (node : state) -> ())
      let removeTypeFinal = ref (fun (node : state) -> ())
      let turnNodeInitial = ref (fun (node : state) -> ())
      let addInitialNode = ref (fun (x : int) (y : int) -> ())
      let addFinalNode = ref (fun (x : int) (y : int) -> ())
      let addTransition = ref (fun (src : state) (trg : state) -> ())
      let removeTransition = ref (fun (srcId : state) (trgId : state) (symb : string) -> ())
      let renameNodeListener = ref (fun (state : state) -> ())
    end

  module ListenersFA = 
    struct
      include Listeners
      include ListenersAutomata
      let createModelListener = ref (fun () -> ())
      let changeDirectionListener = ref (fun () -> ())
      let paintAllProductivesListener = ref (fun () -> ())
      let paintAllReachableListener = ref (fun () -> ())
      let paintAllUsefulListener = ref (fun () -> ())
      let cleanUselessListener = ref (fun () -> ())
      let getDeterministicListener = ref (fun () -> ())
      let defineMinimizedListener = ref (fun () -> ())
      let clearAutoListener = ref (fun () -> ())
    end

  module ListenersTM = 
    struct
      include Listeners
      include ListenersAutomata
      let createModelListener = ref (fun () -> ())
      (*let changeDirectionListener = ref (fun () -> ())*)
      let paintAllProductivesListener = ref (fun () -> ())
      let paintAllReachableListener = ref (fun () -> ())
      let paintAllUsefulListener = ref (fun () -> ())
      let cleanUselessListener = ref (fun () -> ())
      let getDeterministicListener = ref (fun () -> ())
      let clearAutoListener = ref (fun () -> ())
    end

  module ListenersRE =
    struct
      include Listeners
      include ListenersAutomata
      let createModelListener = ref (fun () -> ())
      (*let editModelListener = ref (fun () -> ())*)
      let resultCountListener = ref (fun () -> ())
      let previousTreeListener = ref (fun () -> ())
      let nextTreeListener = ref (fun () -> ())
      let defineNumberTreesListener = ref (fun () -> ())
      let changeDirectionListener = ref (fun () -> ())
  end
  
  module ListenersCFG =
    struct
      include Listeners
      include ListenersAutomata
      let createModelListener = ref (fun () -> ())
      (*let editModelListener = ref (fun () -> ())*)
      let cleanCFGListener = ref (fun () -> ())
      let removeLeftRecursionListener = ref (fun () -> ())
      let leftFactoringListener = ref (fun () -> ())
      let tablesListener = ref (fun () -> ())
      let parsingTableListener = ref (fun () -> ())
      let generateCFGListener = ref (fun () -> ())
      let recursiveDescedentParserListener = ref (fun () -> ())
      let simpleToggleListener = ref (fun () -> ())
      let removeEpsilonListener = ref (fun () -> ())
      let removeUnitListener = ref (fun () -> ())
      let previousNewCFGListener = ref (fun () -> ())
      let nextNewCFGListener = ref (fun () -> ())
      let transformLL1Listener = ref (fun () -> ())
  end
  
  module ListenersEXER = 
    struct
      include Listeners
      let checkExerciseListener = ref (fun () -> ())
      let clearExerciseListener = ref (fun () -> ())
  end
