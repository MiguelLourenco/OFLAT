(*
 * RegularExpressionGraphics.ml
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
open RegExpTypes
open Js_of_ocaml
open Lang
open JS

  module rec RegularExpressionGraphics : sig
    type t = RegExpTypes.t
    type tx = string
    
    type reTree =
        Fail 
        | Tree of word * RegExpTypes.t * reTree list
    
      val modelDesignation: unit -> string
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
          method example : JSon.t

          method xTypeDeclString : string
          method xTypeName : string

          method accept : word -> bool
          method allTrees : word -> unit
          method generate : int -> words
          method tracing : unit

          method alphabet : symbols
          method quasiLanguage : words

          method simplify : RegularExpression.model

          method checkProperty : string -> bool
          method checkExercise : Exercise.exercise -> bool
          method checkExerciseFailures : Exercise.exercise -> words * words * properties
                        
          method moduleName : string
          method toDisplayString : string -> string

    
          method stringAsList1: string -> char list
          method accept1: word -> reTree
    
          method countTree: reTree -> int
    
          method printTree: reTree -> (Cytoscape.cytoscape Js_of_ocaml.Js.t) -> string
    
          method allTrees : word -> unit
    
          method getTrees: word -> reTree list
    
          method startAllTrees: word -> unit
    
          method back: RegularExpressionGraphics.reTree
          method next: RegularExpressionGraphics.reTree
    
          method length: int 
    
          method countRightTrees: word -> int * int
    
          method position: int 
    
          method getRightTrees: reTree
    
          method getWrongTrees: reTree
    
          end
    end
     =
    struct
      type t = RegExpTypes.t
      type tx = string
    
      type reTree =
        Fail 
        | Tree of word * RegExpTypes.t * reTree list
    
      let modelDesignation () = "regular expression"
    
      let cut s = (String.get s 0, String.sub s 1 ((String.length s)-1)) ;;
    
      let rec stringAsList s =
            if s = "" then []
            else
                let (x,xs) = cut s in
                    x::stringAsList xs
        ;;
        let nGetName = ref 0

        let genName () = 
          let prefix = "N" in 
            nGetName := !nGetName + 1;
            prefix ^ (string_of_int !nGetName)
    
      let rec isNotFail t = 
            match t with
              Fail -> false
              | Tree ([], re, []) -> true
              | Tree (w, re, []) -> true
              | Tree ([], re, x::xs) -> (isNotFail x) && (isNotFail (Tree ([], re, xs)))
              | Tree (w, re, x::xs) -> (isNotFail x) && (isNotFail (Tree (w, re, xs)))
    
      class model (arg: (t, tx) Arg.alternatives) =
        object(self) inherit RegularExpression.model arg as super
    
          val mutable allTrees = [Fail]
          val mutable rightTrees = [Fail]
    
          val mutable typeOfTree = "wrong"
    
          val mutable position = 0
    
          method stringAsList1 = stringAsList
    
          method countTree t = 
            match t with
              Fail -> 0
              | Tree (w, re, []) -> 1
              | Tree (w, re, x::xs) -> (self#countTree x) + (self#countTree (Tree (w, re, xs)))
    
          method countRightTrees (w: word): int * int  = 
            let partition w = 
              let rec partX w pword =
                match w with 
                  [] -> Set.empty
                  | x::xs -> let fwp = pword@[x] in
                          Set.add (fwp, xs) (partX xs fwp) in
                 Set.add ([],w) (partX w []) in		
            
            let rec acc w rep = 				
              match rep with 
                | Plus(l, r) -> 
                    let (l1, l2) = acc w l in
                    let (r1, r2) = acc w r in 
                      (l1 + r1, l2 + r2)
                | Seq(l, r) -> let wps =  partition w in
                          let wpl = Set.toList wps in 
                            List.fold_left (fun (a, b) (c,d) -> (a+c, b+d)) (0,0) (List.map (fun (wp1,wp2) -> 
                            let (sl, fl) = acc wp1 l in
                            let (sr, fr) = acc wp2 r in
                              (sl * sr, fl * fr + fl * sr + fr * sl)
                            ) wpl)							
                | Star(re) -> if w = [] then 
                      (1,0)
                    else 						 
                        (let wps = Set.remove ([],w) (partition w) in
                          let wpl = Set.toList wps in 
                           List.fold_left (fun (a, b) (c,d) -> (a+c, b+d)) (0,0) (List.map (fun (wp1,wp2) -> 
                            let (sl, fl) = acc wp1 re in
                            let (sr, fr) = acc wp2 (Star re) in
                              (sl * sr, fl * fr + fl * sr + fr * sl)
                            ) wpl))							
                | Symb(c) -> 
                    if w = [c] then 
                      (1,0)
                    else 
                      (0,1)
                | Empty -> 
                    if w = [] then
                      (1,0) 
                    else
                     (0,1)
                | Zero -> (0,1)
            in	
              
              acc w self#representation
    
          method printTree t (cy: Cytoscape.cytoscape Js_of_ocaml.Js.t) =
            match t with 
              Fail -> let rootName = genName () in 
                      Cytoscape.makeTreeNode cy rootName "Fail";
                      rootName
              | Tree ([], re, []) -> ""
              | Tree ([], re, x::xs) -> let l = (self#printTree x cy) in
                                        ignore ((self#printTree (Tree ([], re, xs)) cy));
                                        l
              | Tree (w, re, []) -> let rootName = genName () in 
                                    let text = String.concat "" (List.map symb2str w) in
                                    let regex = RegExpSyntax.toString re in
                                    let text1 = text ^ "<=>" ^ regex in
                                    Cytoscape.makeTreeNode cy rootName text1;
                                    rootName
              | Tree (w, re, x::xs) -> let rootName = genName () in 
                                       let regex = RegExpSyntax.toString re in
                                       let text = String.concat "" (List.map symb2str w) in
                                       let text1 = text ^ "<=>" ^ regex in
                                       let l = (self#printTree x cy) in 
                                       let l1 = (self#printTree (Tree ([], re, xs)) cy) in 
                                       Cytoscape.makeTreeNode cy rootName text1;
                                       Cytoscape.makeTreeEdge cy rootName l;
                                       if (l1 <> "") then
                                          Cytoscape.makeTreeEdge cy rootName l1;
                                       rootName
    
          method accept1 (w: word): reTree  = 
            let partition w = 
              let rec partX w pword =
                match w with 
                  [] -> Set.empty
                  | x::xs -> let fwp = pword@[x] in
                          Set.add (fwp, xs) (partX xs fwp) in
                 Set.add ([],w) (partX w []) in		
    
            let rec reFind f l =
              match l with 
                [] -> Fail
                | x::xs -> let res = f x in 
                              if res = Fail then
                                reFind f xs
                              else res	in		
            
            let rec acc w rep = 				
              match rep with 
                | Plus(l, r) -> 
                    let l1 = acc w l in
                    let r1 = acc w r in 
                    if l1 = Fail then
                      if r1 = Fail then 
                        Fail
                      else 
                        Tree (w, rep, [r1])
                    else 
                      Tree (w, rep, [l1])
                | Seq(l, r) -> let wps =  partition w in
                          let wpl = Set.toList wps in 
                          reFind (fun (wp1,wp2) -> 
                            let tl = acc wp1 l in
                            let tr = acc wp2 r in
                            if tl = Fail || tr = Fail then 
                              Fail
                            else 
                              Tree (w, rep, [tl; tr])
                            ) wpl							
                | Star(re) -> if w = [] then 
                      Tree (w, rep, []) 
                    else 						 
                        (let wps = Set.remove ([],w) (partition w) in
                          let wpl = Set.toList wps in 
                          reFind (fun (wp1,wp2) -> 
                            let tl = acc wp1 re in
                            let tr = acc wp2 (Star re) in
                            if tl = Fail || tr = Fail then 
                              Fail
                            else 
                              Tree (w, rep, [tl; tr])
                            ) wpl)
                | Symb(c) -> 
                    if w = [c] then 
                      Tree (w, rep, []) 
                    else 
                      Fail
                | Empty -> 
                    if w = [] then
                      Tree (w, rep, []) 
                    else
                     Fail
                | Zero -> Fail
            in	
              
              acc w self#representation     
    
          method getTrees w = 
          let partition w = 
              let rec partX w pword =
                match w with 
                  [] -> Set.empty
                  | x::xs -> let fwp = pword@[x] in
                          Set.add (fwp, xs) (partX xs fwp) in
                 Set.add ([],w) (partX w []) in		
            
            let rec acc w rep = 				
              match rep with 
                | Plus(l, r) -> 
                    let l1 = acc w l in
                    let r1 = acc w r in 
                      List.map (fun t -> Tree (w, rep, [t])) (l1 @ r1)
                | Seq(l, r) -> let wps =  partition w in
                          let wpl = Set.toList wps in 
                          List.flatten (List.map (fun (wp1, wp2) ->  
                            let tl = acc wp1 l in
                            let tr = acc wp2 r in
                            List.flatten (List.map (fun x -> List.map (fun y -> Tree (w, rep, [x; y])) tr) tl)    
                          ) wpl)
                | Star(re) -> if w = [] then 
                      [Tree ([epsilon], rep, [])] 
                    else 						 
                        (let wps = Set.remove ([],w) (partition w) in
                          let wpl = Set.toList wps in 
                            List.flatten (List.map (fun (wp1, wp2) ->  
                            let tl = acc wp1 re in
                            let tr = acc wp2 (Star re) in
                            List.flatten (List.map (fun x -> List.map (fun y -> Tree (w, rep, [x; y])) tr) tl)    
                          ) wpl))
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
              
              acc w self#representation
    
          method startAllTrees w =
            allTrees <- self#getTrees w;
            position <- 0
            
          method next = 
            position <- position + 1;
            if position > (List.length allTrees) -1 then
              position <- 0;
            List.nth allTrees position
    
          method back = 
            position <- position - 1;
            if position < 0 then
              position <- (List.length allTrees) - 1;
            List.nth allTrees position
    
          method length =
            List.length allTrees
    
          method position =
            position +1 
    
          method getRightTrees =
            let rightTrees = List.filter (fun x -> isNotFail x) allTrees in 
            allTrees <- rightTrees;
            List.nth allTrees position
    
          method getWrongTrees =
            List.nth allTrees position
          
        end
    
    end
