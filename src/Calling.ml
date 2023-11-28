(*
 * Calling.ml
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

(* Description: This a component of the controller that is used for the
 * JavaScript to call OCaml functions in the Controller.
 *)

open OCamlFlat
open BasicTypes
open Js_of_ocaml
open Graphics
open Controller
open HtmlPageClient
open Lang
open Listeners
open RegularExpressionGraphics
open JS
open FiniteAutomatonGraphics
open ContextFreeGrammarGraphics
open Lang

let explode s =
  let rec exp i l =
    if i < 0 then l 
    else if s.[i] == ',' || s.[i] == ' ' then (exp (i - 1) (l))
    else (exp (i - 1) (s.[i] :: l)) in
  exp (String.length s - 1) []   

module JSCallingOCaml =
      struct
        Js.Unsafe.global##.jscode :=
            object%js
            
              method newModel =
                !Listeners.createModelListener()
              
              method editModel =
                !Listeners.editModelListener()
                
              method getModel =
                let automaton = !ctrlL#getModel in
                Graphics.bcSend automaton
    
              method fitGraph =
                Cytoscape.fit !ctrlL#getCy_opt
              
              method generateWords =
                let text = JS.prompt (i18nTextMaximumSize ()) "4" in 
                twoBoxes !ctrlL#getCy_opt;
                !changeToControllerCtrlRight();
                match Js.Opt.to_option text with
                | None -> ()
                | Some v -> 
                    let size = int_of_string (Js.to_string v) in 
                      !ctrlL#getWords size
              
              method test =
                oneBox !ctrlL#getCy_opt;
                let text = JS.prompt (Lang.i18nPromptTextTestWord ()) "ab" in 
                match Js.Opt.to_option text with
                | None -> Lwt.return_false
                | Some v -> !ctrlL#accept (Js.to_string v)
                
              method stepbystep =
                let text = JS.prompt (Lang.i18nPromptTextTestWord ()) "ab" in 
                match Js.Opt.to_option text with
                | None -> ()
                | Some v -> !ctrlL#startStep (Js.to_string v)

              method backwards =
                !ctrlL#backStep
              
              method forward =
                !ctrlL#nextStep

              method selectConversions n =
                Controller.conversionTo n

              method readFromFile n =
                let str = Js.to_string n in 
                !Listeners.openEntityListener str

              method exportToFile =
                let modelJson = JSon.toString ((!ctrlL#model)#toJSon) in
                let json = Js.to_string ((Js.encodeURIComponent (Js.string modelJson))) in
                let element = Dom_html.document##createElement (Js.string "a") in
                let modelName = 
                  begin match !ctrlL#model#id.name with
                  | "_" | "" -> "oflatModel"
                  | a -> a
                  end ^ ".json"
                in
                element##setAttribute (Js.string "href") (Js.string ("data:application/json," ^ json));
                element##setAttribute (Js.string "download") (Js.string modelName);
                element##.style##.display := Js.string "none";
                let node = Dom_html.document##.body##appendChild (Js.Unsafe.coerce element) in
                element##click;
                Dom_html.document##.body##removeChild node

              method feedback =
                HtmlPageClient.clearBox1 ();
                !changeToControllerCtrlLeft();
                !ctrlL#feedback
              
              method about  =
                HtmlPageClient.clearBox1 ();
                !changeToControllerCtrlLeft();
                !ctrlL#about

              method selectLang n =
                if n = 1 then
                  (Lang.set_language (Js.string "pt");
                  HtmlPageClient.changeLang())
                else 
                  (if n = 2 then
                    (Lang.set_language (Js.string "en");
                    HtmlPageClient.changeLang()) else
                    if n = 3 then 
                    (Lang.set_language (Js.string "fr");
                    HtmlPageClient.changeLang()))


              method tooltipNewModel =
                let textBox = Dom_html.getElementById "tooltipNewModel" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipNewModel ())

              method tooltipEditModel =
                let textBox = Dom_html.getElementById "tooltipEditModel" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipEditModel ())
              
              method tooltipFitGraph =
                let textBox = Dom_html.getElementById "tooltipFitGraph" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipFitGraph ())

              method tooltipGenerate =
                let textBox = Dom_html.getElementById "tooltipGenerate" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipGenerate ())
              
              method tooltipTest =
                let textBox = Dom_html.getElementById "tooltipTest" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipTest ())
              
              method tooltipStep =
                let textBox = Dom_html.getElementById "tooltipStep" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipStep ())

              method tooltipClear =
                let textBox = Dom_html.getElementById "tooltipClear" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipClear ())

              method tooltipConvert = 
                let textBox = Dom_html.getElementById "tooltipConvert" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipConvert ())

              method tooltipFile = 
                let textBox = Dom_html.getElementById "tooltipFile" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipFile ())
                
              method tooltipExportModel =
                let textBox = Dom_html.getElementById "tooltipExportModel" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipExportModel ())

              method tooltipAbout =
                let textBox = Dom_html.getElementById "tooltipAbout" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipAbout ())

              method tooltipFeedback =
                let textBox = Dom_html.getElementById "tooltipFeedback" in
                textBox##.innerHTML := Js.string (Lang.i18nTooltipFeedback ())

              method tooltipLang =
              let textBox = Dom_html.getElementById "tooltipLang" in
              textBox##.innerHTML := Js.string (Lang.i18nTooltipLang ())

              method start =
              if (!Lang.lang <> "en" && !Lang.lang <> "pt" && !Lang.lang <> "fr") then
                Lang.lang := "en";
              HtmlPageClient.changeLang();
              let examples = Dom_html.getElementById "examplesServer" in
              examples##.innerHTML := Js.string "";
                let lis = Examples.examples in
                List.iter (fun el -> HtmlPageClient.putButton el) lis
    
            end
      end
