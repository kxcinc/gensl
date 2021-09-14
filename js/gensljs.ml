open Js_of_ocaml

[@@@ocaml.warning "-33"]

open Genslib
open Gensl
open Parsing

open Basetypes
open Parsetree

open ParserTypes

open Readermacro

(* roadmap to MVP
 - [ ] make index.html accessible from the Internet via GitHub Pages

 - [x] displayed error message (in red)
 - [x] modify unparse for CommaSeparator and remove the prefix space
 - [x] repl result as if #conv infix then #unparse
 - [x] change color of prompt
 - [x] use monospace font
 - [ ] make input area larger and support multi-line input
 *)

let unparse_for_repl expr =
  Format.asprintf "%a" Unparse.(unparse_pdatum ~fxnconv:`Infix) expr


let trylex str =
  let open Sedlexing in
  let open Format in
  printf "we got %s\n" str;
  let lexbuf = Utf8.from_string str in
  let result = Genslex.token lexbuf in
  (match result with
   | TkNumeric(a,b) -> printf "%s.%s\n" a b
   | _ -> printf "idk\n");
  print_flush()

let tryparse use_unparse str =
  let open Sedlexing in
  let open Format in
  let module P = Parser.Default (Extensions) in
  let lexbuf = Utf8.from_string str in
  (P.read_top (pstate lexbuf), use_unparse) |> function
  | Ok (toplevel, _), true ->  Ok (unparse_for_repl toplevel)
  | Ok (toplevel, _), false ->  Ok (asprintf "%a" ParsetreePrinter.pp_toplevel toplevel)
  | Error errors, _ -> Error (asprintf "%a" ParseError.pp (Parse_errors errors))

let tryparse_console str =
  match tryparse true str with
  | Ok s -> s
  | Error s -> s

let () =
  let api = (object%js
               method trylex str = str |> Js.to_string |> trylex
               method tryparse b str = str |> Js.to_string |> tryparse b
               method lex str =
                 let str = str |> Js.to_string in
                 let open Sedlexing in
                 let lexbuf = Utf8.from_string str in
                 Genslex.token lexbuf
             end) in
  Js.export "Gensl" api

let debug x =
  let msg = Json.output x
  in Js.Unsafe.(fun_call (global##.console##.log) [|msg|>coerce|]) |> ignore

let debug_string msg =
  Js.Unsafe.(fun_call (global##.console##.log) [|msg|>Js.string|>coerce|]) |> ignore

let () =
  if Array.length Sys.argv > 1
  then tryparse true Sys.argv.(1) |> debug
  else ()

let createPrompt doc =
  let textnode = doc##createTextNode (Js.string "gensl> ") in
  let span = Dom_html.createSpan doc in
  span##.style##.color := Js.string "blue";
  span##.style##.verticalAlign := Js.string "top";
  Dom.appendChild span textnode;
  span

let add_input doc text history =
  let pre = Dom_html.createPre doc in
  let prompt = createPrompt doc in
  let textnode = doc##createTextNode text in
  Dom.appendChild pre prompt;
  Dom.appendChild pre textnode;
  Dom.appendChild history pre

let add_output doc text history =
  let p = Dom_html.createP doc in
  let textnode = doc##createTextNode text in
  Dom.appendChild p textnode;
  Dom.appendChild history p

let add_error doc text history =
  let p = Dom_html.createP doc in
  let textnode = doc##createTextNode text in
  p##.style##.color := Js.string "red";
  Dom.appendChild p textnode;
  Dom.appendChild history p

let () =
  let app_main = Dom_html.getElementById "app-main" in
  let unparse_mode = ref true in

  let repl_history = Dom_html.createDiv Dom_html.document in
  repl_history##.id := Js.string "repl-history";
  Dom.appendChild app_main repl_history;

  let repl_input = Dom_html.createP Dom_html.document in
  repl_input##.id := Js.string "repl-input";
  Dom.appendChild app_main repl_input;

  let keypress = Dom.Event.make "keypress" in

  let prompt = createPrompt Dom_html.document in
  Dom.appendChild repl_input prompt;

  let textarea = Dom_html.createTextarea Dom_html.document in
  textarea##.cols := 40;
  textarea##.rows := 1;
  Dom.appendChild repl_input textarea;

  let _ =
    Dom.addEventListener
      textarea keypress
      (Dom.handler (fun e ->
           if e##.key == Js.string "Enter" && e##.ctrlKey then begin
             let text = textarea##.value in
             textarea##.value := Js.string "";
             add_input Dom_html.document text repl_history;
             (match Js.to_string text with
              | "#unparse on" -> unparse_mode := true
              | "#unparse off" -> unparse_mode := false
              | text ->
                 (match tryparse !unparse_mode text with
                   | Ok output ->
                      add_output Dom_html.document (Js.string output) repl_history
                   | Error error ->
                      add_error Dom_html.document (Js.string error) repl_history));
             Js._false
           end else
             Js._true))
      Js._true in

  ()
