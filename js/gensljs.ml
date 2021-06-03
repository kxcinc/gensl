open Js_of_ocaml

[@@@ocaml.warning "-33"]

open Genslib
open Gensl
open Parsing

open Basetypes
open Parsetree

open ParserTypes

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

let tryparse str =
  let open Sedlexing in
  let open Format in
  let module P = Parser.Default in
  let lexbuf = Utf8.from_string str in
  P.read_top (pstate lexbuf) |> function
  | Ok (toplevel,_) ->
     asprintf "%a" ParsetreePrinter.pp_toplevel toplevel
     |> Js.string |> Js.Unsafe.coerce
  | e -> ("parse error", e) |> Json.output |> Js.Unsafe.coerce

let () =
  let api = (object%js
               method trylex str = str |> Js.to_string |> trylex
               method tryparse str = str |> Js.to_string |> tryparse
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
  then tryparse Sys.argv.(1) |> debug
  else ()

let display_text text =
  let app_main = Dom_html.getElementById "app-main" in
  let text = Dom_html.document##createTextNode (Js.string text) in
  let paragraph = Dom_html.createP Dom_html.document in
  Dom.appendChild paragraph text;
  Dom.appendChild app_main paragraph

let display_prompt prompt =
  let app_main = Dom_html.getElementById "app-main" in
  let prompt = Dom_html.document##createTextNode (Js.string prompt) in
  let input_field = Dom_html.createInput Dom_html.document in
  let paragraph  = Dom_html.createP Dom_html.document in
  Dom.appendChild paragraph prompt;
  Dom.appendChild paragraph input_field;
  Dom.appendChild app_main paragraph

let () =
  display_text "history";
  display_prompt "gensl> "
