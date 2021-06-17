open Js_of_ocaml

[@@@ocaml.warning "-33"]

open Genslib
open Gensl
open Parsing

open Basetypes
open Parsetree

open ParserTypes

(* roadmap to MVP
 - [ ] make index.html accessible from the Internet via GitHub Pages

 - [ ] displayed error message (in red)
 - [x] modify unparse for CommaSeparator and remove the prefix space
 - [ ] repl result as if #conv infix then #unparse
 - [x] change color of prompt
 - [x] use monospace font
 - [ ] make input area larger and support multi-line input
 *)

let unparse_for_repl expr =
  Format.asprintf "%a" Unparse.(unparse_pdatum ~fxnconv:`Infix) expr

let rec unparse_error_for_repl = function
  | Unexpected_eof ->
    "unexpected EOF"
  | Unexpected_ending_of_form ->
    "unexpected ending of form"
  | Unexpected_position_of_comma ->
    "unexpected position of comma"
  | Unexpected_positional_datum datum ->
    Format.asprintf "unexpected positional datum: %a" Unparse.(unparse_pdatum ~fxnconv:`Infix) datum
  | Immature_ending_of_form duty ->
     Format.asprintf "immature ending of form: %a" pp_pickduty duty
  | No_enough_nodes_to_grab _ ->
    "no enough nodes to grab"
  | Attempting_to_annotate_non_datum ->
    "attempting to annotate non datum"
  | Previous_datum_not_exists ->
    "previous datum not exists"
  | Lexing_error _ ->
    "lexing error: no next valid token"
  | Invalid_element_in_complex_form _ ->
    "invalid element in complex form"
  | Invalid_form_format `MixedKeywordMapsto ->
    "invalid form format: mixed keyword mapsto"
  | Invalid_form_format `InconsistentCommaUsage ->
    "invalid form format: inconsistent comma usage"
  | Invalid_form_format _ ->
    "invalid form format"
  | Unmatched_graball_count _ ->
    "unmatched graball count"
  | Dimentional_violation _ ->
    "dimentional violation"
  | Parse_errors errors ->
    foldr (fun e acc -> unparse_error_for_repl e ^ "\n" ^ acc) errors ""
  | _ -> "unknown parse error"


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
  | Ok (toplevel, _)->  Ok (unparse_for_repl toplevel)
  | Error errors -> Error (unparse_error_for_repl (Parse_errors errors))

let tryparse_console str =
  match tryparse str with
  | Ok s -> s
  | Error s -> s

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

let add_paragraph ?(color="black") paragraph =
  let repl_history = Dom_html.getElementById "repl-history" in
  (if color = "" then ()
   else Js.Unsafe.set paragraph "style" ("color:" ^ color));
  Dom.appendChild repl_history paragraph

let add_input text =
  let p = Dom_html.createP Dom_html.document in
  let span = Dom_html.createSpan Dom_html.document in
  let prompt = Dom_html.document##createTextNode (Js.string "gensl> ") in
  let textnode = Dom_html.document##createTextNode text in
  Js.Unsafe.set span "style" "color:blue";
  Dom.appendChild span prompt;
  Dom.appendChild p span;
  Dom.appendChild p textnode;
  add_paragraph p

let add_output text =
  let p = Dom_html.createP Dom_html.document in
  let textnode = Dom_html.document##createTextNode text in
  Dom.appendChild p textnode;
  add_paragraph p

let add_error text =
  let p = Dom_html.createP Dom_html.document in
  let textnode = Dom_html.document##createTextNode text in
  Dom.appendChild p textnode;
  add_paragraph ~color:"red" p

let clear_input () =
  let input_field = Dom_html.getElementById "input-field" in
  Js.Unsafe.set input_field "value" ""

let () =
  let open Sedlexing in
  let open Format in
  let module P = Parser.Default in
  let input_field = Dom_html.getElementById "input-field" in
  let keypress = Dom.Event.make "keypress" in
  let _ =
    Dom.addEventListener
      input_field
      keypress
      (Dom.handler (fun e ->
           if e##.key == Js.string "Enter" then
             begin
               let input_value = Js.Unsafe.get input_field "value" in
               let parsed_value = tryparse (Js.to_string input_value) in
               add_input input_value;
               (match parsed_value with
                | Ok output -> add_output (Js.string output)
                | Error error -> add_error (Js.string error));
               clear_input ();
               Js._false
             end
           else
             Js._true))
      Js._true in
  ()
