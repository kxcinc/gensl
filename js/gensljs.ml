open Js_of_ocaml

[@@@ocaml.warning "-33"]

open Gensl
open Parsing

open Basetypes
open Parsetree

open ParserTypes

let trylex str =
  let open Lexing in
  let open Format in
  printf "we got %s\n" str;
  let lexbuf = from_string str in
  let result = Genslex.token lexbuf in
  (match result with
   | TkNumeric(a,b) -> printf "%s.%s\n" a b
   | _ -> printf "idk\n");
  print_flush()

let tryparse str =
  let open Lexing in
  let open Format in
  let module P = Parser.Default in
  let lexbuf = from_string str in
  P.read_datum (pstate lexbuf) |> function
  | Ok (datum,_) ->
     asprintf "%a" ParsetreePrinter.pp_pdatum datum
     |> Js.string |> Js.Unsafe.coerce
  | e -> ("parse error", e) |> Json.output |> Js.Unsafe.coerce
  
let () =
  let api = (object%js
               method trylex str = str |> Js.to_string |> trylex
               method tryparse str = str |> Js.to_string |> tryparse
               method lex str =
                 let str = str |> Js.to_string in
                 let open Lexing in
                 let lexbuf = from_string str in
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

