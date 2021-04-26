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
  P.read_datum (pstate lexbuf) |> function
  | Ok (datum,_) ->
     printf "%a\n" ParsetreePrinter.pp_pdatum datum
  | _e -> printf "parse error\n"
  
let () =
  if Array.length Sys.argv > 1
  then tryparse Sys.argv.(1)
  else ()

