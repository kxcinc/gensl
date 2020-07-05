[@@@ocaml.warning "-33"]

open Gensl
open Basetypes
open Parsetree

open ParserTypes

let output_debug = ref true

let debug ?str datum =
  if !output_debug then
  let open Format in
  (match str with
   | None -> ()
   | Some str -> printf "%s ==> " str);
  printf "%a\n" ParsetreePrinter.pp_pdatum datum;
  print_flush()

let parse str =
  let open Lexing in
  let module P = Parser.Default in
  let lexbuf = from_string str in
  P.read_datum (Parser.Default.pstate lexbuf)

let tryparse str =
  parse str |> function
  | Ok (datum, _) -> debug ~str datum
  | _e -> print_endline ("parse error: "^str)

let%test "simple examples parses" =
  tryparse "1";
  tryparse "(i like \"strings\")";
  tryparse "(1 2 3)";
  tryparse "(list 1 2 3)";
  tryparse "(list 1 2 3 4 .2)";
  tryparse "(list ,3 nested 1 2 3 4)";
  tryparse "(list ,, nested 1 2 3 4)";
  tryparse "(list 1 2 3 4 ..)";
  tryparse "(map :abc 10 :def 20)";
  tryparse "(map :(list 123) 10)";
  tryparse "(do @> 10 abc)";
  tryparse "(do abc @< 10)";
  tryparse "(do @abc 10)";
  tryparse "(b:true b:false)";
  tryparse "(list 1 2 3 .tok)";
  tryparse "(list 1 2 3 .2.tok)";
  tryparse "(list ,tok 1 2 3)";
  tryparse "(list ,2.tok 1 2 3)";
  tryparse "(list ,,nested 1 2 3 4)";
  tryparse "(list nested 1 2 3 4 ..)";
  tryparse "(list nested 1 2 3 4 ..abc)";
  tryparse "(list nested 1 2 3 4 ..abc kk)";
  tryparse "(list nested 1 2 3 4 ..(abc 99 88) kk)";
  tryparse "(list nested 1 2 3 4 ..(abc 99 88))";
  true
