[@@@ocaml.warning "-33"]

open Gensl
open Parsing
open Basetypes
open Parsetree

open ParserTypes

let pp_pdatum = ParsetreePrinter.pp_pdatum

let output_debug = ref true

let debug_msg msg =
  if !output_debug then
  let open Format in
  printf "%s\n" msg;
  print_flush()

let debug ?str datum =
  if !output_debug then
  let open Format in
  (match str with
   | None -> ()
   | Some str -> printf "%s ==> " str);
  printf "%a\n" pp_pdatum datum;
  print_flush()

let parse str =
  let open Lexing in
  let module P = Parser.Default in
  let lexbuf = from_string str in
  P.read_datum (Parser.Default.pstate lexbuf)

let tryparse str =
  parse str |> function
  | Ok (datum, _) -> debug ~str datum
  | _e -> failwith ("parse error: "^str)

let badparse str =
  let bingo() = debug_msg ("failed as expected: "^str) in
  try parse str |> function
      | Ok (datum, _) -> failwith Format.(asprintf "should fail: %s ==> %a" str pp_pdatum datum)
      | _e -> bingo()
  with Failure _ | Parse_error _ -> bingo()

let%test "simple examples parses" =
  tryparse "1";
  tryparse "hex:323454ff39";
  tryparse "b64:MjRU/zk=";
  tryparse "base64:MjRU/zk=";
  tryparse "strbytes:\"24T\2559\"";
  tryparse "(i like \"strings\")";
  tryparse "\"abc\"";
  tryparse "\"a\\tbc\"";
  tryparse "\"a\\\\bc\"";
  tryparse "\"a\\tbc\"";
  tryparse "\"a\\\"bc\"";
  tryparse "\"\\x39\ \\133\"";
  tryparse "(1 2 3 +7 -6)";
  tryparse "(1. 0. 34447 3.254)";
  badparse ".32";
  badparse "(3.14.154)";
  tryparse "(1tz 10tz 3.4tz 0.tz 6t 10Hz 6oz -32degC)";
  tryparse "(21/7 355/113 -1/7 4/5Hz +3/7 0/1)";
  tryparse "(list 1 2 3)";
  tryparse "(list 1 2 3 4 .2)";
  tryparse "(list ,3 nested 1 2 3 4)";
  tryparse "(list ,, nested 1 2 3 4)";
  tryparse "(list 1 2 3 4 ..)";
  tryparse "(map :abc 10 :def 20)";
  tryparse "(map :(list 123) 10)";
  tryparse "(do @> 10 abc)";
  tryparse "(do abc @< 10)";
  tryparse "(do abc @< (list 10 20))";
  tryparse "(do @> 10 @> 20 abc @< 30 @< 40)";
  tryparse "(do @abc 10)";
  tryparse "(b:true b:false)";
  tryparse "(bool:true bool:false)";
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
  tryparse "(1 2 . 3 4 5 ..)";
  tryparse "(1 2 . 3 4 5 .. )";
  tryparse "(1 2 . 3 4 5 .. 6 7)";
  tryparse "(1 2 . 3 4 5 .. 6 7 ..)";
  tryparse "(1 2 . 3 4 5 .. 6 . 7 8 ..)";
  tryparse "(1 . 2 . 3 4 5 .. 6 . 7 8 .. ..)";
  tryparse "(1 ,, 1 2 3 ..)";
  tryparse "(1 ,, 1 . 2 3 ..)";
  tryparse "!envelop";
  tryparse "(!envelop :!hash \"abcdefg\")";
  tryparse "!!app03";
  badparse "!app03";
  badparse "!!envelop";
  tryparse "[1 2 3 @haha]";
  tryparse "#{1 2 3 10}";
  tryparse "#{1 2 @haha 3 10}";
  badparse "#{1 2 :haha yes 3 10}";
  tryparse "{:alice 10 :bob 20}";
  true
