open Gensl


let parse_et_print str =
  let open Lexing in
  let open Format in
  let module P = Parser.Default in
  let lexbuf = from_string str in
  P.read_top (Parsing.ParserTypes.pstate lexbuf) |> function
  | Ok (toplevel,_) ->
     printf "%a" ParsetreePrinter.pp_toplevel toplevel
  | _e -> printf "parse error\n"
  
let () =
  let open Format in
  let rec loop () =
    (* print prompt *)
    printf "gensl> "; print_flush();
    let line = read_line() in
    match line with
    | "#debug" -> Parser.debugging := true; loop()
    | "#undebug" -> Parser.debugging := false; loop()
    | _ ->
       (try Printexc.print parse_et_print line
        with _ -> printf "got some error. try again pls.\n");
       print_flush(); loop()
  in loop()
