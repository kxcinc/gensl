open Gensl


let parse_et_print ?fxnconv use_unparse str =
  let open Lexing in
  let open Format in
  let module P = Parser.Default in
  let lexbuf = from_string str in
  P.read_top (Parsing.ParserTypes.pstate lexbuf) |> function
  | Ok (datum,_) when use_unparse ->
     printf "%a" Unparse.(unparse_pdatum ?fxnconv) datum; print_cut()
  | Ok (toplevel,_) ->
     printf "%a" ParsetreePrinter.pp_toplevel toplevel
  | Error e -> raise Parsing.(Parse_error (Parse_errors e))
  
let () =
  let open Format in
  let unparse_mode = ref (Sys.argv |> Array.mem "-unparse") in
  let fxnconv : [`Infix | `Prefix | `Postfix] option ref = ref None in
  let rec loop () =
    (* print prompt *)
    printf "gensl> "; print_flush();
    let line = read_line() in
    match line with
    | "#debug" -> Parser.debugging := true; loop()
    | "#undebug" -> Parser.debugging := false; loop()
    | "#unparse on" -> unparse_mode := true; loop()
    | "#unparse off" -> unparse_mode := false; loop()
    | "#conv off" -> fxnconv := None; loop()
    | "#conv infix" -> fxnconv := Some `Infix; loop()
    | "#conv prefix" -> fxnconv := Some `Prefix; loop()
    | "#conv postfix" -> fxnconv := Some `Postfix; loop()
    | _ ->
       (try Printexc.print (parse_et_print ?fxnconv:!fxnconv) !unparse_mode line
        with _ -> printf "got some error. try again pls.\n");
       print_flush(); loop()
  in loop()
