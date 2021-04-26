(*
open Genslib

let () =
  let module Lex = Genslex.Lexer in
  let buf = Sedlexing.Utf8.from_channel stdin in
  let rec loop () =
    match Lex.lexer buf with
    | Ok (tok, Parsing.ParserTypes.pstate buf) -> begin
        Format.printf "tok: %a@." Sexplib.Sexp.pp_hum
          (Parsing.ParserTypes.sexp_of_token tok);
        loop ()
      end
    | _ -> failwith "failed" in
  loop()
*)
