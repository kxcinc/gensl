open Genslib

let () =
  let module Lex = Genslex_sedlex.Lexer in
  let buf = Sedlexing.Utf8.from_channel stdin in
  let rec loop () =
    match Lex.lexer buf with
    | tok -> begin
       Format.printf "tok: %a@." Sexplib.Sexp.pp_hum
         (Lex.sexp_of_tmptok tok);
       loop ()
      end in
  loop()

