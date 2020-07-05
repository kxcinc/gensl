{
[@@@ocaml.warning "-33"]

open Gensl

open Basetypes
open Parsetree

open ParserTypes
}

rule token = parse
  ([' ' '\t' '\n']+ as lxm) { TkSpaces lxm }
(* token TkSymbol *)
| (['a'-'z'] ['a'-'z' '0'-'9']* as lxm) { TkSymbol lxm }
(* token TkString *)
| '"' ([^ '"']* as lxm) '"' { TkString lxm }
(* token TkBool *)
| "b:true" { TkBool true }
| "b:false" { TkBool false }
(* XXX no TkBytes for now *)
(* token TkNumeric *)
(* XXX no suffix for now *)
| ['0'-'9']+ as lxm { TkNumeric (lxm, "") }

| '(' { TkParenOpen }
| ')' { TkParenClose }

| "," (['0'-'9']+ as k) { TkPickK (false, int_of_string k) }
| "." (['0'-'9']+ as k) { TkGrabK (false, int_of_string k) }
| "," (['0'-'9']+ as k) "." { TkPickK (true, int_of_string k) }
| "." (['0'-'9']+ as k) "." { TkGrabK (true, int_of_string k) }
| "," { TkPickOne true }
| "." { TkGrabOne true }
(* XXX no head-node for GrabAll for now *)
| ",," { TkPickAll }
| ".." { TkGrabAll }
| "." [' ' '\t' '\n'] { TkGrabPoint }

| ":" { TkKeywordIndicator }
| "@>" { TkAnnoNextIndicator }
| "@<" { TkAnnoPrevIndicator }
| "@" { TkAnnoStandaloneIndicator }

and  whitespace kont = parse
  ([' ' '\t' '\n']+ as lxm) { kont lexbuf lxm }

{

module Lexer : Lexer with
           type buffer = Lexing.lexbuf
       and type location = Lexing.position
  = struct
  open Lexing
  type buffer = Lexing.lexbuf
  type location = Lexing.position
  type nonrec pstate = (buffer, location) pstate

  let loc buf = buf.lex_curr_p
  let source buf = `DirectInput (Some (loc buf).pos_fname)
  let lexer buf =
    let tok = token buf in
    let span = {
        span_start = buf.lex_start_p;
        span_end = buf.lex_curr_p;
        span_leading = NoLeadingInfo;
        span_source = source buf;
      } in
    Ok (tok,span)
end
}
