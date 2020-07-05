{
[@@@ocaml.warning "-33"]

open Gensl

open Basetypes
open Parsetree

open ParserTypes
}

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alpha = lowercase | uppercase
let alphadigit = alpha | digit
let space = [' ' '\t' '\n']

let boolprefix = "b:" | "bool:"

rule token = parse
  (space+ as lxm) { TkSpaces lxm }
(* token TkSymbol *)
| (lowercase alphadigit* as lxm) { TkSymbol lxm }
(* token TkString *)
| '"' ([^ '"']* as lxm) '"' { TkString lxm }
(* token TkBool *)
| boolprefix "true" { TkBool true }
| boolprefix "false" { TkBool false }
(* XXX no TkBytes for now *)
(* token TkNumeric *)
| ((['+' '-']? digit+ '.'? digit*) as num)
  (alpha+ as suffix)?
  { let suffix = Option.value ~default:"" suffix in TkNumeric (num, suffix) }
| ((['+' '-']? digit+ '/' digit+) as num)
  (alpha+ as suffix)?
  { let suffix = Option.value ~default:"" suffix in TkNumeric (num, suffix) }

| '(' { TkParenOpen }
| ')' { TkParenClose }

| "," (digit+ as k) { TkPickK (false, int_of_string k) }
| "." (digit+ as k) { TkGrabK (false, int_of_string k) }
| "," (digit+ as k) "." { TkPickK (true, int_of_string k) }
| "." (digit+ as k) "." { TkGrabK (true, int_of_string k) }
| "," { TkPickOne true }
| "." { TkGrabOne true }
(* XXX no head-node for GrabAll for now *)
| ",," { TkPickAll }
| ".." { TkGrabAll }
| "." space { TkGrabPoint }

| ":" { TkKeywordIndicator }
| "@>" { TkAnnoNextIndicator }
| "@<" { TkAnnoPrevIndicator }
| "@" { TkAnnoStandaloneIndicator }

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
