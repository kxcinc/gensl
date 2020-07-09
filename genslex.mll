{
[@@@ocaml.warning "-33"]

open Gensl
open Parsing

open Basetypes
open Parsetree

open ParserTypes
}

let digit = ['0'-'9']
let octal = ['0'-'7']
let hexalphabet = ['0'-'9' 'a'-'f' 'A'-'F']
let hexbyte = hexalphabet hexalphabet
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alpha = lowercase | uppercase
let alphadigit = alpha | digit
let space = [' ' '\t' '\n']
let base64alphabet = alphadigit | ['+' '/' '=']
let base64digit = base64alphabet base64alphabet base64alphabet base64alphabet

(* per ocaml lexing rules *)
(* ref: https://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sss:character-literals *)
(* XXX \o000 not supported now due to limits of Scanf.unescaped *)
(* XXX implement [unscape] in-house for oc/js compatibility *)
let escape = '\\' (['"' '\\' '\'' 'n' 'r' 't' 'b' ' '] |
                   (digit digit digit) |
                   ('x' hexbyte)
                   (* | ('o' octal octal octal) *)
               )
let instring = [^ '"' '\\'] | escape

let boolprefix = "b:" | "bool:"
let hexprefix = "hex:"
let base64prefix = "b64:" | "base64:"
let strbytesprefix = "strbytes:"

let csymbprefix_std = "!"
let csymbprefix_app = "!!"

rule csymb_std = parse
| "toplevel" { `Toplevel } | "envelop" { `Envelop } | "metadata" { `Metadata }
| "desc" { `Desc } | "hash" { `Hash } | "uuid" { `Uuid } | "version" { `Version }
| "list" { `List } | "vector" { `Vector } | "set" { `Set } | "map" { `Map }
| "int" { `Int } | "uint" { `Uint } | "float" { `Float } | "timestamp" { `Timestamp }

and  csymb_app = parse
| "app01" { `Appsymb01 } | "app02" { `Appsymb02 } | "app03" { `Appsymb03 } | "app04" { `Appsymb04 }
| "app05" { `Appsymb05 } | "app06" { `Appsymb06 } | "app07" { `Appsymb07 } | "app08" { `Appsymb08 }
| "app09" { `Appsymb09 } | "app10" { `Appsymb10 } | "app11" { `Appsymb11 } | "app12" { `Appsymb12 }

and  token = parse
| eof { TkEof }
| (space+ as lxm) { TkSpaces lxm }
(* token TkSymbol *)
| (lowercase alphadigit* as lxm) { TkSymbol lxm }
(* token TkCodifiedSymbol *)
| csymbprefix_std { TkCodifiedSymbol (csymb_std lexbuf) }
| csymbprefix_app { TkCodifiedSymbol (csymb_app lexbuf) }
(* token TkString *)
| '"' (instring* as lxm) '"' { TkString (Scanf.unescaped lxm) }
(* token TkBool *)
| boolprefix "true" { TkBool true }
| boolprefix "false" { TkBool false }
(* token TkNumeric *)
| ((['+' '-']? digit+ '.'? digit*) as num)
  (alpha+ as suffix)?
  { let suffix = Option.value ~default:"" suffix in TkNumeric (num, suffix) }
| ((['+' '-']? digit+ '/' digit+) as num)
  (alpha+ as suffix)?
  { let suffix = Option.value ~default:"" suffix in TkNumeric (num, suffix) }

(* TkBytes *)
| hexprefix (hexbyte+ as lxm) { TkBytes (Hex.to_bytes (`Hex lxm)) }
| base64prefix (base64digit+ as lxm) { TkBytes (Base64.decode_exn lxm |> Bytes.of_string) }
| strbytesprefix '"' (instring* as lxm) '"' { TkBytes (Scanf.unescaped lxm |> Bytes.of_string) }

| '(' { TkParenOpen }
| ')' { TkParenClose }
| '[' { TkBracketOpen }
| ']' { TkBracketClose }
| '{' { TkCurlyOpen }
| "#{" { TkPoundCurlyOpen }
| '}' { TkCurlyClose }

| "," (digit+ as k) { TkPickK (false, int_of_string k) }
| "." (digit+ as k) { TkGrabK (false, int_of_string k) }
| "," (digit+ as k) "." { TkPickK (true, int_of_string k) }
| "." (digit+ as k) "." { TkGrabK (true, int_of_string k) }
| "," { TkPickOne true }
| "." { TkGrabOne true }
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
