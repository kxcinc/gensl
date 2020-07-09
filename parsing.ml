open Gensl
open Basetypes
open Utils
open Parsetree

module ParserTypes = struct
  open Sexplib.Std

  let bytes_of_sexp _ = failwith "noimpl"
  let sexp_of_bytes _ = failwith "noimpl"

  type lexer_error = ..

  type token =
    | TkEof
    | TkSpaces of string
    | TkSymbol of string
    | TkCodifiedSymbol of csymb
    | TkString of string
    | TkBool of bool
    | TkBytes of bytes
    | TkNumeric of string*string
    | TkParenOpen
    | TkParenClose
    | TkBracketOpen
    | TkBracketClose
    | TkCurlyOpen
    | TkPoundCurlyOpen
    | TkCurlyClose
    | TkPickAll | TkGrabAll
    | TkPickK of bool*int | TkGrabK of bool*int
    | TkPickOne of bool | TkGrabOne of bool
    | TkGrabPoint
    | TkKeywordIndicator
    | TkAnnoNextIndicator
    | TkAnnoPrevIndicator
    | TkAnnoStandaloneIndicator
  [@@deriving sexp]

  let pp_token ppf tok =
    sexp_of_token tok
    |> Sexplib.Sexp.pp_hum ppf

  type 'loc lexresult = (token*'loc span, lexer_error*'loc span) result
  type ('x, 'loc) kresult = ('x, (parse_error*'loc span) list) result
  type 'loc pkont = 'loc pnode list -> ('loc pdatum, 'loc) kresult

  type ('buf, 'loc) picking_frame = pickduty*'loc pkont

  and pickduty =
    | PickK of int
    | PickUntil of (token -> bool*bool) (** [token] -> [stop?, consume?] *)

  type 'loc frame_state = { pickduty : int; bucket : 'loc pdatum }
  type ('buf, 'loc) pstate = {
      buf : 'buf;
      withdrew : (token*'loc span) queue;
    }
  type ('x, 'buf, 'loc) presult = ('x*('buf, 'loc) pstate, (parse_error*'loc span) list) result

  let pp_pickduty ppf = Format.(function
    | PickK k -> fprintf ppf "Pick(%d)" k
    | PickUntil _ -> fprintf ppf "PickUntil(_)")
end
open ParserTypes

module type Lexer = sig
  type buffer
  type location
  type nonrec pstate = (buffer, location) pstate

  val loc : buffer -> location
  val source : buffer -> span_source
  val lexer : buffer -> location lexresult
  (** [lexer buf pos] consume and returns next token from position [pos] *)
end

type parse_error +=
 | Unexpected_eof
 | Unexpected_ending_of_form
 | Immature_ending_of_form of pickduty
 | No_enough_nodes_to_grab of { expected : int; available : int; }
 | Attempting_to_annotate_non_datum
 | Previous_datum_to_annotate_not_exists
 | Lexing_error of lexer_error
 | Invalid_element_in_complex_form of form_style

exception Parse_error of parse_error
