open Kxclib
open Gensl
open Basetypes
open Utils
open Parsetree

module ParserTypes = struct
  open Sexplib.Std

  let sexp_of_bytes bytes = Sexplib.Sexp.Atom (Bytes.to_string bytes)
  let bytes_of_sexp = function
    | Sexplib.Sexp.Atom str -> Bytes.of_string str
    | _ -> failwith "sexp_of_bytes"

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
    | TkPoundBracketOpen of int option
    | TkBracketClose
    | TkCurlyOpen
    | TkPoundCurlyOpen
    | TkCurlyClose
    | TkComma (* for the decor-only comma *)
    | TkMapsto (* for the decor-only mapsto, i.e. '->' *)
    | TkPickAll | TkGrabAll
    | TkPickK of bool*int | TkGrabK of bool*int
    | TkPickOne of bool | TkGrabOne of bool
    | TkGrabPoint
    | TkKeywordIndicator
    | TkAnnoNextIndicator
    | TkAnnoPrevIndicator
    | TkAnnoStandaloneIndicator
  [@@deriving sexp]

  let pp_token_class ppf (cls : token -> bool) =
    let token_samples = [
        TkEof, `Same;
        TkSpaces " ", `String "TkSpaces _";
        TkSymbol "symb", `String "TkSymbol _";
        TkCodifiedSymbol `Appsymb01, `String "TkCodifiedSymbol _";
        TkString "str", `String "TkString _";
        TkBool false, `String "TkBool _";
        TkBytes (Bytes.of_string "bytes"), `String "TkBytes _";
        TkNumeric ("0", ""), `String "TkNumeric _";
        TkParenOpen, `Same;
        TkParenClose, `Same;
        TkBracketOpen, `Same;
        TkPoundBracketOpen None, `Same;
        TkPoundBracketOpen (Some 0), `Same;
        TkPoundBracketOpen (Some 1), `Same;
        TkPoundBracketOpen (Some 2), `String "TkPoundBracketOpen (Some >1)";
        TkBracketClose, `Same;
        TkCurlyOpen, `Same;
        TkPoundCurlyOpen, `Same;
        TkCurlyClose, `Same;
        TkPickAll, `Same; TkGrabAll, `Same;
        TkPickK (false, 3), `String "TkPickK (false,_)";
        TkPickK (true, 3), `String "TkPickK (true,_)";
        TkGrabK (false, 3), `String "TkGrabK (false,_)";
        TkGrabK (true, 3), `String "TkGrabK (true,_)";
        TkPickOne true, `Same; TkPickOne false, `Same;
        TkGrabOne true, `Same; TkGrabOne false, `Same;
        TkGrabPoint, `Same;
        TkKeywordIndicator, `Same;
        TkAnnoNextIndicator, `Same;
        TkAnnoPrevIndicator, `Same;
        TkAnnoStandaloneIndicator, `Same;
      ] in
    let open Sexplib in
    let open Format in
    let list =
      token_samples
      |> List.filter (fun (x,_) -> cls x)
      |&> function (x,`Same) -> Sexp.to_string (sexp_of_token x)
                 | (_, `String str) -> str in
    let len = List.length list in
    pp_print_string ppf "TokenClass(approx. [";
    pp_open_box ppf 2;
    (list |> List.iteri @@ fun i str ->
         pp_print_string ppf str;
         if i+1 < len then (pp_print_string ppf ","; pp_print_space ppf()));
    pp_close_box ppf();
    pp_print_string ppf "])"

  let pp_token ppf tok =
    sexp_of_token tok
    |> Sexplib.Sexp.pp_hum ppf

  (* XXX span tracking *)

  type trace = parse_error list
  type 'x kresult = ('x, trace) result
  type pkont = pnode list -> pdatum kresult

  type ('buf, 'loc) picking_frame = pickduty*pkont

  and pickduty =
    | PickK of int
    | PickUntil of (token -> bool*bool) (** [token] -> [stop?, consume?] *)

  type 'loc frame_state = { pickduty : int; bucket : pdatum }

  (** shall be treated as a linear type *)
  type 'buf pstate = {
      buf : 'buf; (** will be used as a linear type *)
      withdrew : token queue;
    }
  type ('x, 'buf) presult = ('x*'buf pstate, trace) result
  type 'buf lexresult = (token, 'buf) presult

  let pstate buf = { buf; withdrew = Queue.empty }
  let pp_pickduty ppf = Format.(function
    | PickK k -> fprintf ppf "Pick(%d)" k
    | PickUntil f ->
       fprintf ppf "PickUntil(%a)"
         pp_token_class (fun tok -> fst (f tok)))
end
open ParserTypes

module type Lexer = sig
  type buffer
  type location
  type nonrec pstate = buffer pstate
  type nonrec lexresult = buffer lexresult

  (* val source : buffer -> span_source *)

  val loc : buffer -> location
  val lexer : buffer -> lexresult
  (** [lexer buf pos] consumes and returns next token in buffer *)
end

type parse_error +=
 | Unexpected_eof
 | Unexpected_ending_of_form
 | Unexpected_position_of_comma
 | Immature_ending_of_form of pickduty
 | No_enough_nodes_to_grab of { expected : int; available : int; }
 | Attempting_to_annotate_non_datum
 | Previous_datum_not_exists
 | Lexing_error of lexer_error
 | Invalid_element_in_complex_form of form_style
 | Dimentional_violation of int
 | Parse_errors of trace

exception Parse_error of parse_error
