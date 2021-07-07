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
    | TkPoundBracketOpen
    | TkAmpersandBracketOpen of int option
    | TkBracketClose
    | TkCurlyOpen
    | TkPoundCurlyOpen
    | TkCurlyClose
    | TkComma (* for the decor-only comma *)
    | TkMapsto (* for the decor-only mapsto, i.e. '->' *)
    | TkPickAll | TkGrabAll of int option
    | TkPickK of bool*int | TkGrabK of bool*int
    | TkPickOne | TkGrabOne
    | TkGrabPoint
    | TkHat
    | TkKeywordIndicator
    | TkAnnoNextIndicator
    | TkAnnoPrevIndicator
    | TkAnnoStandaloneIndicator
    | TkReaderMacro of string*string
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
        TkPoundBracketOpen, `Same;
        TkAmpersandBracketOpen None, `Same;
        TkAmpersandBracketOpen (Some 0), `Same;
        TkAmpersandBracketOpen (Some 1), `Same;
        TkAmpersandBracketOpen (Some 2), `String "TkAmpersandBracketOpen (Some >1)";
        TkBracketClose, `Same;
        TkCurlyOpen, `Same;
        TkPoundCurlyOpen, `Same;
        TkCurlyClose, `Same;
        TkPickAll, `Same; TkGrabAll None, `Same;
        TkPickK (false, 3), `String "TkPickK (false,_)";
        TkPickK (true, 3), `String "TkPickK (true,_)";
        TkGrabK (false, 3), `String "TkGrabK (false,_)";
        TkGrabK (true, 3), `String "TkGrabK (true,_)";
        TkPickOne, `Same; TkPickOne, `Same;
        TkGrabOne, `Same; TkGrabOne, `Same;
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
 | Unexpected_positional_datum of pdatum
 | Immature_ending_of_form of pickduty
 | No_enough_nodes_to_grab of { expected : int; available : int; }
 | Attempting_to_annotate_non_datum
 | Previous_datum_not_exists
 | Lexing_error of lexer_error
 | Invalid_element_in_complex_form of form_style
 | Invalid_form_format of [ `MixedKeywordMapsto | `InconsistentCommaUsage | `TodoMoreDetails ]
 | Unmatched_graball_count of int*int
 | Dimentional_violation of int
 | No_relname
 | Parse_errors of trace

let () =
  let open Gensl.Parsetree.ParseError in
  let open Format in
  let rec pp_handler ppf = function
      | Unexpected_eof ->
        pp_string ppf "unexpected EOF"; true
      | Unexpected_ending_of_form ->
        pp_string ppf "unexpected ending of form"; true
      | Unexpected_position_of_comma ->
        pp_string ppf "unexpected position of comma"; true
      | Unexpected_positional_datum datum ->
        pp_string ppf
          (asprintf "unexpected positional datum: %a"
             Unparse.(unparse_pdatum ~fxnconv:`Infix) datum);
        true
      | Immature_ending_of_form duty ->
        pp_string ppf
          (asprintf "immature ending of form: %a" pp_pickduty duty);
        true
      | No_enough_nodes_to_grab _ ->
        pp_string ppf "no enough nodes to grab"; true
      | Attempting_to_annotate_non_datum ->
        pp_string ppf "attempting to annotate non datum"; true
      | Previous_datum_not_exists ->
        pp_string ppf "previous datum not exists"; true
      | Lexing_error _ ->
        pp_string ppf "lexing error: no next valid token"; true
      | Invalid_element_in_complex_form _ ->
        pp_string ppf "invalid element in complex form"; true
      | Invalid_form_format `MixedKeywordMapsto ->
        pp_string ppf "invalid form format: mixed keyword mapsto"; true
      | Invalid_form_format `InconsistentCommaUsage ->
        pp_string ppf "invalid form format: inconsistent comma usage"; true
      | Invalid_form_format _ ->
        pp_string ppf "invalid form format"; true
      | Unmatched_graball_count _ ->
        pp_string ppf "unmatched graball count"; true
      | Dimentional_violation _ ->
        pp_string ppf "dimentional violation"; true
      | Parse_errors (err :: _) ->
        pp_handler ppf err
      | _ -> false in
  register_parse_error_pp_handler pp_handler

exception Parse_error of parse_error

let () =
  Printexc.register_printer (function
      | Parse_error err ->
        Some (Format.asprintf "%a" Gensl.Parsetree.ParseError.pp err)
      | _ -> None)
