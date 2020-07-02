(* file    : gensl.ml
   created : 2020-06-26 *)

module Basetypes = struct
  type 'a equality = 'a -> 'a -> bool
  type ('a, 'b) assoc = ('a*'b) list*('a equality)
  type 'a set = ('a list)*('a equality)

  type atom =
     | SymbolAtom of string
     | StringAtom of string
     | BytesAtom of bytes
     | NumericAtom of string*string (** (pair numeric suffix) *)
     | BoolAtom of bool
end

module Parsetree = struct
  open Basetypes

  type leading = Leading of string | NoLeadingInfo
  type ghost_source = ..
  type span_source =
    [ `File of string
    | `DirectInput of string option
    | `Ghost of ghost_source ]
  type 'l span = {
      span_start: 'l;
      span_end  : 'l;
      span_leading : leading;   (** leading spaces *)
      span_source  : span_source;
    }

  type flat_location = { line: int; col: int; }
  type stream_location = int

  type parse_error = ..

  type syntax_mode =
    | Infix
    | Prefix  of [ `PickAll | `PickOne | `PickK of int ]
    | Postfix of [ `GrabAll | `GrabOne | `GrabK of int ]
    | Phantomfix
  type form_style =
    | ToplevelForm
    | SimpleForm                (**   ( .. ) *)
    | NotAForm

  (** phantom elements,
      phantom in the sense that
      they don't semantically contribute to the Datatree *)
  type phantom = 
    | GrabPoint        (** .  - the postfix grab-point *)
    | GrabAllOperator  (** .. - the postfix grab-all operator *)
    | PickAllOperator  (** ,, - the prefix pick-all operator *)
    | ParseError of parse_error

  type reader_style =
    | DefaultReader
    | DataReader of string  (**  lexp:.. *)

  type 'l pdatum =
    | PAtom of 'l patom*reader_style
    | PForm of ('l pnode list*form_style*reader_style, 'l) pelem
    | PAnnotated of {
        p_annotated  : 'l pdatum;
        p_anno_front : 'l pdatum list;
        p_anno_back  : 'l pdatum list; (** !!reversed *)
      }
  and  'l pnode =
    | PDatumNode of 'l pdatum
    | PAnnoNode of 'l pdatum
    | PKeywordNode of 'l pdatum * 'l pdatum
    | PPhantomNode of (phantom, 'l) pelem
  and  'l patom = (atom, 'l) pelem

  and  ('x, 'l) pelem = {
      elem: 'x;
      mode: syntax_mode;
      span: 'l span
    }

  let patom atom span mode : 'l patom = { elem = atom; span; mode; }
  let pdatum_atom atom span mode style : 'l pdatum =
    PAtom (patom atom span mode, style)
  let pdatum_form nodes form_style reader_style span mode : 'l pdatum =
    let elem = (nodes, form_style, reader_style) in
    PForm { elem; span; mode; }
  let pdatum_anno_front anno datum : 'l pdatum = match datum with
    | PAnnotated ({ p_anno_front; _ } as r) ->
       PAnnotated ({ r with p_anno_front = anno :: p_anno_front })
    | _ -> PAnnotated {
               p_annotated = datum;
               p_anno_front = [anno];
               p_anno_back = [];
             }
  let pdatum_anno_back anno datum : 'l pdatum = match datum with
    | PAnnotated ({ p_anno_back; _ } as r) ->
       PAnnotated ({ r with p_anno_back = anno :: p_anno_back })
    | _ -> PAnnotated {
               p_annotated = datum;
               p_anno_front = [];
               p_anno_back = [anno];
             }
end

module ParsetreePrinter = struct
  open Basetypes
  open Parsetree
  open Format
  open Sexplib.Type
  open Sexplib

  let sexp_atom = function
    | SymbolAtom str -> Atom ("symb:" ^ str)
    | StringAtom str -> Atom ("str:" ^ str)
    | BytesAtom bytes ->
       let encoded = Bytes.to_string bytes |> Base64.encode_string
       in Atom ("bytes:" ^ encoded)
    | NumericAtom (num,suf) -> Atom (num^suf)
    | BoolAtom b -> Atom (sprintf "bool:%b" b)
  let sexp_patom { elem; _ } = sexp_atom elem

  let rec sexp_pnode = function
    | PDatumNode dtm -> sexp_pdatum dtm
    | PAnnoNode dtm -> List [Atom "anno"; sexp_pdatum dtm]
    | PKeywordNode (kw,value) -> List [Atom "kwnode"; sexp_pdatum kw; sexp_pdatum value]
    | PPhantomNode _ -> Atom "somephantom"

  and     sexp_pdatum = function
    | PAtom (patom,_) -> sexp_patom patom
    | PForm { elem = (nodes, _, _) ; _ } -> List (nodes |> List.map sexp_pnode)
    | PAnnotated { p_annotated; p_anno_front; p_anno_back } ->
       let l = [Atom "annotated"; p_annotated |> sexp_pdatum]
               @ [Atom ":front"] @ (p_anno_front |> List.map sexp_pdatum)
               @ [Atom ":back"] @ (List.rev p_anno_back |> List.map sexp_pdatum)
       in List l

  let composite f g x = f (g x)
  let pp_patom ppf = composite (Sexp.pp_hum ppf) sexp_patom
  let pp_atom ppf = composite (Sexp.pp_hum ppf) sexp_atom
  let pp_pdatum ppf = composite (Sexp.pp_hum ppf) sexp_pdatum
end

open Parsetree

type datafying_error = ..
exception Datafying_error of datafying_error

type datafying_error +=
   | Datafying_noimpl

module ParserTypes = struct
  open Sexplib.Std

  let bytes_of_sexp _ = failwith "noimpl"
  let sexp_of_bytes _ = failwith "noimpl"

  type token =
    | TkSymbol of string
    | TkString of string
    | TkBool of bool
    | TkBytes of bytes
    | TkNumeric of string*string
    | TkParenOpen
    | TkParenClose
    | TkPickAll of bool | TkGrabAll of bool (* true if head-node exists *)
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

  type ('x, 'loc) kresult = ('x, (parse_error*'loc span) list) result
  type 'loc pkont = 'loc pnode list -> ('loc pdatum, 'loc) kresult
  type ('buf, 'loc) picking_frame =
    | PfPickAll of 'loc pkont
    | PfPickK of int*('loc pkont)
  type 'loc frame_state = { pickduty : int; bucket : 'loc pdatum }
  type ('buf, 'loc) pstate = {
      buf : 'buf;
    }
  type ('x, 'buf, 'loc) presult = ('x*('buf, 'loc) pstate, (parse_error*'loc span) list) result
end
open ParserTypes

module type Lexer = sig
  type buffer
  type location
  type nonrec pstate = (buffer, location) pstate

  val loc : buffer -> location
  val source : buffer -> span_source
  val lexer : buffer -> (token*leading, buffer, location) presult
  (** [lexer buf pos] consume and returns next token from position [pos] *)
end

type parse_error +=
 | Unexpected_ending_of_form
 | Immature_ending_of_form of int
 | No_enough_nodes_to_grab of { expected : int; available : int; }
 | Attempting_to_annotate_non_datum
 | Previous_datum_to_annotate_not_exists
