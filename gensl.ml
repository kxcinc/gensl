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

open Basetypes
open Parsetree

type datafying_error = ..
exception Datafying_error of datafying_error

type datafying_error +=
   | Datafying_noimpl

module ParserTypes = struct
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

  type ('buf, 'loc) pkont = 'loc pnode list -> ('loc pdatum, 'buf, 'loc) presult
  and  ('buf, 'loc) picking_frame =
    | PfPickAll of ('buf, 'loc) pkont
    | PfPickK of int*('buf, 'loc) pkont
  and  'loc frame_state = { pickduty : int; bucket : 'loc pdatum }
  and  ('buf, 'loc) pstate = {
      buf : 'buf;
      pos : int;
      frames : ('buf, 'loc) picking_frame*'loc frame_state ;
    }
  and  ('x, 'buf, 'loc) presult = ('x*('buf, 'loc) pstate, (parse_error*'loc span) list) result
end
open ParserTypes

module type Lexer = sig
  type buffer
  type location

  val loc : buffer -> int -> location
  val source : buffer -> span_source
  val lexer : buffer -> int -> (token*leading, buffer, location) presult
  (** [lexer buf pos] consume and returns next token from position [pos] *)
end

type parse_error +=
 | Immature_ending_of_form
 | Attempting_to_annotate_non_datum

module Parser (Lexer : Lexer) = struct
  open Lexer

  type nonrec picking_frame = (buffer, location) picking_frame
  type nonrec pkont = (buffer, location) pkont

  type nonrec pdatum = location pdatum
  type nonrec pnode = location pnode
  type nonrec patom = location patom
  type nonrec 'x pelem = ('x, location) pelem

  type nonrec pstate = (buffer, location) pstate
  type nonrec 'x presult = ('x, buffer, location) presult

  let lexer { buf; pos; _ } = lexer buf pos

  open struct
    let (>>=) : 'x presult -> ('x*pstate -> 'y presult) -> 'y presult =
      fun mx f ->
      match mx with
      | Ok (x,ps) -> f (x,ps)
      | Error e -> Error e
    let ok ps x = Ok (x,ps)
    let fail err span : 'x presult = Error [err, span]
  end

  let rec read_datum : pstate -> pdatum presult =
    fun ps ->
    let span ps' leading =
      let span_source = source ps.buf in
      let span_start = loc ps.buf ps.pos in
      let span_end = loc ps.buf ps'.pos in
      { span_start; span_end; span_source; span_leading = leading }
    in
    let atom_clause (ps,leading) atom =
       let span = span ps leading
       in pdatum_atom atom span Infix DefaultReader |> ok ps
    in
    lexer ps >>= function
    | (TkSymbol symb, leading), ps -> atom_clause (ps,leading) (SymbolAtom symb)
    | (TkString str, leading), ps -> atom_clause (ps,leading) (StringAtom str)
    | (TkBytes bytes, leading), ps -> atom_clause (ps,leading) (BytesAtom bytes)
    | (TkNumeric (num,suffix), leading), ps -> atom_clause (ps,leading) (NumericAtom (num,suffix))
    | (TkBool b, leading), ps -> atom_clause (ps,leading) (BoolAtom b)
    | (TkParenOpen, leading), ps ->
       let kont : pkont = fun nodes ->
         pdatum_form nodes SimpleForm DefaultReader (span ps leading) Infix |> ok ps in
       read_nodes (PfPickAll kont) ps
    | (TkParenClose, leading), ps
    | (TkPickAll _, leading), ps | (TkGrabAll _, leading), ps
    | (TkPickK _, leading), ps | (TkGrabK _, leading), ps
    | (TkPickOne _, leading), ps | (TkGrabOne _, leading), ps
    | (TkGrabPoint, leading), ps
    | (TkKeywordIndicator, leading), ps
    | (TkAnnoPrevIndicator, leading), ps
    | (TkAnnoStandaloneIndicator, leading), ps
      -> fail Immature_ending_of_form (span ps leading)
    | (TkAnnoNextIndicator, leading), ps ->
       read_datum ps >>= fun (anno, ps') ->
       let kont : pkont = function
         | [node] ->
            begin match node with
            | PDatumNode datum -> pdatum_anno_front anno datum |> ok ps'
            | _ -> fail Attempting_to_annotate_non_datum (span ps' leading)
            end
         | _ -> failwith ("panic: " ^ __LOC__)
       in
       read_nodes (PfPickK (1, kont)) ps'
  and     read_nodes : picking_frame -> pstate -> pdatum presult = failwith "noimpl"
end
