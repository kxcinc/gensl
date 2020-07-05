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

(* XXX conversion between the four representations *)

module Canonicaltree = struct
  open Basetypes

  type cdatum =
    | CAtom of atom
    | CForm of {
        cpos : cdatum list;
        ckwd : (cdatum, cdatum) assoc;
      }

  (* XXX sexp_* and pp_* *)
end

module Normaltree = struct
  open Basetypes

  type ndatum =
    | NAtom of atom
    | NForm of {
        n_positionals : ndatum list;
        n_keywordeds  : (ndatum, ndatum) assoc;
        n_annotations : ndatum set;
      }
    | NAnnotated of ndatum * ndatum set

  (* XXX sexp_* and pp_* *)
end

module Datatree = struct
  open Basetypes

  type ddatum =
    | DAtom of atom
    | DForm of dnode list
    | DAnnotated of {
        d_annotated : ddatum;
        d_anno_front : ddatum list;
        d_anno_back : ddatum list;
      }
  and  dnode =
    | DDatumNode of ddatum
    | DAnnoNode of ddatum
    | DKeywordNode of ddatum * ddatum

  (* XXX sexp_* and pp_* *)
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
       let encoded = Bytes.to_string bytes
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

type datafying_error = ..
exception Datafying_error of datafying_error

type datafying_error +=
   | Datafying_noimpl
