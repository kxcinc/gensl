(* file    : gensl.ml
   created : 2020-06-26 *)

module Basetypes = struct
  type 'a equality = 'a -> 'a -> bool
  type ('a, 'b) assoc = ('a*'b) list*('a equality)
  type 'a set = ('a list)*('a equality)

  let rec map_assoc : ('a1 -> 'b1) -> ('a2 -> 'b2) -> 'b1 equality -> ('a1, 'a2) assoc -> ('b1, 'b2) assoc =
    fun f g b_eq (al, _a_eq) ->
    let fg : ('a1 * 'a2) -> ('b1 * 'b2) = fun (x, y) -> (f x, g y) in
    (List.map fg al, b_eq)

  let rec pair : ('a1 -> 'b1) -> ('a2 -> 'b2) -> ('a1 * 'a2) -> ('b1 * 'b2) =
    fun f g (x, y) -> (f x, g y)

  let pair2 : ('a -> 'b) -> ('a * 'a) -> ('b * 'b) = fun f -> pair f f
  
  type atom =
     | SymbolAtom of string
     | StringAtom of string
     | BytesAtom of bytes
     | NumericAtom of string*string (** [numeric, suffix] *)
     | BoolAtom of bool
end

(* XXX ASCII sanity check/unicode normalization (NFC) on StringAtom *)
(* XXX SymbolAtom could only be alphanumeric so fine for now (NFKC in the future) *)

(* XXX conversion between the four representations *)

module Canonicaltree = struct
  open Basetypes

  type cdatum =
    | CAtom of atom
    | CForm of {
        ckwd : (cdatum, cdatum) assoc;
        cpos : cdatum list;
      }

  (* XXX sexp_* and pp_* *)
end

module Normaltree = struct
  open Basetypes

  type ndatum =
    | NAtom of atom
    | NForm of {
        (* 潰された *)
        n_keywordeds  : (ndatum, ndatum) assoc;
        n_positionals : ndatum list;
        n_annotations : ndatum set;
      }
    | NAnnotated of ndatum * ndatum set

  open Canonicaltree
  (* XXX sexp_* and pp_* *)
  let rec
    cdatum_of_ndatum : ndatum -> cdatum =
    fun nt ->
    match nt with
    | NAtom a -> CAtom a
    | NForm {n_keywordeds = nkws; n_positionals = nposes; n_annotations = _ann} ->
      CForm {ckwd = map_assoc cdatum_of_ndatum cdatum_of_ndatum (=) nkws ;
             cpos = List.map cdatum_of_ndatum nposes}
    | NAnnotated (ndat, _annos) -> cdatum_of_ndatum ndat
        
  and eq_ndatum : ndatum equality = fun a b -> (cdatum_of_ndatum a) = (cdatum_of_ndatum b)
  let natom atom = NAtom atom
  let nform : (ndatum*ndatum) list -> ndatum list -> ndatum list -> ndatum =
    fun keywordeds positionals annotations ->
    NForm { n_keywordeds = (keywordeds, eq_ndatum);
            n_positionals = positionals;
            n_annotations = (annotations, eq_ndatum) }
  let nannotated : ndatum -> ndatum list -> ndatum =
    fun annotated annotations ->
    NAnnotated (annotated, (annotations, eq_ndatum))

  let rec ndatum_of_cdatum : cdatum -> ndatum = function
    | CAtom a -> NAtom a
    | CForm {ckwd = ckws; cpos = cposes} ->
      NForm {n_keywordeds = map_assoc ndatum_of_cdatum ndatum_of_cdatum eq_ndatum ckws ;
             n_positionals = List.map ndatum_of_cdatum cposes ;
             n_annotations = ([], eq_ndatum) }
end

module Datatree = struct
  open Basetypes

  (* "AST" of the data term *)
  type ddatum =
    | DAtom of atom (* an atom *)
    | DForm of dnode list (* a form is represented as a list of nodes *)
    | DAnnotated of {
        d_annotated : ddatum;
        d_anno_front : ddatum list;
        d_anno_back : ddatum list;
      }
  and  dnode =
    | DKeywordNode of ddatum * ddatum
    | DDatumNode of ddatum
    | DAnnoNode of ddatum

  (* XXX sexp_* and pp_* *)
  open Normaltree
  let rec ndatum_of_ddatum : ddatum -> ndatum = function
    | DAtom a -> NAtom a
    | DForm dnodes ->
      let f (acckw, accpos, accann) node =
        begin match node with
          | DKeywordNode (x, y) ->
            (pair2 ndatum_of_ddatum (x, y) :: acckw, accpos, accann)
          | DDatumNode datn ->
            (acckw, (ndatum_of_ddatum datn) :: accpos, accann)
          | DAnnoNode annn ->
            (acckw, accpos, (ndatum_of_ddatum annn) :: accann)
        end
      in
      let (kws, posses, anns) = List.fold_left f ([], [], []) dnodes in
      NForm {n_keywordeds = (kws, eq_ndatum);
             n_positionals = posses;
             n_annotations = (anns, eq_ndatum)}
    | DAnnotated {d_annotated = dat;
                  d_anno_front = front_anns;
                  d_anno_back = back_anns} ->
      let front = List.map ndatum_of_ddatum front_anns in
      let back  = List.map ndatum_of_ddatum back_anns  in
      NAnnotated (ndatum_of_ddatum dat, (front @ back, eq_ndatum))

  let eqv_ddatum : ddatum equality =
    fun a b -> eq_ndatum (ndatum_of_ddatum a) (ndatum_of_ddatum b)
  
  let rec ddatum_of_ndatum : ndatum -> ddatum = function
    | NAtom a -> DAtom a
    | NForm {n_keywordeds = (nkws, _);
             n_positionals = nposes;
             n_annotations = (nanns, _)} ->
      let dkws =
        nkws
        |&> (pair2 ddatum_of_ndatum)
        |&> (fun (x,y) -> DKeywordNode (x,y)) in
      let danns =
        nanns
        |&> ddatum_of_ndatum
        |&> (fun x -> DAnnoNode x) in 
      begin match nposes with
        | [] -> DForm (dkws @ danns)
        | head :: rest ->
          let head = head |> ddatum_of_ndatum |> (fun x -> DDatumNode x) in
          let rest = rest |&> ddatum_of_ndatum |&> (fun x -> DDatumNode x) in
          DForm (head :: dkws @ danns @ rest)
      end
    | NAnnotated (ndat, (nanns, _)) ->
      let ddat = ddatum_of_ndatum ndat in
      let danns = List.map ddatum_of_ndatum nanns in
      DAnnotated {d_annotated = ddat;
                  d_anno_front = danns;
                  d_anno_back = []}
 
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
    | PKeywordNode of 'l pdatum * 'l pdatum
    | PDatumNode of 'l pdatum
    | PAnnoNode of 'l pdatum
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

(* XXX unparse_datum *)
end

(* XXX move ParsetreePrinter into Parsetree *)
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
  let pp_toplevel ppf = function
    | PForm { elem = (nodes, ToplevelForm,_); _ } ->
       let open Format in
       let len = List.length nodes in
       pp_print_flush ppf();
       nodes |> List.iteri (fun i node ->
           Sexp.pp_hum ppf (sexp_pnode node);
           if i+1 = len then pp_print_cut ppf() else pp_print_space ppf())
    | datum -> pp_pdatum ppf datum; pp_print_cut ppf()
end

type datafying_error = ..
exception Datafying_error of datafying_error

type datafying_error +=
   | Datafying_noimpl
