(* file    : gensl.ml
   created : 2020-06-26 *)

module Basetypes = struct
  type 'a equality = 'a -> 'a -> bool
  type ('a, 'b) assoc = ('a*'b) list*('a equality)
  type 'a set = ('a list)*('a equality)

  type csymb =
    [ | `Toplevel | `Envelop | `Metadata   (* 0..2 *)
      | `Desc | `Hash | `Uuid | `Version   (* 3..6 *)
      | `List | `Vector | `Set | `Map      (* 7..10 *)
      | `Int | `Uint | `Float | `Timestamp (* 11..14 *)
      (* 15..19: reserved *)
      | `Appsymb01 | `Appsymb02 | `Appsymb03 | `Appsymb04 (* 20..23 *)
      | `Appsymb05 | `Appsymb06 | `Appsymb07 | `Appsymb08 (* 24..27 *)
      | `Appsymb09 | `Appsymb10 | `Appsymb11 | `Appsymb12 (* 28..31 *) ]
  type atom =
     | SymbolAtom of string
     | CodifiedSymbolAtom of csymb
     | StringAtom of string
     | BytesAtom of bytes
     | NumericAtom of string*string (** [numeric, suffix] *)
     | BoolAtom of bool

  let name_of_csymb = function
    | `Toplevel -> "toplevel" | `Envelop -> "envelop" | `Metadata -> "metadata"
    | `Desc -> "desc" | `Hash -> "hash" | `Uuid -> "uuid" | `Version -> "version"
    | `List -> "list" | `Vector -> "vector" | `Set -> "set" | `Map -> "map"
    | `Int -> "int" | `Uint -> "uint" | `Float -> "float" | `Timestamp -> "timestamp"
    | `Appsymb01 -> "app01" | `Appsymb02 -> "app02" | `Appsymb03 -> "app03" | `Appsymb04 -> "app04"
    | `Appsymb05 -> "app05" | `Appsymb06 -> "app06" | `Appsymb07 -> "app07" | `Appsymb08 -> "app08"
    | `Appsymb09 -> "app09" | `Appsymb10 -> "app10" | `Appsymb11 -> "app11" | `Appsymb12 -> "app12"

  let csymb_of_name = function
    | "toplevel" -> `Toplevel | "envelop" -> `Envelop | "metadata" -> `Metadata
    | "desc" -> `Desc | "hash" -> `Hash | "uuid" -> `Uuid | "version" -> `Version
    | "list" -> `List | "vector" -> `Vector | "set" -> `Set | "map" -> `Map
    | "int" -> `Int | "uint" -> `Uint | "float" -> `Float | "timestamp" -> `Timestamp
    | "app01" -> `Appsymb01 | "app02" -> `Appsymb02 | "app03" -> `Appsymb03 | "app04" -> `Appsymb04
    | "app05" -> `Appsymb05 | "app06" -> `Appsymb06 | "app07" -> `Appsymb07 | "app08" -> `Appsymb08
    | "app09" -> `Appsymb09 | "app10" -> `Appsymb10 | "app11" -> `Appsymb11 | "app12" -> `Appsymb12
    | _ -> raise Not_found

  let code_of_csymb csymb =
    let rec find x k = function
      | [] -> None
      | head :: _ when head = x -> Some k
      | _ :: rest -> find x (k+1) rest in
    let (kstd, standard) = 0, [
        `Toplevel; `Envelop; `Metadata;
        `Desc; `Hash; `Uuid; `Version;
        `List; `Vector; `Set; `Map;
        `Int; `Uint; `Float; `Timestamp;
      ] in
    let (kapp, application) = 20, [
     `Appsymb01; `Appsymb02; `Appsymb03; `Appsymb04;
     `Appsymb05; `Appsymb06; `Appsymb07; `Appsymb08;
     `Appsymb09; `Appsymb10; `Appsymb11; `Appsymb12;
      ] in
    (match find csymb kstd standard with
     | Some code -> Some code
     | None -> find csymb kapp application)
    |> Option.get

  let kind_of_csymb csymb =
    match code_of_csymb csymb with
    | code when code >= 0 && code < 20 -> `Standard
    | code when code >= 20 && code < 32 -> `Application
    | _ -> raise Not_found

  let csymb_of_sexp =
    let open Sexplib.Conv_error in
    let open Sexplib.Sexp in
    let prefixed pre str = pre = Str.string_before str (String.length pre) in
    let prefix = "csymb:" in
    function | Atom str when prefixed prefix str ->
                let name = Str.string_before str (String.length prefix)
                in csymb_of_name name
             | sexp -> unexpected_stag "?csymb" sexp

  let sexp_of_csymb csymb =
    let open Sexplib.Sexp in
    Atom ("csymb:" ^ (name_of_csymb csymb))

  (** !!this is to serve as the specification of atom ordering *)
  let compare_atom : atom -> atom -> int = fun a1 a2 ->
    let catprec = function
     | CodifiedSymbolAtom _ -> 0
     | SymbolAtom _ -> 1
     | BoolAtom _ -> 2
     | NumericAtom _ -> 3
     | StringAtom _ -> 4
     | BytesAtom _ -> 5 in
    let (>>=) x f = if x <> 0 then x else f() in
    compare (catprec a1)  (catprec a2) >>= fun () ->
    match a1, a2 with
    | CodifiedSymbolAtom c1, CodifiedSymbolAtom c2 ->
       compare (code_of_csymb c1) (code_of_csymb c2)
    | SymbolAtom s1, SymbolAtom s2 ->
       (* XXX use a unicode comparison function *)
       compare s1 s2
    | StringAtom s1, StringAtom s2 ->
       (* XXX use a unicode comparison function *)
       compare s1 s2
    | BoolAtom b1, BoolAtom b2 -> compare b1 b2 (* as per ocaml, false < true *)
    | NumericAtom (num1,suf1), NumericAtom (num2,suf2) ->
       (* note that we don't interpret the number part *)
       compare num1 num2 >>= fun () -> compare suf1 suf2
    | BytesAtom b1, BytesAtom b2 ->
       let len = Bytes.length in
       let tr bytes = bytes |> Bytes.to_seq |> List.of_seq in
       compare (len b1) (len b2) >>= fun () -> compare (tr b1) (tr b2)
    | _ -> failwith ("panic: "^__LOC__)

    (* XXX csymb_of_name, csymb_of_code *)
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

  (** !!this is to serve as the specification of atom ordering *)
  let rec cdatum_ordering : cdatum -> cdatum -> int = fun x y ->
    (* CAtom < CForm *)
    match x, y with
    | CAtom a1, CAtom a2 -> compare_atom a1 a2
    | CAtom _, CForm _ -> -1
    | CForm _, CAtom _ -> 1
    | CForm { ckwd = (ckwd1,_); cpos = cpos1 },
      CForm { ckwd = (ckwd2,_); cpos = cpos2 } ->
       let open struct
             type node = Kw of cdatum*cdatum | Pos of cdatum
             let mkkw (k,v) = Kw (k,v) and mkpos x = Pos x
           end in
       let compare_kw (k1,_) (k2,_) = cdatum_ordering k1 k2 in
       let compare_node n1 n2 = match n1, n2 with
         (* Kw < Pos *)
         | Kw _, Pos _ -> -1
         | Pos _, Kw _ -> 1
         | Kw (k1,v1), Kw (k2,v2) -> compare_kw (k1,v1) (k2,v2)
         | Pos d1, Pos d2 -> cdatum_ordering d1 d2 in
       (* XXX maybe we should throw when either ckwd has duplications *)
       let (ckwd1, ckwd2) =
         let sort = List.sort_uniq compare_kw
         in sort ckwd1, sort ckwd2 in
       let combine kwd pos =
         let kwd = kwd |&> mkkw and pos = pos |&> mkpos in
         match pos with
         | head :: tail -> head :: kwd @ tail
         | [] -> pos in
       let compare = function
         | [], [] -> 0
         | _, [] -> 1
         | [], _ -> -1
         | h1::_, h2::_ -> compare_node h1 h2 in
       compare (combine ckwd1 cpos1, combine ckwd2 cpos2)

  (** semantical equivalence of two datums *)
  let eqv_cdatum x y = cdatum_ordering x y = 0

  (* XXX sexp_* and pp_* *)
end

module Normaltree = struct
  open Basetypes

  type ndatum =
    | NAtom of atom
    | NForm of {
        n_keywordeds  : (ndatum, ndatum) assoc;
        n_positionals : ndatum list;
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
    | DKeywordNode of ddatum * ddatum
    | DDatumNode of ddatum
    | DAnnoNode of ddatum

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
    | ListForm                  (**   [ .. ] *)
    | VectorForm                (** #k[ .. ], k could be omitted *)
    | MapForm                   (**   { .. } *)
    | SetForm                   (**  #{ .. } *)
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
    | CodifiedSymbolAtom csymb -> Atom ("csymb:" ^ (name_of_csymb csymb))
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
