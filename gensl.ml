type 'a equality = 'a -> 'a -> bool
type ('a, 'b) assoc = ('a*'b) list*('a equality)
type 'a set = ('a list)*('a equality)

module Datatree = struct
  type datum =
    | Atom of atom
    | Annotated of datum set*datum
    | Form of
        { psnodes : node list;          (** positional nodes *)
          kwnodes : (kw, node) assoc; } (** keyword nodes *)
  and  node =
    | DatumNode of datum        (** datum node *)
    | AnnoNode of datum         (** annotation node *)
  and  kw = KW of string
  and  atom =
     | SymbolAtom of string
     | StringAtom of string
     | BytesAtom of bytes
     | NumericAtom of string*string (** (pair numeric suffix) *)

  let datum_equality = (=)
  let keyword_equality : kw equality = (=)
  let annotation_equality _ _ = false

  let pp_kw ppf (KW kw) = Format.fprintf ppf ":%s" kw
  let pp_atom ppf =
    let open Format in
    let pstr = pp_print_string ppf in
    let printf = fprintf ppf in
    function
     (* XXX handle complex symbols *)
     | SymbolAtom str -> pstr str
     (* XXX better display for very complex strings *)
     | StringAtom str -> printf "\"%s\"" (String.escaped str)
     | BytesAtom bytes -> printf "hex:%s" (Bytes.to_string  bytes |> Base64.encode_exn)
     | NumericAtom (num, suffix) -> num^suffix |> pstr

  module Atoms = struct
    let symb x = Atom (SymbolAtom x)
    let atom_of_int ?suffix x =
      let suffix = Option.value ~default:"" suffix in
      Atom (NumericAtom ((string_of_int x), suffix))
  end

  module StdSymbols = struct
    open Atoms

    let finlist = symb "finlist"
    let karray = symb "karray"
    let finset = symb "finset"
    let finmap = symb "finmap"
  end
end

module Parsetree = struct
  open Datatree

  type leading = Leading of string
  type ghost_source = ..
  type 'l span = {
      span_start: 'l;
      span_end  : 'l;
      span_leading : leading;   (** leading spaces *)
      span_source  : [ `File of string
                     | `DirectInput of string option
                     | `Ghost of ghost_source ];
    }

  type flat_location = { line: int; col: int; }
  type stream_location = int
  type flat_span = flat_location span
  type stream_span = stream_location span

  type parse_error = ..

  type syntax_mode =
    | Commonfix | Phantomfix
    | Prefix  of [ `GrabAll | `GrabOne | `GrabK of int ]
    | Postfix of [ `GrabAll | `GrabOne | `GrabK of int ]
  type form_style =
    | ToplevelForm
    | SimpleForm                (**   ( .. ) *)
    | FinlistForm               (**   [ .. ] *)
    | KarrayForm of int option  (** #d[ .. ] *)
    | FinmapForm                (**   { .. } *)
    | FinsetForm                (**  #{ .. } *)
  (* XXX support lexer_style *)
  type lexer_style =
    | DefaultLexer
    | DataLexer     (**  lexp:.. *)
    | HandlerLexer  (** ?lexp:.. *)

  (** phantom elements,
      phantom in the sense that
      they don't semantically contribute to the Datatree *)
  type phantom = 
    | GrabPoint (** .    - the postfix grab-point *)
    | EndOfDoc  (** .||  - the end-of-doc mark *)
    | Mapsto    (** ->   - the "mapsto arrow" *)
    | SepComma  (** XXX, - the "separator comma" *)
    | Hole of string option  (** ??name - a hole to be filled *)
    | ParseError of parse_error

  type 'l pdatum =
    | PAtom of 'l patom*lexer_style
    | PAnnotated of { p_annotated  : 'l pdatum;
                      p_anno_front : 'l pdatum list;
                      p_anno_back  : 'l pdatum list; }
    | PForm of (('l pnode list, 'l) pe*form_style*lexer_style, 'l) pe
  and  'l pnode =
    | PDatumNode of 'l pdatum
    | PAnnoNode of 'l pdatum
    | PKeywordNode of 'l pkw * 'l pdatum
    | PPhantomNode of (phantom, 'l) pe
  and  'l pkw = (kw, 'l) pe
  and  'l patom = (atom, 'l) pe*[ `Source of string | `Intrinsic ]

  and  ('x, 'l) pe = { elem: 'x; mode: syntax_mode; span: 'l span }

  type 'x fpe = ('x, flat_location) pe
  type 'x spe = ('x, stream_location) pe
end

open Datatree
open Parsetree

type datafying_error = ..
exception Datafying_error of datafying_error

type datafying_error +=
   | Datafying_noimpl

open struct
  let datum_set : datum list -> datum set = fun xs -> (xs, datum_equality)
  let annotation_set : datum list -> datum set = fun xs -> (xs, annotation_equality)
end
[@@ocaml.warning "-32"]

(* XXX : report error and hole-phantoms - now they are simply ignored *)
let rec datum_of_parsetree : 'l pdatum -> datum = function
  | PAtom ({ elem; _ }, _) -> Atom elem
  | PAnnotated { p_annotated = elem;
                 p_anno_front = front;
                 p_anno_back = back; } ->
     let annotations =
       let tr = List.map datum_of_parsetree in
       (tr front) @ (tr back) |> annotation_set
     in Annotated (annotations, (datum_of_parsetree elem))
  | PForm { elem = ({ elem = nodes; _ }, style); _} ->
     let psnodes : node list =
       let rec loop acc = function
         | [] -> acc
         | PDatumNode datum :: rest ->
            loop (DatumNode (datum_of_parsetree datum) :: acc) rest
         | PAnnoNode datum :: rest ->
            loop (AnnoNode (datum_of_parsetree datum) :: acc) rest
         | _ :: rest -> loop acc rest
       in loop [] nodes in
     let kwnodes : (kw, node) assoc =
       let rec loop acc = function
         | [] -> acc
         | PKeywordNode ({ elem = kw; _ }, datum) :: rest ->
            loop ((kw, DatumNode (datum_of_parsetree datum)) :: acc) rest
         | _ :: rest -> loop acc rest
       in (loop [] nodes, keyword_equality) in
     let psnodes =
       let open Atoms in
       let open StdSymbols in
       match style with
       | ToplevelForm | SimpleForm -> psnodes
       | FinlistForm -> (DatumNode finlist) :: psnodes
       | KarrayForm k ->
          let k = Option.value ~default:1 k in
          (DatumNode karray) :: (DatumNode (atom_of_int k)) :: psnodes
       | FinmapForm -> (DatumNode finmap) :: psnodes
       | FinsetForm -> (DatumNode finset) :: psnodes
     in Form { psnodes; kwnodes; }

(* XXX support preserve_leadings *)
let rec pp_parsetree ?preserve_leadings:_ ppf pdatum =
  let open Format in
  let print datum = pp_parsetree ppf datum; pp_print_space ppf () in
  let mode = match pdatum with
  | PAtom ({ mode; _ }, _) -> Some mode
  | PForm { mode; _ } -> Some mode
  | _ -> None in
  let prefix = begin match mode with
  | None | Some Commonfix | Some Phantomfix -> ""
  | Some (Prefix `GrabAll) -> ",,"
  | Some (Prefix `GrabOne) -> ","
  | Some (Prefix (`GrabK k)) -> sprintf ",%d." k
  | Some (Postfix `GrabAll) -> ".."
  | Some (Postfix `GrabOne) -> "."
  | Some (Postfix (`GrabK k)) -> sprintf ".%d." k
  end in
  pp_print_string ppf prefix;
  match pdatum with
  | PAtom (_, `Source src) -> pp_print_string ppf src
  | PAtom ({ elem; _ }, `Intrinsic) -> pp_atom ppf elem
  | PAnnotated { p_annotated = elem;
                 p_anno_front = front;
                 p_anno_back = back; } ->
     (* XXX optimize space emission *)
     List.iter print front;
     print elem;
     List.iter print back
  | PForm { elem = ({ elem = nodes; _ }, style); _ } ->
     let (opening, closing) = match style with
       | ToplevelForm -> "", ""
       | SimpleForm -> "(", ")"
       | FinlistForm -> "[", "]"
       | KarrayForm k -> begin
           match k with
           | Some k -> sprintf "#%d[" k, "]"
           | None ->  "#[", "]"
         end
       | FinmapForm -> "{", "}"
       | FinsetForm -> "#{", "}"
     in
     let print_node = function
       | PDatumNode datum -> print datum
       | PAnnoNode anno -> pp_print_string ppf "@@"; print anno
       | PKeywordNode ({ elem = kw; _ }, datum) ->
          pp_kw ppf kw;
          pp_print_space ppf ();
          print datum
       | PPhantomNode { elem = phantom; _ } ->
          let str = match phantom with
            | GrabPoint -> "."
            | EndOfDoc -> ".||"
            | Mapsto -> "->"
            | SepComma -> ","
            | Hole var ->
               let var = Option.value ~default:"" var in
               "??" ^ var
            | ParseError _err -> "<*parse_error*>"
          in pp_print_string ppf str in
     pp_print_string ppf opening;
     pp_print_cut ppf ();
     List.iter print_node nodes;
     pp_print_cut ppf ();
     pp_print_string ppf closing
