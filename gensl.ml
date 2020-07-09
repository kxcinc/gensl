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
  open Sexplib.Std

  type parse_error = ..

  (* syntax_mode and form_style should only concern forms *)
  type form_fixness =
    | Infix
    | Prefix  of [ `PickAll | `PickOne | `PickK of int ]*bool (* with-head-node? *)
    | Postfix of [ `GrabAll | `GrabOne | `GrabK of int ]*bool (* with-head-node? *)
  type form_style =
    | ToplevelForm
    | SimpleForm                (**   ( .. ) *)
    | ListForm                  (**   [ .. ] *)
    | VectorForm of int option  (** #k[ .. ], k could be omitted *)
    | MapForm                   (**   { .. } *)
    | SetForm                   (**  #{ .. } *)
  [@@deriving sexp]

  (** decor elements, who exists in the wirestring but
      not semantically contributing to the Datatree *)
  type decor =
    | GrabPoint        (** .  - the postfix grab-point *)
    | CommaSeparator   (** ,  - the comma separator *)
    | ParseError of parse_error

  (** a phantom is an element that is a desugering of other
      elements in the wirestring, that doesn't not appear directly
       representation in the wirestring *)
  type representation = [ `Direct | `Phantom | `ReaderMacro of string*macro_body ]
  (* XXX leading on `Direct and `ReaderMacro *)

  and  macro_body =
    | LiteralMacroBody of string
    | StringMacroBody of string
    | FormMacroBody of pform

  and  pdatum =
    | PAtom of patom
    | PForm of pform
    | PAnnotated of pannotated_record pelem
  and  pnode =
    | PKeywordNode of pdatum * pdatum
    | PDatumNode of pdatum
    | PAnnoNode of pdatum
    | PDecorNode of decor pelem
  and  patom = atom pelem
  and  pform = (pnode list*form_style*form_fixness) pelem
  and  pannotated_record = {
        p_annotated  : pdatum;
        p_anno_front : pdatum list;
        p_anno_back  : pdatum list; (** !!reversed *)
      }

  and  'x pelem = {
      elem: 'x;
      repr: representation;
      (* XXX span *)
    }

  let patom atom repr : patom = { elem = atom; repr; }
  let pdatum_atom atom repr : pdatum =
    PAtom (patom atom repr)
  let pdatum_form nodes fstyle fix repr : pdatum =
    let elem = (nodes, fstyle, fix) in
    PForm { elem; repr; }
  let pdatum_annofront anno datum : pdatum = match datum with
    | PAnnotated { elem = { p_anno_front; _ } as r; _} ->
       PAnnotated ({ elem = { r with p_anno_front = anno :: p_anno_front };
                     repr = `Direct })
    | _ -> PAnnotated {
               elem = {
                 p_annotated = datum;
                 p_anno_front = [anno];
                 p_anno_back = [];
               };
               repr = `Direct; }
  let pdatum_annoback anno datum : pdatum = match datum with
    | PAnnotated { elem = { p_anno_back; _ } as r; _} ->
       PAnnotated ({ elem = { r with p_anno_back = anno :: p_anno_back };
                     repr = `Direct })
    | _ -> PAnnotated {
               elem = {
                 p_annotated = datum;
                 p_anno_front = [];
                 p_anno_back = [anno];
               };
               repr = `Direct; }
  let pnode_decor decor : pnode = PDecorNode { elem = decor; repr = `Direct }
end

module Unparse = struct
  open Parsetree
  open Basetypes
  open Format

  type anypelem = AnyParsetreeElement : _ pelem -> anypelem

  open struct
    let pstr = pp_print_string
    let pint = pp_print_int
    let psp ppf = pp_print_space ppf ()
    let pcut ppf = pp_print_cut ppf()
    let any elem = AnyParsetreeElement elem
    let pmore pp ppf : 'x list -> unit = fun list ->
      list |> List.iteri @@ fun i x -> if i<>0 then psp ppf; pp ppf x
  end

  let unparse_atom ppf : atom -> unit = function
    | SymbolAtom s -> pstr ppf s
    | CodifiedSymbolAtom csymb ->
       let prefix = match kind_of_csymb csymb with
         | `Standard -> "!"
         | `Application -> "!!" in
       fprintf ppf "%s%s" prefix (name_of_csymb csymb)
    | StringAtom str -> fprintf ppf "\"%s\"" (String.escaped str)
    | BytesAtom b -> fprintf ppf "strbytes:\"%s\"" (b |> Bytes.to_string |> String.escaped)
    | NumericAtom (num, suffix) -> fprintf ppf "%s%s" num suffix
    | BoolAtom b -> pstr ppf (if b then "bool:true" else "bool:false")

  let rec unparse_pelem_common ?fxnconv ppf : anypelem -> unit = function
    | AnyParsetreeElement elem -> begin
        elem |> function
        |  { repr = `Phantom; _ } -> ()
        | { repr = `ReaderMacro (ext, LiteralMacroBody str); _ } ->
           fprintf ppf "%s:%s" ext str
        | { repr = `ReaderMacro (ext, StringMacroBody str); _ } ->
           fprintf ppf "%s:\"%s\"" ext (String.escaped str)
        | { repr = `ReaderMacro (ext, FormMacroBody form); _ } ->
           fprintf ppf "%s:%a" ext (unparse_pform ?fxnconv) form
      end
  [@@ocaml.warning "-8"]

  and unparse_patom ?fxnconv ppf : patom -> unit = function
    | { elem; repr = `Direct } -> unparse_atom ppf elem
    | elem -> unparse_pelem_common ?fxnconv ppf (any elem)

  and unparse_pdatum ?fxnconv ppf : pdatum -> unit = function
    | PAtom atom -> unparse_patom ?fxnconv ppf atom
    | PForm form -> unparse_pform ?fxnconv ppf form
    | PAnnotated { elem = { p_annotated = dtm;
                            p_anno_front = front;
                            p_anno_back = back };
                   repr = `Direct } ->
       let pannos kind list = pmore (unparse_anno ?fxnconv kind) ppf list in
       pannos `Next front; psp ppf;
       unparse_pdatum ?fxnconv ppf dtm; psp ppf;
       pannos `Previous back
    | PAnnotated elem -> unparse_pelem_common ?fxnconv ppf (any elem)

  and unparse_anno ?fxnconv kind ppf : pdatum -> unit = fun dtm ->
    let leading_symbol = match kind with
      | `Standalone -> "@"
      | `Previous -> "@<"
      | `Next -> "@>" in
    pstr ppf leading_symbol; pcut ppf; unparse_pdatum ?fxnconv ppf dtm

  and unparse_pform ?fxnconv ppf : pform -> unit =
    function
    | { repr = (`Phantom | `ReaderMacro _); _ } as elem -> unparse_pelem_common ?fxnconv ppf (any elem)
    | { repr = `Direct;
        elem = (nodes, style, fxn) } -> begin
        let headnodes withhead = match withhead, nodes with
           | true, PDatumNode head :: rest -> Some head, rest
           | false, _ -> None, nodes
           | _ -> failwith ("panic: "^__LOC__) in
        let pohead' prefix = function
            | Some head -> pstr ppf prefix; unparse_pdatum ?fxnconv ppf head
            | None -> () in
        let cleaned =
          let rec loop acc = function
            | PDecorNode _ :: rest -> loop acc rest
            | node :: rest -> loop (node :: acc) rest
            | [] -> List.rev acc in
          loop [] nodes in
        let head_first() = match cleaned with
          | PDatumNode (PAtom _) :: _ -> true | _ -> false in
        let headsingleton() = match cleaned with
          | [PDatumNode (PAtom _); PDatumNode _] -> true | _ -> false in
        let singlepos() = match cleaned with
          | [PDatumNode _] -> true | _ -> false in
        let pohead = pohead' "" in
        let (nodes, fxn) = match style, fxnconv with
          | _, None -> nodes, fxn
          | (ToplevelForm | ListForm | VectorForm _ | MapForm | SetForm ), _ -> nodes, fxn
          | SimpleForm, Some `Infix -> cleaned, Infix
          | SimpleForm, Some `Prefix -> cleaned,
             if singlepos() then Prefix (`PickK 1, false)
             else if headsingleton() then Prefix (`PickOne, true)
             else if head_first() then Prefix (`PickK (List.length cleaned - 1), true)
             else Prefix (`PickK (List.length cleaned), false)
          | SimpleForm, Some `Postfix -> cleaned, 
             if singlepos() then Postfix (`GrabK 1, false)
             else if headsingleton() then Postfix (`GrabOne, true)
             else if head_first() then Postfix (`GrabK (List.length cleaned - 1), true)
             else Postfix (`GrabK (List.length cleaned), false)
        in
         match style, fxn with
         | ToplevelForm, Infix -> pmore (unparse_pnode ?fxnconv) ppf nodes
         | SimpleForm, Infix ->
            pstr ppf "("; pcut ppf;
            pmore (unparse_pnode ?fxnconv) ppf nodes;
            pcut ppf; pstr ppf ")"
         | ListForm, Infix ->
            pstr ppf "["; pcut ppf;
            pmore (unparse_pnode ?fxnconv) ppf nodes;
            pcut ppf; pstr ppf "]"
         | VectorForm k, Infix ->
            let k = Option.(map string_of_int k |> value ~default:"") in
            fprintf ppf "#%s[" k; pcut ppf;
            pmore (unparse_pnode ?fxnconv) ppf nodes;
            pcut ppf; pstr ppf "]"
         | MapForm, Infix ->
            pstr ppf "{"; pcut ppf;
            pmore (unparse_pnode ?fxnconv) ppf nodes;
            pcut ppf; pstr ppf "}"
         | SetForm, Infix ->
            pstr ppf "#{"; pcut ppf;
            pmore (unparse_pnode ?fxnconv) ppf nodes;
            pcut ppf; pstr ppf "}"
         | (ToplevelForm | ListForm | VectorForm _ | MapForm | SetForm ), _ ->
            failwith ("panic: "^__LOC__)
         | SimpleForm, Prefix (`PickAll, wh) ->
            let (head, nodes) = headnodes wh in
            pstr ppf ",,"; pohead head; psp ppf;
            pmore (unparse_pnode ?fxnconv) ppf nodes
         | SimpleForm, Prefix (`PickOne, wh) ->
            let (head, nodes) = headnodes wh in
            pstr ppf ","; pohead head; psp ppf;
            pmore (unparse_pnode ?fxnconv) ppf nodes
         | SimpleForm, Prefix (`PickK k, wh) ->
            let (head, nodes) = headnodes wh in
            pstr ppf ","; pint ppf k; pohead' "." head; psp ppf;
            pmore (unparse_pnode ?fxnconv) ppf nodes
         | SimpleForm, Postfix (`GrabAll, wh) ->
            let (head, nodes) = headnodes wh in
            pmore (unparse_pnode ?fxnconv) ppf nodes; psp ppf;
            pstr ppf ".."; pohead head;
         | SimpleForm, Postfix (`GrabOne, wh) ->
            let (head, nodes) = headnodes wh in
            pmore (unparse_pnode ?fxnconv) ppf nodes; psp ppf;
            pstr ppf "."; pohead head;
         | SimpleForm, Postfix (`GrabK k, wh) ->
            let (head, nodes) = headnodes wh in
            pmore (unparse_pnode ?fxnconv) ppf nodes; psp ppf;
            pstr ppf "."; pint ppf k; pohead' "." head;
      end

  and unparse_pnode ?fxnconv ppf : pnode -> unit = function
    | PKeywordNode (k,v) -> pstr ppf ":"; unparse_pdatum ?fxnconv ppf k; psp ppf; unparse_pdatum ?fxnconv ppf v
    | PDatumNode dtm -> unparse_pdatum ?fxnconv ppf dtm
    | PAnnoNode anno -> unparse_anno ?fxnconv `Standalone ppf anno
    | PDecorNode { elem = GrabPoint; _ } -> pstr ppf "."
    | PDecorNode { elem = CommaSeparator; _ } -> pstr ppf ","
    | PDecorNode { elem = (ParseError _); _ } -> pstr ppf "??parse_error"
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

  let sexp_decor = function
    | GrabPoint -> Atom "decor:GrabPoint"
    | CommaSeparator -> Atom "decor:CommaSeparator"
    | ParseError _ -> Atom "decor:ParseError(_)"

  let rec sexp_pnode = function
    | PDatumNode dtm -> sexp_pdatum dtm
    | PAnnoNode dtm -> List [Atom "anno"; sexp_pdatum dtm]
    | PKeywordNode (kw,value) -> List [Atom "kwnode"; sexp_pdatum kw; sexp_pdatum value]
    | PDecorNode { elem = d; _ } -> sexp_decor d

  and     sexp_pdatum = function
    | PAtom patom -> sexp_patom patom
    | PForm { elem = (nodes, SimpleForm, _) ; _ } -> List (nodes |> List.map sexp_pnode)
    | PForm { elem = (nodes, fstyle, _) ; _ } ->
       List (Atom "#cf" :: (sexp_of_form_style fstyle) :: (nodes |> List.map sexp_pnode))
    | PAnnotated { elem = { p_annotated; p_anno_front; p_anno_back }; _ } ->
       let l = [Atom "annotated"; p_annotated |> sexp_pdatum]
               @ [Atom ":front"] @ (p_anno_front |> List.map sexp_pdatum)
               @ [Atom ":back"] @ (List.rev p_anno_back |> List.map sexp_pdatum)
       in List l

  let composite f g x = f (g x)
  let pp_patom ppf = composite (Sexp.pp_hum ppf) sexp_patom
  let pp_atom ppf = composite (Sexp.pp_hum ppf) sexp_atom
  let pp_pdatum ppf = composite (Sexp.pp_hum ppf) sexp_pdatum
  let pp_toplevel ppf = function
    | PForm { elem = (nodes, ToplevelForm, _); _ } ->
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
