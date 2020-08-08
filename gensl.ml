(* file    : gensl.ml
   created : 2020-06-26 *)

module Basetypes = struct
  type 'a equality = 'a -> 'a -> bool
  type ('a, 'b) assoc = ('a*'b) list
  type 'a set = ('a list)

  type csymb =
    [ | `Toplevel | `Envelop | `Metadata   (* 0..2 *)
      | `Desc | `Hash | `Uuid | `Version   (* 3..6 *)
      | `List | `Vector | `Set | `Map      (* 7..10 *)
      | `Int | `Uint | `Float | `Timestamp (* 11..14 *)
      (* 15..19: reserved *)
      | `Appsymb01 | `Appsymb02 | `Appsymb03 | `Appsymb04 (* 20..23 *)
      | `Appsymb05 | `Appsymb06 | `Appsymb07 | `Appsymb08 (* 24..27 *)
      | `Appsymb09 | `Appsymb10 | `Appsymb11 | `Appsymb12 (* 28..31 *) ]

  let map_assoc : ('a1 -> 'b1) -> ('a2 -> 'b2) -> ('a1, 'a2) assoc -> ('b1, 'b2) assoc =
    fun f g al ->
    let fg : ('a1 * 'a2) -> ('b1 * 'b2) = fun (x, y) -> (f x, g y) in
    (List.map fg al)

  let pair : ('a1 -> 'b1) -> ('a2 -> 'b2) -> ('a1 * 'a2) -> ('b1 * 'b2) =
    fun f g (x, y) -> (f x, g y)

  let pair2 : ('a -> 'b) -> ('a * 'a) -> ('b * 'b) = fun f -> pair f f
  
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

  let sexp_atom =
    let open Format in
    let open Sexplib.Sexp in
    function
    | SymbolAtom str -> Atom ("symb:" ^ str)
    | CodifiedSymbolAtom csymb -> Atom ("csymb:" ^ (name_of_csymb csymb))
    | StringAtom str -> Atom ("str:" ^ str)
    | BytesAtom bytes ->
       let encoded = Bytes.to_string bytes
       in Atom ("bytes:" ^ encoded)
    | NumericAtom (num,suf) -> Atom (num^suf)
    | BoolAtom b -> Atom (sprintf "bool:%b" b)

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

  let composite f g x = f (g x)

    (* XXX csymb_of_name, csymb_of_code *)
end

(* XXX ASCII sanity check/unicode normalization (NFC) on StringAtom *)
(* XXX SymbolAtom could only be alphanumeric so fine for now (NFKC in the future) *)

module Canonicaltree = struct
  open Basetypes
  open Sexplib.Type
  open Sexplib

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
    | CForm { ckwd = (ckwd1); cpos = cpos1 },
      CForm { ckwd = (ckwd2); cpos = cpos2 } ->
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

  let catom atom = CAtom atom
  let cform : (cdatum * cdatum) list -> cdatum list -> cdatum =
    fun keywordeds positionals ->
    CForm { ckwd = (keywordeds :> (cdatum, cdatum) assoc);
            cpos = positionals }

  let rec sexp_cdatum = function
    | CAtom a -> sexp_atom a
    | CForm { ckwd = ckws;
              cpos = cposes } ->
      let sexp_kw = fun (k, v) -> List [Atom "kwnode"; sexp_cdatum k; sexp_cdatum v] in
      List (List.map sexp_kw ckws @ List.map sexp_cdatum cposes)

  let pp_cdatum ppf = composite (Sexp.pp_hum ppf) sexp_cdatum
end

module Normaltree = struct
  open Basetypes
  open Sexplib.Type
  open Sexplib

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
      CForm {ckwd = map_assoc cdatum_of_ndatum cdatum_of_ndatum nkws ;
             cpos = List.map cdatum_of_ndatum nposes}
    | NAnnotated (ndat, _annos) -> cdatum_of_ndatum ndat
        
  and eq_ndatum : ndatum equality = fun a b -> eqv_cdatum (cdatum_of_ndatum a) (cdatum_of_ndatum b)
  let natom atom = NAtom atom
  let nform : (ndatum*ndatum) list -> ndatum list -> ndatum list -> ndatum =
    fun keywordeds positionals annotations ->
    NForm { n_keywordeds = keywordeds;
            n_positionals = positionals;
            n_annotations = annotations }
  let nannotated : ndatum -> ndatum list -> ndatum =
    fun annotated annotations ->
    NAnnotated (annotated, annotations)

  let rec ndatum_of_cdatum : cdatum -> ndatum = function
    | CAtom a -> NAtom a
    | CForm {ckwd = ckws; cpos = cposes} ->
      NForm {n_keywordeds = map_assoc ndatum_of_cdatum ndatum_of_cdatum ckws ;
             n_positionals = List.map ndatum_of_cdatum cposes ;
             n_annotations = [] }

  let rec sexp_ndatum = function
    | NAtom a -> sexp_atom a
    | NForm { n_keywordeds = nkws;
              n_positionals = nposes;
              n_annotations = nanns } ->
      let sexp_kw = fun (k, v) -> List [Atom "kwnode"; sexp_ndatum k; sexp_ndatum v] in
      let sexp_ann = fun dat -> List [Atom "anno"; sexp_ndatum dat] in
      List (List.map sexp_kw nkws @ List.map sexp_ndatum nposes @ List.map sexp_ann nanns)
    | NAnnotated (ndat, nannos) ->
      List (Atom "annotated" :: sexp_ndatum ndat ::
            Atom ":front" ::
            List.map sexp_ndatum nannos)

  let pp_ndatum ppf = composite (Sexp.pp_hum ppf) sexp_ndatum
end

module Datatree = struct
  open Basetypes
  open Sexplib.Type
  open Sexplib

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
      let kws = kws |> List.sort @@ fun (k1,_) (k2,_) ->
                                    let tr = cdatum_of_ndatum in
                                    Canonicaltree.cdatum_ordering (tr k1) (tr k2) in
      let anns = anns |> List.sort @@ fun d1 d2 ->
                                    let tr = cdatum_of_ndatum in
                                    Canonicaltree.cdatum_ordering (tr d1) (tr d2) in
      NForm {n_keywordeds = kws;
             n_positionals = posses;
             n_annotations = anns}
    | DAnnotated {d_annotated = dat;
                  d_anno_front = front_anns;
                  d_anno_back = back_anns} ->
      let front = List.map ndatum_of_ddatum front_anns in
      let back  = List.map ndatum_of_ddatum back_anns  in
      (* XXX sort the annotations *)
      NAnnotated (ndatum_of_ddatum dat, front @ back)

  let eqv_ddatum : ddatum equality =
    fun a b -> eq_ndatum (ndatum_of_ddatum a) (ndatum_of_ddatum b)
  
  let rec ddatum_of_ndatum : ndatum -> ddatum = function
    | NAtom a -> DAtom a
    | NForm {n_keywordeds = nkws;
             n_positionals = nposes;
             n_annotations = nanns} ->
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
          DForm (head :: dkws @ rest @ danns)
      end
    | NAnnotated (ndat, nanns) ->
      let ddat = ddatum_of_ndatum ndat in
      let danns = List.map ddatum_of_ndatum nanns in
      DAnnotated {d_annotated = ddat;
                  d_anno_front = danns;
                  d_anno_back = []}

  let datom atom = DAtom atom
  let dform nodes = DForm nodes
  let dannotated : ddatum -> ddatum list -> ddatum list -> ddatum =
    fun d_ann d_front d_back ->
    DAnnotated { d_annotated = d_ann;
                 d_anno_front = d_front;
                 d_anno_back = d_back }
  let dkeywordnode kw dat = DKeywordNode (kw, dat)
  let ddatumnode d = DDatumNode d
  let dannonode d = DAnnoNode d

  let rec sexp_dnode = function
    | DKeywordNode (k, v) -> List [Atom "kwnode"; sexp_ddatum k; sexp_ddatum v]
    | DDatumNode dat -> sexp_ddatum dat
    | DAnnoNode dat -> List [Atom "anno"; sexp_ddatum dat]
  and sexp_ddatum = function
    | DAtom a -> sexp_atom a
    | DForm nodes -> List (List.map sexp_dnode nodes)
    | DAnnotated { d_annotated = dat ;
                   d_anno_front = fronts ;
                   d_anno_back = backs } ->
      let l = [Atom "annotated"; sexp_ddatum dat]
              @ [Atom ":front"] @ (List.map sexp_ddatum fronts)
              @ [Atom ":back"] @ (List.map sexp_ddatum backs)
      in List l

  let pp_ddatum ppf = composite (Sexp.pp_hum ppf) sexp_ddatum
  
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

  open Datatree
  let rec ddatum_of_pdatum : pdatum -> ddatum = function
    | PAtom { elem = a;
              _} -> DAtom a
    | PForm { elem = (nodes, style, _fixness);
              _ } ->
      begin
        let convert nodes =
          let process acc = function
            | PDecorNode _ -> acc
            | PKeywordNode (k, v) ->
              let node = DKeywordNode (ddatum_of_pdatum k, ddatum_of_pdatum v) in
              node :: acc
            | PDatumNode dat -> DDatumNode (ddatum_of_pdatum dat) :: acc
            | PAnnoNode dat -> DAnnoNode (ddatum_of_pdatum dat) :: acc
          in List.fold_left process [] nodes |> List.rev
        in
        let append_head csymb nodes =
          let head = DDatumNode (DAtom (CodifiedSymbolAtom csymb)) in
          head :: nodes
        in
        match style with
        | ToplevelForm ->
          DForm (append_head `Toplevel (convert nodes))
        | SimpleForm -> DForm (convert nodes)
        | ListForm -> DForm (append_head `List (convert nodes))
        | MapForm -> DForm (append_head `Map (convert nodes))
        | VectorForm _ -> failwith "VectorForm is not yet supported!"
        | SetForm ->
          let process' acc = function
            | PDecorNode _ -> acc
            | PKeywordNode (_, _) -> failwith "Keyword node is invalid in SetForm!"
            | PDatumNode dat ->
              DKeywordNode (ddatum_of_pdatum dat, DAtom (BoolAtom true)) :: acc
            | PAnnoNode dat -> DAnnoNode (ddatum_of_pdatum dat) :: acc
          in
          let nodes = List.fold_left process' [] nodes |> List.rev in
          DForm (append_head `Set nodes)
      end
    | PAnnotated {
        elem = {
          p_annotated  = datum;
          p_anno_front = front;
          p_anno_back  = back;
        };
        _ ; } ->
      DAnnotated {
        d_annotated  = ddatum_of_pdatum datum;
        d_anno_front = List.map ddatum_of_pdatum front;
        d_anno_back  = List.map ddatum_of_pdatum back;
      }

  let rec pnode_of_dnode = function
    | DKeywordNode (k, v) ->
      PKeywordNode (pdatum_of_ddatum k, pdatum_of_ddatum v)
    | DDatumNode dat -> PDatumNode (pdatum_of_ddatum dat)
    | DAnnoNode dat -> PAnnoNode (pdatum_of_ddatum dat)
  and pdatum_of_ddatum : ddatum -> pdatum = function
    | DAtom a -> PAtom {elem = a; repr = `Direct}
    | DForm nodes ->
      let form_type =
        match List.hd nodes with
        | DDatumNode (DAtom (CodifiedSymbolAtom `Toplevel)) ->
          `Toplevel
        | DDatumNode (DAtom (CodifiedSymbolAtom `List)) ->
          `List
        | DDatumNode (DAtom (CodifiedSymbolAtom `Map)) ->
          `Map
        | DDatumNode (DAtom (CodifiedSymbolAtom `Vector)) ->
          failwith "VectorForm handling is not implemented yet!"
        | _ -> `Regular
      in
      let nodes =
        match form_type with
        | `Regular -> List.map pnode_of_dnode nodes
        | _ -> List.map pnode_of_dnode (List.tl nodes)
      in (* List.map pnode_of_dnode nodes in *)
      let elem =
        match form_type with
        | `Regular -> (nodes, SimpleForm, Infix)
        | `Toplevel -> (nodes, ToplevelForm, Infix)
        | `List -> (nodes, ListForm, Infix)
        | `Map -> (nodes, MapForm, Infix)
      in
      PForm {elem = elem; repr = `Direct}
    | DAnnotated { d_annotated  = ddat;
                   d_anno_front = dfront;
                   d_anno_back  = dback } ->
      let pdat = pdatum_of_ddatum ddat in
      let pfront = List.map pdatum_of_ddatum dfront in
      let pback = List.map pdatum_of_ddatum dback in
      let pann = { p_annotated  = pdat;
                   p_anno_front = pfront;
                   p_anno_back  = pback } in
      PAnnotated {elem = pann; repr = `Direct}
  
end

(* XXX move ParsetreePrinter into Parsetree *)
module ParsetreePrinter = struct
  open Basetypes
  open Parsetree
  open Format
  open Sexplib.Type
  open Sexplib

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
       List (Atom Format.(asprintf "#cf:%a" Sexp.pp_hum (sexp_of_form_style fstyle)) :: (nodes |> List.map sexp_pnode))
    | PAnnotated { elem = { p_annotated; p_anno_front; p_anno_back }; _ } ->
       let l = [Atom "annotated"; p_annotated |> sexp_pdatum]
               @ [Atom ":front"] @ (p_anno_front |> List.map sexp_pdatum)
               @ [Atom ":back"] @ (List.rev p_anno_back |> List.map sexp_pdatum)
       in List l

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
