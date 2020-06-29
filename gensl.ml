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

  type ('x, 'loc) kresult = ('x, (parse_error*'loc span) list) result
  type 'loc pkont = 'loc pnode list -> ('loc pdatum, 'loc) kresult
  type ('buf, 'loc) picking_frame =
    | PfPickAll of 'loc pkont
    | PfPickK of int*('loc pkont)
  type 'loc frame_state = { pickduty : int; bucket : 'loc pdatum }
  type ('buf, 'loc) pstate = {
      buf : 'buf;
      pos : int;
    }
  type ('x, 'buf, 'loc) presult = ('x*('buf, 'loc) pstate, (parse_error*'loc span) list) result
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
 | Unexpected_ending_of_form
 | Immature_ending_of_form of int
 | No_enough_nodes_to_grab of { expected : int; available : int; }
 | Attempting_to_annotate_non_datum
 | Previous_datum_to_annotate_not_exists

module Parser (Lexer : Lexer) = struct
  open Lexer

  type nonrec picking_frame = (buffer, location) picking_frame
  type nonrec pkont = location pkont

  type nonrec pdatum = location pdatum
  type nonrec pnode = location pnode
  type nonrec patom = location patom
  type nonrec 'x pelem = ('x, location) pelem

  type nonrec pstate = (buffer, location) pstate
  type nonrec 'x presult = ('x, buffer, location) presult
  type nonrec 'x kresult = ('x, location) kresult

  let lexer { buf; pos; _ } = lexer buf pos

  open struct
    let (>>=) : 'x kresult -> ('x -> 'y kresult) -> 'y kresult = Result.bind
    let ok ps x = Ok (x,ps)
    let fail span err : 'x presult = Error [err, span]
    let kont_ok x = Ok x
    let kont_fail span err : 'x kresult = Error [err, span]
    (* XXX lift_result might not be a good name *)
    let lift_result ps : 'x kresult -> 'x presult = function
      | Ok x -> Ok (x, ps)
      | Error err -> Error err
    module [@ocaml.warning "-32"] List = struct
      include List
      let split n : 'a list -> 'a list*'a list = fun l ->
        let rec loop l acc n =
          if n = 0 then (List.rev acc), l
          else match l with
               | hd :: tail -> loop tail (hd :: acc)(n-1)
               | [] -> raise (Invalid_argument "list too short")
        in loop l [] n
      let take n : 'a list -> 'a list = fun l -> split n l |> fst
    end
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
       let kont = kont_simple_form (span ps leading) Infix
       in read_nodes (PfPickAll kont) ps
    | (TkParenClose, leading), ps
    | (TkPickAll _, leading), ps | (TkGrabAll _, leading), ps
    | (TkPickK _, leading), ps | (TkGrabK _, leading), ps
    | (TkPickOne _, leading), ps | (TkGrabOne _, leading), ps
    | (TkGrabPoint, leading), ps
    | (TkKeywordIndicator, leading), ps
    | (TkAnnoPrevIndicator, leading), ps
    | (TkAnnoStandaloneIndicator, leading), ps
      -> Unexpected_ending_of_form |> fail (span ps leading)
    | (TkAnnoNextIndicator, leading), ps ->
       read_datum ps >>= fun (anno, ps') ->
       let kont : pkont = function
         | [node] ->
            begin match node with
            | PDatumNode datum -> pdatum_anno_front anno datum |> kont_ok
            | _ -> Attempting_to_annotate_non_datum |> kont_fail (span ps' leading)
            end
         | _ -> failwith ("panic: " ^ __LOC__)
       in
       read_nodes (PfPickK (1, kont)) ps'
  and     read_nodes : picking_frame -> pstate -> pdatum presult =
    fun pf ps ->
    let span ps' leading =
      let span_source = source ps.buf in
      let span_start = loc ps.buf ps.pos in
      let span_end = loc ps.buf ps'.pos in
      { span_start; span_end; span_source; span_leading = leading }
    in
    let (duty, kont) = match pf with
      | PfPickAll kont -> (-1, kont)
      | PfPickK (k, kont) -> (k, kont) in
    (* XXX spans might be inaccurate at several places *)
    let rec loop duty bucket ps0 =
      let push_node node ps = loop (duty-1) (node :: bucket) ps in
      let push_datum datum ps = push_node (PDatumNode datum) ps in
      let finish_with_kont ps = kont (List.rev bucket) |> lift_result ps in
      if duty = 0 then finish_with_kont ps0
      else begin
          let rec go mode lexer_result =
            let mode m = Option.value ~default:m mode in
            match lexer_result with
            | (TkParenClose, leading), ps ->
               if duty > 0
               then Immature_ending_of_form duty |> fail (span ps leading)
               else finish_with_kont ps
            | (TkPickAll false, leading), ps ->
               let kont = kont_simple_form (span ps leading) (Prefix `PickAll |> mode) in
               read_nodes (PfPickAll kont) ps >>= fun (datum, ps) ->
               push_datum datum ps
            | (TkPickAll true, leading), ps ->
               read_datum ps >>= fun (head, ps) ->
               let kont = kont_simple_form_head head (span ps leading) (Prefix `PickAll |> mode) in
               read_nodes (PfPickAll kont) ps >>= fun (datum, ps) ->
               push_datum datum ps
            | (TkPickK (false, k), leading), ps ->
               let kont = kont_simple_form (span ps leading) (Prefix (`PickK k) |> mode) in
               read_nodes (PfPickK (k, kont)) ps >>= fun (datum, ps) ->
               push_datum datum ps
            | (TkPickK (true, k), leading), ps ->
               read_datum ps >>= fun (head, ps) ->
               let kont = kont_simple_form_head head (span ps leading) (Prefix (`PickK k) |> mode) in
               read_nodes (PfPickK (k, kont)) ps >>= fun (datum, ps) ->
               push_datum datum ps
            | (TkPickOne have_head, leading), ps ->
               go (Some (Prefix `PickOne)) ((TkPickK (have_head,1), leading), ps)
            | (TkGrabAll false, leading), ps ->
               let kont = kont_simple_form (span ps leading) (Postfix `GrabAll |> mode) in
               kont (List.rev bucket) >>= fun datum ->
               loop (duty+(List.length bucket)-1) [PDatumNode datum] ps
            | (TkGrabAll true, leading), ps ->
               read_datum ps >>= fun (head, ps) ->
               let kont = kont_simple_form_head head (span ps leading) (Postfix `GrabAll |> mode) in
               kont (List.rev bucket) >>= fun datum ->
               loop (duty+(List.length bucket)-1) [PDatumNode datum] ps
            (* XXX standalone annotation nodes? *)
            | (TkGrabK (false, k), leading), ps ->
               let kont = kont_simple_form (span ps leading) (Postfix (`GrabK k) |> mode) in
               (try List.split k bucket |> kont_ok
                with Invalid_argument _ ->
                  No_enough_nodes_to_grab {
                      expected = k;
                      available = (List.length bucket);
                    } |> kont_fail (span ps leading)) >>= fun (nodes, rbucket) ->
               kont (List.rev nodes) >>= fun datum ->
               loop (duty+k-1) (PDatumNode datum :: rbucket) ps
            | (TkGrabK (true, k), leading), ps ->
               read_datum ps >>= fun (head, ps) ->
               let kont = kont_simple_form_head head (span ps leading) (Postfix (`GrabK k) |> mode) in
               (try List.split k bucket |> kont_ok
                with Invalid_argument _ ->
                  No_enough_nodes_to_grab {
                      expected = k;
                      available = (List.length bucket);
                    } |> kont_fail (span ps leading)) >>= fun (nodes, rbucket) ->
               kont (List.rev nodes) >>= fun datum ->
               loop (duty+k-1) (PDatumNode datum :: rbucket) ps
            | (TkGrabOne have_head, leading), ps ->
               go (Some (Postfix `GrabOne)) ((TkGrabK (have_head,1), leading), ps)
            (* XXX grab-point support *)
            | (TkGrabPoint, _leading), _ps -> failwith "grab-point not supported yet"
            | (TkKeywordIndicator, _leading), ps ->
               read_datum ps >>= fun (kw, ps) ->
               read_datum ps >>= fun (datum, ps) ->
               let node = PKeywordNode (kw, datum)
               in push_node node ps
            | (TkAnnoPrevIndicator, leading), ps ->
               read_datum ps >>= fun (anno, ps) ->
               (* XXX there might be more corner cases that should be handled.. *)
               begin match bucket with
               | (PDatumNode datum) :: rbucket ->
                  let annotated = pdatum_anno_back anno datum in
                  let node = PDatumNode annotated in
                  loop duty (node :: rbucket) ps
               | _ -> Previous_datum_to_annotate_not_exists |> fail (span ps leading)
               end
            | (TkAnnoStandaloneIndicator, _leading), ps ->
               read_datum ps >>= fun (anno, ps) ->
               push_node (PAnnoNode anno) ps
            | _ ->
               read_datum ps0 >>= fun (datum, ps) ->
               push_datum datum ps
          in lexer ps0 >>= (go None)
        end
    in loop duty [] ps

  and kont_simple_form span mode : pkont = fun nodes ->
    pdatum_form nodes SimpleForm DefaultReader span mode |> kont_ok
  and kont_simple_form_head head span mode : pkont = fun nodes ->
    let nodes = (PDatumNode head) :: nodes
    in pdatum_form nodes SimpleForm DefaultReader span mode |> kont_ok
end
