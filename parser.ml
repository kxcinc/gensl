open Kxclib
open Gensl
open Utils
open Parsing

open Basetypes
open Parsetree

open ParserTypes

let debugging = ref false

module Make (Lexer : Lexer) = struct
  open Lexer

  type nonrec picking_frame = (buffer, location) picking_frame

  type nonrec 'x presult = ('x, buffer) presult

  let debug_token' msg token_result =
    if !debugging then
    let ppf = Format.std_formatter in
    let ((tok,_),_) = token_result in
    Format.(
      pp_print_string ppf msg;
      pp_token ppf tok; pp_print_newline ppf (); print_flush())

  let debug_token msg tok =
    if !debugging then
    let ppf = Format.std_formatter in
    Format.(
      pp_print_string ppf msg;
      pp_token ppf tok; pp_print_newline ppf (); print_flush())

  let debug_msg msg =
    if !debugging then
    let ppf = Format.std_formatter in
    Format.(
      pp_print_string ppf msg;
      pp_print_newline ppf (); print_flush())

  open [@ocaml.warning "-32-33"] struct
    let (>>=) = Result.bind
    let ok ps x = Ok (x,ps)
    let fail err : 'x presult =
      if !debugging
      then raise (Parse_error err)
      else Error [err]
    let kont_ok x = Ok x
    let kont_fail err : 'x kresult = Error [err]
    let seq_result rs =
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | head :: rest -> head >>= fun hd -> loop (hd :: acc) rest
      in loop [] rs
    (* XXX lift_result might not be a good name *)
    let lift_result ps : 'x kresult -> 'x presult = function
      | Ok x -> Ok (x, ps)
      | Error err -> Error err
    let wrap_lexresult : lexresult -> token presult = fun x -> x
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

  let lex ({ buf; withdrew } as ps) : token presult =
    match Queue.take withdrew with
    | None -> Lexer.lexer buf |> wrap_lexresult
    | Some (tok, withdrew) ->
       ok { ps with withdrew } tok

  let unlex tok ps = { ps with withdrew = Queue.add ps.withdrew tok }

  let tok_form_ending = function
    | TkParenClose -> true
    | TkEof -> true
    | _ -> false

  let tok_eof = function
    | TkEof -> true
    | _ -> false

  (* NB that pstate should be used as a linear type and it needs manual unlexing *)
  (* psst: we probably want to assert linearity on pstate *)

  module FormValidatorAutomaton = struct
    type alphabet = [ `Datum | `Comma | `Keyword | `Mapsto ]

    open Sexplib.Std

    type track_c = [ `Any | `CommaOnly | `NoCommaOnly ] [@@deriving sexp]
    type track_km = [ `Any | `MapstoOnly | `KeywordOnly ] [@@deriving sexp]
    type st0 = track_c * track_km * int [@@deriving sexp]
    type st = st0 kresult

    let pp_st0 ppf x = Format.(fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_st0 x))
    let pp_result pp_elem ppf elem =
      let printf fmt = Format.(fprintf ppf) fmt in
      match elem with
      | Ok elem -> printf "Ok(%a)" pp_elem elem
      | Error _ -> printf "Error(_)"
    let pp_st = pp_result pp_st0

    let initst : st = Ok (`Any, `Any, 0)
    let stepst : alphabet -> st -> st = fun alphabet st ->
      let error e = Error [e] in
      st >>= fun (c, km, ost) ->
      (match ost, alphabet with
       | 0, `Datum   -> Ok 1
       | 0, `Keyword -> Ok 2
       | 1, `Datum   -> Ok 1
       | 1, `Comma   -> Ok 0
       | 1, `Keyword -> Ok 2
       | 1, `Mapsto  -> Ok 3
       | 2, `Datum   -> Ok 1
       | 3, `Datum   -> Ok 4
       | 4, `Datum   -> Ok 1
       | 4, `Comma   -> Ok 0
       | _ -> error (Invalid_form_format `TodoMoreDetails)) >>= fun ost' ->
      (if ost = 1 || ost = 4 then
         match c, alphabet with
         | `Any, `Comma -> Ok `CommaOnly
         | `Any, `Datum -> Ok `NoCommaOnly
         | `CommaOnly, `Datum -> error (Invalid_form_format `InconsistentCommaUsage)
         | `NoCommaOnly, `Comma -> error (Invalid_form_format `InconsistentCommaUsage)
         | _ -> Ok c
       else Ok c) >>= fun c' ->
      (match km, alphabet with
       | `Any, `Keyword -> Ok `KeywordOnly
       | `Any, `Mapsto -> Ok `MapstoOnly
       | `KeywordOnly, `Mapsto -> error (Invalid_form_format `MixedKeywordMapsto)
       | `MapstoOnly, `Keyword -> error (Invalid_form_format `MixedKeywordMapsto)
       | _ -> Ok km) >>= fun km' ->
      Ok (c', km', ost')
  end


  module FVA = FormValidatorAutomaton

  let rec read_datum : pstate -> pdatum presult =
    fun ps ->
    debug_msg "entering read_datum";
    lex ps >>= fun (tok, ps) ->
    (debug_token "read_datum: " tok);
    let atom_clause ps atom = pdatum_atom atom `Direct |> ok ps in
    match tok, ps with
    | TkSpaces _, ps -> read_datum ps
    | TkSymbol symb, ps -> atom_clause ps (SymbolAtom symb)
    | TkCodifiedSymbol csymb, ps -> atom_clause ps (CodifiedSymbolAtom csymb)
    | TkString str, ps -> atom_clause ps (StringAtom str)
    | TkBytes bytes, ps -> atom_clause ps (BytesAtom bytes)
    | TkNumeric (num,suffix), ps -> atom_clause ps (NumericAtom (num,suffix))
    | TkBool b, ps -> atom_clause ps (BoolAtom b)
    | TkParenOpen, ps ->
       let kont = kont_simple_form()
       in read_nodes (PickUntil (fun tok -> tok = TkParenClose, true), kont) ps
    (* XXX restrictions in complex forms *)
    | TkBracketOpen, ps ->
       let kont = kont_complex_form ListForm
       in read_nodes (PickUntil (fun tok -> tok = TkBracketClose, true), kont) ps
    | TkCurlyOpen, ps ->
       let kont pnodes =
         (foldr (fun node acc -> match node, acc with
              | _, (Error _ as acc) -> acc
              | PDatumNode pdatum, Ok _ -> Unexpected_positional_datum pdatum |> kont_fail
              | node, Ok acc -> kont_ok (node :: acc))
             pnodes (Ok [])) >>= fun pnodes ->
         kont_complex_form MapForm pnodes
       in read_nodes (PickUntil (fun tok -> tok = TkCurlyClose, true), kont) ps
    | TkPoundCurlyOpen, ps ->
       let kont = kont_complex_form SetForm
       in read_nodes (PickUntil (fun tok -> tok = TkCurlyClose, true), kont) ps
    (* XXX dimentional check *)
    | TkPoundBracketOpen (None as k), ps
    | TkPoundBracketOpen (Some 1 as k), ps ->
       let kont = kont_complex_form (VectorForm k)
       in read_nodes (PickUntil (fun tok -> tok = TkBracketClose, true), kont) ps
    | TkPoundBracketOpen (Some k), ps ->
       let kont = kont_complex_form_vector_k (Some k)
       in read_nodes (PickUntil (fun tok -> tok = TkBracketClose, true), kont) ps
    | TkParenClose, _ps
    | TkBracketClose, _ps
    | TkCurlyClose, _ps
    | TkComma, _ps | TkMapsto, _ps
    | TkPickAll, _ps | TkGrabAll _, _ps
    | TkPickK _, _ps | TkGrabK _, _ps
    | TkPickOne, _ps | TkGrabOne, _ps
    | TkGrabPoint, _ps
    | TkKeywordIndicator, _ps
    | TkAnnoPrevIndicator, _ps
    | TkAnnoStandaloneIndicator, _ps
      -> Unexpected_ending_of_form |> fail 
    | TkAnnoNextIndicator, ps ->
       read_datum ps >>= fun (anno, ps) ->
       let kont : pkont = function
         | [node] ->
            begin match node with
            | PDatumNode datum -> pdatum_annofront anno datum |> kont_ok
            | _ -> Attempting_to_annotate_non_datum |> kont_fail 
            end
         | _ -> failwith ("panic: " ^ __LOC__)
       in
       read_nodes (PickK 1, kont) ps
    | TkEof, _ -> Unexpected_eof |> fail 


  (* XXX keyword duplication checks *)
  and     read_nodes : ?st:FVA.st -> picking_frame -> pstate -> pdatum presult =
    fun ?(st=FVA.initst) (duty, kont) ps ->
    let rec loop duty buckets ps st =
      let bucketsize = buckets |> List.map List.length |> List.foldl (+) 0 in
      let headbucket = List.hd buckets in
      let restbuckets = List.tl buckets in
      debug_msg (Format.asprintf "entering loop (duty=%a, buckets.len=%d, buckets[].size=%d, st=%a)"
                   pp_pickduty duty (List.length buckets) bucketsize
                   FVA.pp_st st);
      let dutyadj by = function PickK k -> PickK (k+by) | d -> d in
      let dutydec = dutyadj (-1) in
      let picktillend consuming = PickUntil (fun tok -> tok_form_ending tok, consuming) in
      let push_node node ps new_st =
        loop (dutydec duty) (((node, st) :: headbucket) :: restbuckets) ps new_st in
      let push_datum datum ps new_st = push_node (PDatumNode datum) ps new_st in
      let finish_with_kont ps = kont (List.concat buckets |> List.rev |> List.map fst) |> lift_result ps in
      let nodatanode() =
        let rec loop = function
          | PAnnoNode _ :: rest -> loop rest
          | PDecorNode _ :: rest -> loop rest
          | [] -> true
          | _ -> false
        in List.concat buckets |> List.map fst |> loop in
      (* XXX there might be more corner cases that should be handled.. *)
      let with_prev_datum_node kont st =
        let rec loopy = function
          | (PDatumNode prev_datum, prev_st) :: rbucket ->
             kont prev_datum >>= fun (node, ps) ->
             loop duty (((node, prev_st) :: rbucket) :: restbuckets) ps st
          | (PAnnoNode _, _) :: rbucket -> loopy rbucket
          | _ -> Previous_datum_not_exists |> fail
        in loopy headbucket
      in
      let collect k =
        let rec loop acc = function
          | 0, rest -> (List.rev acc, rest) |> kont_ok
          | k, (PDecorNode _, _) :: rest -> loop acc (k, rest)
          | k, ((PAnnoNode _, _) as node_st) :: rest -> loop (node_st :: acc) (k, rest)
          | k, node_st :: rest -> loop (node_st :: acc) (k-1, rest)
          | _, [] -> No_enough_nodes_to_grab {
                          expected = k;
                          available = -1; (* XXX dummy value *)
                        } |> kont_fail
        in loop [] (k, headbucket) in
      match duty with
      | PickK duty when duty = 0 ->
        st >>= fun _ ->
        finish_with_kont ps
      | _ -> begin
          let rec go (fxn : form_fixness option) (tok, ps) =
            debug_token "read_nodes.go " tok;
            let fxn f = Option.value ~default:f fxn in
            match tok, duty with
            | TkSpaces _, _ -> loop duty buckets ps st
            | tok, PickUntil delim when fst (delim tok) ->
              st >>= fun _ ->
              finish_with_kont (if snd (delim tok) then ps else (unlex tok ps))
            | tok, PickK k when tok_form_ending tok && k > 0 ->
               Immature_ending_of_form duty |> fail 
            | tok, PickUntil _ when tok_form_ending tok ->
               Immature_ending_of_form duty |> fail 
            | _ -> begin
                match tok with
                | tok when tok_form_ending tok -> failwith ("panic @"^__LOC__)
                | TkComma -> 
                   if nodatanode() then Unexpected_position_of_comma |> fail
                   else let node = pnode_decor CommaSeparator in
                     loop duty (((node, st) :: headbucket) :: restbuckets) ps (FVA.stepst `Comma st)
                | TkPickAll ->
                   let kont = kont_simple_form ~fxn:(Prefix (`PickAll, false) |> fxn) () in
                   read_nodes (picktillend false, kont) ps >>= fun (datum, ps) ->
                   push_datum datum ps (FVA.stepst `Datum st)
                | TkPickK (false, k) ->
                   let kont = kont_simple_form  ~fxn:(Prefix (`PickK k, false) |> fxn) () in
                   read_nodes (PickK k, kont) ps >>= fun (datum, ps) ->
                   push_datum datum ps (FVA.stepst `Datum st)
                | TkPickK (true, k) ->
                   read_datum ps >>= fun (head, ps) ->
                   let kont = kont_simple_form_head head ~fxn:(Prefix (`PickK k, true) |> fxn) in
                   read_nodes (PickK k, kont) ps >>= fun (datum, ps) ->
                   push_datum datum ps (FVA.stepst `Datum st)
                | TkPickOne ->
                   go (Some (Prefix (`PickOne, true))) ((TkPickK (true,1), ps))
                | TkGrabAll count ->
                   (* perform a lex ahead to determing whether there is a head-node *)
                   let process ?(headopt=None) ps =
                     let kont =
                       match headopt with
                       | None -> kont_simple_form ~fxn:(Postfix (`GrabAll, false) |> fxn) ()
                       | Some head -> kont_simple_form_head ~fxn:(Postfix (`GrabAll, true) |> fxn) head
                     in
                     let nodes = List.rev headbucket |> List.map fst in
                     let rec last_st = function
                       | [] -> FVA.initst
                       | [] :: rbuckets -> last_st rbuckets
                       | ((_, st) :: _) :: _ -> st in
                     let st = last_st restbuckets in
                     kont nodes >>= fun datum ->
                     match count with
                     | Some check_length when List.length nodes <> check_length ->
                       Unmatched_graball_count (List.length nodes, check_length) |> fail
                     | _ ->
                       loop (dutyadj (List.length headbucket - 1) duty)
                         (match restbuckets with
                          | [] -> [(PDatumNode datum, st)] :: []
                          | hd :: tail -> ((PDatumNode datum, st) :: hd) :: tail)
                         ps (FVA.stepst `Datum st) in
                   lex ps >>= begin function
                     | TkSpaces _, ps ->
                       (* no head-node *)
                       process ps
                     | tok, ps when tok_form_ending tok ->
                       (* no head-node *)
                       let ps = unlex tok ps in
                       process ps
                     | tok, ps ->
                       (* having head-node *)
                       let ps = unlex tok ps in
                       read_datum ps >>= fun (head, ps) ->
                       process ps ~headopt:(Some head)
                   end
                | TkGrabK (false, k) ->
                   let kont = kont_simple_form ~fxn:(Postfix (`GrabK k, false) |> fxn) () in
                   collect k >>= fun (node_sts, rbucket) ->
                   let node_sts = List.rev node_sts in
                   kont (List.map fst node_sts) >>= fun datum ->
                   let st = (List.hd node_sts |> snd)  in
                   loop
                     (dutyadj (k-1) duty)
                     (((PDatumNode datum, st) :: rbucket) :: restbuckets)
                     ps (FVA.stepst `Datum st)
                | TkGrabK (true, k) ->
                   read_datum ps >>= fun (head, ps) ->
                   let kont = kont_simple_form_head ~fxn:(Postfix (`GrabK k, true) |> fxn) head in
                   collect k >>= fun (node_sts, rbucket) ->
                   let node_sts = List.rev node_sts in
                   kont (List.map fst node_sts) >>= fun datum ->
                   let st = (List.hd node_sts |> snd) in
                   loop
                     (dutyadj (k-1) duty)
                     (((PDatumNode datum, st) :: rbucket) :: restbuckets)
                     ps (FVA.stepst `Datum st)
                | TkGrabOne ->
                   go (Some (Postfix (`GrabOne, true))) (TkGrabK (true,1), ps)
                | TkGrabPoint ->
                   let decor = PDecorNode { elem = GrabPoint; repr = `Direct } in
                   loop duty ([] :: ((decor, st) :: headbucket) :: restbuckets) ps st
                | TkKeywordIndicator ->
                   read_datum ps >>= fun (kw, ps) ->
                   read_datum ps >>= fun (datum, ps) ->
                   let node = PKeywordNode (kw, datum)
                   in push_node node ps (FVA.stepst `Keyword st |> FVA.stepst `Datum)
                | TkMapsto ->
                   read_datum ps >>= fun (datum, ps) ->
                   with_prev_datum_node (fun kw ->
                       PKeywordNode (kw, datum) |> ok ps)
                     (FVA.stepst `Mapsto st |> FVA.stepst `Datum)
                | TkAnnoPrevIndicator ->
                   read_datum ps >>= fun (anno, ps) ->
                   with_prev_datum_node (fun datum ->
                      let annotated = pdatum_annoback anno datum
                      in PDatumNode annotated |> ok ps) st
                | TkAnnoStandaloneIndicator ->
                   read_datum ps >>= fun (anno, ps) ->
                   push_node (PAnnoNode anno) ps st
                | _ ->
                   debug_msg (Format.sprintf "%s" __LOC__);
                   read_datum (unlex tok ps) >>= fun (datum, ps) ->
                   push_datum datum ps (FVA.stepst `Datum st)
              end
          in lex ps >>= (go None)
        end
    in loop duty [[]] ps st

  and  read_top ps =
    let kont : pkont = function
      | [PDatumNode datum] -> datum |> kont_ok
      | nodes -> pdatum_form nodes ToplevelForm Infix `Direct |> kont_ok
    in read_nodes (PickUntil (fun tok -> tok_eof tok, true), kont) ps

  and kont_simple_form ?fxn:(fxn=Infix) () : pkont = fun nodes ->
    pdatum_form nodes SimpleForm fxn `Direct |> kont_ok

  and kont_complex_form_vector_k k  : pkont = fun nodes ->
    let fstyle = VectorForm k in
    (match k with
     | Some 0 ->
        if (nodes
            |> List.find_all (function PDatumNode _ | PKeywordNode _ -> true | _ -> false)
            |> List.length) = 1
        then kont_ok () else kont_fail (Dimentional_violation 0)
     | None | Some 1 -> kont_ok ()
     | _ -> failwith "multi-dimentional vector not yet supported") >>= fun () ->
    pdatum_form nodes fstyle Infix `Direct |> kont_ok
  and kont_complex_form fstyle : pkont = fun nodes ->
    pdatum_form nodes fstyle Infix `Direct |> kont_ok
  and kont_simple_form_head ?fxn:(fxn=Infix) ?repr:(repr=`Direct) head : pkont = fun nodes ->
    let nodes = (PDatumNode head) :: nodes
    in pdatum_form nodes SimpleForm fxn repr |> kont_ok
end

module Default = Make(Genslex.Lexer)
