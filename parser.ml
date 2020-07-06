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
  type nonrec pkont = location pkont

  type nonrec pdatum = location pdatum
  type nonrec pnode = location pnode
  type nonrec patom = location patom
  type nonrec 'x pelem = ('x, location) pelem

  type nonrec 'x presult = ('x, buffer, location) presult
  type nonrec 'x kresult = ('x, location) kresult

  let debug_token' msg token_result =
    if !debugging then
    let ppf = Format.std_formatter in
    let ((tok,_),_) = token_result in
    Format.(
      pp_print_string ppf msg;
      pp_token ppf tok; pp_print_newline ppf (); print_flush())

  let debug_token msg token_result =
    if !debugging then
    let ppf = Format.std_formatter in
    let (tok,_) = token_result in
    Format.(
      pp_print_string ppf msg;
      pp_token ppf tok; pp_print_newline ppf (); print_flush())

  let debug_msg msg =
    if !debugging then
    let ppf = Format.std_formatter in
    Format.(
      pp_print_string ppf msg;
      pp_print_newline ppf (); print_flush())

  let pstate : buffer -> pstate = fun buf -> { buf; withdrew = Queue.empty }

  open struct
    let (>>=) = Result.bind
    let ok ps x = Ok (x,ps)
    exception Parse_error of parse_error
    let fail span err : 'x presult =
      if !debugging
      then raise (Parse_error err)
      else Error [err, span]
    let kont_ok x = Ok x
    let kont_fail span err : 'x kresult = Error [err, span]
    (* XXX lift_result might not be a good name *)
    let lift_result ps : 'x kresult -> 'x presult = function
      | Ok x -> Ok (x, ps)
      | Error err -> Error err
    let wrap_lexresult ps : 'loc lexresult -> (token*'loc span) presult = function
      | Ok x -> Ok (x, ps)
      | Error (err, span) -> Error [Lexing_error err, span]
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

  let lex ({ buf; withdrew } as ps) : (token*Lexer.location span) presult =
    match Queue.take withdrew with
    | None -> Lexer.lexer buf |> wrap_lexresult ps
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

  let rec read_datum : pstate -> pdatum presult =
    debug_msg "entering read_datum";
    fun ps ->
    let span _ps' leading =
      let span_source = source ps.buf in
      let span_start = loc ps.buf in
      let span_end = loc ps.buf in
      { span_start; span_end; span_source; span_leading = leading }
    in
    let atom_clause (ps,leading) atom =
       let span = span ps leading
       in pdatum_atom atom span Infix DefaultReader |> ok ps
    in
    lex ps >>= fun (lexer_result, ps) ->
    (debug_token "read_datum: " lexer_result);
    match lexer_result, ps with
    | (TkSpaces _, _), ps -> read_datum ps
    | (TkSymbol symb, span), ps -> atom_clause (ps,span.span_leading) (SymbolAtom symb)
    | (TkString str, span), ps -> atom_clause (ps,span.span_leading) (StringAtom str)
    | (TkBytes bytes, span), ps -> atom_clause (ps,span.span_leading) (BytesAtom bytes)
    | (TkNumeric (num,suffix), span), ps -> atom_clause (ps,span.span_leading) (NumericAtom (num,suffix))
    | (TkBool b, span), ps -> atom_clause (ps,span.span_leading) (BoolAtom b)
    | (TkParenOpen, span), ps ->
       let kont = kont_simple_form span Infix
       in read_nodes (PickUntil (fun tok -> tok = TkParenClose, true), kont) ps
    | (TkParenClose, span), _ps
    | (TkPickAll, span), _ps | (TkGrabAll, span), _ps
    | (TkPickK _, span), _ps | (TkGrabK _, span), _ps
    | (TkPickOne _, span), _ps | (TkGrabOne _, span), _ps
    | (TkGrabPoint, span), _ps
    | (TkKeywordIndicator, span), _ps
    | (TkAnnoPrevIndicator, span), _ps
    | (TkAnnoStandaloneIndicator, span), _ps
      -> Unexpected_ending_of_form |> fail span
    | (TkAnnoNextIndicator, span), ps ->
       read_datum ps >>= fun (anno, ps') ->
       let kont : pkont = function
         | [node] ->
            begin match node with
            | PDatumNode datum -> pdatum_anno_front anno datum |> kont_ok
            | _ -> Attempting_to_annotate_non_datum |> kont_fail span
            end
         | _ -> failwith ("panic: " ^ __LOC__)
       in
       read_nodes (PickK 1, kont) ps'
    | (TkEof, span), _ -> Unexpected_eof |> fail span
  and     read_nodes : picking_frame -> pstate -> pdatum presult =
    fun pf ps ->
    let (duty, kont) = pf in
    (* XXX spans might be inaccurate at several places *)
    let rec loop duty buckets ps0 =
      let bucketsize = buckets |> List.map List.length |> List.foldl (+) 0 in
      let headbucket = List.hd buckets in
      let restbuckets = List.tl buckets in
      debug_msg (Format.asprintf "entering loop (duty=%a, buckets.len=%d, buckets[].size=%d)"
                   pp_pickduty duty (List.length buckets) bucketsize);
      let dutyadj by = function PickK k -> PickK (k+by) | d -> d in
      let dutydec = dutyadj (-1) in
      let picktillend consuming = PickUntil (fun tok -> tok_form_ending tok, consuming) in
      let push_node node ps = loop (dutydec duty) ((node :: headbucket) :: restbuckets) ps in
      let push_datum datum ps = push_node (PDatumNode datum) ps in
      let finish_with_kont ps = kont (List.concat buckets |> List.rev) |> lift_result ps in
      match duty with
      | PickK duty when duty = 0 -> finish_with_kont ps0
      | _ -> begin
          let rec go mode ((tok, span), ps) =
            (debug_token' "go lexer_result: " ((tok, span), ps));
            let mode m = Option.value ~default:m mode in
            match tok, duty with
            | TkSpaces _, _ -> loop duty buckets ps0
            | tok, PickUntil delim when fst (delim tok) ->
               finish_with_kont (if snd (delim tok) then ps else (unlex (tok, span) ps))
            | tok, PickK k when tok_form_ending tok && k > 0 ->
               Immature_ending_of_form duty |> fail span
            | tok, PickUntil _ when tok_form_ending tok ->
               Immature_ending_of_form duty |> fail span
            | _ -> begin
                match tok with
                | tok when tok_form_ending tok -> failwith ("panic @"^__LOC__)
                | TkPickAll ->
                   let kont = kont_simple_form span (Prefix `PickAll |> mode) in
                   read_nodes (picktillend false, kont) ps >>= fun (datum, ps) ->
                   push_datum datum ps
                | TkPickK (false, k) ->
                   let kont = kont_simple_form span (Prefix (`PickK k) |> mode) in
                   read_nodes (PickK k, kont) ps >>= fun (datum, ps) ->
                   push_datum datum ps
                | TkPickK (true, k) ->
                   read_datum ps >>= fun (head, ps) ->
                   let kont = kont_simple_form_head head span  (Prefix (`PickK k) |> mode) in
                   read_nodes (PickK k, kont) ps >>= fun (datum, ps) ->
                   push_datum datum ps
                | TkPickOne have_head ->
                   go (Some (Prefix `PickOne)) ((TkPickK (have_head,1), span), ps)
                | TkGrabAll ->
                   (* perform a lex ahead to determing whether there is a head-node *)
                   lex ps >>= begin function
                   | (TkSpaces _, span), ps ->
                      (* no head-node *)
                      let kont = kont_simple_form span (Postfix `GrabAll |> mode) in
                      kont (List.rev headbucket) >>= fun datum ->
                      loop (dutyadj (List.length headbucket - 1) duty)
                        (match restbuckets with
                         | [] -> [PDatumNode datum] :: []
                         | hd :: tail -> (PDatumNode datum :: hd) :: tail)
                        ps
                   | (tok, span), ps when tok_form_ending tok ->
                      (* no head-node *)
                      let ps = unlex (tok, span) ps in
                      let kont = kont_simple_form span (Postfix `GrabAll |> mode) in
                      kont (List.rev headbucket) >>= fun datum ->
                      loop (dutyadj (List.length headbucket - 1) duty)
                        (match restbuckets with
                         | [] -> [PDatumNode datum] :: []
                         | hd :: tail -> (PDatumNode datum :: hd) :: tail)
                        ps
                   | tokspan, ps ->
                      (* having head-node *)
                      let ps = unlex tokspan ps in
                      read_datum ps >>= fun (head, ps) ->
                      let kont = kont_simple_form_head head span (Postfix `GrabAll |> mode) in
                      kont (List.rev headbucket) >>= fun datum ->
                      loop (dutyadj (List.length headbucket - 1) duty)
                        (match restbuckets with
                         | [] -> [PDatumNode datum] :: []
                         | hd :: tail -> (PDatumNode datum :: hd) :: tail)
                        ps
                   end
                | TkGrabK (false, k) ->
                   let kont = kont_simple_form span (Postfix (`GrabK k) |> mode) in
                   (try List.split k headbucket |> kont_ok
                    with Invalid_argument _ ->
                      No_enough_nodes_to_grab {
                          expected = k;
                          available = (List.length headbucket);
                        } |> kont_fail span) >>= fun (nodes, rbucket) ->
                   kont (List.rev nodes) >>= fun datum ->
                   loop (dutyadj (k-1) duty) ((PDatumNode datum :: rbucket) :: restbuckets) ps
                | TkGrabK (true, k) ->
                   read_datum ps >>= fun (head, ps) ->
                   let kont = kont_simple_form_head head span (Postfix (`GrabK k) |> mode) in
                   (try List.split k headbucket |> kont_ok
                    with Invalid_argument _ ->
                      No_enough_nodes_to_grab {
                          expected = k;
                          available = (List.length headbucket);
                        } |> kont_fail span) >>= fun (nodes, rbucket) ->
                   kont (List.rev nodes) >>= fun datum ->
                   loop (dutyadj (k-1) duty) ((PDatumNode datum :: rbucket) :: restbuckets) ps
                | TkGrabOne have_head ->
                   go (Some (Postfix `GrabOne)) ((TkGrabK (have_head,1), span), ps)
                | TkGrabPoint -> loop duty ([] :: buckets) ps
                | TkKeywordIndicator ->
                   read_datum ps >>= fun (kw, ps) ->
                   read_datum ps >>= fun (datum, ps) ->
                   let node = PKeywordNode (kw, datum)
                   in push_node node ps
                | TkAnnoPrevIndicator ->
                   read_datum ps >>= fun (anno, ps) ->
                   (* XXX there might be more corner cases that should be handled.. *)
                   begin match headbucket with
                   | (PDatumNode datum) :: rbucket ->
                      let annotated = pdatum_anno_back anno datum in
                      let node = PDatumNode annotated in
                      loop duty ((node :: rbucket) :: restbuckets) ps
                   | _ -> Previous_datum_to_annotate_not_exists |> fail span
                   end
                | TkAnnoStandaloneIndicator ->
                   read_datum ps >>= fun (anno, ps) ->
                   push_node (PAnnoNode anno) ps
                | _ ->
                   debug_msg (Format.sprintf "%s" __LOC__);
                   read_datum (unlex (tok, span) ps) >>= fun (datum, ps) ->
                   push_datum datum ps
              end
          in lex ps0 >>= (go None)
        end
    in loop duty [[]] ps

  and  read_top ps =
    let span _ps' leading =
      let span_source = source ps.buf in
      let span_start = loc ps.buf in
      let span_end = loc ps.buf in
      { span_start; span_end; span_source; span_leading = leading } in
    let kont : pkont = function
      | [PDatumNode datum] -> datum |> kont_ok
      | nodes -> pdatum_form nodes ToplevelForm DefaultReader (span ps NoLeadingInfo) Infix |> kont_ok
    in read_nodes (PickUntil (fun tok -> tok_eof tok, true), kont) ps

  and kont_simple_form span mode : pkont = fun nodes ->
    pdatum_form nodes SimpleForm DefaultReader span mode |> kont_ok
  and kont_simple_form_head head span mode : pkont = fun nodes ->
    let nodes = (PDatumNode head) :: nodes
    in pdatum_form nodes SimpleForm DefaultReader span mode |> kont_ok
end

module Default = Make(Genslex.Lexer)
