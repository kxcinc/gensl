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

  open struct
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
       let kont = kont_complex_form `List ListForm
       in read_nodes (PickUntil (fun tok -> tok = TkBracketClose, true), kont) ps
    | TkCurlyOpen, ps ->
       let kont = kont_complex_form `Map MapForm
       in read_nodes (PickUntil (fun tok -> tok = TkCurlyClose, true), kont) ps
    | TkPoundCurlyOpen, ps ->
       let kont = kont_complex_form_set
       in read_nodes (PickUntil (fun tok -> tok = TkCurlyClose, true), kont) ps
    (* XXX dimentional check *)
    | TkPoundBracketOpen None, ps
    | TkPoundBracketOpen (Some 1), ps ->
       let kont = kont_complex_form `Vector (VectorForm (Some 1))
       in read_nodes (PickUntil (fun tok -> tok = TkBracketClose, true), kont) ps
    | TkPoundBracketOpen (Some k), ps ->
       let kont = kont_complex_form_vector_k (Some k)
       in read_nodes (PickUntil (fun tok -> tok = TkBracketClose, true), kont) ps
    | TkParenClose, _ps
    | TkBracketClose, _ps
    | TkCurlyClose, _ps
    | TkPickAll, _ps | TkGrabAll, _ps
    | TkPickK _, _ps | TkGrabK _, _ps
    | TkPickOne _, _ps | TkGrabOne _, _ps
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
  and     read_nodes : picking_frame -> pstate -> pdatum presult =
    fun (duty, kont) ps ->
    let rec loop duty buckets ps =
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
      | PickK duty when duty = 0 -> finish_with_kont ps
      | _ -> begin
          let rec go (fxn : form_fixness option) (tok, ps) =
            debug_token "read_nodes.go " tok;
            let fxn f = Option.value ~default:f fxn in
            match tok, duty with
            | TkSpaces _, _ -> loop duty buckets ps
            | tok, PickUntil delim when fst (delim tok) ->
               finish_with_kont (if snd (delim tok) then ps else (unlex tok ps))
            | tok, PickK k when tok_form_ending tok && k > 0 ->
               Immature_ending_of_form duty |> fail 
            | tok, PickUntil _ when tok_form_ending tok ->
               Immature_ending_of_form duty |> fail 
            | _ -> begin
                match tok with
                | tok when tok_form_ending tok -> failwith ("panic @"^__LOC__)
                | TkPickAll ->
                   let kont = kont_simple_form ~fxn:(Prefix (`PickAll, false) |> fxn) () in
                   read_nodes (picktillend false, kont) ps >>= fun (datum, ps) ->
                   push_datum datum ps
                | TkPickK (false, k) ->
                   let kont = kont_simple_form  ~fxn:(Prefix (`PickK k, false) |> fxn) () in
                   read_nodes (PickK k, kont) ps >>= fun (datum, ps) ->
                   push_datum datum ps
                | TkPickK (true, k) ->
                   read_datum ps >>= fun (head, ps) ->
                   let kont = kont_simple_form_head head ~fxn:(Prefix (`PickK k, false) |> fxn) in
                   read_nodes (PickK k, kont) ps >>= fun (datum, ps) ->
                   push_datum datum ps
                | TkPickOne have_head ->
                   go (Some (Prefix (`PickOne, have_head))) ((TkPickK (have_head,1), ps))
                | TkGrabAll ->
                   (* perform a lex ahead to determing whether there is a head-node *)
                   lex ps >>= begin function
                   | TkSpaces _, ps ->
                      (* no head-node *)
                      let kont = kont_simple_form ~fxn:(Postfix (`GrabAll, false) |> fxn) () in
                      kont (List.rev headbucket) >>= fun datum ->
                      loop (dutyadj (List.length headbucket - 1) duty)
                        (match restbuckets with
                         | [] -> [PDatumNode datum] :: []
                         | hd :: tail -> (PDatumNode datum :: hd) :: tail)
                        ps
                   | tok, ps when tok_form_ending tok ->
                      (* no head-node *)
                      let ps = unlex tok ps in
                      let kont = kont_simple_form ~fxn:(Postfix (`GrabAll, false) |> fxn) () in
                      kont (List.rev headbucket) >>= fun datum ->
                      loop (dutyadj (List.length headbucket - 1) duty)
                        (match restbuckets with
                         | [] -> [PDatumNode datum] :: []
                         | hd :: tail -> (PDatumNode datum :: hd) :: tail)
                        ps
                   | tok, ps ->
                      (* having head-node *)
                      let ps = unlex tok ps in
                      read_datum ps >>= fun (head, ps) ->
                      let kont = kont_simple_form_head ~fxn:(Postfix (`GrabAll, true) |> fxn) head in
                      kont (List.rev headbucket) >>= fun datum ->
                      loop (dutyadj (List.length headbucket - 1) duty)
                        (match restbuckets with
                         | [] -> [PDatumNode datum] :: []
                         | hd :: tail -> (PDatumNode datum :: hd) :: tail)
                        ps
                   end
                | TkGrabK (false, k) ->
                   let kont = kont_simple_form ~fxn:(Postfix (`GrabK k, false) |> fxn) () in
                   (try List.split k headbucket |> kont_ok
                    with Invalid_argument _ ->
                      No_enough_nodes_to_grab {
                          expected = k;
                          available = (List.length headbucket);
                        } |> kont_fail ) >>= fun (nodes, rbucket) ->
                   kont (List.rev nodes) >>= fun datum ->
                   loop (dutyadj (k-1) duty) ((PDatumNode datum :: rbucket) :: restbuckets) ps
                | TkGrabK (true, k) ->
                   read_datum ps >>= fun (head, ps) ->
                   let kont = kont_simple_form_head ~fxn:(Postfix (`GrabK k, true) |> fxn) head in
                   (try List.split k headbucket |> kont_ok
                    with Invalid_argument _ ->
                      No_enough_nodes_to_grab {
                          expected = k;
                          available = (List.length headbucket);
                        } |> kont_fail ) >>= fun (nodes, rbucket) ->
                   kont (List.rev nodes) >>= fun datum ->
                   loop (dutyadj (k-1) duty) ((PDatumNode datum :: rbucket) :: restbuckets) ps
                | TkGrabOne have_head ->
                   go (Some (Postfix (`GrabOne, have_head))) (TkGrabK (have_head,1), ps)
                | TkGrabPoint ->
                   let decor = PDecorNode { elem = GrabPoint; repr = `Direct } in
                   loop duty ([] :: (decor :: headbucket) :: restbuckets) ps
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
                      let annotated = pdatum_annoback anno datum in
                      let node = PDatumNode annotated in
                      loop duty ((node :: rbucket) :: restbuckets) ps
                   | _ -> Previous_datum_to_annotate_not_exists |> fail 
                   end
                | TkAnnoStandaloneIndicator ->
                   read_datum ps >>= fun (anno, ps) ->
                   push_node (PAnnoNode anno) ps
                | _ ->
                   debug_msg (Format.sprintf "%s" __LOC__);
                   read_datum (unlex tok ps) >>= fun (datum, ps) ->
                   push_datum datum ps
              end
          in lex ps >>= (go None)
        end
    in loop duty [[]] ps

  and  read_top ps =
    let kont : pkont = function
      | [PDatumNode datum] -> datum |> kont_ok
      | nodes -> pdatum_form nodes ToplevelForm Infix `Direct |> kont_ok
    in read_nodes (PickUntil (fun tok -> tok_eof tok, true), kont) ps

  and kont_simple_form ?fxn:(fxn=Infix) () : pkont = fun nodes ->
    pdatum_form nodes SimpleForm fxn `Direct |> kont_ok

  and kont_complex_form_vector_k k  : pkont = fun nodes ->
    let (csymb, fstyle) = `Vector, VectorForm k in
    let head = pdatum_atom (CodifiedSymbolAtom csymb) `Phantom in
    (match k with
     | Some 0 ->
        if (nodes
            |> List.find_all (function PDatumNode _ | PKeywordNode _ -> true | _ -> false)
            |> List.length) = 1
        then kont_ok () else kont_fail (Dimentional_violation 0)
     | None | Some 1 -> kont_ok ()
     | _ -> failwith "multi-dimentional vector not yet supported") >>= fun () ->
    pdatum_form (PDatumNode head :: nodes) fstyle Infix `Direct |> kont_ok
  and kont_complex_form csymb fstyle : pkont = fun nodes ->
    let head = pdatum_atom (CodifiedSymbolAtom csymb) `Phantom in
    pdatum_form (PDatumNode head :: nodes) fstyle Infix `Direct |> kont_ok
  and kont_complex_form_set : pkont = fun nodes ->
    let true_ = pdatum_atom (BoolAtom true) `Phantom in
    let tr = function
      | PDatumNode dtm -> PKeywordNode (dtm, true_) |> kont_ok
      | PKeywordNode _ -> Invalid_element_in_complex_form SetForm |> kont_fail
      | node -> node |> kont_ok in
    nodes |&> tr |> seq_result >>= fun nodes ->
    let csymb = `Set in
    let head = pdatum_atom (CodifiedSymbolAtom csymb) `Phantom in
    let head = PDatumNode head in
    pdatum_form (head :: nodes) SetForm Infix `Direct |> kont_ok
  and kont_simple_form_head ?fxn:(fxn=Infix) ?repr:(repr=`Direct) head : pkont = fun nodes ->
    let nodes = (PDatumNode head) :: nodes
    in pdatum_form nodes SimpleForm fxn repr |> kont_ok
end

module Default = Make(Genslex.Lexer)
