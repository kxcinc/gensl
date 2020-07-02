open Gensl

open Basetypes
open Parsetree

open ParserTypes

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

  let lexer { buf; _ } =
    Format.(printf "lexer\n"; print_flush());
    lexer buf |>
    Result.map Format.(fun (((tok,_),_) as r) ->
      let ppf = std_formatter in
      pp_token ppf tok; pp_print_newline ppf (); print_flush(); r)

  let pstate : buffer -> pstate = fun buf -> { buf }

  open struct
    let (>>=) : 'x kresult -> ('x -> 'y kresult) -> 'y kresult = Result.bind
    let ok ps x = Ok (x,ps)
    (* let fail span err : 'x presult = Error [err, span] *)
    exception Parse_error of parse_error
    let fail _span err : 'x presult = raise (Parse_error err)
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
    Format.(printf "read_datum\n"; print_flush());
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
    Format.(printf "read_nodes\n"; print_flush());
    fun pf ps ->
    let span _ps' leading =
      let span_source = source ps.buf in
      let span_start = loc ps.buf in
      let span_end = loc ps.buf in
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

module Default = Make(Genslex.Lexer)
