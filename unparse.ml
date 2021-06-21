open Gensl

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
        | (ToplevelForm | ListForm | VectorForm _ | MapForm | SetForm | RelForm), _ -> nodes, fxn
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
      | RelForm, _ -> failwith "unimplemented"
    end

and unparse_pnode ?fxnconv ppf : pnode -> unit = function
  | PKeywordNode (k,v) -> pstr ppf ":"; unparse_pdatum ?fxnconv ppf k; psp ppf; unparse_pdatum ?fxnconv ppf v
  | PDatumNode dtm -> unparse_pdatum ?fxnconv ppf dtm
  | PAnnoNode anno -> unparse_anno ?fxnconv `Standalone ppf anno
  | PDecorNode { elem = GrabPoint; _ } -> pstr ppf "."
  | PDecorNode { elem = CommaSeparator; _ } -> pstr ppf ","
  | PDecorNode { elem = (ParseError _); _ } -> pstr ppf "??parse_error"

