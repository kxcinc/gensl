open Kxclib
open Gensl
open Parsing

open Basetypes
open Parsetree

module Extensions : Extensions = struct
  module Helper = struct
    let string_of_uchar_array us : string =
      let b = Buffer.create (Array.length us * 4) in
      Array.iter (fun u -> Buffer.add_utf_8_uchar b u) us;
      Buffer.contents b
  end
  open Helper

  let t : unicode_reader_macro =
    (module struct
      let advertised_prefix = "t"
      let process _ =
        pdatum_atom
          (BoolAtom true)
          (`ReaderMacro (advertised_prefix, LiteralMacroBody "true"))
    end)

  let f : unicode_reader_macro =
    (module struct
      let advertised_prefix = "f"
      let process _ =
        pdatum_atom
          (BoolAtom false)
          (`ReaderMacro (advertised_prefix, LiteralMacroBody "false"))
    end)

  let base64n : unicode_reader_macro =
    (module struct
      let advertised_prefix = "base64n"
      let process src =
        let module S = (val src : SourceStream with type t = Uchar.t) in
        let rec get_size acc =
          let u = Array.get (S.take 1) 0 in
          if Uchar.equal u (Uchar.of_char ':') then
            int_of_string acc
          else if Uchar.compare (Uchar.of_char '0') u <= 0 &&
                  Uchar.compare u (Uchar.of_char '9') <= 0 then
            get_size (acc ^ (Uchar.to_char u |> String.make 1))
          else
            raise (Invalid_argument (string_of_uchar_array [|u|])) in
        let size = get_size "" in
        let s = S.take size |> string_of_uchar_array in
        let bs = Base64.decode_exn s |> Bytes.of_string in
        pdatum_atom
          (BytesAtom bs)
          (`ReaderMacro (advertised_prefix, StringMacroBody (string_of_int size ^ s)))
    end)

  let json : unicode_reader_macro =
    (module struct
      let advertised_prefix = "json"
      let process src =
        let module S = (val src : SourceStream with type t = Uchar.t) in
        let (>>=) o f = Option.bind o f in
        let pdatum_atom_macro atom body =
          pdatum_atom atom (`ReaderMacro (advertised_prefix, body)) in
        let pdatum_form_macro nodes fstyle fix =
          let form = {elem = (nodes, fstyle, fix); repr = `Direct} in
          let repr = `ReaderMacro (advertised_prefix, FormMacroBody form) in
          pdatum_form nodes fstyle fix repr in
        let decoder = Jsonm.decoder ~encoding:`UTF_8 `Manual in
        let rec lexeme () = match Jsonm.decode decoder with
          | `Lexeme lxm -> Some lxm
          | `End | `Error _ -> None
          | `Await ->
             let b =
               (try S.take 1 with Not_found -> [||])
               |> string_of_uchar_array |> Bytes.of_string in
             let l = Bytes.length b in
             Jsonm.Manual.src decoder b 0 l;
             lexeme () in
        let rec read_v = function
          | `Null ->
             Some (pdatum_atom_macro
                     (SymbolAtom "null")
                     (LiteralMacroBody "null"))
          | `Bool b ->
             Some (pdatum_atom_macro
                     (BoolAtom b)
                     (LiteralMacroBody (string_of_bool b)))
          | `String s ->
             Some (pdatum_atom_macro
                     (StringAtom s)
                     (StringMacroBody s))
          | `Float f ->
             Some (pdatum_atom_macro
                     (NumericAtom (string_of_float f, ""))
                     (LiteralMacroBody (string_of_float f)))
          | `As ->
             lexeme () >>= fun lxm ->
             read_a lxm >>= fun nodes ->
             Some (pdatum_form_macro nodes ListForm Infix)
          | `Os ->
             lexeme () >>= fun lxm ->
             read_o lxm >>= fun nodes ->
             Some (pdatum_form_macro nodes MapForm Infix)
          | _ -> None
        and read_a = function
          | `Ae -> Some []
          | lxm ->
             read_v lxm >>= fun v ->
             lexeme () >>= fun lxm ->
             read_a lxm >>= fun nodes ->
             Some (PDatumNode v :: nodes)
        and read_o = function
          | `Oe -> Some []
          | `Name name ->
             lexeme () >>= fun lxm1 ->
             read_v lxm1 >>= fun v ->
             lexeme () >>= fun lxm2 ->
             read_o lxm2 >>= fun nodes ->
             Some (PKeywordNode
                     (pdatum_atom_macro (SymbolAtom name) (LiteralMacroBody name), v)
                   :: nodes)
          | _ -> None in
        let json_opt =
          lexeme () >>= fun lxm ->
          read_v lxm in
        Option.get json_opt
    end)

  let csv : unicode_reader_macro =
    (module struct
      let advertised_prefix = "csv"
      let process src =
        let module S = (val src : SourceStream with type t = Uchar.t) in
        let pdatum_atom_macro atom body =
          pdatum_atom atom (`ReaderMacro (advertised_prefix, body)) in
        let pdatum_form_macro nodes fstyle fix =
          let form = {elem = (nodes, fstyle, fix); repr = `Direct} in
          let repr = `ReaderMacro (advertised_prefix, FormMacroBody form) in
          pdatum_form nodes fstyle fix repr in
        let take_while p =
          let rec loop acc =
            if p acc then acc
            else loop (acc ^ (string_of_uchar_array (S.take 1))) in
          loop "" in
        let end_mark =
          take_while
            (fun src ->
               if String.length src < 1 then false
               else Str.last_chars src 1 = "\n")
          |> (fun src -> "\n" ^ String.sub src 0 (String.length src - 1)) in
        let end_mark_len = String.length end_mark in
        let csv_body_str =
          take_while
            (fun src ->
               if String.length src < end_mark_len then false
               else Str.last_chars src end_mark_len = end_mark)
          |> (fun src -> String.sub src 0 (String.length src - end_mark_len)) in
        let csv_body = Csv.of_string csv_body_str in
        let rec loop acc =
          let row_opt = try Some (Csv.next csv_body) with End_of_file -> None in
          match row_opt, acc with
          | None, [] ->
             pdatum_form_macro
               [PKeywordNode (pdatum_atom_macro
                                (SymbolAtom "header")
                                (LiteralMacroBody "header"),
                              pdatum_form_macro [] ListForm Infix);
                PKeywordNode (pdatum_atom_macro
                                (SymbolAtom "body")
                                (LiteralMacroBody "body"),
                              pdatum_form_macro [] ListForm Infix)]
               MapForm Infix
          | None, header :: body ->
             pdatum_form_macro
               [PKeywordNode
                  (pdatum_atom_macro (SymbolAtom "header") (LiteralMacroBody "header"),
                   header);
                PKeywordNode
                  (pdatum_atom_macro (SymbolAtom "body") (LiteralMacroBody "body"),
                   pdatum_form_macro
                     (List.map (fun datum -> (PDatumNode datum)) body)
                     ListForm Infix)]
               MapForm Infix
          | Some row, _ ->
             let nodes =
               List.map
                 (fun s ->
                    PDatumNode (pdatum_atom_macro (StringAtom s) (StringMacroBody s)))
                 row in
             let datum = (pdatum_form_macro nodes ListForm Infix) in
             loop (datum :: acc) in
        loop []
    end)

  let unicode_reader_macros = [
    t; f; base64n; json; csv;
  ]
  let byte_reader_macros = []
end
