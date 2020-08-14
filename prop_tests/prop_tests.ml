[@@@warning "-all"]
open Gensl

(** TEST: identity function *)
let () =
  let identity x =
    Crowbar.check_eq x x in
  Crowbar.(add_test ~name:"identity function" [int] (fun i -> identity i))

module BasetypesGen = struct
  open Crowbar
  open Basetypes

  let csymb_list =
    [   `Toplevel ; `Envelop ; `Metadata   (* 0..2 *)
      ; `Desc ; `Hash ; `Uuid ; `Version   (* 3..6 *)
      ; `List ; `Vector ; `Set ; `Map      (* 7..10 *)
      ; `Int ; `Uint ; `Float ; `Timestamp (* 11..14 *)
      (* 15..19: reserved *)
      ; `Appsymb01 ; `Appsymb02 ; `Appsymb03 ; `Appsymb04 (* 20..23 *)
      ; `Appsymb05 ; `Appsymb06 ; `Appsymb07 ; `Appsymb08 (* 24..27 *)
      ; `Appsymb09 ; `Appsymb10 ; `Appsymb11 ; `Appsymb12 (* 28..31 *) ]

  let csymb = csymb_list |&> const |> choose
  let lowercase = map [range ~min:97 26] char_of_int
  let num = map [range ~min:48 10] char_of_int
  let ident = map [lowercase; list (choose [lowercase; num])] @@
                fun lead body ->
                let body = Array.of_list body in
                String.init (Array.length body + 1) @@
                  function 0 -> lead
                         | idx -> body.(idx-1)

  let atom_symb = map [ident] @@ fun x -> SymbolAtom x
  let atom_csymb = map [csymb] @@ fun x -> CodifiedSymbolAtom x
  let atom_string = map [ident] @@ fun x -> StringAtom x (* XXX more variations *)
  let atom_bytes = map [bytes] @@ fun x -> BytesAtom (Bytes.of_string x)
  let atom_numeric = (* XXX more variations *)
    map [int; choose [const ""; ident]] @@
      fun num suffix -> NumericAtom (string_of_int num, suffix)
  let atom_bool = map [bool] @@ fun x -> BoolAtom x
  let atom = choose [ atom_symb; atom_csymb; atom_string; atom_bytes;
                      atom_numeric; atom_bool;]
end

module TreeGen = struct
  open Basetypes
  open BasetypesGen
  open Crowbar

  type cdatum = Gensl.Canonicaltree.cdatum =
    | CAtom of atom
    | CForm of {
        ckwd : (cdatum, cdatum) assoc;
        cpos : cdatum list;
      }
  let cdatum = fix (fun cdatum ->
      let kwd = pair cdatum cdatum in
      let pos = cdatum in
      choose [
        map [atom] @@ (fun x -> CAtom x);
        map [list kwd; list pos] @@
          fun ckwd cpos -> CForm { ckwd; cpos }
      ]
    ) |> with_printer Canonicaltree.pp_cdatum

  type ndatum = Gensl.Normaltree.ndatum =
    | NAtom of atom
    | NForm of {
        n_keywordeds  : (ndatum, ndatum) assoc;
        n_positionals : ndatum list;
        n_annotations : ndatum set;
      }
    | NAnnotated of ndatum * ndatum set

  let ndatum = fix (fun ndatum ->
      let entry = pair ndatum ndatum in
      let item = ndatum in
      choose [
        map [atom] @@ (fun x -> NAtom x);
        map [list entry; list item; list item] @@
        (fun nkwd npos nann -> NForm { n_keywordeds  = nkwd;
                                       n_positionals = npos;
                                       n_annotations = nann });
        map [item; list item] @@ (fun dat ann -> NAnnotated (dat, ann))
      ]) |> with_printer Normaltree.pp_ndatum

  type ddatum = Gensl.Datatree.ddatum =
    | DAtom of atom (* an atom *)
    | DForm of dnode list (* a form is represented as a list of nodes *)
    | DAnnotated of {
        d_annotated : ddatum;
        d_anno_front : ddatum list;
        d_anno_back : ddatum list;
      }
  and  dnode = Gensl.Datatree.dnode =
    | DKeywordNode of ddatum * ddatum
    | DDatumNode of ddatum
    | DAnnoNode of ddatum

  let ddatum = fix (fun ddatum ->
      let dnode = choose [
          map [ddatum; ddatum] @@ (fun k v -> DKeywordNode (k, v));
          map [ddatum] @@ (fun x -> DDatumNode x);
          map [ddatum] @@ (fun x -> DAnnoNode x)] in
      choose [
        map [atom] @@ (fun x -> DAtom x);
        map [list dnode] @@ (fun nodes -> DForm nodes);
        map [ddatum; list ddatum; list ddatum] @@
        (fun a b c -> DAnnotated {d_annotated  = a;
                                  d_anno_front = b;
                                  d_anno_back  = c})
      ]) |> with_printer Datatree.pp_ddatum
end

let () =
  let open Crowbar in
  let open Normaltree in
  let open TreeGen in
  let prop cdatum =
    check_eq (cdatum_of_ndatum (ndatum_of_cdatum cdatum)) cdatum in
  add_test ~name:"cdatum_to_ndatum \\o ndatum_of_cdatum is id"
    [cdatum]
    prop

let () =
  let open Crowbar in
  let open Normaltree in
  let open Datatree in
  let open TreeGen in
  let prop ndatum =
    let wirestringify ppf ndatum =
      ndatum |> ddatum_of_ndatum |> Parsetree.pdatum_of_ddatum |>
      Unparse.unparse_pdatum ppf in
    let ndatum' = ndatum_of_ddatum (ddatum_of_ndatum ndatum) in
    if ndatum <> ndatum' then
      begin
        Format.printf "%a\n%a\n" pp_ndatum ndatum
          pp_ndatum ndatum';
      end;
    check_eq (ndatum_of_ddatum (ddatum_of_ndatum ndatum)) ndatum in
  add_test ~name:"ndatum_to_ddatum \\o ddatum_of_ndatum is id"
    [ndatum]
    prop

let () =
  let open Crowbar in
  let open Normaltree in
  let open Datatree in
  let open Parsetree in
  let open TreeGen in
  let prop ddatum =
    let ddatum' = ddatum_of_pdatum (pdatum_of_ddatum ddatum) in
    if ddatum <> ddatum' then
      begin
        Format.printf "%a\n%a\n" pp_ddatum ddatum
          pp_ddatum ddatum';
      end;
    check_eq (ddatum_of_pdatum (pdatum_of_ddatum ddatum)) ddatum in
  add_test ~name:"ddatum_to_pdatum \\o pdatum_of_ddatum is id"
    [ddatum]
    prop

(** TEST: cdatum_ordering indeed seems like linear ordering *)
(* let () =
  let open Crowbar in
  let open Canonicaltree in
  let open CtreeGen in
  let (<<=) a b = cdatum_ordering a b <= 0 in
  let antisymmetry a b =
    guard (a <<= b && b <<= a);
    if a <> b
    then Format.(
      printf "yes? %d %d\n" (cdatum_ordering a b) (cdatum_ordering b a));
    check_eq ~pp:pp_cdatum a b in
  let transitivity a b c =
    guard (a <<= b && b <<= c);
    check (a <<= c) in
  add_test ~name:"cdatum_ordering indeed seems like linear ordering"
    [cdatum; cdatum; cdatum; bool]
    (fun a b c switch ->
      if switch then
        (antisymmetry a b; antisymmetry b a; antisymmetry a c)
      else antisymmetry a a;
      if a <<= b
      then transitivity a b c
      else transitivity b a c) *)
