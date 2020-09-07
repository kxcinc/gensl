[@@@warning "-all"]
open Gensl

let test_id = QCheck.(Test.make ~count:100 int (fun x -> x = x))

module BasetypesGen = struct
  open Basetypes
  open QCheck
  open Gen

  let csymb_list =
    [   `Toplevel ; `Envelop ; `Metadata   (* 0..2 *)
    ; `Desc ; `Hash ; `Uuid ; `Version   (* 3..6 *)
    ; `List ; `Vector ; `Set ; `Map      (* 7..10 *)
    ; `Int ; `Uint ; `Float ; `Timestamp (* 11..14 *)
    (* 15..19: reserved *)
    ; `Appsymb01 ; `Appsymb02 ; `Appsymb03 ; `Appsymb04 (* 20..23 *)
    ; `Appsymb05 ; `Appsymb06 ; `Appsymb07 ; `Appsymb08 (* 24..27 *)
    ; `Appsymb09 ; `Appsymb10 ; `Appsymb11 ; `Appsymb12 (* 28..31 *) ]

  let csymb = csymb_list |&> pure |> oneof
  let lowercase = int_range 26 97 >|= char_of_int
  let num = int_range 10 48 >|= char_of_int
  let ident =
    let f lead body =
      let body = Array.of_list body in
      String.init (Array.length body + 1) @@
      function 0 -> lead
             | idx -> body.(idx-1)
    in
    map2 f lowercase @@ (oneof [lowercase; num] |> list)

  let atom_symb = (fun x -> SymbolAtom x) <$> ident
  let atom_csymb = (fun x -> CodifiedSymbolAtom x) <$> csymb
  let atom_string = (fun x -> StringAtom x) <$> ident
  (* XXX no native bytes in qcheck, what to do? *)
  let atom_bytes = (fun x -> BytesAtom (Bytes.of_string x)) <$> string
  let atom_numeric =
    let f num suffix = NumericAtom (string_of_int num, suffix) in
    map2 f int (oneof [pure ""; ident])
  let atom_bool = (fun x -> BoolAtom x) <$> bool
  let atom = oneof [ atom_symb; atom_csymb; atom_string; atom_bytes;
                     atom_numeric; atom_bool ]
end

module TreeGen = struct
  open Basetypes
  open BasetypesGen
  open QCheck
  open Gen

  type cdatum = Gensl.Canonicaltree.cdatum =
    | CAtom of atom
    | CForm of {
        ckwd : (cdatum, cdatum) assoc;
        cpos : cdatum list;
      }
      
  (* fix with no i.e. trivial parameter *)
  let fix1 (f: 'a t -> 'a t): 'a t = fix (fun f () -> f ()) ()
  
  let cdatum_gen = sized_size small_nat @@ fix (fun self n ->
      match n with
      | 0 -> (fun x -> CAtom x) <$> atom;
      | _ ->
        frequency [
          1, (fun x -> CAtom x) <$> atom;
          2, map2 (fun ckwd cpos -> CForm { ckwd; cpos })
            (list @@ pair (self (n/20)) (self (n/20)))
            (list @@ self (n/10))])

  let cdatum_printer cdatum =
    let open Canonicaltree in
    let open Format in
    fprintf str_formatter "%a" pp_cdatum cdatum;
    flush_str_formatter ()

  let cdatum = make ~print:cdatum_printer cdatum_gen
      
  type ndatum = Gensl.Normaltree.ndatum =
    | NAtom of atom
    | NForm of {
        n_keywordeds  : (ndatum, ndatum) assoc;
        n_positionals : ndatum list;
        n_annotations : ndatum set;
      }
    | NAnnotated of ndatum * ndatum set

  let ndatum_gen = sized_size small_nat @@ fix (fun self n ->
      frequency [
        1, (fun x -> NAtom x) <$> atom;
        3, map3
          (fun nkwd npos nann -> NForm { n_keywordeds  = nkwd ;
                                         n_positionals = npos ;
                                         n_annotations = nann })
          (list (pair (self (n/30)) (self (n/30))))
          (list (self (n/15)))
          (list (self (n/15)));
        2, map2
          (fun dat ann -> NAnnotated (dat, ann))
          (self (n/10))
          (list (self (n/20)))
      ])

  let ndatum_printer ndatum =
    let open Normaltree in
    let open Format in
    fprintf str_formatter "%a" pp_ndatum ndatum;
     flush_str_formatter ()

  let ndatum = make ~print:ndatum_printer ndatum_gen
end

let test_cdatum_ndatum =
  let open Normaltree in
  let open TreeGen in
  QCheck.(Test.make ~name:"cdatum_of_ndatum \\o ndatum_of_cdatum is id"
            ~count:20
            cdatum
            (fun cdatum -> (cdatum_of_ndatum (ndatum_of_cdatum cdatum)) = cdatum))

let test_ndatum_ddatum =
  let open Datatree in
  let open TreeGen in
  QCheck.(Test.make ~name:"ndatum_of_ddatum \\o ddatum_of_ndatum is id"
            ~count:20
            ndatum
            (fun ndatum -> (ndatum_of_ddatum (ddatum_of_ndatum ndatum)) = ndatum))

let () = ignore (|&>)

let () =
  QCheck_runner.run_tests_main
    ~n:20
    [ test_id;
      test_cdatum_ndatum;
      test_ndatum_ddatum
    ]
