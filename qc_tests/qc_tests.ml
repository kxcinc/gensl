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
  let trivial_atom = pure (SymbolAtom "atom")

  type atom = Basetypes.atom =
    | SymbolAtom of string
    | CodifiedSymbolAtom of csymb
    | StringAtom of string
    | BytesAtom of bytes
    | NumericAtom of string*string (** [numeric, suffix] *)
    | BoolAtom of bool

  let atom_shrink (a: atom) (yield: atom -> unit) =
    let open Shrink in
    match a with
    | SymbolAtom s -> yield (SymbolAtom "") (* string s @@ fun s' -> yield (SymbolAtom s) *)
    | CodifiedSymbolAtom _ -> ()
    | StringAtom s -> yield (StringAtom "") (* string s @@ fun s' -> yield (StringAtom s) *)
    | BytesAtom b -> yield (BytesAtom Bytes.empty) (* string (Bytes.unsafe_to_string b) @@ fun b' -> yield (BytesAtom (Bytes.of_string b')) *)
    | NumericAtom (n, a) -> yield (NumericAtom ("0", "")) (*(pair nil string) (n, a) @@ fun (n', a') -> yield (NumericAtom (n', a'))*)
    | BoolAtom _ -> ()

  let (_: atom Shrink.t) = atom_shrink
  
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
  
  let cdatum_gen = sized @@ fix (fun self n ->
      match n with
      | 0 | 1 -> (fun x -> CAtom x) <$> atom;
      | _ ->
         (1 -- (max 1 (n/6))) >>= fun factor1 -> 
         (1 -- (max 1 (n/6))) >>= fun factor2 -> 
         map2 (fun ckwd cpos -> CForm { ckwd; cpos })
           (list_size (pure (n/factor1*2/9)) @@ pair (self factor1) (self factor1))
           (list_size (pure (n/factor2*7/9)) @@ self (factor2)))

  let cdatum_printer cdatum =
    let open Canonicaltree in
    let open Format in
    fprintf str_formatter "%a" pp_cdatum cdatum;
    flush_str_formatter ()

  let cdatum_shrink (cdatum: cdatum) (yield: cdatum -> unit) =
    let open Shrink in
    match cdatum with
    | CAtom a -> atom_shrink a @@ fun a' -> yield (CAtom a')
    | _ -> ()

  let (_: cdatum Shrink.t) = cdatum_shrink
  
  let cdatum = make ~print:cdatum_printer ~shrink:cdatum_shrink cdatum_gen
      
  type ndatum = Gensl.Normaltree.ndatum =
    | NAtom of atom
    | NForm of {
        n_keywordeds  : (ndatum, ndatum) assoc;
        n_positionals : ndatum list;
        n_annotations : ndatum set;
      }
    | NAnnotated of ndatum * ndatum set

  let ndatum_gen = sized @@ fix (fun self n ->
      match n with
      | 0 | 1 -> (fun x -> NAtom x) <$> atom
      | _ ->
         let fsize = (max 1 (n/6)) in
         (1 -- fsize) >>= fun fac1 -> 
         (1 -- fsize) >>= fun fac2 -> 
         (1 -- fsize) >>= fun fac3 ->
         map3
          (fun nkwd npos nann -> NForm { n_keywordeds  = nkwd ;
                                         n_positionals = npos ;
                                         n_annotations = nann })
          (list_size (pure (n/fac1*2/9)) @@ pair (self fac1) (self fac1))
          (list_size (pure (n/fac2*5/9)) @@ self (fac2))
          (list_size (pure (n/fac3*2/9)) @@ self (fac3)))

  let ndatum_printer ndatum =
    let open Normaltree in
    let open Format in
    fprintf str_formatter "%a" pp_ndatum ndatum;
    flush_str_formatter ()

  let rec ndatum_shrink (ndatum: ndatum) (yield: ndatum -> unit) =
    let open Shrink in
    match ndatum with
    | NAtom a -> atom_shrink a @@ fun a' -> yield (NAtom a')
    | NAnnotated (dat, anns) ->
      (pair ndatum_shrink (list ~shrink:ndatum_shrink)) (dat, anns) @@
      fun (dat', anns') -> yield (NAnnotated (dat', anns'))
    | NForm { n_keywordeds; n_positionals; n_annotations } ->
      let shrink_kw: (ndatum * ndatum) Shrink.t = pair ndatum_shrink ndatum_shrink in
      (triple list list (list ~shrink:ndatum_shrink))
      (*(triple (list ~shrink:shrink_kw) (list ~shrink:ndatum_shrink) (list ~shrink:ndatum_shrink))*)
        (n_keywordeds, n_positionals, n_annotations) @@
      (fun (kws', poses', anns') -> yield (NForm { n_keywordeds  = kws';
                                                   n_positionals = poses';
                                                   n_annotations = anns' }))

  let (_: ndatum Shrink.t) = ndatum_shrink

  let ndatum = make ~print:ndatum_printer ~shrink:ndatum_shrink ndatum_gen
end

let rec size_of_cdatum = Canonicaltree.(
    function
    | CAtom _ -> 1
    | CForm {ckwd; cpos} ->
       let count datums =
         datums |&> size_of_cdatum
         |> List.foldl (+) 0 in
       let ckwd = List.fmap (fun (a,b) -> [a; b]) ckwd in
       count ckwd + count cpos + 1)

let test_cdatum_ndatum =
  let open Normaltree in
  let open TreeGen in
  QCheck.(Test.make ~name:"cdatum_of_ndatum \\o ndatum_of_cdatum is id"
            ~count:100
            cdatum
            (fun cdatum -> (cdatum_of_ndatum (ndatum_of_cdatum cdatum)) = cdatum))

let test_ndatum_ddatum =
  let open Datatree in
  let open TreeGen in
  QCheck.(Test.make ~name:"ndatum_of_ddatum \\o ddatum_of_ndatum is id"
            ~count:100
            ndatum
            (fun ndatum -> (ndatum_of_ddatum (ddatum_of_ndatum ndatum)) = ndatum))

let () =
  let open QCheck_runner in
  (* set_seed 80837877; *)
  run_tests_main
    ~n:20
    [ test_id;
      test_cdatum_ndatum;
      test_ndatum_ddatum;
    ]
