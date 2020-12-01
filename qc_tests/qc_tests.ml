[@@@warning "-all"]
open Gensl
open Intf

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
    | SymbolAtom s -> string s @@ fun s' -> yield (SymbolAtom s)
    | CodifiedSymbolAtom _ -> ()
    | StringAtom s -> string s @@ fun s' -> yield (StringAtom s)
    | BytesAtom b -> string (Bytes.to_string b) @@ fun s' -> yield (BytesAtom (Bytes.unsafe_of_string s'))
    | NumericAtom (n, a) -> (pair int string) (int_of_string n, a) @@ fun (n', a') -> yield (NumericAtom (string_of_int n', a'))
    | BoolAtom _ -> ()
  
  let atoma = make ~shrink:atom_shrink atom
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

  let rec cdatum_shrink (cdatum: cdatum) (yield: cdatum -> unit) =
    let open Shrink in
    match cdatum with
    | CAtom a -> atom_shrink a @@ fun a' -> yield (CAtom a')
    | CForm { ckwd; cpos } ->
      (pair (list ~shrink:(pair cdatum_shrink cdatum_shrink)) (list ~shrink:cdatum_shrink))
        (ckwd, cpos) @@ 
        (fun (ckwd', cpos') -> yield (CForm { ckwd = ckwd'; cpos = cpos' }))

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
          (fun nkwd npos nann ->
            Normaltree.nform nkwd npos nann)
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
      fun (dat', anns') -> yield (Normaltree.nannotated dat' anns')
    | NForm { n_keywordeds; n_positionals; n_annotations } ->
      let shrink_kw: (ndatum * ndatum) Shrink.t = pair ndatum_shrink ndatum_shrink in
      (* CAUTION: can be very, very slow! *)
      (triple (list ~shrink:shrink_kw) (list ~shrink:ndatum_shrink) (list ~shrink:ndatum_shrink))
        (n_keywordeds, n_positionals, n_annotations) @@
      (fun (kws', poses', anns') -> yield (NForm { n_keywordeds  = kws';
                                                   n_positionals = poses';
                                                   n_annotations = anns' }))

  let (_: ndatum Shrink.t) = ndatum_shrink

  let ndatum = make ~print:ndatum_printer ~shrink:ndatum_shrink ndatum_gen

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

  let ddatum_gen = sized @@ fix (fun self n ->
      match n with
      | 0 | 1 -> (fun x -> DAtom x) <$> atom
      | _ -> 
         let fsize = (max 1 (n/6)) in
         let dnode_gen = fun g n m ->
           oneof
             [ map (fun d -> DDatumNode d) (g n);
               map (fun d -> DAnnoNode d) (g n);
               map2 (fun d d' -> (DKeywordNode (d, d'))) (g n) (g m) ] in
         (1 -- fsize) >>= fun fac1 ->
         (1 -- fsize) >>= fun fac2 ->
         map (fun dnodes -> Datatree.dform dnodes)
           (list_size (pure (n/fac1*2/9)) @@ dnode_gen self fac1 fac2))   

  let ddatum_printer ddatum =
    let open Datatree in
    let open Format in
    fprintf str_formatter "%a" pp_ddatum ddatum;
    flush_str_formatter ()   

  let rec ddatum_shrink (ddatum: ddatum) (yield: ddatum -> unit) =
    let open Shrink in
    match ddatum with
    | DAtom a -> atom_shrink a @@ fun a' -> yield (DAtom a')
    | DAnnotated {d_annotated = dat;
                  d_anno_front = anns_f;
                  d_anno_back = anns_b} ->
       (* (pair ndatum_shrink (list ~shrink:ndatum_shrink)) (dat, anns) @@
         fun (dat', anns') -> yield (Normaltree.nannotated dat' anns')
    | NForm { n_keywordeds; n_positionals; n_annotations } ->
       let shrink_kw: (ndatum * ndatum) Shrink.t = pair ndatum_shrink ndatum_shrink in
       (* CAUTION: can be very, very slow! *)
       (triple (list ~shrink:shrink_kw) (list ~shrink:ndatum_shrink) (list ~shrink:ndatum_shrink))
         (n_keywordeds, n_positionals, n_annotations) @@
         (fun (kws', poses', anns') -> yield (NForm { n_keywordeds  = kws';
                                                      n_positionals = poses';
                                                      n_annotations = anns' })) *)
       [%noimplval]

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

let unless b msg = if not b then QCheck.Test.fail_report msg else true

let test_cdatum_ndatum =
  let open Normaltree in
  let open TreeGen in
  QCheck.(Test.make ~name:"cdatum_of_ndatum \\o ndatum_of_cdatum is id"
            ~count:100
            cdatum
            (fun cdatum -> (cdatum_of_ndatum (ndatum_of_cdatum cdatum)) = cdatum))

(* minimum failing case: ((anno (bytes:)) (anno ())) *)
let test_ndatum_ddatum =
  let open Datatree in
  let open Normaltree in
  let open TreeGen in
  QCheck.(Test.make ~name:"ndatum_of_ddatum \\o ddatum_of_ndatum is id"
            ~count:100
            ndatum
            (fun ndatum ->
              unless
                ((ndatum_of_ddatum (ddatum_of_ndatum ndatum)) = ndatum)
                Format.(asprintf "ntree : %a@.dtree : %a@.ntree': %a@."
                          pp_ndatum ndatum
                          pp_ddatum (ddatum_of_ndatum ndatum)
                          pp_ndatum (ndatum_of_ddatum (ddatum_of_ndatum ndatum)))))

let test_cdatum_ordering =
  let open Canonicaltree in
  let open TreeGen in
  let open QCheck in
  let (<<=) a b = cdatum_ordering a b <= 0 in
  let antisymmetric ?case a b =
    let res =
      (a <<= b && b <<= a) ==> (a = b) in
    unless res Format.(
      sprintf "antisymm (%s) %d %d"
        (Option.value ~default:"nocase" case)
        (cdatum_ordering a b)
        (cdatum_ordering b a)) in
  let transitive a b c =
    (a <<= b && b <<= c) ==> (a <<= c) in
  Test.make ~name:"cdatum_ordering looks like a linear order"
    ~count:100
    (quad cdatum cdatum cdatum bool)
    (fun (a, b, c, switch) ->
      (if switch then begin
              antisymmetric ~case:"a b" a b
           && antisymmetric ~case:"b c" b c
           && antisymmetric ~case:"a c" a c
         end
        else unless (antisymmetric a a) "antisymm a a") &&
       (if a <<= b
        then unless (transitive a b c) "trans a b c"
        else unless (transitive b a c) "trans b a c"))

let test_atom_dest_cons_canonical = 
  let open CanonicaltreeFlavor in
  let open BasetypesGen in
  QCheck.(Test.make ~name:"Test constructors and destructors for CanonicaltreeFlavor"
            ~count:100
            atoma
            (fun a -> CanonicaltreeFlavor.atom (mkatom a) = a))

let test_eqv_self_canonical = 
  let open CanonicaltreeFlavor in
  let open TreeGen in
  QCheck.(Test.make ~name:"Test that a tree is equivalent to itself (CanonicaltreeFlavor)"
            ~count:100
            cdatum
            (fun c -> CanonicaltreeFlavor.eqv c c))

let test_atom_dest_cons_normal = 
  let open Normaltreeflavor in
  let open BasetypesGen in
  QCheck.(Test.make ~name:"Test constructors and destructors for Normaltreeflavor"
            ~count:100
            atoma
            (fun a -> Normaltreeflavor.atom (mkatom a) = a))

let test_atom_dest_cons_data = 
  let open Datatreeflavor in
  let open BasetypesGen in
  QCheck.(Test.make ~name:"Test constructors and destructors for Datatreeflavor"
            ~count:100
            atoma
            (fun a -> Datatreeflavor.atom (mkatom a) = a))

let test_atom_dest_cons_parse = 
  let open Parsetreeflavor in
  let open BasetypesGen in
  QCheck.(Test.make ~name:"Test constructors and destructors for Parsetreeflavor"
            ~count:100
            atoma
            (fun a -> Parsetreeflavor.atom (mkatom a) = a))

let test_eqv_self_parse = 
  let open Parsetreeflavor in
  let open TreeGen in
  QCheck.(Test.make ~name:"Test that a tree is equivalent to itself (Parsetreeflavor)"
            ~count:100
            pdatum
            (fun c -> Parsetreeflavor.eqv c c))

let () =
  let open QCheck_runner in
  run_tests_main
    ~n:20
    [
      test_id;
      test_cdatum_ordering;
      test_cdatum_ndatum;
      test_ndatum_ddatum;
      test_atom_dest_cons_canonical;
      test_atom_dest_cons_normal;
      test_atom_dest_cons_data;
      test_atom_dest_cons_parse;
      test_eqv_self_canonical;
    ]
