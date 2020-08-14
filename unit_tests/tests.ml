open Gensl

let output_debug = ref false

let%test "nform does not distinguish keyword node ordering" =
  let open Datatree in
  let stratom str = datom (StringAtom str) in
  let symbatom symb = datom (SymbolAtom symb) in
  let kwn1 = dkeywordnode (stratom "abc") (symbatom "abc") in
  let kwn2 = dkeywordnode (stratom "cde") (symbatom "cde") in
  let dform1 = dform [kwn1; kwn2] in
  let dform2 = dform [kwn2; kwn1] in
  let nform1 = ndatum_of_ddatum dform1 in
  let nform2 = ndatum_of_ddatum dform2 in
  Format.(
    if !output_debug then
    let ppd = pp_ddatum in
    let ppn = Normaltree.pp_ndatum in
    print_flush();
    printf "dform1: %a@.dform2: %a@.nform1: %a@.nform2: %a\n@."
     ppd dform1 ppd dform2
     ppn nform1 ppn nform2
  );
  (* assertion *)
  nform1 = nform2 &&
  Normaltree.eq_ndatum nform1 nform2

let%test "nform does not distinguish annotation node ordering" =
  let open Datatree in
  let stratom str = datom (StringAtom str) in
  let an1 = dannonode (stratom "abc") in
  let an2 = dannonode (stratom "cde") in
  let dform1 = dform [an1; an2] in
  let dform2 = dform [an2; an1] in
  let nform1 = ndatum_of_ddatum dform1 in
  let nform2 = ndatum_of_ddatum dform2 in
  Format.(
    if !output_debug then
    let ppd = pp_ddatum in
    let ppn = Normaltree.pp_ndatum in
    print_flush();
    printf "dform1: %a@.dform2: %a@.nform1: %a@.nform2: %a\n@."
     ppd dform1 ppd dform2
     ppn nform1 ppn nform2
  );
  (* assertion *)
  nform1 = nform2 &&
  Normaltree.eq_ndatum nform1 nform2

let%test "desugaring list form" =
  let open Datatree in
  let open Parsetree in
  let pdatum = pdatum_atom (StringAtom "abc") `Direct in
  let pnode = PDatumNode pdatum in
  let pform = pdatum_form [pnode] ListForm Infix `Direct in
  let ddat = ddatum_of_pdatum pform in
  let expect = dform [ddatumnode (datom (CodifiedSymbolAtom `List));
                      ddatumnode (ddatum_of_pdatum pdatum)] in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      let ppp = ParsetreePrinter.pp_pdatum in
      print_flush ();
      printf "pform: %a@.dform: %a@.expect: %a@."
        ppp pform
        ppd ddat
        ppd expect
  );
  ddat = expect

let%test "desugaring set form" =
  let open Datatree in
  let open Parsetree in
  let pdatum1 = pdatum_atom (StringAtom "abc") `Direct in
  let pdatum2 = pdatum_atom (StringAtom "def") `Direct in
  let pnode1 = PDatumNode pdatum1 in
  let pnode2 = PAnnoNode pdatum2 in
  let pform = pdatum_form [pnode1; pnode2] SetForm Infix `Direct in
  let ddat = ddatum_of_pdatum pform in
  let expect = dform [ddatumnode (datom (CodifiedSymbolAtom `Set));
                      dkeywordnode (ddatum_of_pdatum pdatum1)
                        (DAtom (BoolAtom true));
                      dannonode (ddatum_of_pdatum pdatum2)
                     ] in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      let ppp = ParsetreePrinter.pp_pdatum in
      print_flush ();
      printf "pform: %a@.dform: %a@.expect: %a@."
        ppp pform
        ppd ddat
        ppd expect
  );
  ddat = expect

let%test "desugaring map form" =
  let open Datatree in
  let open Parsetree in
  let pdatum1 = pdatum_atom (StringAtom "abc") `Direct in
  let pdatum2 = pdatum_atom (StringAtom "def") `Direct in
  let pnode = PKeywordNode (pdatum1, pdatum2) in
  let pform = pdatum_form [pnode] MapForm Infix `Direct in
  let ddat = ddatum_of_pdatum pform in
  let expect = dform [ddatumnode (datom (CodifiedSymbolAtom `Map));
                      dkeywordnode (ddatum_of_pdatum pdatum1)
                        (ddatum_of_pdatum pdatum2)
                     ] in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      let ppp = ParsetreePrinter.pp_pdatum in
      print_flush ();
      printf "pform: %a@.dform: %a@.expect: %a@."
        ppp pform
        ppd ddat
        ppd expect
  );
  ddat = expect

let%test "resugaring list form" =
  let open Datatree in 
  let open Parsetree in
  let ddatum = dform [ddatumnode (datom (CodifiedSymbolAtom `List));
                      ddatumnode (datom (StringAtom "abc"))] in
  let pdatum = pdatum_of_ddatum ddatum in
  let pnode = PDatumNode (pdatum_atom (StringAtom "abc") `Direct) in
  let expect = pdatum_form [pnode] ListForm Infix `Direct in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      let ppp = ParsetreePrinter.pp_pdatum in
      print_flush ();
      printf "dform: %a@.pform: %a@.expect: %a@."
        ppd ddatum
        ppp pdatum
        ppp expect
  );
  pdatum = expect

let%test "resugaring set form" =
  let open Datatree in 
  let open Parsetree in
  let ddatum = dform [ddatumnode (datom (CodifiedSymbolAtom `Set));
                      dkeywordnode (datom (StringAtom "abc"))
                        (DAtom (BoolAtom true));
                      dannonode (datom (StringAtom "def"))
                     ] in
  let pdatum = pdatum_of_ddatum ddatum in
  let pdatum1 = pdatum_atom (StringAtom "abc") `Direct in
  let pdatum2 = pdatum_atom (StringAtom "def") `Direct in
  let pnode = PKeywordNode (pdatum1, pdatum2) in
  let expect = pdatum_form [pnode] MapForm Infix `Direct in
  Format.(
    (* if !output_debug then *)
      let ppd = pp_ddatum in
      let ppp = ParsetreePrinter.pp_pdatum in
      print_flush ();
      printf "dform: %a@.pform: %a@.expect: %a@."
        ppd ddatum
        ppp pdatum
        ppp expect
  );
  pdatum = expect

let%test "resugaring map form" =
  let open Datatree in 
  let open Parsetree in
  let ddatum = dform [ddatumnode (datom (CodifiedSymbolAtom `Map));
                      dkeywordnode (datom (StringAtom "abc"))
                        (datom (StringAtom "def"))
                     ] in
  let pdatum = pdatum_of_ddatum ddatum in
  let pdatum1 = pdatum_atom (StringAtom "abc") `Direct in
  let pdatum2 = pdatum_atom (StringAtom "def") `Direct in
  let pnode = PKeywordNode (pdatum1, pdatum2) in
  let expect = pdatum_form [pnode] MapForm Infix `Direct in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      let ppp = ParsetreePrinter.pp_pdatum in
      print_flush ();
      printf "dform: %a@.pform: %a@.expect: %a@."
        ppd ddatum
        ppp pdatum
        ppp expect
  );
  pdatum = expect

let%test "nform -> dform ordering w/o positional nodes" =
  let open Normaltree in
  let open Datatree in
  let natom_string str = natom (StringAtom str) in
  let datom_string str = datom (StringAtom str) in
  let ann = natom_string "ann" in
  let kw1 = (natom_string "k1", natom_string "v1") in
  let kw2 = (natom_string "k2", natom_string "v2") in
  let ndatum = nform [kw1; kw2] [] [ann] in
  let ddatum = ddatum_of_ndatum ndatum in
  let dkw1 = dkeywordnode (datom_string "k1") (datom_string "v1") in
  let dkw2 = dkeywordnode (datom_string "k2") (datom_string "v2") in
  let dann = dannonode (datom_string "ann") in
  let expect = DForm [dkw1; dkw2; dann] in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      let ppn = pp_ndatum in
      print_flush ();
      printf "nform: %a@.dform: %a@.expect: %a@."
      ppn ndatum
      ppd ddatum
      ppd expect
  );
  ddatum = expect

let%test "nform -> dform ordering w/ one positional node" =
  let open Normaltree in
  let open Datatree in
  let natom_string str = natom (StringAtom str) in
  let datom_string str = datom (StringAtom str) in
  let pos = natom_string "pos" in
  let ann = natom_string "ann" in
  let kw1 = (natom_string "k1", natom_string "v1") in
  let kw2 = (natom_string "k2", natom_string "v2") in
  let ndatum = nform [kw1; kw2] [pos] [ann] in
  let ddatum = ddatum_of_ndatum ndatum in
  let dkw1 = dkeywordnode (datom_string "k1") (datom_string "v1") in
  let dkw2 = dkeywordnode (datom_string "k2") (datom_string "v2") in
  let dpos = ddatumnode (datom_string "pos") in
  let dann = dannonode (datom_string "ann") in
  let expect = DForm [dpos; dkw1; dkw2; dann] in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      let ppn = pp_ndatum in
      print_flush ();
      printf "nform: %a@.dform: %a@.expect: %a@."
      ppn ndatum
      ppd ddatum
      ppd expect
  );
  ddatum = expect

let%test "nform -> dform ordering w/ three positional nodes" =
  let open Normaltree in
  let open Datatree in
  let natom_string str = natom (StringAtom str) in
  let datom_string str = datom (StringAtom str) in
  let head = natom_string "pos1" in
  let pos2 = natom_string "pos2" in
  let pos3 = natom_string "pos3" in
  let ann = natom_string "ann" in
  let kw1 = (natom_string "k1", natom_string "v1") in
  let kw2 = (natom_string "k2", natom_string "v2") in
  let ndatum = nform [kw1; kw2] [head; pos2; pos3] [ann] in
  let ddatum = ddatum_of_ndatum ndatum in
  let dkw1 = dkeywordnode (datom_string "k1") (datom_string "v1") in
  let dkw2 = dkeywordnode (datom_string "k2") (datom_string "v2") in
  let dhead = ddatumnode (datom_string "pos1") in
  let dpos2 = ddatumnode (datom_string "pos2") in
  let dpos3 = ddatumnode (datom_string "pos3") in
  let dann = dannonode (datom_string "ann") in
  let expect = DForm [dhead; dkw1; dkw2; dpos2; dpos3; dann] in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      let ppn = pp_ndatum in
      print_flush ();
      printf "nform: %a@.dform: %a@.expect: %a@."
        ppn ndatum
        ppd ddatum
        ppd expect
  );
  ddatum = expect

let%test "eqv_cdatum disregards ordering" =
  let open Canonicaltree in
  let catom_string str = catom (StringAtom str) in
  let k1 = catom_string "k1" in
  let v1 = catom_string "v1" in
  let k2 = catom_string "k2" in
  let v2 = catom_string "v2" in
  let c1 = cform [(k1, v1); (k2, v2)] [] in
  let c2 = cform [(k2, v2); (k1, v1)] [] in
  let res = eqv_cdatum c1 c2 in
  let expect = true in
  Format.(
    if !output_debug then
      let ppc = pp_cdatum in
      print_flush ();
      printf "nform 1: %a@.nform2: %a@."
        ppc c1
        ppc c2
  );
  res = expect
        
let%test "eqv_ndatum disregards ordering" =
  let open Normaltree in
  let natom_string str = natom (StringAtom str) in
  let k1 = natom_string "k1" in
  let v1 = natom_string "v1" in
  let k2 = natom_string "k2" in
  let v2 = natom_string "v2" in
  let n1 = nform [(k1, v1); (k2, v2)] [] [] in
  let n2 = nform [(k2, v2); (k1, v1)] [] [] in
  let res = eq_ndatum n1 n2 in
  let expect = true in
  Format.(
    if !output_debug then
      let ppn = pp_ndatum in
      print_flush ();
      printf "nform 1: %a@.nform2: %a@."
        ppn n1
        ppn n2
  );
  res = expect

let%test "eqv_ndatum disregards annotations (1)" =
  let open Normaltree in
  let natom_string str = natom (StringAtom str) in
  let s1 = natom_string "foo" in
  let a1 = natom_string "bar" in
  let n1 = nform [] [s1] [a1] in
  let n2 = nform [] [s1] [] in
  let res = eq_ndatum n1 n2 in
  let expect = true in
  Format.(
    if !output_debug then
      let ppn = pp_ndatum in
      print_flush ();
      printf "cform 1: %a@.cform2: %a@."
        ppn n1
        ppn n2
  );
  res = expect

let%test "eqv_ndatum disregards annotations (2)" =
  let open Normaltree in
  let natom_string str = natom (StringAtom str) in
  let s1 = natom_string "foo" in
  let s2 = natom_string "bar" in
  let n1 = s1 in
  let n2 = nannotated s1 [s2] in
  let res = eq_ndatum n1 n2 in
  let expect = true in
  Format.(
    if !output_debug then
      let ppn = pp_ndatum in
      print_flush ();
      printf "nform 1: %a@.nform2: %a@."
        ppn n1
        ppn n2
  );
  res = expect

let%test "eqv_ddatum disregards ordering and annotations (1)" =
  let open Datatree in
  let datom_string str = datom (StringAtom str) in
  let k1 = datom_string "k1" in
  let v1 = datom_string "v1" in
  let k2 = datom_string "k2" in
  let v2 = datom_string "v2" in
  let ann1 = datom_string "foo" in
  let ann2 = datom_string "bar" in
  let d1 = dform [dkeywordnode k1 v1; dkeywordnode k2 v2; dannonode ann1] in
  let d2 = dform [dkeywordnode k2 v2; dkeywordnode k1 v1; dannonode ann2] in
  let res = eqv_ddatum d1 d2 in
  let expect = true in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      print_flush ();
      printf "dform 1: %a@.dform2: %a@."
        ppd d1
        ppd d2
  );
  res = expect

let%test "eqv_ddatum disregards ordering and annotations (2)" =
  let open Datatree in
  let datom_string str = datom (StringAtom str) in
  let s1 = datom_string "foo" in
  let s2 = datom_string "bar" in
  let s3 = datom_string "test" in
  let d1 = s1 in
  let d2 = dannotated s1 [s2] [s3] in
  let res = eqv_ddatum d1 d2 in
  let expect = true in
  Format.(
    if !output_debug then
      let ppd = pp_ddatum in
      print_flush ();
      printf "dform 1: %a@.dform2: %a@."
        ppd d1
        ppd d2
  );
  res = expect
