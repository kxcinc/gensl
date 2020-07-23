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
