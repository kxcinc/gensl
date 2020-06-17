[@@@ocaml.warning "-33"]

open Gensl
open Datatree
open Parsetree
open Atoms

let ppf = Format.std_formatter

let nospan mode elem =
  { elem; mode; span = {
        span_start = 0;
        span_end = 0;
        span_leading = Leading " ";
        span_source = `DirectInput None; }
  }

open Format

let _ =
  let neko m = PAtom (nospan m (SymbolAtom "neko"), `Intrinsic) in
  List.iter
    (fun m -> pp_parsetree ppf (neko m); print_newline())
    [Commonfix; Phantomfix;
     Prefix `GrabOne;
     Prefix `GrabAll;
     Prefix (`GrabK 3);
     Postfix `GrabOne;
     Postfix `GrabAll;
     Postfix (`GrabK 3);
    ];
  let neko = neko Commonfix in
  let f1 = PForm
             (([neko; neko] |&> (fun x -> PDatumNode x) |> nospan Commonfix,
               FinmapForm)
              |> nospan (Postfix (`GrabK 4))) in
  pp_parsetree ppf f1; print_newline()

