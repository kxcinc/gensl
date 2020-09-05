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

  let atom_symb = ident >|= fun x -> SymbolAtom x
  let atom_csymb = csymb >|= fun x -> CodifiedSymbolAtom x
  let atom_string = ident >|= fun x -> StringAtom x
  (* XXX no native bytes in qcheck, what to do? *)
  let atom_bytes = string >|= fun x -> BytesAtom (Bytes.of_string x)
  let atom_numeric =
    let f num suffix = NumericAtom (string_of_int num, suffix) in
    map2 f int (oneof [pure ""; ident])
  let atom_bool = bool >|= fun x -> BoolAtom x
  let atom = oneof [ atom_symb; atom_csymb; atom_string; atom_bytes;
                     atom_numeric; atom_bool ]
end

let () =
  QCheck_runner.run_tests_main
    [test_id]
