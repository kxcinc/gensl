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

module CtreeGen = struct
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
    )

end

(** TEST: cdatum_ordering indeed seems like linear ordering *)
let () =
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
      else transitivity b a c)

