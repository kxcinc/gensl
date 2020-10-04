type sexp = Ppx_sexp_conv_lib.Sexp.t
open Gensl
open Basetypes

type formatter = Format.formatter
type atom = Basetypes.atom

type ctree = Canonicaltree.cdatum
type ntree = Normaltree.ndatum
type dtree = Datatree.ddatum
type ptree = Parsetree.pdatum

type _ treeflavor =
  | Ctree : ctree treeflavor
  | Ntree : ntree treeflavor
  | Dtree : dtree treeflavor
  | Ptree : ptree treeflavor

let string_of_treeflavor : type x. x treeflavor -> string = function
  | Ctree -> "canonicaltree"
  | Ntree -> "normaltree"
  | Dtree -> "datatree"
  | Ptree -> "parsetree"

module type Treeflavor = sig
  type datum
  type flavor
  val treeflavor : flavor treeflavor

  (** utilities *)

  val compare : datum -> datum -> int
  val eqv : datum -> datum -> bool
  val pp : formatter -> datum -> unit
  val to_string : ?pretty:bool -> datum -> string
  val datum_of_sexp : sexp -> datum
  val sexp_of_datum : datum -> sexp
  
  (** destructors *)

  val atom : datum -> atom
  val npos : pos:int -> datum -> datum
  val kval : key:datum -> datum -> datum
  val root : datum -> datum
  val anno : datum -> datum list
  
  (** constructors & case analyzer *)

  val mkatom : atom -> datum
  val mkform : keyworded:(datum*datum) list ->
               positional:datum list ->
               datum
  val case : atom:(datum -> 'r) ->
             form:(keyworded:(datum*datum) list ->
                   positional:datum list ->
                   'r) ->
             datum -> 'r

  (** updaters *)

  val update_atom : atom -> datum -> datum
  val update_npos : pos:int   -> datum option -> datum -> datum
  val update_kval : key:datum -> datum option -> datum -> datum
  val update_root : datum -> datum -> datum
  val update_anno : datum list -> datum -> datum

  (** converters *)

  val to_canonicaltree : datum -> ctree
  val to_normaltree    : datum -> ntree
  val to_datatree      : datum -> dtree
  val to_parsetree     : datum -> ptree
end

let update_assoc (k : 'a) (nv : 'b) (l : ('a * 'b) list) : ('a * 'b) list =
  let go (key, v) acc = if k = key then (key, nv) :: acc else (key, v) :: acc in
  List.fold_right go [] l

let update (n: int) (x: 'a) (xs: 'a list): 'a list =
  List.mapi (fun i v -> if n = i then x else v) xs

let remove (n: int) (xs: 'a list): 'a list =
  List.filteri (fun i _ -> n = i) xs

(* TASK 1 *)
(* module CanonicaltreeFlavor : Treeflavor = struct
 *   (\* something here *\)
 * end *)
(* and for the other trees *)
module CanonicaltreeFlavor : Treeflavor = struct
  open Canonicaltree
  type datum = Canonicaltree.cdatum
  (*
    | CAtom of atom
    | CForm of {
        ckwd : (cdatum, cdatum) assoc;
        cpos : cdatum list;
      } *)
      
  type flavor = ctree
      
  let treeflavor : ctree treeflavor = Ctree

  let compare : datum -> datum -> int = cdatum_ordering
  let eqv c c': bool = cdatum_ordering c c' = 0
  let pp : formatter -> datum -> unit = pp_cdatum
  let to_string ?pretty:(_=false) = Format.asprintf "%a" pp
  let datum_of_sexp : sexp -> datum = [%noimplval]
  let sexp_of_datum : datum -> sexp = sexp_cdatum
  
  (** destructors *)

  let atom dat =
    match dat with
    | CAtom a -> a
    | _ -> failwith "Not an atom!"
             
  let npos ~pos dat =
    match dat with
    | CForm { ckwd = _; cpos = poses } ->
      List.nth poses pos
    | _ -> failwith "No positional arguments!"
  
  let kval ~key dat =
    match dat with
    | CForm { ckwd = kws; cpos = _ } ->
      (match List.assoc_opt key kws with
       | Some v -> v
       | None -> failwith "Invalid key!")
    | _ -> failwith "No keyword arguments!"
  
  let root : datum -> datum = fun d -> d
  (* No annotations at the Canonicaltree level *)
  let anno : datum -> datum list = fun _ -> []
  
  (** constructors & case analyzer *)

  let mkatom : atom -> datum = fun a -> CAtom a
  let mkform ~keyworded ~positional =
    CForm {ckwd = keyworded; cpos = positional}
  (* val case : atom:(datum -> 'r) ->
             form:(keyworded:(datum*datum) list ->
                   positional:datum list ->
                   'r) ->
             datum -> 'r*)
  let case ~atom ~form d =
    match d with
    | CAtom _ -> atom d
    | CForm {ckwd; cpos} ->
      form ~keyworded:ckwd ~positional:cpos

  (** updaters *)

  let update_atom a dat =
    match dat with
    | CAtom _ -> CAtom a
    | _ -> dat

  let update_npos ~pos (nd: datum option) (dat: datum): datum =
    match dat with
    | CAtom _ -> dat
    | CForm {ckwd; cpos} ->
      (match nd with
       | Some n -> CForm {ckwd = ckwd; cpos = update pos n cpos}
       | _ -> CForm {ckwd = ckwd; cpos = remove pos cpos})
      
  let update_kval ~key (nd: datum option) (dat: datum): datum =
    match dat with
    | CAtom _ -> dat
    | CForm {ckwd; cpos} ->
       (match nd with
        | Some n -> CForm {ckwd = update_assoc key n ckwd; cpos = cpos}
        | _ -> CForm {ckwd = List.remove_assoc key ckwd; cpos = cpos})
  
  let update_root : datum -> datum -> datum = fun _ d -> d
  let update_anno : datum list -> datum -> datum = fun _ dat -> dat

  (** converters *)

  let to_canonicaltree : datum -> ctree = fun d -> d
  let to_normaltree    : datum -> ntree = Normaltree.ndatum_of_cdatum
  let to_datatree      : datum -> dtree =
    composite Datatree.ddatum_of_ndatum Normaltree.ndatum_of_cdatum 
  let to_parsetree     : datum -> ptree =
    composite Parsetree.pdatum_of_ddatum (composite Datatree.ddatum_of_ndatum Normaltree.ndatum_of_cdatum)
      
end

module Normaltreeflavor : Treeflavor = struct
  open Normaltree
  type datum = Normaltree.ndatum
  (*
    
    | NAtom of atom
    | NForm of {
        (* 潰された *)
        n_keywordeds  : (ndatum, ndatum) assoc;
        n_positionals : ndatum list;
        n_annotations : ndatum set;
      }
    | NAnnotated of ndatum * ndatum set *)
      
  type flavor = ntree
  let treeflavor : ntree treeflavor = Ntree

  let compare n n' = Canonicaltree.cdatum_ordering (cdatum_of_ndatum n) (cdatum_of_ndatum n')
  let eqv n n': bool = compare n n' = 0
  let pp : formatter -> datum -> unit = pp_ndatum
  let to_string ?pretty:(_=false) = Format.asprintf "%a" pp
  let datum_of_sexp : sexp -> datum = [%noimplval]
  let sexp_of_datum : datum -> sexp = sexp_ndatum
  
  (** destructors *)

  let atom dat =
    match dat with
    | NAtom a -> a
    | _ -> failwith "Not an atom!"
             
  let npos ~pos dat =
    match dat with
    | NForm { n_positionals = poses; _} ->
      List.nth poses pos
    | _ -> failwith "No positional arguments!"
  
  let kval ~key dat =
    match dat with
    | NForm { n_keywordeds = kws; _ } ->
      (match List.assoc_opt key kws with
       | Some v -> v
       | None -> failwith "Invalid key!")
    | _ -> failwith "No keyword arguments!"
  
  let root dat =
    match dat with
    | NAnnotated (root, _) -> root
    | _ -> failwith "No annotations!"

  let anno dat =
    match dat with
    | NAnnotated (_, anns) -> anns
    | _ -> failwith "No annotations!"
  
  (** constructors & case analyzer *)

  let mkatom : atom -> datum = fun a -> NAtom a
  let mkform ~keyworded ~positional =
    NForm {n_keywordeds = keyworded; n_positionals = positional; n_annotations = []}
  (* val case : atom:(datum -> 'r) ->
             form:(keyworded:(datum*datum) list ->
                   positional:datum list ->
                   'r) ->
             datum -> 'r*)
  let rec case ~atom ~form d =
    match d with
    | NAtom _ -> atom d
    | NForm {n_keywordeds = kws; n_positionals = poses ; _} ->
       form ~keyworded:kws ~positional:poses
    | NAnnotated (d', _) -> case ~atom ~form d'

  (** updaters *)

  let update_atom a dat =
    match dat with
    | NAtom _ -> NAtom a
    | _ -> dat

  let update_npos ~pos (nd: datum option) (dat: datum): datum =
    match dat with
    | NForm {n_keywordeds = kws; n_positionals = poses; n_annotations = anns} ->
      (match nd with
       | Some n -> NForm {n_keywordeds = kws;
                          n_positionals = update pos n poses;
                          n_annotations = anns}
       | _ -> NForm {n_keywordeds = kws;
                     n_positionals = remove pos poses;
                     n_annotations = anns})
    | _ -> dat

  let update_kval ~key (nd: datum option) (dat: datum): datum =
    match dat with
    | NForm {n_keywordeds = kws; n_positionals = poses; n_annotations = anns} ->
       (match nd with
        | Some n -> NForm {n_keywordeds = update_assoc key n kws;
                           n_positionals = poses;
                           n_annotations = anns}
        | _ -> NForm {n_keywordeds = List.remove_assoc key kws;
                      n_positionals = poses;
                      n_annotations = anns})
    | _ -> dat
      
  let update_root (nd: datum) (dat: datum): datum =
    match dat with
    | NAnnotated (_, anns) -> NAnnotated (nd, anns)
    | _ -> dat
  
  let update_anno (nanns: datum list) (dat: datum): datum =
    match dat with
    | NAnnotated (d, _) -> NAnnotated (d, nanns)
    | _ -> dat

  (** converters *)

  let to_canonicaltree : datum -> ctree = Normaltree.cdatum_of_ndatum
  let to_normaltree    : datum -> ntree = fun d -> d
  let to_datatree      : datum -> dtree = Datatree.ddatum_of_ndatum
  let to_parsetree     : datum -> ptree =
    composite Parsetree.pdatum_of_ddatum Datatree.ddatum_of_ndatum
end

module type Zipperlib = functor (Flavor : Treeflavor) -> sig
  type t
  type datum = Flavor.datum

  (** basics *)

  val walk : datum -> t
  val focus  : t -> datum
  val unwalk : t -> datum

  (** walkers *)

  val walk_upwards : t -> t
  val walk_root : t -> t
  val walk_anno : t -> t list
  val walk_npos : pos:int   -> t -> t
  val walk_kval : key:datum -> t -> t
  val walk_positionals : t -> t list
  val walk_keywordeds  : t -> (t*t) list

  (** primitive updaters *)

  val update_node : datum -> t -> t (* focus remains the same afterwards *)
  val remove_node : t -> t          (* automatically walk-up afterwards *)

  (** utility updaters *)

  val update_npos : pos:int   -> datum option -> t -> t
  val update_kval : key:datum -> datum option -> t -> t
  val update_atom : atom -> t -> t
  val update_root : datum -> t -> t
  val update_anno : datum list -> t -> t
end

(* TASK 2 *)
(* module GenericZipperlib : Zipperlib = struct
 *   (\* todo *\)
 * end *)
