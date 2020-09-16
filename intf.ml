type sexp = Ppx_sexp_conv_lib.Sexp.t
open Gensl

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

