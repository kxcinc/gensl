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
  (* val datum_of_sexp : sexp -> datum *)
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
(* must make this transparent using with! very important otherwise basically impossible 
   to use and test *)
module CanonicaltreeFlavor : (Treeflavor with type datum = Canonicaltree.cdatum) = struct
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
  (* let datum_of_sexp : sexp -> datum = failwith "placeholder" *)
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
  (* let datum_of_sexp : sexp -> datum = failwith "placeholder" *)
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

module Datatreeflavor : (Treeflavor with type datum = Datatree.ddatum) = struct
  open Datatree
  type datum = Datatree.ddatum
  (*
    | DAtom of atom (* an atom *)
    | DForm of dnode list (* a form is represented as a list of nodes *)
    | DAnnotated of {
        d_annotated : ddatum;
        d_anno_front : ddatum list;
        d_anno_back : ddatum list;
      }
  and  dnode =
    | DKeywordNode of ddatum * ddatum
    | DDatumNode of ddatum
    | DAnnoNode of ddatum *)
      
  type flavor = dtree
  let treeflavor : dtree treeflavor = Dtree

  let compare d d' = Canonicaltree.cdatum_ordering (Normaltree.cdatum_of_ndatum (ndatum_of_ddatum d)) (Normaltree.cdatum_of_ndatum (ndatum_of_ddatum d'))
  let eqv d d': bool = compare d d' = 0
  let pp : formatter -> datum -> unit = pp_ddatum
  let to_string ?pretty:(_=false) = Format.asprintf "%a" pp
  (* let datum_of_sexp : sexp -> datum = failwith "placeholder" *)
  let sexp_of_datum : datum -> sexp = sexp_ddatum
  
  (** destructors *)

  let atom dat =
    match dat with
    | DAtom a -> a
    | _ -> failwith "Not an atom!"

  let is_datum_node (node: dnode): bool =
    match node with DDatumNode _ -> true | _ -> false

  let npos ~pos dat =
    let get_nth_datum n (nodes: dnode list): ddatum =
      match List.nth (List.filter is_datum_node nodes) n with
      | DDatumNode d -> d | _ -> failwith "Impossible!"
    in
    match dat with
    | DForm dnodes -> get_nth_datum pos dnodes
    | _ -> failwith "No positional nodes!"
             
  let is_kw_node (node: dnode): bool =
      match node with DKeywordNode (_, _) -> true | _ -> false
  
  let kval ~key dat =
    let raw_nodes nodes = List.map
                      (fun n -> match n with DKeywordNode (k, w) -> (k, w)
                                           | _ -> failwith "Impossible!")
                      (List.filter is_kw_node nodes) in
    let get_kval k (nodes: dnode list): ddatum =
      match List.assoc_opt k (raw_nodes nodes) with
      | Some v -> v
      | None -> failwith "Invalid key!"
    in
    match dat with
    | DForm dnodes -> get_kval key dnodes
    | _ -> failwith "No positional nodes!"
  
  let root dat =
    match dat with
    | DAnnotated {d_annotated; _} -> d_annotated
    | _ -> failwith "Not an annotated tree!"

  let anno dat =
    match dat with
    | DAnnotated {d_anno_front = front; d_anno_back = back; _} -> front @ back
    | _ -> failwith "No annotations!"
  
  (** constructors & case analyzer *)

  let mkatom : atom -> datum = fun a -> DAtom a

  let mkform ~keyworded ~positional =
    let kw_nodes = List.map (fun (k, v) -> DKeywordNode (k, v)) keyworded in
    let pos_nodes = List.map (fun d -> DDatumNode d) positional in
    DForm (kw_nodes @ pos_nodes)
    
  (* val case : atom:(datum -> 'r) ->
             form:(keyworded:(datum*datum) list ->
                   positional:datum list ->
                   'r) ->
             datum -> 'r *)
  let rec case ~atom ~form d =
    match d with
    | DAtom _ -> atom d
    | DForm dnodes ->
       let data = List.filter is_datum_node dnodes in
       let kws = List.filter is_kw_node dnodes in
       let data_plain =
         List.map (fun n -> match n with DDatumNode d -> d | _ -> failwith "Impossible!") data in
       let kws_plain =
         List.map (fun n -> match n with DKeywordNode (k, w) -> (k, w)
                                       | _ -> failwith "Impossible!") kws in
       form ~keyworded:kws_plain ~positional:data_plain
    | DAnnotated {d_annotated = dat; _} -> case ~atom:atom ~form:form dat
       

  (** updaters *)

  let update_atom a dat =
    match dat with
    | DAtom _ -> DAtom a
    | _ -> dat

  let update_npos ~pos (nd: datum option) (dat: datum): datum =
    match dat with
    | DForm dnodes ->
       begin
         let data = List.filter is_datum_node dnodes in
         let kws = List.filter is_kw_node dnodes in
         let data_plain =
           List.map (fun n -> match n with DDatumNode d -> d | _ -> failwith "Impossible!") data in
         let data_new =
           match nd with
           | Some d -> update pos d data_plain
           | None -> remove pos data_plain
         in
         let nodes = List.map (fun d -> DDatumNode d) data_new @ kws in
         DForm nodes
       end
    | _ -> failwith "Not a form!"
     
    

  let update_kval ~key (nd: datum option) (dat: datum): datum =
   match dat with
    | DForm dnodes ->
       begin
         let data = List.filter is_datum_node dnodes in
         let kws = List.filter is_kw_node dnodes in
         let kw_plain =
           List.map (fun n -> match n with DKeywordNode (k, v) -> (k, v) | _ -> failwith "Impossible!") kws in
         let kw_new =
           match nd with
           | Some d -> update_assoc key d kw_plain
           | None -> List.remove_assoc key kw_plain
         in
         let nodes = data @ List.map (fun (k, v) -> DKeywordNode (k, v)) kw_new in
         DForm nodes
       end
    | _ -> failwith "Not a form!"
      
  let update_root (nd: datum) (dat: datum): datum =
    match dat with
    | DAnnotated ({d_annotated = _;
                   d_anno_front = _;
                   d_anno_back = _} as ann) ->
       DAnnotated {ann with d_annotated = nd}
    | _ -> dat
  
  let update_anno (nanns: datum list) (dat: datum): datum =
    match dat with
    | DAnnotated ({d_annotated = _;
                   d_anno_front = _;
                   d_anno_back = _} as ann) ->
       DAnnotated {ann with d_anno_front = nanns}
    | _ -> dat

  (** converters *)

  let to_canonicaltree : datum -> ctree = composite Normaltree.cdatum_of_ndatum ndatum_of_ddatum
  let to_normaltree    : datum -> ntree = ndatum_of_ddatum
  let to_datatree      : datum -> dtree = fun d -> d
  let to_parsetree     : datum -> ptree = Parsetree.pdatum_of_ddatum
end

module Parsetreeflavor : (Treeflavor with type datum = Parsetree.pdatum) = struct
  open Parsetree
  open ParsetreePrinter
  type datum = Parsetree.pdatum
  (*
    | DAtom of atom (* an atom *)
    | DForm of dnode list (* a form is represented as a list of nodes *)
    | DAnnotated of {
        d_annotated : ddatum;
        d_anno_front : ddatum list;
        d_anno_back : ddatum list;
      }
  and  dnode =
    | DKeywordNode of ddatum * ddatum
    | DDatumNode of ddatum
    | DAnnoNode of ddatum *)
      
  type flavor = ptree
  let treeflavor : ptree treeflavor = Ptree

  let compare p p' = Canonicaltree.cdatum_ordering (Normaltree.cdatum_of_ndatum (Datatree.ndatum_of_ddatum (ddatum_of_pdatum p))) (Normaltree.cdatum_of_ndatum (Datatree.ndatum_of_ddatum (ddatum_of_pdatum p')))
  let eqv p p': bool = compare p p' = 0
  let pp : formatter -> datum -> unit = pp_pdatum
  let to_string ?pretty:(_=false) = Format.asprintf "%a" pp
  (* let datum_of_sexp : sexp -> datum = failwith "placeholder" *)
  let sexp_of_datum : datum -> sexp = sexp_pdatum
  
  (** destructors *)

  let atom dat =
    match dat with
    | PAtom { elem = a; _ } -> a
    | _ -> failwith "Not an atom!"

  let is_datum_node (node: pnode): bool =
    match node with PDatumNode _ -> true | _ -> false

  let npos ~pos dat =
    let get_nth_datum n (nodes: pnode list): pdatum =
      match List.nth (List.filter is_datum_node nodes) n with
      | PDatumNode d -> d | _ -> failwith "Impossible!"
    in
    match dat with
    | PForm {elem = (nodes, _, _); _} -> get_nth_datum pos nodes
    | _ -> failwith "Not a form!"
             
  let is_kw_node (node: pnode): bool =
      match node with PKeywordNode (_, _) -> true | _ -> false
  
  let kval ~key dat =
    let raw_nodes nodes = List.map
                      (fun n -> match n with PKeywordNode (k, w) -> (k, w)
                                           | _ -> failwith "Impossible!")
                      (List.filter is_kw_node nodes) in
    let get_kval k (nodes: pnode list): pdatum =
      match List.assoc_opt k (raw_nodes nodes) with
      | Some v -> v
      | None -> failwith "Invalid key!"
    in
    match dat with
    | PForm {elem = (pnodes, _, _); _} -> get_kval key pnodes
    | _ -> failwith "No positional nodes!"
  
  let root dat =
    match dat with
    | PAnnotated { elem = {p_annotated; _}; _} -> p_annotated
    | _ -> failwith "Not an annotated tree!"

  let anno dat =
    match dat with
    | PAnnotated { elem = {p_anno_front = front;
                           p_anno_back = back; _}; _} -> front @ back
    | _ -> failwith "No annotations!"
  
  (** constructors & case analyzer *)

  let mkatom : atom -> datum = fun a -> PAtom {elem = a; repr = `Direct}

  let mkform ~keyworded ~positional =
    let kw_nodes = List.map (fun (k, v) -> PKeywordNode (k, v)) keyworded in
    let pos_nodes = List.map (fun d -> PDatumNode d) positional in
    PForm {elem = (kw_nodes @ pos_nodes, SimpleForm, Infix); repr = `Direct}
    
  (* val case : atom:(datum -> 'r) ->
             form:(keyworded:(datum*datum) list ->
                   positional:datum list ->
                   'r) ->
             datum -> 'r *)
  let rec case ~atom ~form d =
    match d with
    | PAtom _ -> atom d
    | PForm {elem = (dnodes, _, _); repr = _} ->
       let data = List.filter is_datum_node dnodes in
       let kws = List.filter is_kw_node dnodes in
       let data_plain =
         List.map (fun n -> match n with PDatumNode d -> d 
                                       | _ -> failwith "Impossible!") data in
       let kws_plain =
         List.map (fun n -> match n with PKeywordNode (k, w) -> (k, w)
                                       | _ -> failwith "Impossible!") kws in
       form ~keyworded:kws_plain ~positional:data_plain
    | PAnnotated {elem = {p_annotated = d';_}; repr = _} -> case ~atom:atom ~form:form d'
       

  (** updaters *)

  let update_atom a dat =
    match dat with
    | PAtom {elem = _; repr = repr} -> PAtom {elem = a; repr = repr}
    | _ -> dat

  let update_npos ~pos (nd: datum option) (dat: datum): datum =
    match dat with
    | PForm {elem = (pnodes, style, fixness); repr = repr} ->
       begin
         let data = List.filter is_datum_node pnodes in
         let kws = List.filter is_kw_node pnodes in
         let data_plain =
           List.map (fun n -> match n with PDatumNode d -> d | _ -> failwith "Impossible!") data in
         let data_new =
           match nd with
           | Some d -> update pos d data_plain
           | None -> remove pos data_plain
         in
         let nodes = List.map (fun d -> PDatumNode d) data_new @ kws in
         PForm {elem = (nodes, style, fixness); repr = repr}
       end
    | _ -> failwith "Not a form!"

  let update_kval ~key (nd: datum option) (dat: datum): datum =
   match dat with
    | PForm {elem = (pnodes, style, fixness); repr = repr} ->
       begin
         let data = List.filter is_datum_node pnodes in
         let kws = List.filter is_kw_node pnodes in
         let kw_plain =
           List.map (fun n -> match n with PKeywordNode (k, v) -> (k, v) | _ -> failwith "Impossible!") kws in
         let kw_new =
           match nd with
           | Some d -> update_assoc key d kw_plain
           | None -> List.remove_assoc key kw_plain
         in
         let nodes = data @ List.map (fun (k, v) -> PKeywordNode (k, v)) kw_new in
         PForm {elem = (nodes, style, fixness); repr = repr}
       end
    | _ -> failwith "Not a form!"
      
  let update_root (nd: datum) (dat: datum): datum =
    match dat with
    | PAnnotated {elem = {p_annotated = _;
                          p_anno_front = front_anns;
                          p_anno_back = back_anns};
                  repr = repr } ->
       PAnnotated  {elem = {p_annotated = nd;
                          p_anno_front = front_anns;
                          p_anno_back = back_anns};
                    repr = repr }
    | _ -> dat
  
  let update_anno (nanns: datum list) (dat: datum): datum =
    match dat with
    | PAnnotated {elem = {p_annotated = root;
                          p_anno_front = _;
                          p_anno_back = _};
                  repr = repr } ->
       PAnnotated  {elem = {p_annotated = root;
                            p_anno_front = nanns;
                            p_anno_back = []};
                    repr = repr }
    | _ -> dat

  (** converters *)

  let to_canonicaltree : datum -> ctree = composite Normaltree.cdatum_of_ndatum (composite Datatree.ndatum_of_ddatum ddatum_of_pdatum)
  let to_normaltree    : datum -> ntree = composite Datatree.ndatum_of_ddatum ddatum_of_pdatum
  let to_datatree      : datum -> dtree = ddatum_of_pdatum
  let to_parsetree     : datum -> ptree = fun d -> d
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
  val walk_keywordeds  : t -> t list

  (** primitive updaters *)

  val update_node : datum -> t -> t (* focus remains the same afterwards *)
  val remove_node : t -> int * t    (* automatically walk-up afterwards *)
  val remove_node_imm : t -> t      (* remove_node but assert [depth of walk-up] equals one *)

  (** utility updaters *)

  val update_npos : pos:int   -> datum option -> t -> t
  val update_kval : key:datum -> datum option -> t -> t
  (* val update_atom : atom -> t -> t *)
  val update_root : datum -> t -> t
  (* val update_anno : datum list -> t -> t *)
end with type datum = Flavor.datum

(* TASK 2 *)
(* module GenericZipperlib : Zipperlib = struct
 *   (\* todo *\)
 * end *)


module GenericZipperlib = 
  functor (Flavor : Treeflavor) ->
  struct
    type datum = Flavor.datum

    type path_component = 
      | PCroot of {
          root_focused : datum;
        }
      | PCanno of {
          anno_focused : datum;
          anno_pos : int;
        }
      | PCnpos of {
          npos_pos : int;
          npos_focused : datum;
        }
      | PCkval of {
          kval_key : datum;
          kval_focused : datum;
        }

    (* A *stack* of path components, i.e. the 1st element is at the deepest *)
    type t = datum * path_component list 

    let walk (d: datum): t = (d, [])

    let focus : t -> datum = function
      | d, [] -> d
      | _, PCroot { root_focused = d } :: _ -> d
      | _, PCanno { anno_focused = d; _} :: _ -> d
      | _, PCnpos { npos_focused = d; _} :: _ -> d
      | _, PCkval { kval_focused = d; _} :: _ -> d

    let walk_root ((d, path) as z: t): t = 
      d, PCroot { root_focused = Flavor.root (focus z) } :: path

    let walk_anno ((d, path) as z: t): t list =
      let annos = Flavor.anno (focus z) in
      List.mapi (fun i anno ->
          d, PCanno { anno_focused = anno;
                      anno_pos = i } :: path) annos

    let walk_npos ~pos ((d, path) as z: t): t =
      d, PCnpos { npos_focused = Flavor.npos ~pos (focus z);
                  npos_pos = pos } :: path

    let walk_kval ~key ((d, path) as z: t): t =
      d, PCkval { kval_focused = Flavor.kval ~key (focus z);
                  kval_key = key } :: path

    let walk_positionals ((d, path) as z: t): t list =
      let poses = Flavor.case ~atom:(fun _ -> []) 
                    ~form:(fun ~keyworded:_ ~positional -> positional) (focus z) in
      List.mapi (fun i pos ->
          d, PCnpos { npos_focused = pos;
                      npos_pos = i } :: path) poses
    
    let walk_keywordeds ((d, path) as z: t): t list =
      let kws = Flavor.case ~atom:(fun _ -> []) 
                  ~form:(fun ~keyworded ~positional:_ -> keyworded) (focus z) in
      List.map (fun (k, v) ->
          d, PCkval { kval_focused = v;
                      kval_key = k } :: path) kws
    
    let update_focus (nd: datum) = function
        | d, PCroot _ :: rest ->
           d, PCroot { root_focused = nd } :: rest
        | d, PCanno r :: rest ->
           d, PCanno { r with anno_focused = nd } :: rest
        | d, PCnpos r :: rest ->
           d, PCnpos { r with npos_focused = nd } :: rest
        | d, PCkval r :: rest ->
           d, PCkval { r with kval_focused = nd } :: rest
        | _, [] -> nd, []
    

    let walk_upwards (z: t): t =
      match z with
      | _, [] -> invalid_arg "Already at the top, can't walk upwards!"
      | _, PCroot { root_focused = f } :: _ -> 
         update_focus (Flavor.update_root f (focus z)) z       
      | _, PCanno { anno_focused = f; anno_pos = pos } :: _ -> 
         let annos = Flavor.anno (focus z) in
         let annos' = update pos f annos in
         let d' = Flavor.update_anno annos' (focus z) in
         update_focus d' z
      | _, PCnpos { npos_focused = f; npos_pos = pos } :: _ -> 
         update_focus (Flavor.update_npos ~pos (Some f) (focus z)) z
      | _, PCkval { kval_focused = f; kval_key = key } :: _ ->
         update_focus (Flavor.update_kval ~key (Some f) (focus z)) z

    let rec unwalk = function
      | d, [] -> d
      | z -> unwalk (walk_upwards z)

    let update_node (nd: datum): t -> t = function
      | _, [] -> nd, []
      | d, PCroot _ :: rest -> d, PCroot {root_focused = nd} :: rest
      | d, PCanno r :: rest -> d, PCanno {r with anno_focused = nd} :: rest
      | d, PCnpos r :: rest -> d, PCnpos {r with npos_focused = nd} :: rest
      | d, PCkval r :: rest -> d, PCkval {r with kval_focused = nd} :: rest

    let _ = remove

    let remove_node (z: t) =
      let rec loop upwards_depth = function
        | _, [] -> failwith "Nothing to remove!"
        | d, PCroot _ :: rest ->
           loop (upwards_depth+1) (d, rest)
        | d, PCanno r :: rest ->
           let parent = focus (d, rest) in
           let orig = Flavor.anno parent in
           let updated = remove r.anno_pos orig in
           let parent = Flavor.update_anno updated parent in
           1, ((d, rest) |> update_node parent)
        | d, PCnpos r :: rest ->
           let parent = focus (d, rest) in
           let parent = Flavor.update_npos ~pos:r.npos_pos None parent in
           1, ((d, rest) |> update_node parent)
        | d, PCkval r :: rest ->
           let parent = focus (d, rest) in
           let parent = Flavor.update_kval ~key:r.kval_key None parent in
           1, ((d, rest) |> update_node parent)
                (* | _ -> [%noimplval] *)
      (* | d, _ :: rest -> walk_upwards (d, rest) *) in
      loop 0 z

    let remove_node_imm t =
      let depth, nt = remove_node t in
      if depth = 1
      then nt
      else invalid_arg "assertion (depth=1) failed for remove_node_imm"
    
    let update_npos ~pos new_datum z: t =
      match new_datum with
      | None ->
         walk_npos ~pos z
         |> remove_node_imm
      | Some nd ->
         walk_npos ~pos z
         |> update_node nd
         |> walk_upwards

    let update_kval ~key new_datum z: t =
      match new_datum with
      | None ->
         walk_kval ~key z
         |> remove_node_imm
      | Some nd ->
         walk_kval ~key z
         |> update_node nd
         |> walk_upwards
    
    let update_root nd z = 
      walk_root z
      |> update_node nd
end
