
open Core

type t =
  | Polymorphic_types (* as reason, includes errors in instantiations of polymorphic types, and any code where error directly involves a polymorphic type *)
  | Variants
  | Intersection_types
  | Recursive_functions
  | Mu_types
  | Higher_order_functions (* includes first class functions *)
  | Subtyping
  | Type_casing
  | OOP_style
  | Refinement_types
  | Dependent_types
  | Parametric_types (* not including List *)
  | Records (* as reason, includes errors related to projection *)
  | Wrap_required
  | Assertions
  | Operator_misuse (* e.g. 1 + true, 0 0, etc. *)
  | Return_type (* where return type is just fundamentally wrong, e.g. f (x : int) : int = true *)
  | Match (* uses match anywhere, not necessarily as the source of the error *)
  [@@deriving variants, sexp, compare, enumerate, equal]

let sexp_all : Sexp.t =
  List.sexp_of_t sexp_of_t all

let to_string = function
| Polymorphic_types      -> "Polymorphic types"
| Variants               -> "Variants"
| Intersection_types     -> "Intersection types"
| Recursive_functions    -> "Recursive functions"
| Mu_types               -> "Mu types"
| Higher_order_functions -> "Higher order functions"
| Subtyping              -> "Subtyping"
| Type_casing            -> "Type casing"
| OOP_style              -> "OOP-style"
| Refinement_types       -> "Refinement types"
| Dependent_types        -> "Dependent types"
| Parametric_types       -> "Parametric types"
| Records                -> "Records"
| Wrap_required          -> "Wrap required"
| Assertions             -> "Assertions"
| Operator_misuse        -> "Operator misuse"
| Return_type            -> "Return type"
| Match                  -> "Match"

let to_string_with_underline = function
| Polymorphic_types      -> "\\underline{P}olymorphic types"  
| Variants               -> "\\underline{V}ariants"           
| Intersection_types     -> "\\underline{I}ntersection types" 
| Recursive_functions    -> "\\underline{R}ecursive functions"
| Mu_types               -> "\\underline{M}u types"           
| Higher_order_functions -> "\\underline{H}igher order functions"
| Subtyping              -> "\\underline{S}ubtyping"             
| Type_casing            -> "\\underline{T}ype casing"           
| OOP_style              -> "\\underline{O}OP-style"            
| Refinement_types       -> "Re\\underline{f}inement types"     
| Dependent_types        -> "\\underline{D}ependent types"      
| Parametric_types       -> "P\\underline{a}rametric types"     
| Records                -> "Re\\underline{c}ords"              
| Wrap_required          -> "\\underline{W}rap required"        
| Assertions             -> "Assertio\\underline{n}s"           
| Operator_misuse        -> "Operator mis\\underline{u}se"      
| Return_type            -> "Return t\\underline{y}pe"          
| Match                  -> "Match (X)"                   

let to_string_short = function
| Polymorphic_types      -> "P"
| Variants               -> "V"
| Intersection_types     -> "I"
| Recursive_functions    -> "R"
| Mu_types               -> "M"
| Higher_order_functions -> "H"
| Subtyping              -> "S"
| Type_casing            -> "T"
| OOP_style              -> "O"
| Refinement_types       -> "F"
| Dependent_types        -> "D"
| Parametric_types       -> "A"
| Records                -> "C"
| Wrap_required          -> "W"
| Assertions             -> "N"
| Operator_misuse        -> "U"
| Return_type            -> "Y"
| Match                  -> "X"

let sort_list =
  List.sort ~compare:(fun a b -> Int.compare (Variants.to_rank a) (Variants.to_rank b))