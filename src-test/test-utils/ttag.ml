
open Core

module V1 = struct
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

  let to_name = function
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

  let to_name_with_underline = function
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

  let to_char = function
  | Polymorphic_types      -> 'P'
  | Variants               -> 'V'
  | Intersection_types     -> 'I'
  | Recursive_functions    -> 'R'
  | Mu_types               -> 'M'
  | Higher_order_functions -> 'H'
  | Subtyping              -> 'S'
  | Type_casing            -> 'T'
  | OOP_style              -> 'O'
  | Refinement_types       -> 'F'
  | Dependent_types        -> 'D'
  | Parametric_types       -> 'A'
  | Records                -> 'C'
  | Wrap_required          -> 'W'
  | Assertions             -> 'N'
  | Operator_misuse        -> 'U'
  | Return_type            -> 'Y'
  | Match                  -> 'X'

  let sort_list =
    List.sort ~compare:(fun a b -> Int.compare (Variants.to_rank a) (Variants.to_rank b))
end

module V2 = struct
  (*
    Complete metadata:
    
    (***
      (
        (features (Polymorphic_types Refinement_types Dependent_types Modules Mu_types Parametric_types First_class_types Variants Records Recursive_functions Higher_order_functions Subtyping OOP_style Return_error Usage_error Other))
        (reasons (Polymorphic_types Refinement_types Dependent_types Modules Mu_types Parametric_types First_class_types Variants Records Recursive_functions Higher_order_functions Subtyping OOP_style Return_error Usage_error Other))
        (speed <Fast or Slow>)
        (typing <Well_typed or Ill_typed>)
      )
    *)

  *)

  type t =
    | Polymorphic_types
    | Refinement_types
    | Dependent_types
    | Modules
    | Mu_types
    | Parametric_types
    | First_class_types
    | Variants
    | Records
    | Recursive_functions
    | Higher_order_functions
    | Subtyping
    | OOP_style
    | Return_error
    | Usage_error
    | Other
    [@@deriving variants, sexp, compare, enumerate, equal]

  (* Description, if not totally trivial *)
  (* Lists are often excluded because they are trivial, built in, and used so often *)
  let to_description = function
    | Polymorphic_types      -> "Untouchable polymorphic values"
    | Refinement_types       -> "Type is refined with a predicate"
    | Dependent_types        -> "Codomain depends on a value"
    | Modules                -> "Modules and module types are used, or dependent record types"
    | Mu_types               -> "Recursive types, excluding lists"
    | Parametric_types       -> "Type functions with simple parameters, excluding lists"
    | First_class_types      -> "Types are used as values for more than simple parametric types"
    | Variants               -> "Excluding lists"
    | Records                -> "Not including dependent records types, which are modules"
    | Recursive_functions    -> "Including mutual recursion"
    | Higher_order_functions -> "Including intersection/match types"
    | Subtyping              -> "The program uses subtyping, or the error is in the spirit of subtyping"
    | OOP_style              -> "Self-referential records"
    | Return_error           -> "The value returned from a function has the wrong type"
    | Usage_error            -> "A function is used with the wrong type, including built-in operators"
    | Other                  -> "Anything not included in the above"

  let to_name = function
    | Polymorphic_types      -> "Polymorphic types"
    | Refinement_types       -> "Refinement types"
    | Dependent_types        -> "Dependent types"
    | Modules                -> "Modules"
    | Mu_types               -> "Mu types"
    | Parametric_types       -> "Parametric types"
    | First_class_types      -> "First class types"
    | Variants               -> "Variants"
    | Records                -> "Records"
    | Recursive_functions    -> "Recursive functions"
    | Higher_order_functions -> "Higher order functions"
    | Subtyping              -> "Subtyping"
    | OOP_style              -> "OOP-style"
    | Return_error           -> "Return error"
    | Usage_error            -> "Usage error"
    | Other                  -> "Other"

  let to_name_with_underline = function
    | Polymorphic_types      -> "\\underline{P}olymorphic types"
    | Refinement_types       -> "Re\\underline{f}inement types"
    | Dependent_types        -> "\\underline{D}ependent types"
    | Modules                -> "\\underline{M}odules"
    | Mu_types               -> "M\\underline{u} types"
    | Parametric_types       -> "P\\underline{a}rametric types"
    | First_class_types      -> "First class \\underline{t}ypes"
    | Variants               -> "\\underline{V}ariants"
    | Records                -> "Re\\underline{c}ords"
    | Recursive_functions    -> "\\underline{R}ecursive functions"
    | Higher_order_functions -> "\\underline{H}igher order functions"
    | Subtyping              -> "\\underline{S}ubtyping"
    | OOP_style              -> "\\underline{O}OP-style"
    | Return_error           -> "Retur\\underline{n} error"
    | Usage_error            -> "Usa\\underline{g}e error"
    | Other                  -> "Oth\\underline{e}r"

  let to_char = function
    | Polymorphic_types      -> 'P'
    | Refinement_types       -> 'F'
    | Dependent_types       -> 'D'
    | Modules                -> 'M'
    | Mu_types               -> 'U'
    | Parametric_types       -> 'A'
    | First_class_types      -> 'T'
    | Variants               -> 'V'
    | Records                -> 'C'
    | Recursive_functions    -> 'R'
    | Higher_order_functions -> 'H'
    | Subtyping              -> 'S'
    | OOP_style              -> 'O'
    | Return_error           -> 'N'
    | Usage_error            -> 'G'
    | Other                  -> 'E'
end
