# tuple type

`TYPE`
`OrderedType = Stdlib.Map.OrderedType`
`HashedType = Stdlib.Hashtbl.HashedType`

# container type

`PERSISTENT_MAPS`
`IMPERATIVE_MAPS`

# property type

`PROPERTY`
`SEMI_LATTICE`

# internal

`SOLVER`
`SOLUTION`

# utils

# module type

`MEMOIZER`
`TABULATOR`

# module functor

`Fix` : `Make` `ForOrderedType` `ForHashedType` `ForType`, `ForIntSegment`
`DataFlow`: `Run` `ForOrderedType` `ForHashedType` `ForType`
`Memoize` : `Make` `ForOrderedType` `ForHashedType`, `Char` `Int` `String`
`Tabulate`: `Make` `ForOrderedType` `ForHashedType` `ForType`, `ForIntSegment`


module ValueSet = Prop.Set (struct
  type t = Set.M(Id).t

  let empty = Set.empty (module Id)

  let equal s1 s2 = Set.equal s1 s2
end)

module AValueSet = Fix__.Set.Set (struct
  type t = avalue Set.Poly.t

  let empty = Set.Poly.empty

  let equal s1 s2 = Set.Poly.equal s1 s2
end)