module Dictionary

type Dict = Dict of Set<string>
let empty () = Dict Set.empty
let insert s (Dict dict) = Dict (Set.add s dict)
let lookup s (Dict dict) = Set.contains s dict