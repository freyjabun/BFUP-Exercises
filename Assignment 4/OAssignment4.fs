//Exercise Green
module MultiSet

type MultiSet<'a when 'a: comparison> = MS of Map<'a, uint32>

let empty = MS Map.empty
let isEmpty (MS s) = Map.isEmpty s

let size (MS s) =
    Map.fold (fun count _ value -> count + value) 0u s

let contains a (MS s) = Map.containsKey a s

let numItems a (MS s) =
    Map.tryFind a s |> Option.defaultValue 0u

let add a n (MS s) = MS(Map.add a (numItems a (MS s) + n) s)
let addSingle a (MS s) = add a 1u (MS s)

let remove a (n: uint32) (MS s) =
    let check = (int ((numItems a (MS s)))) - (int n)

    if check < 0 then
        MS(Map.remove a s)
    else
        MS(Map.add a (uint32 check) s)
let removeSingle a (MS s) = remove a 1u (MS s)
let fold f acc (MS s) = Map.fold f acc s
let foldBack f (MS s) acc = Map.foldBack f s acc