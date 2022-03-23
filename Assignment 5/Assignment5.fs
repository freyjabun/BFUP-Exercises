//Exercise 5.1
let sum m n =
    let rec sumA m n acc =
        match n with
        | 0 -> acc + m
        | n -> sumA m (n - 1) (acc + m + n)

    sumA m n 0

//Exercise 5.2
let length xs =
    let rec lengthA xs acc =
        match xs with
        | [] -> acc
        | _ :: xs -> lengthA (xs) (acc + 1)

    lengthA xs 0

//Exercise 5.3
let rec foldBackBaby folder lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> folder x (foldBackBaby folder xs acc)

let foldBack folder lst acc =
    let rec aux folder lst acc c =
        match lst with
        | []        -> c acc
        | x :: xs -> aux folder xs acc (fun r -> c (folder x r))
    aux folder lst acc id

//Exercise 5.4
let factC x =
    let rec aux c x =
        match x with
        | 0 -> c 1
        | x -> aux (fun r -> c (x * r)) (x - 1)

    aux id x
