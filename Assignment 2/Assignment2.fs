//Exercise 2.1
let rec downto1 n = if n > 0 then n :: downto1 (n-1) else []

let rec downto2 =
    function
    | n when n > 0 -> n :: downto2 (n-1)
    | _ -> []

//Exercise 2.2
let rec removeOddIdx =
    function
    | n :: m :: x -> n :: removeOddIdx x
    | n -> n

//Exercise 2.3
let rec combinePair =
    function
    | []           -> []
    | x :: y :: xs -> (x,y) :: combinePair xs
    | _            -> []

//Exercise 2.4
