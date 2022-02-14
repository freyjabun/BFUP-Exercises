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
type complex = float * float
let mkComplex x y = (x,y)
let complexToPair (x,y) = (x,y)
//Addition
let (|+|) ((a:float),(b:float)) ((c:float),(d:float)) = ((a+c),(b+d))
//Multiplication
let (|*|) ((a:float),(b:float)) ((c:float),(d:float)) = ((a*c-b*d),(b*c+a*d))
//Subtraction
let (|-|) (x:complex) (y:complex) = x |+| (-(fst y), -(snd y)) 
//Division
exception DotnetGG
let (|/|) (x:complex) ((a:float),(b:float)) =
    if a = 0 || b = 0 then
       raise DotnetGG
    else
    let den = a**2 + b**2
    x |*| (a/den,(-b)/den)

//Exercise 2.5
let explode1 s:string = s.ToCharArray()