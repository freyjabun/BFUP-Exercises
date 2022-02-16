type aExp =
| N of int // Integer value
| V of string // Variable
| WL // Length of the word
| PV of aExp // Point value of character at specific word index
| Add of aExp * aExp // Addition
| Sub of aExp * aExp // Subtraction
| Mul of aExp * aExp // Multiplication

type word = (char * int) list
let hello = [('H',4); ('E',1); ('L',1); ('L',1); ('O',1)]

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

//Exercise 3.1
let rec arithEvalSimple a = 
    match a with
    | N x -> x
    | Add (x,y) -> arithEvalSimple x + arithEvalSimple y
    | Sub (x,y) -> arithEvalSimple x - arithEvalSimple y
    | Mul (x,y) -> arithEvalSimple x * arithEvalSimple y

//Exercise 3.2
let a6 = V "x";;
let a7 = N 4 .+. (V "y" .-. V "z");;

let rec arithEvalState a s =
    match a with
    | N x -> x
    | V x -> Map.tryFind x s |> Option.defaultValue 0
    | Add(x,y) -> 
        let vx = arithEvalState x s
        let vy = arithEvalState y s
        vx + vy
    | Sub(x,y) ->
        let vx = arithEvalState x s
        let vy = arithEvalState y s
        vx - vy
    | Mul(x,y) -> 
        let vx = arithEvalState x s
        let vy = arithEvalState y s
        vx * vy

//Exercise 3.3
let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

let rec arithEval a (w:word) s =
    match a with 
    | PV a -> arithEval a w s |> snd w.[]
    | WL -> w.Length
