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
    | N x -> x
    | V x -> Map.tryFind x s |> Option.defaultValue 0
    | WL -> w.Length
    | Add (x,y) ->
        let vx = arithEval x w s 
        let vy = arithEval y w s
        vx + vy
    | Sub (x,y) ->
        let vx = arithEval x w s 
        let vy = arithEval y w s
        vx - vy
    | Mul (x,y) ->
        let vx = arithEval x w s 
        let vy = arithEval y w s
        vx * vy
    | PV a -> snd w.[arithEval a w s]

//Exercise 3.4
type cExp =
| C of char (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character,
non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character,
non-letters are unchanged *)
| CV of aExp (* Character lookup at word index *)

