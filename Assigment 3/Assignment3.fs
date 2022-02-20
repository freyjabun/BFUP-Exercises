type aExp =
    | N of int // Integer value
    | V of string // Variable
    | WL // Length of the word
    | PV of aExp // Point value of character at specific word index
    | Add of aExp * aExp // Addition
    | Sub of aExp * aExp // Subtraction
    | Mul of aExp * aExp // Multiplication

type word = (char * int) list

let hello =
    [ ('H', 4)
      ('E', 1)
      ('L', 1)
      ('L', 1)
      ('O', 1) ]

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)

//Exercise 3.1
let rec arithEvalSimple a =
    match a with
    | N x -> x
    | Add (x, y) -> arithEvalSimple x + arithEvalSimple y
    | Sub (x, y) -> arithEvalSimple x - arithEvalSimple y
    | Mul (x, y) -> arithEvalSimple x * arithEvalSimple y

//Exercise 3.2
let a6 = V "x"
let a7 = N 4 .+. (V "y" .-. V "z")

let rec arithEvalState a s =
    match a with
    | N x -> x
    | V x -> Map.tryFind x s |> Option.defaultValue 0
    | Add (x, y) ->
        let vx = arithEvalState x s
        let vy = arithEvalState y s
        vx + vy
    | Sub (x, y) ->
        let vx = arithEvalState x s
        let vy = arithEvalState y s
        vx - vy
    | Mul (x, y) ->
        let vx = arithEvalState x s
        let vy = arithEvalState y s
        vx * vy

//Exercise 3.3
let arithSingleLetterScore = PV(V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV(V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV(V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

let rec arithEval a (w: word) s =
    match a with
    | N x -> x
    | V x -> Map.tryFind x s |> Option.defaultValue 0
    | WL -> w.Length
    | Add (x, y) ->
        let vx = arithEval x w s
        let vy = arithEval y w s
        vx + vy
    | Sub (x, y) ->
        let vx = arithEval x w s
        let vy = arithEval y w s
        vx - vy
    | Mul (x, y) ->
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

let rec charEval c (w: word) s =
    match c with
    | C c -> c
    | ToUpper c -> System.Char.ToUpper(charEval c w s)
    | ToLower c -> System.Char.ToLower(charEval c w s)
    | CV a -> fst w.[arithEval a w s]

//Exercise 3.5
type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)
    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)

let (.<=.) a b =
    a .<. b
    .||. ~~(a .<>. b) (* numeric less than or equal to *)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b)
    .&&. (a .>=. b) (* numeric greater than *)

let rec boolEval b (w: word) s =
    match b with
    | TT -> true
    | FF -> false
    | AEq (x, y) -> arithEval x w s = arithEval y w s
    | ALt (x, y) -> arithEval x w s < arithEval y w s
    | Not b -> not (boolEval b w s)
    | Conj (x, y) -> (boolEval x w s) && (boolEval y w s)
    | IsDigit c -> System.Char.IsDigit(charEval c w s)
    | IsLetter c -> System.Char.IsLetter(charEval c w s)
    | IsVowel c ->
        let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u'; 'y' ]
        let ch = System.Char.ToLower(charEval c w s)
        List.contains ch vowels

//YELLOW
//Exercise 3.6
let isConsonant (c: cExp) = Not(IsVowel c)

//Exercise 3.7
type stmnt =
    | Skip (* does nothing *)
    | Ass of string * aExp (* variable assignment *)
    | Seq of stmnt * stmnt (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt (* while statement *)

let rec evalStmnt stm (w: word) s =
    match stm with
    | Skip -> s
    | Ass (x, a) ->
        let v = arithEval a w s
        Map.add x v s
    | Seq (stm1, stm2) ->
        let s' = evalStmnt stm1 w s
        evalStmnt stm2 w s'
    | ITE (guard, stm1, stm2) ->
        let b = boolEval guard w s

        if b then
            evalStmnt stm1 w s
        else
            evalStmnt stm2 w s
    | While (guard, stm) ->
        let b = boolEval guard w s

        if b then
            let s' = evalStmnt stm w s
            evalStmnt (While(guard, stm)) w s'
        else
            s

//Exercise 3.8
type squareFun = word -> int -> int -> int

let stmntToSquareFun stm (w: word) pos acc =
    let pEval = evalStmnt stm w (Map [ ("_pos_", pos); ("_acc_", acc) ])
    Map.find "_result_" pEval

let singleLetterScore = stmntToSquareFun (Ass("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass("_result_", arithTripleWordScore))

let containsNumbers =
    stmntToSquareFun (Seq(
        Ass("_result_", V "_acc_"),
        While(
            V "i" .<. WL,
            ITE(IsDigit(CV(V "i")), Seq(Ass("_result_", V "_result_" .*. N -1), Ass("i", WL)), Ass("i", V "i" .+. N 1))
        )
    ))
