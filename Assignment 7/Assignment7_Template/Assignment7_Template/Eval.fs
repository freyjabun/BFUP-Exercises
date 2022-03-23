module Eval

open StateMonad

(* Code for testing *)
let hello =
    [ ('H', 4)
      ('E', 1)
      ('L', 1)
      ('L', 1)
      ('O', 1) ]

let state =
    mkState [ ("x", 5); ("y", 42) ] hello [ "_pos_"; "_result_" ]

let emptyState = mkState [] [] []

let add a b = a >>= fun x -> b >>= fun y -> ret (x + y) 
let div a b = a >>= fun x -> b >>= fun y ->
    match y with
    | 0 -> fail DivisionByZero
    | _ -> ret (x / y)
let sub a b = a >>= fun x -> b >>= fun y -> ret (x - y)
let mul a b = a >>= fun x -> b >>= fun y -> ret (x * y)
let modu a b = a >>= fun x -> b >>= fun y ->
    match y with
    | 0 -> fail DivisionByZero
    | _ -> ret (x % y)
let isVowel c = 
        let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u'; 'y' ]
        let ch = System.Char.ToLower c
        List.contains ch vowels

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | Div of aExp * aExp
    | Mod of aExp * aExp
    | CharToInt of cExp

and cExp =
    | C of char (* Character value *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

type bExp =
    | TT (* true *)
    | FF (* false *)

    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)

    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)

    | IsVowel of cExp (* check for vowel *)
    | IsLetter of cExp (* check for letter *)
    | IsDigit of cExp (* check for digit *)

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (./.) a b = Div(a, b)
let (.%.) a b = Mod(a, b)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.->.) b1 b2 =
    (~~b1) .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b)
    .&&. (a .>=. b) (* numeric greater than *)

let rec arithEval a : SM<int> =
    match a with
    | N x -> ret x 
    | V x -> lookup x
    | WL -> wordLength
    | PV x -> arithEval x >>= fun x' -> pointValue x'
    | Add (x, y) -> add (arithEval x) (arithEval y)
    | Sub (x, y) -> sub (arithEval x) (arithEval y)
    | Mul (x, y) -> mul (arithEval x) (arithEval y)
    | Div (x, y) -> div (arithEval x) (arithEval y)
    | Mod (x, y) -> modu (arithEval x) (arithEval y)
    | CharToInt x -> charEval x >>= fun x' -> int x' |> ret

and charEval c : SM<char> =
    match c with
    | C x -> ret x
    | CV x -> arithEval x >>= fun x' -> characterValue x'
    | ToUpper x -> charEval x >>= fun x' -> System.Char.ToUpper x' |> ret 
    | ToLower x -> charEval x >>= fun x' -> System.Char.ToLower x' |> ret
    | IntToChar x -> arithEval x >>= fun x' -> char x' |> ret

let rec boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false
    | AEq (x, y) -> arithEval x >>= fun x' -> arithEval y >>= fun y' -> ret (x' = y')
    | ALt (x, y) -> arithEval x >>= fun x' -> arithEval y >>= fun y' -> ret (x' < y')
    | Not x -> boolEval x >>= fun x' -> ret (not x')
    | Conj (x, y) -> boolEval x >>= fun x' -> boolEval y >>= fun y' -> ret (x' && y')
    | IsLetter x -> charEval x >>= fun x' -> System.Char.IsLetter x' |> ret
    | IsDigit x -> charEval x >>= fun x' -> System.Char.IsDigit x' |> ret
    | IsVowel x -> charEval x >>= fun x' -> isVowel x' |> ret


type stm =
    (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm (* while statement *)

let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)

let prog = new StateBuilder()

let arithEval2 a = failwith "Not implemented"
let charEval2 c = failwith "Not implemented"
let rec boolEval2 b = failwith "Not implemented"

let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *)

type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>

let stmntToSquareFun stm = failwith "Not implemented"


type coord = int * int

type boardFun = coord -> Result<squareFun option, Error>

let stmntToBoardFun stm m = failwith "Not implemented"

type board =
    { center: coord
      defaultSquare: squareFun
      squares: boardFun }

let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
