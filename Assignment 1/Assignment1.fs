//Exercise 1.1
let sqr x = x*x

//Exercise 1.2
let pow x n = System.Math.Pow(x,n)

//Exercise 1.3
let rec sum = 
    function
    |0 -> 0
    |n -> n + sum(n-1) 

//Exercise 1.4
let rec fib =
    function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n-1)+fib(n-2)

//Exercise 1.5
let dup (s:string) = s+s

//Exercise 1.6
let rec dupn s =
    function
    | 0 -> ""
    | n -> s + (dupn s (n-1))

//Exercise 1.7
let rec bin =
    function
    | (n,_) when n = 0 -> 1
    | (_,k) when k = 0 -> 1
    | (n,k) when k >= n -> 1
    | (n,k) -> bin(n-1,k-1) + bin(n-1,k)

//Exercise 1.8
let timediff (h1,m1) (h2,m2) = (h2*60+m2) - (h1*60+m1)

//Exercise 1.9
let minutes (h,m) = timediff (0,0) (h,m)

//Exercise 1.10
let curry f = fun a b -> f(a,b)
let uncurry f = fun (a,b) -> f a b

