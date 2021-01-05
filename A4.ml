(* CSC 345 Assignment #4
   On my honor, <Jacob Winemiller>, this assignment is my own work and I have not shared my solution with anyone. *)
 
 
(* 1. Write a function threeDifferent that returns true if no two of the three arguments are equal, and false otherwise.  
      For example, threeDifferent 1 2 2;; returns false *)	  
let threeDifferent : int -> int -> int -> bool = fun x y z -> if (x = y) || (y = z) || (x = z) then false else true ;;


(* 2. Write a function sum that uses recursion to compute the sum of all numbers from 1 to n, where n is greater than or equal to 1.
      For example, sum 3;; returns 6 *)
let rec sum : int -> int = fun n -> if n = 1 then 1 else n + sum (n - 1);;


(* 3. Write a function getBinary that uses recursion to convert an integer n (where n is greater than or equal to 0) to its binary representation.
      For example, getBinary 12;; returns 1100 
                   getBinary 7;;  returns 111
                   getBinary 42;; returns 101010 
      Hint:    if n's binary representation is 10010101011
            (n / 2)'s binary representation is 1001010101
            (N % 2)'s binary representation is           1 *)	  
let rec getBinary : int -> int = fun n -> match n with
                                          0 -> 0
                                          | _ -> n mod 2 + 10 * getBinary(n/2);;


(* 4. Write a function howManyDigits that uses recursion to count the digits of an integer n (where n is greater than or equal to 1).
      For example, howManyDigits 978;; returns 3 *)
let rec howManyDigits : int -> int = fun n -> match n with
                                              0 -> 0
                                              | _ -> howManyDigits (n / 10 ) + 1;;


(* 5. Write a function orderTriple that takes a triple, and returns a version in increasing order.
      For example, orderTriple (2, 1, 3);; returns (1, 2, 3) *)
let orderTriple : int * int * int -> int * int * int = fun (x, y, z) -> let (a,b) = if (x < y) then (x,y) else (y, x) in if (z < a) then (z, a, b) else if (z > b) then (a,b,z) else (a,z,b);;