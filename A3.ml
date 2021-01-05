(* CSC 345 Assignment #3
   On my honor, <Jacob Winemiller>, this assignment is my own work and I have not shared my solution with anyone. *)
 
 
(* 1. Write a function opp that returns the boolean complement (opposite) of its argument.  
      For example, not (1 = 2);; returns true *)	  
let opp : bool -> bool = fun x -> if x then false else true;;


(* 2. Write a function dividesEvenlyByFive that returns whether some dividend is evenly divisible by the divisor 5.
      For example, dividesEvenlyByFive 10;; returns true *)
let dividesEvenlyByFive : int -> bool = fun x -> if x mod 5 = 0 then true else false;;


(* 3. Write a function middle that returns the second greatest of three given arguments.
      For example, middle 1 2 3;; returns 2 *)	  
let middle : 'a -> 'a -> 'a -> 'a = fun x y z -> let (a, b) = if (x < y) then (x, y) else (y, x) in if (z < a) then a else if (z > b) then b else z;; 


(* 4. Write a function nor that computes the NOR gate of two boolean arguments.  (Look up " NOR gate" if you do not know what it is.)
      For example, nor (1=2) (2=3);; returns true *)
let nor : bool -> bool -> bool = fun x y -> if x = false && y = false then true else false;;


(* 5. Write a function triangleArea that computers the area of a triangle given its base and height. 
      For example, triangleArea 2 3;; returns 3 *)
let triangleArea : int -> int -> float = fun x y -> let tri = (x * y) in float_of_int tri /. 2.0;;


(* 6. Write a function ceilingDecimal that calculates the ceiling of a float, but returns it as an int rather than a float. 
      For example, ceilingDecimal 15.1;; returns 16 *)
let ceilingDecimal : float -> int = fun x -> (int_of_float (ceil x));;


(* 7. Write a function letterGrade that returns the equivalent letter grade for a given numerical integer grade below.

		Letter Grade	Numerical Grade
		A	93-100
		A-	90-92
		B+	87-89
		B	83-86
		B-	80-82
		C+	77-79
		C	73-76
		C-	70-72
		D+	67-69
		D	63-66
		D-	60-62
		F	< 60%

      For example, letterGrade 91;; returns A- *)
let letterGrade : int -> string = fun x -> 
if x > 92 && x <= 100 then "A" 
else if x > 89 &&  x <= 92 then "A-" 
else if x > 86 && x <= 89 then "B+" 
else if x > 82 && x <= 86 then "B"
else if x > 79 && x <= 82 then "B-"
else if x > 76 && x <= 79 then "C+"
else if x > 72 && x <= 76 then "C"
else if x > 69 && x <= 72 then "C-"
else if x > 66 && x <= 69 then "D+"
else if x > 62 && x <= 66 then "D"
else if x > 59 && x <= 62 then "B-"
else "F";;


(* 8. Write a function averageThree to return the average of three integers. 
      For example, averageThree (-1) 1 2;; returns 0.66666666666666663 *)
let averageThree : int -> int -> int -> float = fun x y z -> let averageThree = x + y + z  in float_of_int averageThree /. 3.0;;


(* 9. Write a function howManyAboveAverage that returns how many of three integer inputs are above its average value.  
      For example, howManyAboveAverage 1 3 3;; returns 2 *)
let howManyAboveAverage : int -> int -> int -> int = fun x y z -> 

let a = x + y + z / 3 in if x > a && y > a then 2 else if x > a && z > a then 2 else if y > a && z > a then 2 else 1;;


(* 10. Write a function howManyWithinThreshold that returns how many of the first three arguments are within the threshold (the fourth argument) of the average of the first three arguments. 
       For example, howManyWithinThreshold 10 1 2 3.5;; returns 2 *)
let howManyWithinThreshold : int -> int -> int -> float -> int = fun x y z t -> 
let avg = (x + y + z) in let a = float_of_int avg /. 3.0 in let upper = a +. t in let lower = a -. t in 
if (lower < float_of_int x) && (float_of_int x < upper) 
&& (lower < float_of_int y) && (float_of_int y < upper) 
&& (lower < float_of_int z) && (float_of_int z < upper) then 3 
else if (lower < float_of_int x) && (float_of_int x < upper) 
		&& (lower < float_of_int y) && (float_of_int y < upper) 
		&& (lower > float_of_int z) || (float_of_int z > upper) then 2  
else if (lower < float_of_int x) && (float_of_int x < upper) 
		&& (lower > float_of_int y) || (float_of_int y > upper) 
		&& (lower < float_of_int z) && (float_of_int z < upper) then 2 
else if (lower > float_of_int x) || (float_of_int x > upper) 
		&& (lower < float_of_int y) && (float_of_int y < upper) 
		&& (lower < float_of_int z) && (float_of_int z < upper) then 2 
else if (lower < float_of_int x) && (float_of_int x < upper) 
		&& (lower > float_of_int y) || (float_of_int y > upper) 
		&& (lower > float_of_int z) || (float_of_int z > upper) then 1 
else if (lower > float_of_int x) || (float_of_int x > upper) 
		&& (lower > float_of_int y) || (float_of_int y > upper) 
		&& (lower < float_of_int z) && (float_of_int z < upper) then 1 
else if (lower > float_of_int x) || (float_of_int x > upper) 
		&& (lower < float_of_int y) && (float_of_int y < upper) 
		&& (lower > float_of_int z) || (float_of_int z > upper) then 1 else 0;;