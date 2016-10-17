open Num

(* Question A.1 *)

(* The space complexity is O(n). This is because since the expression has been
 	fully evaluated, the memory used in evaluating is returned the
 	expression. In the case of n = 7, the memory used will be in evaluating
 	n = 6 to n = 0, which gives us n times memory is used (since the memory is
 	returned). This is different (and less) from the time complexity because
	many evaluations must be evaluated multiple times, thus taking more time.
	Meanwhile, they occupy the same space so new space does not need to be
	allocated every time the same thing is evaluated. *)

(* Question A.2 *)

(* Part a:

	sine 12.15 =
		p(sine(4.05)):
			sine(4.05) =
				p(sine(1.35))
					sine(1.35) = 
						p(sine(0.45))
							sine(0.45) = 
								p(sine(0.15))
									sine(0.15) = 
										p(sine(0.05))
											sine(0.05) =
												0.05

	As seen above, p is applied 5 times when sine 12.15 is evaluated. *)

(* Part b:
	
	Since functions cube and p are not recursive and are simply a formula that
	does not depend on the size of n, they perform a constant number of steps.
	Thus, they also require a constant amount of memory space. The sine function
	has both a space and time complexity of O(logn) since in each recursive call
	of sine, that does not end the evaluation by being less then 0.1, the angle 
	is divided by 3. Thus, it only has to go through at most 1/3 of n 
	iterations. *)

(* Question A.3 *)

(* Part 1. This is a non-iterative function that does exponentiation using 
 	successive squaring. It uses pattern matching on the n argument *)

let rec fast_expt b n = 
	let is_even m = m mod 2 = 0 in
	let square m = m * m in
		match n with
			| 0 -> 1
			| n' when is_even n' -> square (fast_expt b (n / 2))
			| _ -> b * fast_expt b (n - 1)

	
(* Part 2. This is an iterative function that does exponentation. It uses
 	a recursive helpter function to find the final result of b^n. *)

let ifast_expt b n =
	let is_even m = m mod 2 = 0 in
	let square m = m * m in
	let rec iter a b n =
		match n with
			| 0 -> a
			| n' when is_even n' -> iter a (square b) (n'/2)
			| _ -> iter (a * b) b (n - 1) in
	iter 1 b n


(* Question A.4 *)

(* This is a fast multiplication that is analogous to the expt function
 	from above. It uses doubling and halving to recursively come to the
 	result of multiplying two numbers a and b *)

let rec fast_mult a b = 
	let double x = x + x in
	let halve x = x/2 in 
	let is_even m = m mod 2 = 0 in
		match  b with
			| 0 -> 0
			| b' when is_even b' -> fast_mult (double a) (halve b')
			| _ -> a + fast_mult (a) (b - 1)

(* Question A.5 *)

(* This function performs an iterative version of the fast multiplication
	seen in Question A.4. It again uses doubling and halving, as well as
	a recursive iteration helper function. It also uses pattern matching *)

let ifast_mult a b = 
	let double x = x + x in
	let halve x = x/2 in
	let is_even m = m mod 2 = 0 in
	let rec ifast_iter x a b =
		match b with
			| 0 -> x
			| b' when is_even b' -> ifast_iter x (double a) (halve b)
			| _ -> ifast_iter (x + a) a (b - 1) in 
	ifast_iter 0 a b

(* Question A.6 *)

(* The space complexity of this function is O(logn). This is because each time
	foo is called, it is called on the result of f on half n. This means that
	in every non terminating iteration, the input is halved. Thus, only logn
	evaluations must be stored. The time complexity of this function is O(n). 
	This is because it is a linear function. *)

(* Question A.7 *)

(* This function represents a linear recursive process. Linear recursion is 
	when an action has a simple repetitive structure consisting of a basic
	step followed by the action again. Here, there is a simple step (checking
	for the base case) and then a call to the action again in case the base
	case fails. Furthermore, since p0 + p1 happens at the end of the recursive 
	call, there is a linear number of pending operations. Thus, this is 
	linear recursive. 
	This function is O(n) in both space and time complexity.*)

(* Question B.1 *)

(* Question B.1.a *)

(fun x y -> x * (2 + y)) 20 (2 * 4)

(* Question B.1.b *)

(fun a b c -> sqrt(b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0

(* Question B.1.c *)

(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1

(* Question B.1.d *)

(fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1

(* Question B.2 *)

(* Desugaring let to fun *)
(* 
(fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
Subsitute for the outermost fun: 
	x -> (2 * 10)
	y -> (3 + 4)
	fun (2 * 10) (3 + 4):
		evaluate 2 * 10 -> 20
		evaluate (3 + 4) -> 7
	fun (20) (7):
		Subsitute for the first inside function: fun y
			y -> 14
			fun (14):
				Substitute for the innermost function: fun z
					z -> 22
					fun (22):
						Subsitute for function call:
						x * y * z
							Substitute for x, y and z:
								x -> 20
								y -> 14 (* Shielding *)
								z -> 22
						fun(22) -> 20 * 14 * 22
							evaluate 20 * 14 * 22 -> 6160 *)

(* Question B.3 *)

(* This doesn't work because the x in y = x * 2 is not the same x as in x = 10. 
	Similarily, the y in z = y + 3 is not the same y as the y in y = x * 2. 
	Look at the following desugared version of this code. In passing in the 
	arguments 10, x * 2 and y + 3, Ocaml does not recognize what the x and y
	are referring to. Thus, an unbound value error will come up. *)

(fun x y z -> x + y + z) 10 (x * 2) (y + 3)

(* The following code fixes this problem. *)

let x = 10 in
	let y = x * 2 in
		let z = y + 3 in
			x + y + z

(fun x -> (fun y -> (fun z -> x + y + z) (y + 3)) (x * 2)) 10


(* Part C *)

let ni = num_of_int

(* Question C.1 *)

(* This is an iterative version of the sum function defined in the 
	problem set *)

let isum term a next b = 
	let rec iter a result = 
		if a > b 
			then result
		else iter (next a) ((term a) +/ result)
	in
		iter a (ni 0)

(* Question C.2 *)

(* This returns the product of the values of a function at points over a given
	range. It is a recursively defined function *)

let rec product_rec term a next b =
	if a > b
		then (ni 1)
		else term a */ (product_rec term (next a) next b)

(* This does the same thing as product_rec. However, it is an iteratively
	defined function *)

let product_iter term a next b = 
	let rec iter a result = 
		if a > b
			then result
		else iter (next a) ((term a) */ result)
	in
		iter a (ni 1)

(* This uses the product_rec funciton to find the factorial of a number *)

let factorial_rec n = 
	let term a = a in
	let next b = b +/ (ni 1) in
	product_rec term (ni 1) next n

(* This uses the product_iter function to find the factorial of a number *)

let factorial_iter n = 
	let term a = a in
	let next b = b +/ (ni 1) in
	product_iter term (ni 1) next n

(* The following shows how the product function can be used to compute 
	approximations to pi *)

let pi_product n = 
	let term a = a in
	let next b = b +/ (ni 2) in
	((ni 4) */ product_rec term (ni 4) next (ni n)) // 
	((ni 2) */ product_rec term (ni 3) next (ni n))

let pi_approx = 
	(ni 4) */ pi_product (1000)

(* Question C.3 *)

(* This is a recursive accumulate function *)

let rec accumulate_rec combiner null_value term a next b = 
	if a > b
		then null_value
	else (combiner) term a (accumulate term (next a) next b)

(* This is an iterative accumulate function *)

let accumulate_iter combiner null_value term a next b = 
	let rec iter a result = 
		if a > b
			then null_value
		else iter (next a) ( (combiner) (term a) result)
	in
		iter a null_value

(* This sum function uses the recursive accumulate function *)

let sum term a next b =
	accumulate_rec +/ 0 term a next b

(* This is an iterative accumulate function *)

let product term a next b = 
	accumulate_rec */ 1 term a next b

(* Question C.4 *)

(* This returns the composite of two functions *)

let compose f g x = 
	f (g x)

(* Question C.5 *)

(* This uses the compose function to repeat a function n times. *)

let rec repeated f n = 
	if n = 0
		then fun x -> x
	else compose f (repeated f (n - 1))

(* Question C.6 *)

(* This smooths a function by computing the average of f(x-dx), f(x) and 
	f(x+dx) *)

let smoothed f dx =
	fun x -> 
		(f (x - dx) + f (x) + f (dx)) / 3

(* This smooths the function n times using the repeated function *)

let nsmoothed f n dx =
	let helper f = 
		smoothed f dx 
	in
		repeated helper n

(* Question D.1 *)

(*  The is prime function recursively check all values from 2 up to the square 
	root of n to see if they divide n *)

let is_prime n = 
	let rec iter x result =
		if x < 2 || result = false then result
		else iter (x - 1) (n mod x != 0)
	in
		if n <= 1 then false
		else iter (int_of_float (sqrt (float_of_int n))) true

(* Question D.2 *)

(* This function uses the is_prime function to iteratively find the 
	smallest prime factor of an input number n. If the input is prime or
	less than 2, an error is raised. *)

let smallest_prime_factor n = 
	let rec iter x result = 
		if result = true && is_prime x then x
		else iter (x + 1) (n mod (x + 1) = 0)
	in
		if is_prime n = true || n < 2 then failwith "invalid_arg"
		else iter 1 false
