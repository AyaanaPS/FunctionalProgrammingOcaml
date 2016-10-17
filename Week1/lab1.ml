(* Question A.1 *)

(* 1. 10 gives - : int = 10 *)

(* 2. 10. gives - : float = 10. *)

(* 3. 5 + 3 + 4 gives - : int = 12 *)

(* 4. 3.2 + 4.2 gives an error saying the expression
 * has type float, but should be type int. *)

(* 5. 3 +. 4 gives an error saying the expression has
 * type int, but should be of type float. *)

(* 6. 3 + 4.2 gives an error saying the expression has
 * type float, but should be of type int. *)

(* 7. 3 +. 4.2 gives an error saying the expression has
 * type int, but should be of type float. *)

(* 8. 3.0 +. 4.2 gives - : float = 7.2 *)

(* 10. 9 - 3 - 1 gives - : int = 5 *)

(* 11. let a = 3 gives val a : int = 3 *)

(* 12. let b = a + 1 gives val b : int = 4 *)

(* 13. a = b gives - : bool = false *)

(* 14. [1; 2; 3] = [1; 2; 3] gives - : bool = true *)

(* 15. [1; 2; 3] == [1; 2; 3] gives - : bool = false
 	this is different from #14 because = and == do not
 	mean the same thing. = is equivalence, while == means
 	the exact same data (in memory). *)

(* 16. [1, 2, 3] gives - : (int * int * int) list = [(1, 2, 3)] 
 ';'' separates elements in a list vs ',' which separates elements
 of a tuple. Thus, [1, 2, 3] denotes a list of a 3-tuple. *)

(* 17. this expression gives - : int = 4 *)

(* 18. this expression gives a syntax error *)

(* 19. this expression gives - : int = 6 *)

(* 20. this expression gives an error saying the expression
  is of type int, but should be of type unit. Ocaml does not assume
  the else will be of type int as well. Since the then and else 
  statements must be the same type, this will throw an error. *)

 (* Question A.2 *)

(* This function takes in 3 integers and then returns the sum of the
 squares of the two largest ones *)

let sum_of_squares_of_two_largest a b c = 
	if a >= b then 
		if b >= c then (a * a) + (b * b)
		else (a * a) + (c * c)
	else 
		if a >= c then (b * b) + (a * a)
		else (b * b) + (c * c)

 (* Question A.3 *)
 (* This function will decide the type of operator and then
  apply it to the two integers inputted. It outputs the sum of
  a and the absolute value of b by subtracting if b is negative
  and adding if b is positive. *)

(* Question B.1 *)

(* Clearly a call to p() will hang. This is because the
 * recursive function p() simply calls p() infinitely. In the
 * applicative order evaluation, calling test will hang because
 * the function tries to evaluate p(), since the interpreter 
 * first evaluates the operator and operands and then applies
 * the procedure. Meanwhile, in normal order evaluation, the
 * test will return 0. This is because in this method, operands
 * are not evaluated until neither. Thus, in this case, 
 * x will be evaluated first. Since it is equal to 0, the call
 * will return 0 without looking at the value of y. *)

(* Question B.2 *)

(* When Alyssa attempts to use this, she will run into an infinite loop. 
  This is because, since Ocaml is applicative order, it will evaluate
  both the operands. Thus, it will continuously evaluate the recursive 
statement. This tells us that the if syntax in ocaml is normal order form. *)


(* Question B.3 *)

(* add_a a b is a recursive process *)

(* 
add_a 2 5:
if a  = 0
	substitute a with 2
	evaluate 2 -> 2
	evaluate =
	evaluate 0 -> 0
	apply = to 2, 0 -> false

else inc (add_a dec(a) b)
	evaluate add_a dec(a) b
		evaluate dec(a)
			substiute a with 2
			apply dec to 2 -> 2 - 1
				evaluate 2 -> 2
				evaluate -
				evaluate 1 -> 1
				apply - to 2, 1 -> 1
		evaluate b
			substitute b with 5
			evaluate 5 -> 5

		evaluate add_a 1 5
			if a = 0
				substiute a with 1
				evaluate 1 -> 1
				evaluate =
				evaluate 0 -> 0
				apply = to 1, 0 -> false
			else inc (add_a dec(a) b)
				evaluate add_a dec(a) b
					evaluate dec(a)
						substitute a with 1
						apply dec to 1 -> 1 - 1
							evaluate 1 -> 1
							evaluate -
							evaluate 1 -> 1
							apply - to 1, 1 -> 0
					evaluate b
						substiute b with 5
						evaluate 5 -> 5

					evaluate add_a 0 5
						if a = 0
							substitute a with 0
							evaluate 0 -> 0
							evaluate =
							evaluate 0 -> 0
							apply = to 0, 0 -> true

							then b
								substitute b with 5
								evaluate 5 -> 5
				evaluate inc 5
					apply inc to 5 -> 5 + 1
						evaluate 5 -> 5
						evaluate +
						evaluate 1 -> 1
						apply + to 5, 1 -> 6

	evaluate inc 6
		apply inc to 6 -> 6 + 1
			evaluate 6 -> 6
			evaluate + 
			evaluate 1 -> 1
			apply + to 6, 1 -> 7


(* add_b a b is an iterative process *)

add_b 2 5:
if a = 0
	substitute a with 2
	evaluate 2 -> 2
	evaluate =
	evaluate 0 -> 0
	apply = to 2, 0 -> false

else add_b (dec a) (inc b)

	evaluate dec a
		substitute a with 2
		apply dec to 2 -> 2 - 1
			evaluate 2 -> 2
			evaluate -
			evaluate 1 -> 1
			apply - to 2, 1 -> 1

	evaluate inc b
		substiute b with 5
		apply inc to 5 -> 5 + 1
			evaluate 5 -> 5
			evaluate +
			evaluate 1 -> 1
			apply + to 5, 1 -> 6

	evaluate add_b 1 6
		if a = 0
			substiute a with 1
			evaluate 1 -> 1
			evaluate = 
			evaluate 0 -> 0
			apply = to 1, 0 -> false

		else add_b (dec a) (inc b)

			evaluate dec a
				substitute a with 1 
				apply dec to 1 -> 1 - 1
					evaluate 1 -> 1
					evaluate -
					evaluate 1 -> 1
					apply - to 1, 1 -> 0

			evaluate inc b
				substitute b with 6
				apply inc to 6 -> 6 + 1
					evaluate 6 -> 6
					evaluate +
					evaluate 1 -> 1
					apply + to 6, 1 -> 7

			evaluate add_b 0 7
				if a = 0
					substitute a with 0
					evaluate 0 -> 0
					evaluate =
					evaluate 0 -> 0
					apply = to 0, 0 -> true

					then b
						substitute b with 7
						evaluate 7 -> 7


*)

(* Part C.1 *)

(* This function computes the factorial of the input number,
	which for a number n is equal to n * (n-1) * ... * 1. *)
let rec factorial n = 
	if n = 0 then 1 else n * factorial (n - 1)

(* Question C.1.a *)

(* This function computes the x'th term of the expansion. 
 This computation is using the formula 1/x! *)
let e_term x = 
	1.0 /. float_of_int(factorial x)

(* Question C.1.b *)

(* This function computes the e_approximation at the a'th term
 by summing all of the e_terms from 0 to a *)
let rec e_approximation a =
	if (float_of_int a) = 0.0 then 1.0
	else (e_term a) +. e_approximation(a - 1)

(* Question C.1.c *)

(* Calling e_approximation 20 gives the floating point
 	number 2.71828182845904553.
   Calling exp 1.0 gives the floating point number
	2.71828182845904509. *)

(* Question C.1.d *)

(* Calling e_approximation 100 gives the response: 
	- : float = infinity. This happens because the
	numbers become too large to compute. i.e 100! is past
	the capacity of an integer in Ocaml. *)

(* Question C.2 *)

(* These mutually recursive functions report whether the
 inputted number is even or odd *)

let rec is_even a =
	if a = 0 then true
	else is_odd (a - 1)
and is_odd a = 
	if a = 0 then false
	else is_even (a - 1)

(* Question C.3 *)

(* This recursive function calculates 
	f(n-1) + 2*f(n-2) + 3*f(n-3) if n >= 3. *)

let rec f_rec n = 
	if n < 3 then n
	else f_rec (n - 1) + (2 * f_rec (n - 2)) + (3 * f_rec (n - 3))

(* This iterative function uses a helper function to store
	the previous 3 answers for n *)

let rec f_help f1 f2 f3 cnt = 
	if cnt = 0 then f3
	else f_help (f1 + (2 * f2) + (3 * f3)) f1 f2 (cnt - 1)

let rec f_iter n = 
	f_help 2 1 0 n

(* Question C.4 *)

(* This recursive function finds the element of pascal's 
 triangle at the given coordinates. It uses pattern matching
 to do this. *)

let rec pascal_coefficient x y =

	match (x, y) with
		| x', y' when x' < 1 -> failwith "invalid arguments"
		| x', y' when y' > x' -> failwith "invalid arguments"
		| (_, 1) -> 1
		| x', y' when x' = y' -> 1
		| (_, _) -> pascal_coefficient (x - 1) (y - 1) + pascal_coefficient (x - 1) y
