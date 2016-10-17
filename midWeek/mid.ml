(* Question 1.a 
	Since the let bindings are recursive, this can not be desugared to a fun
	expression.

	f 3 4 0
		Evaluate f ==> itself
		Evaluate 3 ==> 3
		Evaluate 4 ==> 4
		Evaluate 0 ==> 0
		Apply f to 3 4 0:
			Substitute y ==> 3
			Substitute z ==> 4
			Substitute acc ==> 0
			Evaluate if 0 > 4:
				Evaluate 0 ==> 0
				Evaluate 4 ==> 4
				Evaluate > ==> >
				Apply > to 0 and 4 ==> false
			Evaluate f (y + z) (z + acc) (acc + y)
				Evaluate f ==> itself
				Evaluate (y + z):
					Substitute y ==> 3
					Substitute z ==> 4
					Evaluate 3 ==> 3
					Evaluate 4 ==> 4
					Evaluate + ==> +
					Apply + to 3 and 4 ==> 7
				Evaluate (z + acc):
					Substitute z ==> 4
					Subsitute acc ==> 0
					Evaluate 4 ==> 4
					Evaluate 0 ==> 0
					Evaluate + ==> +
					Apply + to 4 and 0 ==> 4
				Evaluate (acc + y)
					Substitute acc ==> 0
					Substitute y ==> 3
					Evaluate 0 ==> 0
					Evaluate 3 ==> 3
					Evaluate + ==> +
					Apply + to 0 and 3 ==> 3
				Apply f to 7 4 3
					Substitute y ==> 7
					Substitute z ==> 4
					Substitute acc ==> 3
					Evaluate if 3 > 4:
						Evaluate 3 ==> 3
						Evaluate 4 ==> 4
						Evaluate > ==> >
						Apply > to 3 and 4 ==> false
					Evaluate f (y + z) (z + acc) (acc + y)
						Evaluate f ==> itself
						Evaluate (y + z):
							Substitute y ==> 7
							Substitute z ==> 4
							Evaluate 7 ==> 7
							Evaluate 4 ==> 4
							Evaluate + ==> +
							Apply + to 7 and 4 ==> 11
						Evaluate (z + acc):
							Substitute z ==> 4
							Substitute acc ==> 3
							Evaluate 4 ==> 4
							Evaluate 3 ==> 3
							Evaluate + ==> +
							Apply + to 4 and 3 ==> 7
						Evaluate (acc + y):
							Substitute acc ==> 3
							Substitute y ==> 7
							Evaluate 3 ==> 3
							Evaluate 7 ==> 7
							Evaluate + ==> +
							Apply + to 3 and 7 ==> 10
						Apply f to 11 7 10:
							Substitute y ==> 11
							Substitute z ==> 7
							Substitute acc ==> 10
							Evaluate if 10 > 7:
								Evaluate 10 ==> 10
								Evaluate 7 ==> 7
								Evaluate > ==> >
								Apply > to 10, 7 ==> true
									Evaluate acc ==> 10

*)

(* Question 1.b

Evaluate (fun x y -> 
	((fun y -> x * y) x) - ((fun x -> y * x * y) y)) (x + y) (x - y):

Evaluate (x + y)
	This can be desugared as follows:
		let x = 7 in
		let y = x * 3 in
		(x + y)

		Desugared Version: (fun x -> (fun y -> x + y) (x * 3)) 7

	Evaluating the desugared version:
	Evaluate 7 ==> 7
	Evaluate fun x -> (fun y -> x + y) (x * 3)) ==> itself
	Apply fun x to 7:
		Substitute x with 7 in the body of fun x
		Evaluate (7 * 3):
			Evaluate 7 ==> 7
			Evaluate 3 ==> 3
			Evaluate * ==> *
			Apply * to 7 and 3 ==> 21
		Evaluate fun y -> x + y ==> itself
		Substitute y with 21 in the body of fun y
		Lambda Shielding occurs here for the x in fun y
		Evaluate (7 + 21):
			Evaluate 7 ==> 7
			Evaluate 21 ==> 21
			Evaluate + ==> +
			Apply + to 7 and 21 ==> 28

Evaluate (x - y)
	This can be desugared as follows:
		let x = 7 in
		let y = x * 3 in
		(x - y)

		Desugared version: (fun x -> (fun y -> x - y) (x * 3)) 7

	Evaluating the desugared version:
	Evaluate 7 ==> 7
	Evaluate fun x -> (fun y -> x + y) (x * 3)) ==> itself
	Aplly fun x to 7:
		Substitute x with 7 in the body of fun x
		Evaluate (7 * 3):
			Evaluate 7 ==> 7
			Evaluate 3 ==> 3
			Evaluate * ==> *
			Apply * to 7 and 3 ==> 21
		Evaluate fun y -> x + y ==> itself
		Substitute y with 21 in the body fo fun y
		Lambda Shielding occurs here for the x in fun y
		Evaluate (7 - 21):
			Evaluate 7 ==> 7
			Evaluate 21 ==> 21
			Evaluate - ==> -
			Apply - to 7 and 21 ==> -14

Evaluate fun x y -> ((fun y -> x * y) x) - ((fun x -> y * x * y) y)) ==> itself
Apply fun x y to 28 and -14: 
	Subsitute x for 28
	Subsitute y for -14
	Evaluate ((fun y -> x * y) x):
		Substitute outside x for 28
		Evaluate fun y -> x * y ==> itself
		Apply fun y -> x * y to 28:
			Substitute y for 28
			Lambda Shielding Occurs here: 
			Substitute x for 28
			Evaluate 28 ==> 28
			Evaluate 28 ==> 28
			Evaluate * ==> *
			Apply * to 28 and 28 ==> 784
	Evaluate ((fun x -> y * x * y) y)
		Substitute outer y for -14
		Evaluate fun x -> y * x * y ==> itself
		Apply fun x -> y * x * y to -14
			Substitute x for -14
			Lambda Shielding Occurs here:
			Substitute y for -14
			Evaluate -14 ==> -14
			Evaluate -14 ==> -14
			Evaluate -14 ==> -14
			Evaluate * ==> *
			Evaluate * ==> *
			Apply * and * to -14, -14 ==> 196
			Apply * to 196, -14 ==> -2744
	Evaluate - ==> -
	Apply - to 784 and -2744 ==> 3528
*)

(* Question 2.a *)

(* The function solve_quad will run in constant time O(1). This is 
 * because the speed of execution of the function is not dependent 
 * on the size of the inputs. The function consists of arithmetic
 * operations, which, as the question specifies, run in constant 
 * time. Thus, for each of the arguments to the function, solve_quad
 * runs in constant time. *)

(* Question 2.b *)

(* This function will keep on recursively calling f while the inputted
 * arguments x and y are greater than 0. Therefore, the function runs in linear
 * time since its running time increases exactly as much as its inputs increase.
 * Thus, for x, the function will take O(x) time since it will run x times 
 * (until x = 0). Similarily, for y, the function will take O(y) time since 
 * it will run y times (until y = 0). Thus, the overall function f will be 
 * O(x) + O(y) time. This is equivalent to O(x + y). Thus the time complexity 
 * is O(n) where n is sum of the two arguments. *)

(* Question 2.c *)

(* This function runs in O(log n) time. This is because the terminating case
 * for the helper iterative function is when k > y and k doubles every time
 * iter is called. Thus, we don't need to recursively call iter y - k times. 
 * We can call it less times then that. *)

(* Question 2.d *)

(* For ramanujan_number:

 * In the worst case the ramanujan_number function won't find a ramanujan
 * number and will thus have to look at all the possible values until 
 * n1 > max_n. Thus, for n4, iter is called, in the worst case max_n + 1 times.
 * Then, once n4 > max_n, iter is called for every possible combination of
 * numbers for n3 and n4, which is a total of (max_n + 1)^2 times. Similarily, 
 * iter is called (max_n + 1)^3 times for n2 (for every combination of numbers
 * for n2, n3 and n4) and then (max_n + 1)^4 times for n1 (for the combinations
 * for n1, n2, n3 and n4). Thus the total asymptotic time complexity for this 
 * algorithm is O(n^4) where n is the max_n. *)

(* For ramanujan_number2:

 * This function returns the result as soon as it finds it, unlike the first
 * version of the function. If it doesn't find one, it keeps making different
 * combinations of n1 n2 n3 n4 until n1 > max_n. Once this case is reached, it
 * returns None, indicating no ramanujan number was found. Because this function
 * will return the result once it is found, it has a better best case time
 * complexity than the first version of the function. However, in the worst
 * case, the case where no ramanujan number is found, this function has the
 * same time complexity as the first version - O(n^4) because it runs in the
 * same way *)

(* Question 3.1 *)

(* This function uses a recursive iterator to find the smallest element in
 * an inputted list, as well as the index of that element. It returns a tuple
 * containing the smallest element and its index. *)

let find_smallest listy = 
	let rec iter lst index smallest cnt =
		match lst with
			| [] -> (smallest, index)
			| hd :: tl when hd < smallest -> iter tl cnt hd (cnt + 1)
			| hd :: tl -> iter tl index smallest (cnt + 1)
		in
	match listy with
		| [] -> failwith "find_smallest: empty list"
		| hd::tl -> iter (listy) (0) (max_int) (0)

(* This function returns a new list that results in replacing the inputted
 * index of the inputted old list with the inputted new integer. It uses
 * a recursive iterative helper function to do this. *)

let replace index numb lst = 
	let rec iter lst newLst index numb cnt = 
		match lst with
			| [] -> failwith "replace: no place to put value"
			| hd :: tl when index = cnt -> newLst @ (numb :: []) @ tl
			| hd :: tl -> iter tl (newLst @ (hd :: [])) index numb (cnt + 1)
	in
	iter lst [] index numb 0

(* This function returns a list of integers where the first element of the 
 * inputted list is swapped with the smallest element of the inputted list *)

let swap_smallest_with_first lst = 
	let do_replace lst =
		let firstVal = List.hd(lst) in
		let smallIndex = find_smallest lst in
		let newLst = replace 0 (fst smallIndex) lst in
		replace (snd smallIndex) firstVal newLst
	in
	match lst with
		| [] -> failwith "swap_smallest_with_first: no smallest element"
		| _ -> do_replace lst
	
(* This function uses the above function to implement the minimum sort
 * algorithm. It returns the sorted version of the inputted list *)

let rec minimum_element_sort lst = 
	match lst with
		| [] -> lst
		| _ -> List.hd(swap_smallest_with_first lst) :: 
			minimum_element_sort (List.tl(swap_smallest_with_first lst))

(* Question 3.2 *)

(* This function flips a specified part of the inputted lst. The specified
 * part begins from the start index inputted and goes for the number of
 * elements inputted *)

let flip lst startIndex numElems = 
	let endIndex = startIndex + numElems in 
	let rec iter lst befFlip flippedLst aftFlip index cnt =
		match lst with
			| [] when cnt < index -> failwith "flip: invalid inputs"
			| [] when endIndex > cnt -> failwith "flip: invalid inputs"
			| [] -> befFlip @ flippedLst @ aftFlip
			| hd :: tl when cnt < index -> iter 
				tl (befFlip @ (hd :: [])) flippedLst aftFlip index (cnt + 1)
			| hd :: tl when cnt >= index && cnt < endIndex -> 
				iter tl befFlip (hd :: flippedLst) aftFlip index (cnt + 1)
			| hd :: tl when cnt >= endIndex -> iter 
				tl befFlip flippedLst (aftFlip @ (hd :: [])) index (cnt + 1)
			| _ -> failwith "flip: invalid inputs"
		in
	iter lst [] [] [] startIndex 0

(* This function uses the flip function to flip an inputted list from the
 * start index for 4 values. It ensures that the start index is between 0
 * and 2 inclusive. *)

let flip_rubik_1d lst startIndex = 
	match startIndex with
		| x when x > 2 -> failwith "flip_rubik_1d: invalid input"
		| x when x < 0 -> failwith "flip_rubik_1d: invalid input"
		| _ -> flip lst startIndex 4

(* This function uses the flip_rubik_1d function to flip the inputted
 * list a series of times according to the values in the movesLst *)

let rec rubik_1d puzzleLst movesLst =
	match movesLst with 
		| [] -> puzzleLst
		| hd :: tl -> rubik_1d (flip_rubik_1d puzzleLst hd) tl

(* Question 3.3 *)

(* This function converts the inputted binary list representation into
 * an integer *)

let binary_to_integer bList = 
	let rec iter lst sum pow2 =
		match lst with 
			| [] -> sum
			| hd :: tl -> iter tl (sum + (hd * pow2)) (pow2 * 2)
	in
	iter bList 0 1

(* exclusive-or on two binary digits. *)

let bxor d1 d2 = if d1 = d2 then 0 else 1

(* "and" on two binary digits. *)

let band d1 d2 = if d1 = 1 && d2 = 1 then 1 else 0

(* "or" on two binary digits. *)

let bor d1 d2 = if d1 = 1 || d2 = 1 then 1 else 0

(* This function adds 3 digits using the above functions *)

let add_digits d1 d2 carry = 
	let d = bxor d1 (bxor d2 carry) in
	let carryOut = bor (band d1 d2) (band carry (bxor d1 d2)) in
	(d, carryOut)

(* This function adds two binary list representations of the same length
 * using the add_digits function *)

let badd b1 b2 = 
	let rec iter b1 b2 sumLst carryVal = 
		match (b1, b2) with 
			| ([], []) -> 
				if carryVal = 1 then sumLst @ (carryVal :: [])
				else sumLst
			| ([], _) -> failwith "badd: unequal lists"
			| (_, []) -> failwith "badd: unequal lists"
			| (hd :: tl, hd1 :: tl1) -> 
				iter tl tl1 
				(sumLst @ ((fst (add_digits hd hd1 carryVal)) :: [])) 
				(snd (add_digits hd hd1 carryVal)) 
	in
	iter b1 b2 [] 0

(* Question 4.1 *)

(* This function take the inputted function and initial value and generates
 * an entire list *)

let unfold f init = 
	let rec iter lst f init =
		match (f init) with
			| None -> lst
			| Some (x, y) -> iter (lst @ (x :: [])) f y
	in
	iter [] f init

(* The following uses the helper function ascending and the unfold function
 * to generate a list of numbers from -100 to 100 in increments of 4. The
 * ascending function returns None when the inputted value is greater than 
* 100 *)

let ascending i = if i > 100 then None else Some (i, (i + 4))

let ascending_list = unfold ascending (-100)

(* The all_suffixes function uses the helper function and the unfold function
 * to generate a list of all of the suffixes of an inputted list. The helper
 * function gets the inputted list itself as the suffix and then pa=sses
 * the tail of the list as the next initial value. It returns None when
 * the inputted list is empty. *)

let all_suffixes lst = 
	let get_suffix lst =
		match lst with
			| [] -> None
			| hd :: tl -> Some(lst, tl)
	in
	unfold get_suffix lst

(* Question 4.2 *)

(* Ben's solution is not efficient in terms of space or time utilization. 
 * This is because his solution first calls the map function to map
 * the function f onto every element of the list. It then calls the
* accumulate helper function to accumulate the elements of the new list. 
* Instead of generating a new list and then accumulating on that, we can
* just accumulate while we map - apply the function to element 1, accumulate
* it, apply the function to element 2, accumulate it, etc. The below functions
* do this process in two ways, recursively and iteratively *)

let rec mapReduce1_rec f op init lst = 
	match lst with
		| [] -> init
		| hd :: tl -> op (f hd) (mapReduce1_rec f op init tl)

let mapReduce1_iter f op init lst = 
	let rec iter f op lst acc = 
		match lst with 
			| [] -> acc
			| hd :: tl -> iter f op tl (op acc (f hd))
	in
	iter f op lst init

(* As they are written, mapReduce1_rec and mapReduce1_iter do give the
 * the same result for commutative operators, but not for non-commutative
 * operators. This is because the order of the arguments being passed to the
 * accumulating operator is not the same in both functions, which means
 * non commutative operators will not be evaluated in the same direction.
 * It doesn't make that much sense to use the mapReduce functions
 * for non commutative operators since it is difficult to tell which order
 * the user would desire the arguments to be evaluated.
*)

(* Question 4.3 *)

(* This defines the tree function for the mapReduce_tree function *)

type 'a tree =
	| Leaf of 'a
	| Branch of 'a tree * 'a tree

(* This function does a map reduce on a tree structure using an iterative
 * procedure *)

let mapReduce_tree f op tr =
	let rec iter f op tr = 
		match tr with
			| Leaf x -> f x 
			| Branch (x, y) -> op (iter f op x) (iter f op y)
	in
	iter f op tr

(* This function does a map reduce on a list of lists using a function that
 * maps from a list to a single value *)

let rec mapReduceN f op acc lstLst = 
	match lstLst with
		| [] -> failwith "mapReduceN: no sequence"
		| [] :: _ -> acc
		| hd :: tl -> op (f (List.map List.hd lstLst))
			(mapReduceN f op acc (List.map List.tl lstLst))
