(* Question A.1 *)

(* This declares the type mobile and branch *)
type mobile = Mobile of branch * branch
and branch = 
	| Weight of int * int
	| Structure of int * mobile

(* These functions make the mobile, weight and structure *)
let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* This function accesses the left branch of the mobile *)
let left_branch = function
	| Mobile (x', y') -> x'

(* This function accesses the right branch of the mobile *)
let right_branch = function
	| Mobile (x', y') -> y'

(* This function gets the length of the branch *)
let branch_length = function
	| Weight (x', y') -> x'
	| Structure (x', y') -> x'

(* This function gets the branch structure, either a weight or sub-mobile *)
let branch_structure = function
	| Weight (x', y') -> `Weight y'
	| Structure (x', y') -> `Structure y'

(* This function computes the total weight of the branch without using the
	above functions *)
let rec branch_weight1 = function
	| Weight (x', y') -> y'
	| Structure (x', y') -> total_weight1 y' 
and total_weight1 = function
	| Mobile (x', y') -> branch_weight1 x' + branch_weight1 y'

(* This computes the total weight using the above functions *)
let rec branch_weight2 branch =
	match branch_structure branch with
	| `Weight x' -> x'
	| `Structure x' -> total_weight2 (x')
and total_weight2 mob = 
	branch_weight2 (left_branch mob) + branch_weight2 (right_branch mob)

(* This recursively checks if the mobile and all its sub mobiles are balanced *)
let rec is_balanced mob =
	let helper branch = 
		match branch_structure branch with
			| `Weight x' -> true
			| `Structure x' -> is_balanced x' in
	(branch_length (left_branch mob) * branch_weight2 (left_branch mob)) =
		(branch_length (right_branch mob) * branch_weight2 (right_branch mob))
	&& (helper (left_branch mob)) && (helper (right_branch mob))

(* This declares an alternate type of the mobile, branch and contents *)
type mobile' = { left: branch'; right: branch' }
and branch' = Branch' of int * contents
and contents = Weight' of int | Structure' of mobile'

(* This makes the mobile *)
let make_mobile' l r =
	let new_mob = 
		let left = l in
		let right = r in
			{left; right} in
	new_mob

(* This makes the weight *)
let make_weight' l w =
	Branch' (l, Weight' w)

(* This makes the structure *)
let make_structure' l m =
	Branch' (l, Structure' m)

(* This accesses the left branch of the mobile *)
let left_branch' mob =
	mob.left

(* This accesses the right branch of the mobile *)
let right_branch' mob = 
	mob.right

(* This gets the length of the branch *)
let branch_length' (Branch' (x, y)) = 
	x

(* This gets the structure of the branch *)
let branch_structure' (Branch' (x, y)) =
	match y with
	| Weight' x' -> `Weight x'
	| Structure' x' -> `Structure x'

(* This recursively find the weight of the branch using the above functions *)
let rec branch_weight' branch =
	match branch_structure' branch with
		| `Weight x' -> x'
		| `Structure x' -> total_weight' (x')
and total_weight' mob = 
	branch_weight' (left_branch' mob) + branch_weight' (right_branch' mob)

(* This recursively checks if the mobile and all of the sub mobiles 
	are balanced *)
let rec is_balanced' mob =
	let helper branch = 
		match branch_structure' branch with
			| `Weight x' -> true
			| `Structure x' -> is_balanced' x' in
	(branch_length' (left_branch' mob) * branch_weight' (left_branch' mob)) =
		(branch_length' (right_branch' mob) * branch_weight' (right_branch' mob))
	&& (helper (left_branch' mob)) && (helper (right_branch' mob))

(* Question A.2 *)

(* This defines the new type tree *)
type tree = Tree of elem list
and elem = 
	| Num of int
	| Sub of tree

(* This recursively squares all of the elements of the tree *)
let rec square_tree (Tree (tr))  = 
	let rec do_square tr1 =
		match tr1 with
		| [] -> []
		| Num h :: t -> Num (h * h) :: do_square t
		| Sub h :: t -> Sub (square_tree h) :: (do_square t) in
	Tree(do_square tr)

(* This uses map and recursion to square all of the elements of the tree *)
let rec square_tree' (Tree (tr)) = 
	let do_square tr1 = 
		match tr1 with
			| Num h -> Num (h * h)
			| Sub h -> Sub (square_tree h) in
	Tree(List.map do_square tr)

(* Question A.3 *)

(* This abstracts the above functions so that any function can be applied
	to all the elements of the tree *)
let rec tree_map f (Tree (tr)) = 
	let helper tr1 = 
		match tr1 with
			| Num h -> Num (f h)
			| Sub h -> Sub (tree_map f h) in
	Tree(List.map helper tr)

let square_tree'' tree = tree_map (fun n -> n * n) tree

(* Question A.4 *)

(* This returns all the subsets of a set *)
let rec subsets = function
	| [] -> [[]]
	| h :: t -> let rest = subsets t in
		rest @ (List.map(fun x -> h::x) rest)

(* This function uses pattern matching to find the subsets. The base case is
	an empty set, whose subset would just be [[]]. It then matches against a 
	set containing a head and tail. It then recursively calls subsets on t and
	appends the result to the result of mapping the function fun x -> h :: x
	to the contents of rest. This cons the head of each recursively called upon
	list to every element of rest. Thus, it slowly builds up all of the subsets.
*)

(* Question A.5 *)

(* This function appliesthe operator to the sequence *)
let rec accumulate op initial sequence = 
	match sequence with
		| [] -> initial
		| h :: t -> op h (accumulate op initial t)

(* This uses the accumulate function to map p to the elements in a sequence *)
let map p sequence = 
	accumulate (fun x r -> p x :: r ) [] sequence

(* This appends two sequences together *)
let append seq1 seq2 = 
	accumulate (fun x r -> x :: r) seq2 seq1

(* This computes the length of the sequence *)
let length sequence = 
	accumulate (fun x y -> y + 1) 0 sequence

(* Question A. 6 *)

(* This applies the operator to combine the first element of each sub_list, the
	second element, etc *)
let rec accumulate_n op init seqs = 
	match seqs with
		| [] -> failwith "empty list"
		| [] :: _ -> []
		| h :: t -> accumulate op init (List.map List.hd seqs) :: 
			accumulate_n op init (List.map List.tl seqs)

(* Question A. 7 *)

(* This creates a map function that applies the function to two lists *)
let rec map2 f x y =
	match (x, y) with
      | ([], []) -> []
      | ([], _) -> failwith "unequal lists"
      | (_, []) -> failwith "unequal lists"
      | (x', y') -> (f (List.hd x') (List.hd y')) :: 
      		(map2 f (List.tl x') (List.tl y'))

(* This returns the dot product of two vectors *)
let dot_product v w = accumulate (+) 0 (map2 ( * ) v w) 

(* This returns the result of multiplying a matrix by a vector *)
let matrix_times_vector m v = List.map (fun row -> dot_product v row) m

(* This returns the result of transposing a matrix *)
let transpose mat = accumulate_n (fun x y -> x :: y) [] mat

(* This returns the result of multiplying a matrix by a matrix *)
let matrix_times_matrix m n =
	let cols = transpose n in 
		List.map (fun row -> matrix_times_vector cols row) m

(* Question B.1 *)

(* This is the given filter function *)
let rec filter predicate sequence = 
	match sequence with
		| [] -> []
		| h :: t when predicate h -> h :: filter predicate t
		| _ :: t -> filter predicate t

(* This sorts the inputted list using the quicksort algorithm in the order
	determined by the comparison operator inputted *)
let rec quicksort lst cmp = 
	match lst with
		| [] -> []
		| [x] -> [x]
		| h :: t -> (quicksort (filter (fun x -> cmp x h) t) cmp) @ 
			(h :: []) @ (quicksort (filter (fun x -> not (cmp x h)) t) cmp)


(* Question B.2 *)

(* The quicksort algorithm is generative recursion for the same reason that
	the mergesort algorithm discussed in the slides is generative - because
	it recurses on subparts of data that is being generated. In this case,
	lists of 'smaller than pivot' and 'larger than pivot' values are being
	generated and then recursively sorted. Meanwhile, structural recursion acts
	on data that is naturally a subpart of the given data (ex. first element and
	rest of the elements). Since quicksort, does not behave in that manner, it
	is generative recursion. *)

(* Question B.3 *)

(* Ben's version of the function leads to a stack overflow during recursion
	error. This is because the looping recursion can never terminate because
	it does not reach a base case. This is because in splitting up a 
	one element list into even and odd, one will be empty and the other will 
	contain one element. This one element list will try to split and again lead
	to an empty list and a one element list. Thus, it will never end, which 
	leads to a stack overflow error. This is why two base cases are required.*)

(* Question B.4 *)

(* This is the given insert in order function *)
let rec insert_in_order new_result a_list cmp = 
	match a_list with
		| [] -> [new_result]
		| h :: t when cmp new_result h -> new_result :: a_list
		| h :: t -> h :: insert_in_order new_result t cmp

(* This uses the insert in order function to perform an insertion sort on an
	inputted list in the order determined by the comparison operator inputted *)
let rec insertion_sort a_list cmp = 
	match a_list with
		| [] -> []
		| h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(* Question C.1.1 *)

(* This is the given type definition for the algebraic expressions *)
type expr =
    Int of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Pow of expr * int

(* This function, from Lab 3, returns the result of raising x to y *)
let rec pow x y = 
	if y = 0 then 1
	else x * pow x (y - 1)

(* This function simplifies a given expression according to a set of rules *)
 let rec simplify1 expr = 
 	match expr with
 		| Var (x') -> Var (x')
 		| Int (x') -> Int (x')
 		| Add (x', Int 0) -> x'
 		| Add (Int 0, y') -> y'
 		| Add (Int x', Int y') -> Int (x' + y')
 		| Add (x', y') -> Add((simplify1 x'), (simplify1 y'))
 		| Mul (x', Int 0) -> Int (0)
 		| Mul (Int 0, y') -> Int (0)
 		| Mul (Int 1, y') -> y'
 		| Mul (x', Int 1) -> x'
 		| Mul (Int x', Int y') -> Int(x' * y')
 		| Mul (x', y') -> Mul((simplify1 x'), (simplify1 y'))
 		| Pow (x', 0) -> Int (1)
 		| Pow (x', 1) -> x' 
 		| Pow (Int x', y') -> Int(pow x' y')
 		| Pow (x', y') -> Pow((simplify1 x'), y')

(* This is the given simplify function *)
let rec simplify expr = 
	let e = simplify1 expr in
		if expr = e
			then expr
		else simplify e


(* Question C.1.2 *)

(* This implements differentiation rules for a given expression differentiated
	to the inputted variable *)
let rec deriv expr va = 
	match expr with
		| Int (x') -> Int 0
		| Var x' when x' = va -> Int 1
		| Var _ -> Int 0 
		| Add(x', y') -> Add(deriv x' va, deriv y' va)
		| Mul(x', y') -> Add(Mul(deriv x' va, y'), Mul(x', deriv y' va))
		| Pow(x', y') -> Mul (Mul (Pow(x', (y'- 1)), Int y'), (deriv x' va))

(* This is the given derivative function *)
let derivative expr var =
	let e = simplify expr in
    let d = deriv e var in
    	simplify d
