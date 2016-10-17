open List

(* Question A.1 *)

(* This defines the point and segment types. *)

type point = {x : float; y : float}
type segment = {startp: point; endp: point}

(* This function finds and returns the midpoint of a line segment *)

let midpoint_segment seg = 
	let midPoint = 
		let x = (seg.startp.x +. seg.endp.x) /. 2.0 in
		let y = (seg.startp.y +. seg.endp.y) /. 2.0 in
			{x; y} in
	midPoint 

(* This function finds the length of the segment by computing the distance
	between the start point and end point of the segment *)

let segment_length seg = 
	let inside1 = (seg.endp.x -. seg.startp.x) *. (seg.endp.x -. seg.startp.x) in
	let inside2 = (seg.endp.y -. seg.startp.y) *. (seg.endp.y -. seg.startp.y) in
	sqrt(inside1 +. inside2)

(* This function prints out the point in the form (x, y). *)

let print_point pnt = 
	Printf.printf "(%g %g)\n" pnt.x pnt.y

(* This function takes in two coordinates and makes a point out of them *)

let make_point xVal yVal = 
	let new_pt = 
		let x = xVal in
		let y = yVal in
			{x; y} in
	new_pt

(* This function takes in two points and makes a segment out of them *)

let make_segment pt1 pt2 = 
	let new_seg = 
		let startp = pt1 in
		let endp = pt2 in
			{startp; endp} in
	new_seg

(* This function returns the coordinates of an inputted point as a tuple *)

let get_coords pt = 
	(pt.x, pt.y)

(* This function returns the start and end points of an inputted segment as a
	tuple *)

let get_points seg = 
	(seg.startp, seg.endp)

(* Question A.2 *)

(* This defines the first rectangle type - the rectangle is 
	represented by the lower-left corner points and the upper-right
	corner points. *)

type rectangle = {low: point; upper: point}

(* This returns the segment representing the bottom part of the rectangle *)

let rectangle_lower_segment rect =
	let pt2 = make_point rect.upper.x rect.low.y in
		let seg = make_segment rect.low pt2 in
	seg

(* This returns the segment representing the top part of the rectangle *)

let rectangle_upper_segment rect = 
	let pt2 = make_point rect.low.x rect.upper.y in
		let seg = make_segment pt2 rect.upper in
	seg

(* This returns the segment representing the left part of the rectangle *)

let rectangle_left_segment rect = 
	let pt2 = make_point rect.low.x rect.upper.y in
		let seg = make_segment pt2 rect.low in
	seg

(* This returns the segment representing the right part of the rectangle *)

let rectangle_right_segment rect =
	let pt2 = make_point rect.upper.x rect.low.y in
		let seg = make_segment pt2 rect.upper in
	seg 

(* This returns the perimeter of the rectangle. This is computed by adding the
	doubled segment length of the right and bottom segments *)

let rectangle_perimeter rect =
	(2.0 *. segment_length (rectangle_right_segment rect)) +. 
		(2.0 *. segment_length (rectangle_lower_segment rect))

(* This returns the area of the rectangle. This is computed by multiplying the
	right and bottom segment lengths *)

let rectangle_area rect = 
	segment_length (rectangle_right_segment rect) *.
		segment_length (rectangle_lower_segment rect)

(* This defines the second rectangle type - the rectangle is represented
	by the lower x and coordinates and the upper x and y coordinates as 4
	separate float values *)

type rectangle2 = {lowerX: float; upperX: float; lowerY: float; upperY: float}

(* This returns the segment representing the bottom part of the rectangle *)

let rectangle_lower_segment2 rect = 
	let pt1 = make_point rect.lowerX rect.lowerY in
	let pt2 = make_point rect.upperX rect.lowerY in
		let seg = make_segment pt1 pt2 in
	seg

(* This returns the segment representing the top part of the rectangle *)

let rectangle_upper_segment2 rect = 
	let pt1 = make_point rect.lowerX rect.upperY in
	let pt2 = make_point rect.upperX rect.upperY in
		let seg = make_segment pt1 pt2 in 
	seg

(* This returns the segment representing the left part of the rectangle *)

let rectangle_left_segment2 rect = 
	let pt1 = make_point rect.lowerX rect.lowerY in
	let pt2 = make_point rect.lowerX rect.upperY in
		let seg = make_segment pt1 pt2 in
	seg

(* This returns the segment representing the right part of the rectangle *)

let rectangle_right_segment2 rect = 
	let pt1 = make_point rect.upperX rect.lowerY in
	let pt2 = make_point rect.upperX rect.upperY in
		let seg = make_segment pt1 pt2 in
	seg

(* This returns the perimeter of the rectangle. This is computed by adding the
	doubled segment length of the right and bottom segments *)

let rectangle_perimeter2 rect = 
	(2.0 *. segment_length (rectangle_right_segment2 rect)) +. 
		(2.0 *. segment_length (rectangle_lower_segment2 rect))

(* This returns the area of the rectangle. This is computed by multiplying the
	right and bottom segment lengths *)

let rectangle_area2 rect = 
	segment_length (rectangle_right_segment2 rect) *.
		segment_length (rectangle_lower_segment2 rect)

(* This takes two points and makes a rectangle of type 1 *)

let make_rectangle pt1 pt2 = 
	let rect = 
		let low = pt1 in
		let upper = pt2 in
			{low; upper} in
	rect

(* This takes 4 floats and makes a rectangle of type 2 *)

let make_rectangle2 xLow xUp yLow yUp = 
	let rect = 
		let lowerX = xLow in
		let upperX = xUp in
		let lowerY = yLow in
		let upperY = yUp in
			{lowerX; upperX; lowerY; upperY} in
	rect

(* Question 3 *)

let make_pair x y = fun m -> m x y
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(* first z = first (make_pair x y) = first (fun m -> m x y). 
	Then z (fun x y -> x) = (fun m -> m x y) (fun x y -> x) Since z is a 
	function representing the pair, the function fun x y -> x will extract
	its first value. *)

(* Substitution Model Evaluation for second (make_pair 1 2): 
	second (make_pair 1 2)
		evaluate make_pair 1 2
			evaluate make_pair -> make_pair x y = fun m -> m x y
			evaluate 1 
			evaluate 2
			apply make_pair to 1 and 2 -> make_pair 1 2 = fun m -> m 1 2
		evaluate second -> second z = z (fun x y -> y)
		apply second to make_pair 1 2:
			second (fun m -> m 1 2) -> (fun m -> m 1 2) (fun x y -> y)
				evaluate fun m -> m 1 2
				evaluate fun x y -> y
				apply fun x y -> y:
					fun 1 2 -> 2 
*)

(* Question 4 *)

(* This function takes two int arguments and returns the first raised to the
	power of the second *)

let rec pow x y = 
	if y = 0 then 1
	else x * pow x (y - 1)

(* This function takes two int arguments and returns the amount of times the
	first evenly divides the second *)

let int_log x y = 
	let rec iter a x y = 
		if y mod x != 0 then a
		else iter (a + 1) x (y / x) in
	iter 0 x y

(* This uses the pow function to represent a pair of integers x and y as 
	(2^x) * (3^y)*)

let make_pairi x y = (pow 2 x) * (pow 3 y)

(* This uses the int_log function to find the first integer by seeing how many
	times 2 divides the product *)

let firsti product = int_log 2 product 

(* This uses the int_log function to find the second integer by seeing how many
	times 3 divides the product *)

let secondi product = int_log 3 product

(* Question 5 *)

(* initializes zero unary *)
let zero = []

(* checks if the unary is zero *)
let is_zero = function
	| [] -> true
	| () :: _ -> false

(* returns the successor of the inputted unary *)
let succ u = () :: u

(* returns the previous of the inputted unary *)
let rec prev = function
	| [] -> invalid_arg "unary is zero"
	| [x1] -> []
	| [x1; x2] -> x1 :: []
	| hd :: tl -> hd :: prev tl

(* gets the unary version of the inputted integer *)
let integer_to_unary x = 
	let rec getU u a x =
		if a = x then u
		else getU (succ u) (a + 1) x in
	getU [] 0 x

(* gets the integer version of the inputted unary *)
let unary_to_integer u = 
	let rec getCount u num =
		if u = [] then num
		else getCount (prev u) (num + 1) in
	getCount u 0

(* returns the unary that is the result of adding the two inputted unaries
	together *)
let unary_add u1 u2 =
	let rec result u1 u2 = 
		if u2 = [] then u1
		else result (succ u1) (prev u2) in
	result u1 u2

(* defines the new type *)
type nat = Zero | Succ of nat

(* initializes the zero *)
let zero' = Zero

(* checks if it is a zero *)
let is_zero' = function
	| Zero -> true
	| Succ _ -> false

(* returns the successor of the unary *)
let succ' u = Succ u

(* returns the previous of the unary *)
let prev' = function
	| Zero -> invalid_arg "unary is zero"
	| Succ t -> t

(* gets the unary version of the integer *)
let integer_to_unary' x = 
	let rec getU u a x =
		if a = x then u
		else getU (succ' u) (a + 1) x in
	getU Zero 0 x

(* gets the integer version of the unary *)
let unary_to_integer' u = 
	let rec getCount u num =
		if u = Zero then num
		else getCount (prev' u) (num + 1) in
	getCount u 0

(* returns the result of adding two unaries together *)
let unary_add' u1 u2 = 
	let rec result u1 u2 = 
		if u2 = Zero then u1
		else result (succ' u1) (prev' u2) in
	result u1 u2

(* The integer_to_unary, unary_to_integer and unary_add functions do not have 
	to change their definitions for this new representation.  *)

(* Question A.6 *)

(* defines the first 10 church numerals *)
let zero s z = z
let add1 n s z = s (n s z)

let one s z = s z 
let two s z = s (s z)
let three s z = s (s (s z))
let four s z = s (s (s (s z)))
let five s z = s (s (s (s (s z))))
let six s z = s (s (s (s (s (s z)))))
let seven s z = s (s (s (s (s (s (s z))))))
let eight s z = s (s (s (s (s (s (s (s z)))))))
let nine s z = s (s (s (s (s (s (s (s (s z))))))))
let ten s z = s (s (s (s (s (s (s (s (s (s z)))))))))

(* adds two church numerals together *)
let add m n s z = (m s (n s z))

let church_to_integer n =  n (fun s -> (s + 1)) 0

(* Question A.7 *)

(* the type of church_to_integer is val church_to_integer : 
	((int -> int) -> int -> 'c) -> 'c. For all the church numerals from
	two to ten, the type is ('a -> 'a) -> 'a -> 'a. Thus, it is clear
	why church_to_integer of all the numerals from two to ten return an 
	int if we map the type of the numerals to the type of church_to_integer:
	('a -> 'a) maps to the (int -> int) part, which confirms that the type
	of 'a is int. Then, the 'c part of church_to_integer also maps to 'a, 
	which thus means that church_to_integer returns an int, since 'a is an
	int. To see why church_to_integer one and church_to_integer zero returns
	an int, we can perform a similar mapping procedure.
	The type of val zero is 'a -> 'b -> 'b. By mapping this to the type of
	church_to_integer, we see that 'a corresponds to the (int -> int) part,
	the first 'b corresponds to the second int and the third 'b corresponds
	to the 'c part. Since we know 'b is an int (because of the first b in 
	zero's type), 'c must be of type int. Thus, church_to_integer zero 
	returns an int. 
	The type of val one is ('a -> 'b) -> 'a -> 'b. Thus by mapping to the
	type of church_to_integer, ('a -> 'b) corresponds to (int -> int), which
	means 'a = int and 'b = int. The second 'a corresponds to int and the 
	second 'b corresponds to 'c. Thus, since we know 'b = int, 'c must also
	be an int. Thus, church_to_integer returns an int.

*)

(* Question B.1 *)

(* This function returns a list containing the last item in a list. *)

let rec last_sublist = function
	| [x1; x2] -> x2 :: []
	| hd :: tl -> last_sublist tl
	| [] -> invalid_arg "last_sublist: empty list"

(* Question B.2 *)

(* This function returns a list that is the reverse of the inputted list *)

let reverse lst = 
	let rec getLst newLst = function
		| [] -> newLst
		| hd :: tl -> getLst (hd :: newLst) tl in
	getLst [] lst

(* Question B.3 *)

(* The following two functions square every item in an inputted list and
	return the result as a list *)

let rec square_list = function
	| [] -> []
	| h :: t -> (h * h) :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(* Question B.4 *)

(* This returns the list in reverse order because of the ((h * h) :: answer)
	part. Each time it moves to the next item in the inputted list, it appends
	the already computed list to its square. This means that each computed
	square goes to the beginning of the list. Thus, at the end of the function,
	the first item in the answer list is the last square computed. *)

(* The second one doesn't work because, while using the :: operator, the
	answer variable isn't the right type. *)

(* The second solution can be resolved like this: 
  let square_list items =
    let rec iter things answer =
      match things with
        | [] -> answer
        | h :: t -> iter t (answer @ ((h * h) :: []))
    in iter items []

    Now, answer is appended with a list containing the square of each element. 
    This is not as efficient *)

(* Question B.5.1 *)

(* This counts the number of negative numbers inside of a list. *)

let count_negative_numbers lst = 
	let rec iter lst numNeg =
		if lst = [] then numNeg
		else 
			if List.hd(lst) < 0 then (iter (List.tl(lst)) (numNeg + 1))
			else (iter (List.tl(lst)) (numNeg))
	in
	iter lst 0

(* Question B.5.2 *)

(* This makes a new list containing the first n powers of 2 where n is the 
	inputted value. *)

let power_of_two_list x = 
	let rec fill lst a b = 
		if a = b then lst
		else (fill (lst @ ((pow 2 a) :: [])) (a + 1) (b)) in
	fill [] 0 x 

(* Question B.5.3 *)

(* This returns a list containing the prefix sum of the original list *)

let prefix_sum lst = 
	let rec fill newLst oldLst curSum = 
		if oldLst = [] then newLst
		else fill (newLst @ ((curSum + List.hd(oldLst)) :: [])) (List.tl(oldLst))
			(curSum + List.hd(oldLst)) in
	fill [] lst 0

(* Question B.6 *)

(* This function reverses a list of lists (including the lists in the lists *)

let deep_reverse lst = 
	let rec getLst newLst = function
		| [] -> newLst
		| hd :: tl -> getLst (reverse hd :: newLst) tl in
	getLst [] lst

(* Question B.7 *)

type 'a nested_list = 
	| Value of 'a
	| List of 'a nested_list list

(* This function reverses a nested list whose type is defined above *)

let deep_reverse_nested lst = 
	let rec iter oldLst newLst = 
		match oldLst with
		| [] -> newLst
		| Value hd :: tl -> iter tl (Value hd :: newLst)
		| List hd :: tl -> iter tl (List (iter hd []) :: newLst) in
	match lst with
		| Value v -> Value v
		| List l -> List (iter l [])
