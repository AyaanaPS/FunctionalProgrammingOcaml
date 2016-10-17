(* Question A.1 *)

(* This imperative version of the fibonacci function uses
 * a while loop to compute the nth fibonacci number, where
 * n is the input. *)

let fibonacci n = 
	let result = ref 0 in
	let nextTerm = ref 1 in 
	let iter = ref 0 in
	let temp = ref 0 in
	begin
		while !iter < n do
			temp := !result + !nextTerm;
			result := !nextTerm;
			nextTerm := !temp;
			iter := !iter + 1
		done;
		!result
	end

(* This imperative version of the fibonacci function uses
 * a for loop to compute the nth fibonacci number, where n 
 * n is the input. *)

let fibonacci2 n = 
	let result = ref 0 in
	let nextTerm = ref 1 in 
	let temp = ref 0 in
	begin
		for i = 1 to n do
			temp := !result + !nextTerm;
			result := !nextTerm;
			nextTerm := !temp;
		done;
		!result
	end

(* Question A.2 *)

(* This implements the bubble sort sorting method using a while loop and
 * a for loop as well as reference variables that change *)

let bubble_sort an_array = 
	let sizeA = Array.length an_array in
	let x = ref 0 in
	while !x < (sizeA - 1) do
		for i = 0 to (sizeA - 2 - !x) do
			if (an_array.(i+1) < an_array.(i)) then 
				begin
					let temp = ref an_array.(i) in
					an_array.(i) <- an_array.(i+1);
					an_array.(i+1) <- !temp;
				end;
		done;
		x := !x + 1
	done

(* Question B.1 *)

(* This adds a line to the given function in order to add the capability
 * of converting inches to meters *)

let meters_per_foot = 0.3048

let get_meters len = 
	match len with
		| `Meter m -> m
		| `Foot f -> f *. meters_per_foot
		| `Inch i -> i /. 12.0 *. meters_per_foot

let length_add a b = `Meter (get_meters a +. get_meters b)

(* Question B.2 *)

(* This implements a mass abstraction using tagged data. The abstraction
 * can handle grams, kilos and slugs *)

let grams_per_slug = 14593.903203

let get_grams mass = 
	match mass with
		| `Gram g -> g
		| `Kilo k -> k *. 1000.0
		| `Slug s -> s *. grams_per_slug

(* It uses the mass abstraction to add together any two kinds of masses *)

let mass_add a b = `Gram (get_grams a +. get_grams b)

(* This implements a time abstraction using tagged data. The abstraction
 * can handle seconds, minutes, hours and days *)

let get_seconds time = 
	match time with
		| `Second s -> s
		| `Minute m -> m *. 60.0
		| `Hour h -> h *. 60.0 *. 60.0
		| `Day d -> d *. 24.0 *. 60.0 *. 60.0

(* It uses the time abstraction to add together any two kinds of times *)

let time_add a b = `Second(get_seconds a +. get_seconds b)

(* Question B.3 *)

let unit_add data1 data2 = 
	match (data1 data2) with
		| (`Length a, `Length b) -> length_add a b
		| (`Mass a, `Mass b) -> mass_add a b
		| (`Time a, `Time b) -> time_add a b
		| (_, _) -> failwith "incompatible data values"

(* Yes, we get into a combinatorial explosion when adding more unit classes. 
  This is because we have to keep adding more and more cases with the amount
  of unit classes added, since the add function will have to check if the inputs
  are compatible with each other. *)

(* Question C.1 *)

let rec make_gram g = 
	let grams_per_slug = 14593.903203 
	in 
		object
			method get_grams = g
			method get_slugs = g /. grams_per_slug
			method unit_type = `Gram
			method compatible other = 
				match other#unit_type with
					| `Gram -> true
					| `Slug -> true
					| _ -> failwith "incompatible type"
			method add other = 
				make_gram (g +. (other#get_grams))
		end

(* Question C.2.a *)

(* Define a number as a message-passing object. *)
(* "value" is an int. *)
let rec make_number i =
	object
		method value = i
		method show = string_of_int i
		method is_zero = i = 0
		method is_number = true
		method evaluate _ _ = make_number i  (* must evaluate to an object *)
		method derive _ = make_number 0  (* derivative of a number is 0 *)
	end

(* Define a variable as a message-passing object. *)
(* "varname" is a string. *)
let rec make_variable v =
	object
		method value = failwith "variable has no numerical value"
		method show  = v
		method is_zero = false
		method is_number = false
		method evaluate v' n =
			if v = v'
				then make_number n
			else make_variable v
		method derive v' =
			if v = v'
				then make_number 1  (* d/dx(x) = 1 *)
			else make_number 0  (* d/dx(y) = 0 *)
	end
  
(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
	match () with
		| _ when expr1#is_zero -> expr2  (* expr + 0 = expr *)
		| _ when expr2#is_zero -> expr1  (* 0 + expr = expr *)
		| _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
			make_number (expr1#value + expr2#value)
		| _ ->  (* create a new object representing the sum *)
			object
				method value = 
					failwith "sum expression has no numerical value"
				method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
				method is_zero = false
				method is_number = false
				method evaluate v n = 
					make_sum (expr1#evaluate v n) (expr2#evaluate v n)
				method derive v = 
					make_sum (expr1#derive v) (expr2#derive v)
			end

(* Evaluate a message-passing expression with a number substituted for a 
 * variable. *)
let evaluate expr v n = expr#evaluate v n
  
(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v

(* Question C.2.a *)
(* Define a product as a message-passing object *)
let rec make_product expr1 expr2 = 
	match () with
		| _ when expr1#is_zero -> make_number 0
		| _ when expr2#is_zero -> make_number 0
		| _ when expr1#is_number && expr1#value = 1 -> expr2
		| _ when expr2#is_number && expr2#value = 1 -> expr1
		| _ when expr1#is_number && expr2#is_number ->
			make_number (expr1#value * expr2#value)
		| _ ->
			object
				method value = 
					failwith "product expression has no numerical value"
				method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
				method is_zero = false
				method is_number = false
				method evaluate v n = 
					make_product (expr1#evaluate v n) (expr2#evaluate v n)
				method derive v = 
					make_sum (make_product (expr1#derive v) (expr2)) 
						(make_product (expr1) (expr2#derive v))
				end

(* Question C.2.b *)

(* 1. val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj> *)

 (* 2. val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj> *)

(* 3. - : string =
"(((x * (x * y)) + (x * ((x * y) + (x * y)))) + (3 * ((x * (y * y)) + 
(x * (y * y)))))" *)

(* 4. - : string =
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))" *)

(* 5. - : string = "558" *)

(* 6. - : string = "396" *)
