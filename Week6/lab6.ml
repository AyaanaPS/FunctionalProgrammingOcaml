(* Question A.1

FRAME 0 (initial environment)
    parent: none
    bindings:
        - : [primitive function -]
        * : [primitive function *]

Function 0 (fun n -> let rec iter m r = if m = 0 then r 
            else iter (m - 1) (r * m) in iter n 1)
    parent: Frame 0
    param: n
    body: let rec iter m r = if m = 0 then r else iter (m - 1) (r * m)
            in iter n 1

Frame 1 (let factorial n = (...))
    parent: Frame 0
    bindings:
        factorial : Function 0

Frame 2 (factorial 3)
    parent: Frame 0
    bindings: n = 3

Frame 3 (let rec iter m r = if m = 0 then r else iter (m - 1) ( r * m))
    parent: Frame 2
    bindings: 
        iter : dummy value

Function 1 (iter m r -> if m = 0 then r else iter (m -1) (r * m))
    enviornment: Frame 3
    param: m and r
    body: if m = 0 then r else iter (m - 1) (r * m)

Frame 3 (let rec iter = (...))
    parent: Frame 2
    bindings: 
        iter : Function 1

Frame 4 (iter n 1)
    parent : Frame 3
    bindings : m = 3
                r = 1

Frame 5 (iter 2 3)
    parent : Frame 3
    bindings : m = 2
                r = 3

Frame 6 (iter 1 6)
    parent : Frame 3
    bindings : m = 1
                r = 6

Frame 7 (iter 0 6)
    parent : Frame 3
    bindings : m = 0
                r = 6


Result = 6

*)

(* Question A.2 *)

(* This computes the factorial without using a recursive expression *)
let factorial = 
    let f = ref (fun n -> 0) in 
    let factFun = fun n -> if n = 0 then 1 else n * !f (n - 1) in
    f := factFun;
    fun n -> factFun n

(* Question B.1 *)

(* This defines the Stat_error exception *)
exception Stat_error of string

(* This makes the statistics object that can hold data *)
let make_stat_1 i =
    let sum = ref 0.0 in 
    let sumsq = ref 0.0 in 
    let n = ref 0 in
    object
        method append x =  
            begin
                sum := !sum +. x;
                sumsq := !sumsq +. (x *. x);
                n := !n + 1;
            end
        method mean = 
            if !n = 0 then raise(Stat_error "need at least one value for mean")
            else !sum /. float_of_int !n
        method variance = 
            if !n = 0 then raise(Stat_error "need at least one value for variance")
            else ((!sumsq) -. (!sum *. !sum /. float_of_int !n)) /. float_of_int !n
        method stdev = 
            if !n = 0 then raise(Stat_error "need at least one value for stdev")
            else sqrt(((!sumsq) -. (!sum *. !sum /. float_of_int !n)) /. float_of_int !n)
        method clear = 
            begin
                sum := 0.0;
                sumsq := 0.0;
                n := 0;
              end  
    end

(* Question B.2 *)

(* This makes another statistics object that is a better version of the
 * make_stat_1 object. This is because it gets rid of the copy pasted code
 * in the variance and stdev part of the object using a self reference *)

let make_stat_2 i =
    let sum = ref 0.0 in 
    let sumsq = ref 0.0 in 
    let n = ref 0 in
    object(self)
        method append x =  
            begin
                sum := !sum +. x;
                sumsq := !sumsq +. (x *. x);
                n := !n + 1;
            end
        method mean = 
            if !n = 0 then raise(Stat_error "need at least one value for mean")
            else !sum /. float_of_int !n
        method private _variance = ((!sumsq) -. (!sum *. !sum /. float_of_int !n)) /. float_of_int !n
        method variance = 
            if !n = 0 then raise(Stat_error "need at least one value for variance")
            else self#_variance
        method stdev = 
            if !n = 0 then raise(Stat_error "need at least one value for stdev")
            else sqrt(self#_variance)
        method clear = 
            begin
                sum := 0.0;
                sumsq := 0.0;
                n := 0;
              end  
    end


(* Question C.1 *)


module type PRIORITY_QUEUE =
    sig
        exception Empty
  
        type elem      (* Abstract type of elements of queue. *)
        type t         (* Abstract type of queue. *)

        val empty      : t                (* The empty queue.         *)
        val is_empty   : t -> bool        (* Check if queue is empty. *)
        val insert     : t -> elem -> t   (* Insert item into queue.  *)
        val find_min   : t -> elem        (* Return minimum element.  *)
        val delete_min : t -> t           (* Delete minimum element.  *)
        val from_list  : elem list -> t   (* Convert list to queue.   *)
    end

(* This implements the PriorityQueue module by defining all of the above
 * functions. It also implements a merge and rank function. *)
module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
    struct
        exception Empty

        type elem = int
        type t = Leaf | Node of int * elem * t * t

        let empty = Leaf
        let is_empty heap = (empty = heap)
        let rank heap = 
            match heap with
            | Leaf -> 0
            | Node (a, b, c, d) -> a
        let rec merge hp1 hp2 =
            let newHeap x hp1 hp2 = 
                if (rank hp1) > (rank hp2) then 
                    Node (rank hp2 + 1, x, hp1, hp2)
                else Node (rank hp1 + 1, x, hp2, hp1) 
            in
            match (hp1, hp2) with
            | (Leaf, _) -> hp2
            | (_, Leaf) -> hp1
            | (Node (r1, i1, lhp1, rhp1), Node (r2, i2, lhp2, rhp2)) ->
                if i1 < i2 then newHeap i1 lhp1 (merge rhp1 hp2)
                else newHeap i2 lhp2 (merge rhp2 hp1)
        let insert heap elem = merge heap (Node (1, elem, Leaf, Leaf))
        let find_min heap = 
            match heap with
            | Leaf -> raise(Empty)
            | Node (a, b, c, d) -> b
        let delete_min heap =
            match heap with
            | Leaf -> raise(Empty)
            | Node (a, b, c, d) -> merge c d
        let rec from_list lst = 
            match lst with
            | [] -> Leaf
            | (hd::tl) -> insert (from_list tl) hd
end

(* This uses the PriorityQueue module to heap_sort a list *)
let heap_sort lst = 
    let rec helper hp lst = 
        if PriorityQueue.is_empty hp then List.rev lst
        else 
            let minElem = PriorityQueue.find_min hp in
            let newHp = PriorityQueue.delete_min hp in
            helper newHp (minElem :: lst)
    in
    helper (PriorityQueue.from_list lst) []

(* Question C.2 *)

(* Type for ordered comparisons. *)
type comparison = LT | EQ | GT

(* Signature for ordered objects. *)
module type ORDERED =
    sig
        type t
        val cmp: t -> t -> comparison
    end

(* Signature for priority queues. *)

module OrderedString =
    struct
      type t = string
      let cmp x y = 
        if x = y then EQ else if x < y then LT else GT
    end

(* This makes the PriorityQueue module more generic by implementing it
 * as a functor *)
module MakePriorityQueue (Elt : ORDERED) 
    : (PRIORITY_QUEUE with type elem = Elt.t) =
    struct
        exception Empty

        type elem = Elt.t
        type t = Leaf | Node of int * elem * t * t

        let empty = Leaf
        let is_empty heap = (empty = heap)
        let rank heap = 
            match heap with
            | Leaf -> 0
            | Node (a, b, c, d) -> a
        let rec merge hp1 hp2 =
            let newHeap x hp1 hp2 = 
                if (rank hp1) > (rank hp2) then 
                    Node (rank hp2 + 1, x, hp1, hp2)
                else Node (rank hp1 + 1, x, hp2, hp1) 
            in
            match (hp1, hp2) with
            | (Leaf, _) -> hp2
            | (_, Leaf) -> hp1
            | (Node (r1, i1, lhp1, rhp1), Node (r2, i2, lhp2, rhp2)) ->
                if Elt.cmp i1 i2 = LT then newHeap i1 lhp1 (merge rhp1 hp2)
                else newHeap i2 lhp2 (merge rhp2 hp1)
        let insert heap elem = merge heap (Node (1, elem, Leaf, Leaf))
        let find_min heap = 
            match heap with
            | Leaf -> raise(Empty)
            | Node (a, b, c, d) -> b
        let delete_min heap =
            match heap with
            | Leaf -> raise(Empty)
            | Node (a, b, c, d) -> merge c d
        let rec from_list lst = 
            match lst with
            | [] -> Leaf
            | (hd::tl) -> insert (from_list tl) hd
    end

module StringPQ = MakePriorityQueue(OrderedString)

(* This uses the functor version of PriorityQueue to heap_sort a list *)
let heap_sort_2 lst = 
    let rec helper hp lst = 
        if StringPQ.is_empty hp then List.rev lst
        else 
            let minElem = StringPQ.find_min hp in
            let newHp = StringPQ.delete_min hp in
            helper newHp (minElem :: lst)
    in
    helper (StringPQ.from_list lst) []
