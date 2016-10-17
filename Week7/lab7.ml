type light_state = On | Off

type game_input = Quit | Coords of int * int
  
let newline () = Printf.printf "\n"

(* Question 1 *)

(* This represents a light object. The game is composed
 * of 25 of these light objects. *)

let make_light () = 
	let state = ref Off in 
	let neighbors = ref [] in
	object(self)
		method state = !state
		method set_state x = state := x
		method toggle =
			match !state with
			| On -> state := Off
			| Off -> state := On 
		method add_neighbor x = 
			neighbors := x :: !neighbors
		method update =
			begin
				self#toggle;
				List.iter (fun x -> x#toggle) !neighbors
			end
	end

(* Question 2 *)

(* This helper function returns true if the inputted row and
 * col are valid locations in the board and false otherwise. *)

let valid_location row col =
	row >= 0 && row <= 4 && col >= 0 && col <= 4

(* This board constructor function makes a board that is an
 * array of 5 arrays, each with 5 light objects. It then 
 * returns a function that takes a row and col and returns
 * the light object at that location if it is a valid position. *)

let make_board () = 
	let board = [|
	[|make_light (); make_light (); make_light (); make_light (); make_light ()|];
	[|make_light (); make_light (); make_light (); make_light (); make_light ()|];
	[|make_light (); make_light (); make_light (); make_light (); make_light ()|];
	[|make_light (); make_light (); make_light (); make_light (); make_light ()|];
	[|make_light (); make_light (); make_light (); make_light (); make_light ()|];
	|] in

	fun row col -> if valid_location row col then board.(row).(col) 
		else failwith "Not a valid location"

(* Question 3 *)

(* This is a game object containing methods allowing one to play the 
 * game. It also has helper functions to initialize the board given
 * an inputted set up, and to connect lights adjacent to each other. *)

  let make_game () =
    (*** Helper functions. ***)
  
    (* Connect all orthogonally adjacent lights to each other. *)
    let connect board =  
    	let get_adjacent row col = 
    		let possibilities = [(row, col-1); (row, col+1); (row-1, col); 
    			(row+1, col)] in
    		List.filter (fun (r, c) -> valid_location r c) possibilities
    	in

    	for row = 0 to 4 do
    		for col = 0 to 4 do
    			List.iter 
    			(fun (r, c) -> (board row col)#add_neighbor (board r c)) 
    			(get_adjacent row col)
    		done
    	done
    in
  
    (* Initialize the board given an array of arrays of on/off values 
       by setting the corresponding lights to those values. *)
    let initialize_board board init = 
    	let rows = Array.length init in
    	let cols = Array.length init.(0) in
    	if rows != 5 || cols != 5 then failwith "init not a valid size"
    	else
    		for row = 0 to 4 do
    			for col = 0 to 4 do
    				(board row col)#set_state (init.(row).(col))
    			done
    		done
    in
  
    (* Print the board states to the terminal in a readable form. *)
    let print_board board =
      begin
        for row = 0 to 4 do
          for col = 0 to 4 do
            Printf.printf "%c" 
              (match (board row col)#state with
                 | On -> '0'
                 | Off -> '.')
          done;
          newline ()
        done;
        newline ()
      end
    in
  
    (*** The message-passing board object. ***)
    let board = make_board () in
      begin
        connect board;
        object (self)
          method init values = initialize_board board values
          method peek row col = 
          	if valid_location row col then (board row col)
          	else failwith "not a valid location: can't peek"
          method play row col = 
          	if valid_location row col then (board row col)#update
          	else failwith "cannot play"
          method play_many moves = 
          	List.iter (fun (r, c) -> if valid_location r c then self#play r c) 
          		moves
           method is_clear = 
          	let boolState = ref Off in
          	begin
          		for row = 0 to 4 do
          			for col = 0 to 4 do
          				if (board row col)#state = On then boolState := On
          			done
          		done;
          		!boolState = Off
          	end

          method print = print_board board
        end
      end


(* The following is the code to interactively play the game *)

(*   let play_game init =
    let is_digit c = c >= '0' && c <= '9' in
    let ok_coords_line line =
      String.length line = 3 
        && is_digit line.[0]
        && line.[1] = ' '
        && is_digit line.[2]
    in
    let get_input () =
      let line = read_line () in
        match line with
          | "quit" -> Quit
          | _ when ok_coords_line line ->
            let row = int_of_string (Printf.sprintf "%c" line.[0]) in
            let col = int_of_string (Printf.sprintf "%c" line.[2]) in
              if valid_location row col
                then Coords (row, col)
                else failwith "invalid coordinates"
          | _ -> failwith "invalid input line"
    in
    let rec run game =
      try
        begin
          Printf.printf "Enter move (row col): ";
          match get_input () with
            | Quit -> ()
            | Coords (row, col) -> 
                begin
                  game#play row col;
                  newline ();
                  game#print;
                  if game#is_clear
                    then Printf.printf "You win!\n\n"
                    else run game
                end
        end
      with Failure msg -> (Printf.printf "ERROR: %s\n\n" msg; run game)
    in
    let game = make_game () in
      begin
        game#init init;
        newline ();
        game#print;
        run game
      end
  
  (* Example interactive usage: *)
  
  let init1 = 
      [| 
         [| On;  On;  On;  Off; Off |];
         [| Off; Off; Off; On;  Off |];
         [| On;  On;  Off; On;  On  |];
         [| Off; Off; Off; On;  Off |];
         [| On;  On;  On;  Off; Off |]
      |] ;;
  
  play_game init1 ; *)
