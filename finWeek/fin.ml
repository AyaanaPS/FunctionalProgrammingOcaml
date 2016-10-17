open Printf         (* for printf and sprintf functions *)
open Final_helpers  (* for utility types/functions/values *)

(* ---------------------------------------------------------
 * Piece type.
 * This could be inferred, but defining it explicitly
 * makes the .mli file much easier to understand.
 * --------------------------------------------------------- *)

type piece =
  < 
    name     : string; 
    len      : len;
    dir      : dir;
    loc      : loc; 
    locs     : loc list; 
    print    : unit;
    if_place : loc -> loc list option; 
    place    : loc -> unit; 
    if_move  : int -> (loc list * loc list * loc list) option;
    move     : int -> unit;
    possible_moves : int list
  >

(* ---------------------------------------------------------
 * Piece object.
 * --------------------------------------------------------- *)

let make_piece _name _len _dir =
  (* -------------------------------------------------------
   * Helper functions.
   * ------------------------------------------------------- *)

  (* Print a representation of the piece to the terminal.
   * For debugging only. *)
  let print_piece locs =
    begin
      printf "PIECE[ name: %s len: %d dir: %s\n"
        _name (int_of_len _len) (string_of_dir _dir);
      printf "       locs: %s ]\n" (string_of_locs locs)
    end
  in



  (* 
   * Return a list (option) of the locations in a line starting from the
   * location "loc" and going "len" spaces in direction "dir".
   * If any such location location would be off the board, return None.
   *)

  let get_locs_in_line loc len dir =
    let options = [] in
    let rec appendToList loc lenInt dir =
      let (row, col) = loc in
        match (lenInt, dir) with
          | (0, _) -> []
          | (_, H) -> 
            if valid_loc (row, col+1) then 
              (options @ (loc :: (appendToList (row, col+1) (lenInt-1) dir)))
            else options @ (loc :: [])
          | (_, V) -> 
            if valid_loc (row+1, col) then 
              (options @ (loc :: (appendToList (row+1, col) (lenInt-1) dir)))
            else options @ (loc :: [])
    in
    let result = appendToList loc (int_of_len len) dir in
    match result with
    | _ when List.length result = (int_of_len len) -> Some result
    | _ -> None

  in

  (*
   * Move the locations n places over in the given direction and return
   * a list (option) of the new locations.  Return None if the locations
   * can't be moved because they would go off the edge of the board.
   *)

  let move_locs locs dir n =
    let rec hUpdate locs n =
      match locs with
        | [] -> []
        | ((x, y) :: t) -> if valid_loc (x, y+n) then 
          ((x, y+n) :: (hUpdate t n))
          else []
      in
    let rec vUpdate locs n =
      match locs with
        | [] -> []
        | ((x, y) :: t) -> if valid_loc (x+n, y) then (x+n, y) :: vUpdate t n
          else []
      in
    match locs with
    | None -> None
    | Some x -> 
        match dir with
          | H -> 
            begin
              let result = hUpdate x n in
              match result with
              | _ when List.length result = List.length x -> Some result
              | _ -> None
            end
          | V -> 
            begin
              let result = vUpdate x n in
              match result with
              | _ when List.length result = List.length x -> Some result
              | _ -> None
            end
  in

  (* 
   * Returns a list of all the columns given a list of location tuples
   *)
  let getCols lst =
    let columns = [] in
    List.flatten (List.map (fun (r, c) -> c :: columns) lst)

  in

  (* 
   * Returns a list of all the rows given a list of location tuples
   *)
  let getRows lst = 
    let rows = [] in
    List.flatten (List.map (fun (r, c) -> r :: rows) lst)

  in

  (*
   * Given a (non-empty) list of locations, a direction and a distance, 
   *   return a tuple (option) of:
   * (a) the newly-vacated locations when you move the locations n places over 
   *     in the  given direction;
   * (b) the vacant locations passed over by the piece during the move
   * (c) the newly-occupied locations when you move the locations n places over 
   *     in the given direction
   * OR: None if the move would be invalid on an empty board 
   *     (because it would go off an end of the board).
   *)
  let if_move_locs locs dir n =
    let computePassedOver locA movedLocs =
      let passedOver = [] in
      match dir with
      | H -> 
        let (row, random) = List.hd locA in
        let realLocs = getCols locA in
        let realMoved = getCols movedLocs in
        let passedOverCols = betweens realLocs realMoved in
        List.flatten (List.map (fun x -> let new_loc = (row, x) in
          new_loc :: passedOver) passedOverCols)
      | V ->
        let (random, col) = List.hd locA in
        let realLocs = getRows locA in
        let realMoved = getRows movedLocs in
        let passedOverRows = betweens realLocs realMoved in
        List.flatten (List.map (fun x -> passedOver @ ((x, col) :: [])) 
                          passedOverRows)
    in    
    let movedLocations = move_locs locs dir n in
    match locs, movedLocations with
    | None, _ -> None
    | _, None -> None
    | Some x, Some y -> 
      let newVacant = difference x y in
      let newLocation = difference y x in
      let passedOver = computePassedOver x y in
      Some (newVacant, passedOver, newLocation)
  in

  (* 
   * Given a list of locations and a direction, compute all the possible 
   * integer displacements that would constitute a move on an empty board.
   * Assume the list of locations is valid i.e. it represents the locations
   * of a piece (contiguous in the horizontal or vertical direction).
   * Note that 0 is not a valid move, since a move must change the position
   * of the piece.
   *)
  let get_possible_moves locs dir =
    let options = [-5; -4; -3; -2; -1; 1; 2; 3; 4; 5] in
    let possible_moves = ref [] in
    begin
      for i = 0 to ((List.length options) - 1) do
        let possibilities = move_locs locs dir (List.nth options i) in
        match possibilities with
          | Some x -> possible_moves := (List.nth options i) :: !possible_moves
          | None -> possible_moves := !possible_moves;
      done;
      !possible_moves
    end
  in
     
  (* 
   * The state variable _loc stores the first location of the piece:
   * the leftmost location for a horizontally-oriented piece or the
   * topmost location for a vertically-oriented piece.
   *)
  let _loc = ref (0, 0) in
    object (self)
      method name  = _name
      method len   = _len
      method dir   = _dir
      method loc   = !_loc

      (* 
       * The locs method returns a list of all the board locations occupied by 
       * the piece. If any locations would be off the board, it returns an 
       * empty list 
       *)
      method locs  = 
        let locsOpt = get_locs_in_line self#loc self#len self#dir in
        match locsOpt with
        | Some x -> x
        | None -> []

      method print = print_piece self#locs

      (* 
       * This method takes a location as an argument and returns a list (option)
       * of the location the piece would occupy if it was placed at that 
       * location. If it cannot be placed there, the method returns None. 
       *)
      method if_place loc =
        if valid_loc loc then
          begin
            let curLocs = get_locs_in_line loc self#len self#dir in
            match curLocs with
              | None -> None
              | Some x -> curLocs
          end
        else invalid_arg "piece: if_place: invalid location"

      (* 
       * This method takes a location as an argument and sets the internal
       * location state variable to the loc with no error checking.
       *) 
      method place new_loc =
        _loc := new_loc

      (* 
       * This method takes a distance as an argument, which represents how
       * many locations to move the piece along its direction of motion. 
       * This returns a tuple of:
       *    a list of the newly vacated locations if the move was made
       *    a list of the vacant locations passed over in the move
       *    a list of the newly occupied locations when the move is made
       *)
      method if_move n =
        if n != 0 then
          begin
            let curLocs = get_locs_in_line self#loc self#len self#dir in
            let moved = move_locs curLocs self#dir n in
            match moved with
              | None -> None
              | Some x -> if_move_locs curLocs self#dir n
          end
        else invalid_arg "piece: if_move: invalid move: 0"

      (* 
       * This method takes a distance argument and changes the internal
       * location state variable to reflect the move.
       *)
      method move n =
        let (row, col) = self#loc in
          match self#dir with
            | H -> _loc := (row, col+n)
            | V -> _loc := (row+n, col)

      (* 
       * This returns all the possible legal moves the piece can make in 
       * its direction of motion, assuming an empty board.
       *)
      method possible_moves = 
        let curLocs = get_locs_in_line self#loc self#len self#dir in
        let possibilities = get_possible_moves curLocs self#dir in
        possibilities
    end


(* ---------------------------------------------------------
 * Board object.
 * --------------------------------------------------------- *)

let make_board () =
  let pieces = Hashtbl.create 25 in              (* piece name -> piece *)
  let piece_at_location = Hashtbl.create 36 in   (* location -> piece name *)

  (*
   * Helper functions.
   *)

  (* 
   * Convert the board contents to a list of lists of strings representation,
   * for debugging purposes.
   *)
  let board_to_list () =
    let result = ref [] in
    begin
      for i = 0 to 5 do
        begin
          let inLst = ref [] in
          for j = 0 to 5 do 
            let pieceName = 
              try
                Hashtbl.find piece_at_location (i, j)
              with 
                Not_found -> "_"
             in
            inLst := pieceName :: !inLst
          done;
          result := (List.rev !inLst) :: !result
        end
      done;
      List.rev (!result)
    end
  in

  (* Clear the hash tables. *)
  let clear_board () =
    begin
      Hashtbl.clear pieces;
      Hashtbl.clear piece_at_location
    end
  in 

  (* Print a representation of the board. *)
  let print_board () =
    begin
      printf "\n     0 1 2 3 4 5\n";
      printf "   +-------------+\n";
      List.iter
        (fun r ->  (* row *)
           begin
             printf " %d | " r;
             List.iter
               (fun c ->  (* column *)
                  let s = 
                    try
                      Hashtbl.find piece_at_location (r, c)
                    with 
                      Not_found -> "_"
                  in printf "%s " s)
               (range 0 board_size);
             if r = 2   (* row 2: where the blue block exits *)
               then printf "\n"  (* leave a "space" *)
               else printf "|\n"
           end)
        (range 0 board_size);
      printf "   +-------------+\n\n"
    end
  in

  (* 
   * Return true if the game has been won i.e. if the blue piece 
   * (piece "@") exists on the board and is positioned at locations 
   * (2, 4) and (2, 5).
   *)
  let board_is_won_game () =
    let winLoc1P = 
    try
      Hashtbl.find piece_at_location (2, 4)
    with
      Not_found -> "_" in
    let winLoc2P =
    try
      Hashtbl.find piece_at_location (2, 5) 
    with
      Not_found -> "_" in
    if winLoc1P = winLoc2P && winLoc1P = "@" then true
    else false
  in

  (* Return true if a location is vacant. *)
  let is_vacant loc =
    let valAtLoc = 
      try
        Hashtbl.find piece_at_location loc
      with
        Not_found -> "_" in
    if valAtLoc = "_" then true
    else false
  in

  (* Vacate a location on the board by adjusting the hash table contents. *)
  let vacate loc = 
     if Hashtbl.mem piece_at_location loc then
      Hashtbl.replace piece_at_location loc "_"
  in
    
   (*
    * Add a binding between a location and a piece by adjusting the 
    * hash table contents.
    *)
  let occupy name loc = 
    if Hashtbl.mem piece_at_location loc then
      Hashtbl.replace piece_at_location loc name
    else Hashtbl.add piece_at_location loc name
  in
    
  (*
   * Make a piece with attributes (name, len, dir) and place it
   * on the board.
   *)
  let board_place_piece name len dir loc =
    let newPiece = make_piece name len dir in
    let locations = newPiece#if_place loc in
    match locations with
    | None -> failwith "board: cannot place piece at inputted location"
    | Some x ->
      begin
        if List.for_all is_vacant x then
        for i = 0 to (List.length x) - 1 do
          let curLoc = List.nth x i in
          occupy name curLoc
        done
        else failwith "board: cannot place piece at inputted location";
        newPiece#place loc;
        Hashtbl.add pieces name newPiece
      end
    
  in

  (*
   * Move a piece (represented by the piece name) on the board 
   * by the specified distance.
   *)

  (* Use if_move to get tuple of newly vacated, passedOver and newly
   * occupied. Check to see iresuf the newly occupied and passedOver locations
   * are vacant. If they are, vacate the newly vacated, and occupy the
   * newly occupied *)
  let board_move_piece name distance =
    let curPiece = Hashtbl.find pieces name in
    let result = (curPiece#if_move distance) in
    match result with
    | None -> failwith "board: move_piece: invalid move"
    | Some (x, y, z) -> 
      let (newlyVacated, passedOver, newlyOcc) = (x, y, z) in
      if List.for_all is_vacant passedOver && List.for_all is_vacant newlyOcc
        then 
          begin
            List.iter (fun x -> vacate x) newlyVacated;
            List.iter (fun x -> (occupy name x)) newlyOcc;
            curPiece#move distance
          end
      else failwith "board: move_piece: invalid move"
  in

  (*
   * Return a list of all the possible moves by a particular piece
   * (represented as the piece name).  Assumes the piece is on the board.
   *)
  let get_all_moves name =
    let possibilities = ref [] in
    let curPiece = Hashtbl.find pieces name in
    let initPos = curPiece#possible_moves in 
    begin
      for i = 0 to ((List.length initPos) - 1) do
        let distance = List.nth initPos i in
        let result = curPiece#if_move distance in
        match result with
        | None -> failwith "board: get_all_piece_moves: error"
        | Some (x, y, z) ->
          let (newlyVacated, passedOver, newlyOcc) = (x, y, z) in
          if (List.for_all is_vacant passedOver) && 
            (List.for_all is_vacant newlyOcc) 
            then possibilities := (name, distance) :: !possibilities
          else possibilities := !possibilities
      done;
      !possibilities
    end
  in

  (*
   * Initialize the board using a list of lists of dimensions 6x6.
   * This assumes that the blue piece is on the correct row.
   * Supplied to the students.
   *)
  let board_initialize_from_list lst =
    (*
     * Return a list of all the (putative) pieces in a list of strings,
     * where each piece is represented as: (name, length, starting_position).
     * "starting_position" is the index of the leftmost location in the list.
     *)
     let get_pieces lst =
       let rec iter lst index results =
         match lst with 
           | [] -> results
           | h :: t ->
               let p = takeWhile (fun s -> s = h) lst in
               let len = List.length p in
                 if len = 1   (* has to be at least 1 *)
                   then iter t (index + 1) results
                   else iter (drop len lst) 
                             (index + len) 
                             ((h, len, index) :: results)
       in iter lst 0 []
     in

     (* 
      * Place all the horizontal or vertical pieces onto the board.
      * loc_maker is a function which takes the row or column coordinate 
      * extracted from the get_pieces call (coord) and converts it into 
      * a location by adding the other coordinate.
      *)
     let place_line lst dir loc_maker =
       List.iter
         (fun (name, len, coord) ->
            (* place the piece onto the board unless the name is "_" *)
            match () with
              | _ when valid_piece_name name && valid_piece_length len ->
                  let loc = loc_maker coord in           (* make the location *)
                  let len' = len_of_int len in
                    board_place_piece name len' dir loc  (* place the piece   *)
              | _ when name = "_" -> ()  (* blank space; do nothing *)
              | _ -> failwith 
                       ("board: initialize_from_list: "
                        ^ "invalid piece name or length"))
         (get_pieces lst)
     in

     (* Place all the horizontal pieces onto the board. *)
     let place_horizontal_pieces lst =
       List.iter
         (fun row ->
           place_line (List.nth lst row) H (fun col -> (row, col)))
         (range 0 board_size)
     in

     (* Place all the vertical pieces onto the board. *)
     let place_vertical_pieces lst =
       let lst' = transpose lst in
       List.iter
         (fun col ->
           place_line (List.nth lst' col) V (fun row -> (row, col)))
         (range 0 board_size)
     in

     (* 
      * The initializing list argument must be a list of lists of 
      * dimensions 6x6. Each sublist must contain only valid piece 
      * names, or "_" for empty locations.
      *)
     let valid_initializing_list lst =
       (List.length lst = board_size)
       &&
       (List.for_all (fun l -> List.length l = board_size) lst)
       &&
       (List.for_all
         (fun s -> valid_piece_name s || s = "_")
         (List.flatten lst))
     in

     begin
       clear_board ();
       if not (valid_initializing_list lst)
         then failwith "invalid initializing list"
         else
           begin
             place_horizontal_pieces lst;
             place_vertical_pieces lst
           end
     end
  in

  object (self)
    (* For testing/debugging only. *)
    method pieces_table = Hashtbl.copy pieces

    (* For testing/debugging only. *)
    method piece_at_location_table = Hashtbl.copy piece_at_location

    (* For testing/debugging only. *)
    method to_list = board_to_list ()

    (* This clears both internal tables *)
    method clear = clear_board ()

    (* This prints the state of the board in a human-readable form *)
    method print = print_board ()

    (* This returns true if the board is in a winning configuration. *)
    method won_game = board_is_won_game ()

    (* 
     * This method makes and places the piece with the inputted name, len and
     * dir at the inputted location, if possible.
     *)
    method place_piece name len dir loc = 
      if valid_piece_name name && valid_loc loc then
        (board_place_piece name len dir loc)
      else invalid_arg "board: place_piece: bad inputs"

    (* 
     * This method moves the desired piece (specified by the inputted name) by
     * the inputted distance if possible.
     *)
    method move_piece name distance = 
      if valid_piece_name name && distance != 0 then
        board_move_piece name distance
      else failwith "board: move_piece: bad inputs"

    (*
     * This method returns a list of all the possible piece moves on the board.
     *)
    method get_all_piece_moves =
      let all_possible = ref [] in
      begin
        let all_pieces = hash_keys pieces in
        for j = 0 to ((List.length all_pieces) - 1) do
          let curName = List.nth all_pieces j in
          all_possible := (get_all_moves curName) :: !all_possible
        done;
        List.flatten !all_possible
      end
    
    (* 
     * This method initializes the board based on the inputted list.
     *)
    method initialize_from_list lst = 
      board_initialize_from_list lst
  end
