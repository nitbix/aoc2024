open Core

let () =
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in
  let up = (-1, 0) in
  let down = (1, 0) in
  let left = (0, -1) in
  let right = (0, 1) in
  let directions = [up; down; left; right] in
  let grid = Array.of_list_map file_lines ~f:(fun x -> String.to_array x) in
  let coloured_grid = Array.make_matrix ~dimx:(Array.length grid) ~dimy:(Array.length grid.(0)) 0 in
  let visited = Array.make_matrix ~dimx:(Array.length grid) ~dimy:(Array.length grid.(0)) false in
  let colours = Array.foldi grid ~init:1 ~f:(fun row acc row' -> 
    Array.foldi row' ~init:acc ~f:(fun col colour _col' -> 
      let rec recolour row col letter = 
        if row < 0 || row >= (Array.length grid) || col < 0 || col >= (Array.length grid.(0)) || visited.(row).(col) then
          colour
        else
          let new_colour = 
            if Char.equal grid.(row).(col) letter then
              let () = coloured_grid.(row).(col) <- colour in
              let () = visited.(row).(col) <- true in
              List.fold ~init:colour [down;right;left] ~f:(fun acc direction -> 
                Int.max acc (recolour (row + (fst direction)) (col + (snd direction)) letter)
              )
            else
              colour + 1;
          in
          Int.max new_colour (colour + 1)
      in
      recolour row col grid.(row).(col)
  ))
  in
  let () = Stdio.print_endline ("Found " ^ (Int.to_string colours) ^ " colours") in
  (* print the coloured grid *)
  let () = Array.iter coloured_grid ~f:(fun row -> 
    Array.iter row ~f:(fun col -> 
      Stdio.print_string (Int.to_string col)
    );
    Stdio.print_endline ""
  ) in
  let grid = coloured_grid in
  let visited = Array.make_matrix ~dimx:(Array.length grid) ~dimy:(Array.length grid.(0)) false in
  let is_valid_coords row col = row >= 0 && row < (Array.length grid) && col >= 0 && col < (Array.length grid.(0)) in
  let regions =
    Array.foldi grid ~init:(visited, []) ~f:(fun row acc row' -> 
    (Array.foldi row' ~init:acc ~f:(fun col (visited, acc') _col' ->
      let seen_corners = ref ([] : (int * int) list) in
      if not (is_valid_coords 0 0) then
        (visited, acc')
      else
      if visited.(row).(col) then
        (visited, acc')
      else
        let current_letter = grid.(row).(col) in
        Stdio.print_endline ("Group: " ^ (Int.to_string current_letter));
        (* current_region is (area, perimeter) *)
        let rec grow_region current_region current_direction row col current_letter =
          let from_row = row in
          let from_col = col in
          let row = row + (fst current_direction) in
          let col = col + (snd current_direction) in
          let tuple_equal = Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal in
          let get_border_value inside check_against seen_corners = 
            let check_letter valid check_against row col letter =
              if not (is_valid_coords row col) then
                valid
              else
                if Int.equal grid.(row).(col) letter
                then
                  0
                else
                  match check_against with
                  | Some value ->
                    let () = Stdio.print_endline ("Checking against: " ^ (Int.to_string value)) in
                    if Int.equal grid.(row).(col) value then
                      1
                    else
                      0
                  | None -> 1
            in
            let corner_count valid check_against row col =
              (* Stdio.print_endline ("Checking corner at: " ^ (Int.to_string row) ^ " " ^ (Int.to_string col)); *)
              if not (is_valid_coords row col) ||
                List.exists !seen_corners ~f:(fun x -> tuple_equal x (row,col))
              then
                0
              else
                let corner_letter = grid.(row).(col) in
                let up = (check_letter valid check_against (row - 1) col corner_letter) in
                let down = (check_letter valid check_against (row + 1) col corner_letter) in
                let left = (check_letter valid check_against row (col - 1) corner_letter) in
                let right = (check_letter valid check_against row (col + 1) corner_letter) in
                Stdio.print_endline ("Checking corner at: " ^ (Int.to_string row) ^ " " ^ (Int.to_string col) ^ " " ^ (Int.to_string up) ^ " " ^ (Int.to_string down) ^ " " ^ (Int.to_string left) ^ " " ^ (Int.to_string right));
                let corner_sum = up + down + left + right in
                (* let () = Stdio.print_endline ("Corner sum: " ^ (Int.to_string corner_sum)) in *)
                let print_seen () = 
                  List.iter !seen_corners ~f:(fun x -> Stdio.print_endline ((Int.to_string (fst x)) ^ " " ^ (Int.to_string (snd x))))
                in
                let corner_count = 
                  if up = down && down <> left && left = right
                  then
                    0
                  else
                  if corner_sum < 2 then 0
                  else 
                    if corner_sum = 4 then 4 
                    else 
                      if valid = 1 then
                        corner_sum - 1
                      else
                        let () = print_seen () in
                        if
                          (
                            (List.exists !seen_corners ~f:(fun x -> tuple_equal x (row - 1, col)) &&
                             List.exists !seen_corners ~f:(fun x -> tuple_equal x (row, col - 1)) &&
                             List.exists !seen_corners ~f:(fun x -> tuple_equal x (row - 1, col - 1))) ||
                             
                            (List.exists !seen_corners ~f:(fun x -> tuple_equal x (row + 1, col)) &&
                             List.exists !seen_corners ~f:(fun x -> tuple_equal x (row + 1, col + 1)) &&
                             List.exists !seen_corners ~f:(fun x -> tuple_equal x (row, col + 1))) ||

                            (List.exists !seen_corners ~f:(fun x -> tuple_equal x (row + 1, col)) &&
                             List.exists !seen_corners ~f:(fun x -> tuple_equal x (row + 1, col - 1)) &&
                             List.exists !seen_corners ~f:(fun x -> tuple_equal x (row, col - 1)) )||

                            (List.exists !seen_corners ~f:(fun x -> tuple_equal x (row -1, col)) &&
                             List.exists !seen_corners ~f:(fun x -> tuple_equal x (row - 1, col + 1)) &&
                             List.exists !seen_corners ~f:(fun x -> tuple_equal x (row, col + 1)))
                          )
                          then
                            let ()  =
                              if (col = 1 && row = 4) then
                                Stdio.print_endline ("Triple cross at: " ^ (Int.to_string row) ^ " " ^ (Int.to_string col))
                            in
                            0
                        else
                          let ()  =
                          if (col = 1 && row = 4) then
                            Stdio.print_endline ("count: " ^ (Int.to_string (corner_sum - 1)));
                          in
                          corner_sum - 1
                in
                let () = if corner_count > 0 then
                  let () = Stdio.print_endline ("Corner at: " ^ (Int.to_string row) ^ " " ^ (Int.to_string col) ^ " " ^ (Int.to_string corner_count)) in
                  let () = seen_corners := (row, col) :: !seen_corners in
                  ()
                in
                corner_count
            in
            corner_count inside check_against row col
          in
          if not (is_valid_coords row col) then
            current_region
          else
            let letter = grid.(row).(col) in
            if visited.(row).(col) then
              if Int.equal letter current_letter then
                (fst current_region, snd current_region + get_border_value 1 None seen_corners)
              else
                (fst current_region, snd current_region + get_border_value 0 (Some grid.(from_row).(from_col)) seen_corners)
            else
              if Int.equal letter current_letter then
                let () = visited.(row).(col) <- true in
                let current_region = (fst current_region + 1, snd current_region + get_border_value 1 None seen_corners) in
                List.fold ~init:current_region directions ~f:(fun acc direction -> 
                  (grow_region acc direction row col current_letter)
                )
              else
                (* let () = Stdio.print_endline ("Different letter: " ^ (Char.to_string letter) ^ " " ^ (Int.to_string row) ^ " " ^ (Int.to_string col)) in *)
                (fst current_region, 
                snd current_region + 
                get_border_value 0 (Some grid.(from_row).(from_col)) seen_corners)
        in
        let region = grow_region (0,0) (0,0) row col current_letter in
        if (fst region) = 0 then
          (visited, acc')
        else
          let () = Stdio.print_endline ("Region " ^ Int.to_string current_letter ^ " :" ^ (Int.to_string row) ^ " " ^ (Int.to_string col) ^ " " ^ (Int.to_string (fst region)) ^ " " ^ (Int.to_string (snd region))) in
          (visited, (region :: acc'))
    )
  )) in
  let total_cost = List.fold (snd regions) ~init:0 ~f:(fun acc region -> acc + (fst region) * (snd region)) in
  (* List.iter (snd regions) ~f:(fun region -> Stdio.print_endline ((Int.to_string (fst region)) ^ "x" ^ (Int.to_string (snd region)))); *)
  Stdio.print_endline (Int.to_string total_cost)