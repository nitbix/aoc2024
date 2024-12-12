open Core

let () =
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in
  let grid = Array.of_list_map file_lines ~f:(fun x -> String.to_array x) in
  let up = (-1, 0) in
  let down = (1, 0) in
  let left = (0, -1) in
  let right = (0, 1) in
 let directions = [up; down; left; right] in
  let visited = Array.make_matrix ~dimx:(Array.length grid) ~dimy:(Array.length grid.(0)) false in
  let is_valid_coords row col = row >= 0 && row < (Array.length grid) && col >= 0 && col < (Array.length grid.(0)) in
  let regions = Array.foldi grid ~init:(visited, []) ~f:(fun row acc row' -> 
    (Array.foldi row' ~init:acc ~f:(fun col (visited, acc') _col' ->
      if not (is_valid_coords 0 0) then
        (visited, acc')
      else
      if visited.(row).(col) then
        (visited, acc')
      else
        let current_letter = grid.(row).(col) in
        (* current_region is (area, perimeter) *)
        let rec grow_region visited current_region row col current_letter=
          if not (is_valid_coords row col) then
            (fst current_region, snd current_region + 1)
          else
            let letter = grid.(row).(col) in
            if visited.(row).(col) then
              if Char.equal letter current_letter then
                current_region
              else
                (fst current_region, snd current_region + 1)
            else
              if Char.equal letter current_letter then
                let () = visited.(row).(col) <- true in
                let current_region = (fst current_region + 1, snd current_region) in
                List.fold ~init:current_region directions ~f:(fun acc direction -> 
                  let r' = (grow_region visited acc (row + (fst direction)) (col + (snd direction)) current_letter) in
                  r'
                )
              else
                (fst current_region, snd current_region + 1)
        in
        let region = grow_region visited (0,0) row col current_letter in
        (visited, (region :: acc'))
    )
  )) in
  let total_cost = List.fold (snd regions) ~init:0 ~f:(fun acc region -> acc + (fst region) * (snd region)) in
  List.iter (snd regions) ~f:(fun region -> Stdio.print_endline ((Int.to_string (fst region)) ^ "x" ^ (Int.to_string (snd region))));
  Stdio.print_endline (Int.to_string total_cost)