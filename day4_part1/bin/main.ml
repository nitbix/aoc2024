open Core

let () =
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in
  let grid = Array.of_list_map file_lines ~f:(fun x -> String.to_array x) in
  let up = (-1, 0) in
  let down = (1, 0) in
  let left = (0, -1) in
  let right = (0, 1) in
  let up_left = (-1, -1) in
  let up_right = (-1, 1) in
  let down_left = (1, -1) in
  let down_right = (1, 1) in
  let directions = [up; down; left; right; up_left; up_right; down_left; down_right] in
  (* now we need to count how many times XMAS appears in the grid *)
  let count_xmas_instances grid row col =
    Stdio.print_endline ("Checking  " ^ (Int.to_string row) ^ "x" ^ (Int.to_string col) ^ " = " ^ (Char.to_string grid.(row).(col)));
    (* let rec count_xmas_instances' grid row col direction =
      let (row', col') = (row + (fst direction), col + (snd direction)) in
      if row' < 0 || row' >= (Array.length grid) || col' < 0 || col' >= Array.length grid.(0) then
        0
      else
        let xmas = Array.of_list(['X'; 'M'; 'A'; 'S']) in
        if Char.equal (grid).(row').(col') xmas.(0) then
          if Array.length xmas = 1 then
            1
          else
            Array.fold ~init:0 xmas ~f:(fun acc _x -> acc + (count_xmas_instances' grid row' col' direction))
        else
          0
    in *)
    let check_letter grid row col letter = 
      if row < 0 || row >= (Array.length grid) || col < 0 || col >= Array.length grid.(0) then
        false
      else
        Char.equal grid.(row).(col) letter
    in
    let xmas = Array.of_list(['X'; 'M'; 'A'; 'S']) in
    let is_xmas grid row col direction =
      Array.foldi xmas ~init:true ~f:(fun i acc letter -> 
        acc && (check_letter grid (row + (fst direction * i)) (col + (snd direction * i)) letter)
      )
    in
    List.fold ~init:0 directions ~f:(fun acc direction -> acc + (if is_xmas grid row col direction then 1 else 0))
  in
  let count_xmas = Array.foldi grid ~init:0 ~f:(fun row acc row' -> 
    acc + (Array.foldi row' ~init:0 ~f:(fun col acc' _col' -> acc' + (count_xmas_instances grid row col)))
  ) in
  Stdio.print_endline (Int.to_string count_xmas)