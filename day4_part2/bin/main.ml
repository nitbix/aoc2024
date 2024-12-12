open Core

let () =
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in
  let grid = Array.of_list_map file_lines ~f:(fun x -> String.to_array x) in
  let up_left = (-1, -1) in
  let up_right = (-1, 1) in
  let down_left = (1, -1) in
  let down_right = (1, 1) in
  let diagonals = [(up_left, down_right); (up_right, down_left)] in
  (* now we need to count how many times XMAS appears in the grid *)
  let count_x_mas_instances grid row col =
    if Char.equal grid.(row).(col) 'A' then
      let get_letter row col direction =
        let row = row + fst direction in
        let col = col + snd direction in
        if row < 0 || row >= (Array.length grid) || col < 0 || col >= Array.length grid.(0) then
          None
        else
          Some(grid.(row).(col))
      in
      let check_diagonal (direction1, direction2) = 
        let letters = (get_letter row col direction1, get_letter row col direction2) in
        match letters with
        | (Some letter1, Some letter2) -> 
            Char.equal letter1 'M' && Char.equal letter2 'S' ||
            Char.equal letter1 'S' && Char.equal letter2 'M'
        | _ -> false
      in
      let r = List.fold ~init:true diagonals ~f:(fun acc diagonal -> acc && check_diagonal diagonal) in
      if r then 1 else 0
    else
      0

    (* let check_letter grid row col letter = 
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
    List.fold ~init:0 directions ~f:(fun acc direction -> acc + (if is_x_mas grid row col direction then 1 else 0)) *)
  in
  let count_xmas = Array.foldi grid ~init:0 ~f:(fun row acc row' -> 
    acc + (Array.foldi row' ~init:0 ~f:(fun col acc' _col' -> acc' + (count_x_mas_instances grid row col)))
  ) in
  Stdio.print_endline (Int.to_string count_xmas)