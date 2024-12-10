open Core

let () = 
  (*let print_list list = 
    List.iter list ~f:(fun x -> print_endline (Int.to_string x))
  in*)
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in 
  let left_list, right_list = List.fold ~init:([], []) file_lines ~f:(fun (left, right) x -> 
    let regex = Re.compile (Re.rep1 Re.space) in
    let matches =  Re.split regex x in
    match matches with
    | [left_num; right_num] ->
      (Int.of_string(left_num) :: left, Int.of_string(right_num) :: right)
    | [] -> (left, right)
    | _ -> let _ = print_endline ("weird match: " ^ Int.to_string (List.length matches)) in (left, right)
    (*match Re.split (Re.compile (Re.str " +")) x with
    | left_num :: right_num :: _ -> (Int.of_string(left_num) :: left, Int.of_string(right_num) :: right)
    | _ -> (left, right)*)
  ) in

  let left_list = List.sort ~compare:Int.compare left_list in
  let right_list = List.sort ~compare:Int.compare right_list in
  let differences = List.fold2_exn left_list right_list ~init:[] ~f:(fun acc left right -> 
    (abs (left - right)) :: acc
  ) in
  (*let () = print_endline (List.fold differences ~init:"" ~f:(fun acc x -> (acc ^ (Int.to_string x) ^ "\n"))) in*)
  print_endline (Int.to_string (List.fold differences ~init:0 ~f:(+)))