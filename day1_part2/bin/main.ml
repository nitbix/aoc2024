open Core
open Base

let () = 
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in
  let left_list, right_list = List.fold ~init:([], []) file_lines ~f:(fun (left, right) x -> 
    let regex = Re.compile (Re.rep1 Re.space) in
    let matches =  Re.split regex x in
    match matches with
    | [left_num; right_num] ->
      (Int.of_string(left_num) :: left, Int.of_string(right_num) :: right)
    | [] -> (left, right)
    | _ -> let _ = Stdio.print_endline ("weird match: " ^ Int.to_string (List.length matches)) in (left, right)
  ) in
  let left_list = List.sort ~compare:Int.compare left_list in
  let right_list = List.sort ~compare:Int.compare right_list in
  let counts = List.fold ~init:(Map.empty (module Int)) right_list ~f:(
    fun map x ->
      match Map.find map x with
      | Some count -> (Map.set map ~key:x ~data:(count + 1))
      | None -> (Map.set map ~key:x ~data:1)
  ) in
  let score = List.fold ~init:0 left_list ~f:(
    fun acc x ->
      match Map.find counts x with
      | Some count -> if count > 0 then acc + x * count else acc
      | None -> acc
  ) in
  Stdio.print_endline (Int.to_string score)