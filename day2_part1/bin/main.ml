open Core
open Base

let is_safe_pair x y prev_diff = 
  if (y-x) * prev_diff < 0 then
    false
  else
    if Int.abs(x-y) = 0 || Int.abs(x-y) > 3 then
      false
    else
      true

let is_safe report =
  let rec is_safe_helper report prev_diff = 
    match report with
    | [] -> true
    | _x :: [] -> true
    | x :: y :: rest ->
      (* Stdio.print_endline (Int.to_string x ^ " " ^ Int.to_string y ^  " " ^ Int.to_string prev_diff ^ Bool.to_string (is_safe_pair x y prev_diff)); *)
      if is_safe_pair x y prev_diff then
        is_safe_helper (y :: rest) (y-x)
      else
        false
  in
  (* Stdio.print_endline (Bool.to_string (is_safe_helper report 0)); *)
  is_safe_helper report 0

let () = 
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in
  (* List.iter file_lines ~f:(fun x -> Stdio.print_endline x); *)
  let reports = List.fold ~init:[] file_lines ~f:(fun lines x -> 
    let regex = Re.compile (Re.rep1 Re.space) in
    let matches =  Re.split regex x in
    let this_report = List.map matches ~f:(fun x -> Int.of_string x) in
    this_report :: lines
  ) in
  let safe_reports = List.filter reports ~f:is_safe in
  let () = Stdio.print_endline (Int.to_string (List.length safe_reports)) in
  ()
