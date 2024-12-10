open Core
open Base

let is_individual_error x y prev_diff = 
  if (y-x) * prev_diff < 0 then
    1
  else
    if Int.abs(x-y) = 0 || Int.abs(x-y) > 3 then
      1
    else
      0


let report_to_string report = 
  List.fold ~init:"" ~f:(fun acc y -> acc ^ " " ^ Int.to_string y) report

(* let count_errors report =
  let rec count_errors_helper report prev_diff = 
    match report with
    | [] -> 0
    | _x :: [] -> 0
    | x :: y :: rest ->
      is_individual_error x y prev_diff + count_errors_helper (y :: rest) (y-x)
  in
  (* Stdio.print_endline (Int.to_string (count_errors_helper report 0)); *)
  count_errors_helper report 0 *)
 
  let count_errors2 report =
    let rec count_errors_helper report prev_diff = 
      match report with
      | [] -> 0
      | _x :: [] -> 0
      | x :: y :: rest ->
        is_individual_error x y prev_diff + count_errors_helper (y :: rest) (y-x)
    in
    (* Stdio.print_endline (Int.to_string (count_errors_helper report 0)); *)
    Stdio.print_endline (report_to_string report ^ " -> " ^ Int.to_string (count_errors_helper report 0));
    count_errors_helper report 0  

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
    (* let safe_reports = List.filter reports ~f:(fun x -> count_errors x = 0) in *)
    (* let maybe_safe_reports = List.filter reports ~f:(fun x -> count_errors x = 1) in *)
    let safe_reports = List.filter reports ~f:(
      fun x ->
        let () = Stdio.print_endline (" = " ^ (report_to_string x)) in
        let all_sublists: int list list = List.init (List.length x) ~f:(fun i -> 
          List.take x (Int.max 0 i) @ List.drop x (Int.min (List.length x) i+1)
        ) in
        let result = List.fold all_sublists ~init:false ~f:(fun acc x -> 
          (count_errors2 x) = 0 || acc
        ) in
        Stdio.print_endline (Bool.to_string result);
        result
    ) in
    let () = Stdio.print_endline (Int.to_string (List.length safe_reports)) in
    ()
