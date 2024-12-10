open Core
open Base

let () =
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in
  let commands = List.fold file_lines ~init:[] ~f:(fun acc x -> acc @ (String.split x ~on:'m')) in
  let commands = List.fold commands ~init:[] ~f:(fun acc x -> acc @ (String.split x ~on:'d')) in
  let sum, _  = List.fold commands ~init:(0, true) ~f:(fun (acc, enabled) line -> 
    let do_regex = Re.compile (Re.Posix.re "^o\\(\\)") in
    let dont_regex = Re.compile (Re.Posix.re "^on't\\(\\)") in
    let enabled = 
      match Re.exec_opt do_regex line with
      | Some x -> 
        Stdio.print_endline(line);
        (Array.length (Re.Group.all x)) > 0 || enabled
      | None -> 
        match Re.exec_opt dont_regex line with
        | Some x -> (not ((Array.length (Re.Group.all x)) > 0)) && enabled
        | None -> enabled
    in
    if not enabled then (acc, enabled) else
      let mul_regex = Re.compile (Re.Posix.re "^ul\\(([0-9]+),([0-9]+)\\)") in
      let matches = 
        match Re.exec_opt mul_regex line with
        | Some m -> 
          (* Stdio.print_endline ((Re.Group.get m 1) ^ "*" ^ (Re.Group.get m 2)); *)
          let match1 = Re.Group.get m 1 in
          let match2 = Re.Group.get m 2 in
          (Int.of_string(match1) * Int.of_string(match2))
        | None -> 0
      in
      (acc + matches, enabled)
  ) in 
  let () = Stdio.print_endline (Int.to_string sum) in
  ()
