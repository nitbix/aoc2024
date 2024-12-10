open Core
open Base

let () =
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in
  let commands = List.fold file_lines ~init:[] ~f:(fun acc x -> acc @ (String.split x ~on:'m')) in
  let sum = List.fold commands ~init:0 ~f:(fun acc x -> 
    (* regex to find all instances of "sum(xx,xx)" *)
    let regex = Re.compile (Re.Posix.re "ul\\(([0-9]+),([0-9]+)\\)") in
    let matches = 
      match Re.exec_opt regex x with
      | Some m -> 
        Stdio.print_endline ((Re.Group.get m 1) ^ "*" ^ (Re.Group.get m 2));
        let match1 = Re.Group.get m 1 in
        let match2 = Re.Group.get m 2 in
        (Int.of_string(match1) * Int.of_string(match2))
      | None -> 0
    in
    acc + matches
  ) in 
  let () = Stdio.print_endline (Int.to_string sum) in
  ()
