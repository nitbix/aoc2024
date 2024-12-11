open Core
open Base

(* THIS IS THE OLD ATTEMPT WITH A TREE TRAVERSAL *)
(* type node = 
  | Node of int

let rec blink_count levels stones cache =
  let add_to_cache key value l cache =
    let subcache = 
      match Hashtbl.find cache value with
      | Some subcache -> subcache
      | None -> Hashtbl.create (module Int)
    in
    Hashtbl.set subcache ~key:l ~data:value;
    Hashtbl.set cache ~key:key ~data:subcache
  in
  match levels with
  | 0 -> List.length stones
  | _ -> 
    (
    let calc_stone stone = 
      (
        match stone with
        | Node (value) ->
          let r = (
            let cached_value =
              match (Hashtbl.find cache value) with
              | None -> None
              | Some subcache ->
                match (Hashtbl.find subcache levels) with
                | None -> None
                | Some cached_value -> Some cached_value
            in
            match cached_value with
            | Some c -> c
            | None ->
              if value = 0 then
                (blink_count (levels - 1) [Node (1)] cache)
              else
                let str_stone = Int.to_string value in
                let strlen = String.length (str_stone) in
                if strlen % 2 = 0 then
                  let left = String.prefix str_stone (strlen / 2) in
                  let right = String.suffix str_stone (strlen / 2) in
                  (blink_count (levels - 1) [
                    Node (Int.of_string left);
                    Node (Int.of_string right)
                  ] cache)
                else
                  (blink_count (levels - 1) [Node (value * 2024)] cache)
          ) in
          add_to_cache value r levels cache;
          r
      )
    in  
    match stones with
    | [] -> 0
    | [stone] -> calc_stone stone
    | stone :: siblings -> calc_stone stone + (blink_count levels siblings cache)
    )  *)

let add_stones stone n freqs =
  match Hashtbl.find freqs stone with
  | Some count -> Hashtbl.set freqs ~key:stone ~data:(count + n)
  | None -> Hashtbl.set freqs ~key:stone ~data:n
let blink_once freqs =
  Hashtbl.fold freqs ~init:(Hashtbl.create (module Int)) ~f:(fun ~key ~data acc -> 
    let () = 
    if key = 0 then
      add_stones 1 data acc
    else
      let str_stone = Int.to_string key in
      let strlen = String.length (str_stone) in
      if strlen % 2 = 0 then
        let left = String.prefix str_stone (strlen / 2) in
        let right = String.suffix str_stone (strlen / 2) in
        add_stones (Int.of_string left) data acc;
        add_stones (Int.of_string right) data acc
      else
        add_stones (key * 2024) data acc
    in
    acc
  )

let () =
  let filename = "input" in
  let file_lines  = In_channel.read_lines filename in
  match file_lines with
  | [] -> Stdio.print_endline "No lines in file"
  | first_line :: _ ->
    let regex = Re.compile (Re.rep1 Re.space) in
    let matches = Re.split regex first_line in
    let stones = List.fold ~init:(Hashtbl.create (module Int)) matches ~f:(fun acc s -> 
      let v = Int.of_string s in
      add_stones v 1 acc;
      acc
    ) in
    let levels = 75 in
    let final_stones = List.fold (List.range 0 levels) ~init:stones ~f:(fun acc _ -> blink_once acc) in
    let stone_count = Hashtbl.fold final_stones ~init:0 ~f:(fun ~key ~data acc -> let _ = key in  acc + data) in
    Stdio.print_endline (Int.to_string stone_count)
    

