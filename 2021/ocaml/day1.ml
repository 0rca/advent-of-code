open Core

let rec go xs acc =
      match xs with
      | x0 :: x1 :: rest ->
        if x1 > x0
        then go (x1 :: rest) (acc + 1)
        else go (x1 :: rest) acc
      | _ -> acc

let read_lines () =
  In_channel.fold_lines In_channel.stdin ~init:[] ~f:(fun acc line -> Int.of_string line :: acc)
  |> List.rev

let ans1 () = go (read_lines ()) 0

let () = printf "Answer 1: %i\n" (ans1 ())
