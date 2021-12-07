open Core

let read_integers () =
    In_channel.input_line_exn In_channel.stdin
    |> String.split ~on:','
    |> List.map ~f:Int.of_string

let init input =
  let arr = ref [| 0; 0; 0; 0; 0; 0; 0; 0; 0; |] in
  List.iter input ~f:(fun i -> !arr.(i) <- !arr.(i) + 1); arr

let step arr =
  let new_fish = !arr.(0) in

  !arr.(0) <- !arr.(1);
  !arr.(1) <- !arr.(2);
  !arr.(2) <- !arr.(3);
  !arr.(3) <- !arr.(4);
  !arr.(4) <- !arr.(5);
  !arr.(5) <- !arr.(6);
  !arr.(6) <- !arr.(7) + new_fish;
  !arr.(7) <- !arr.(8);
  !arr.(8) <- new_fish;

  ()

let sum arr =
  let acc = ref 0 in
  Array.iter arr ~f:(fun x -> acc := !acc + x);
  !acc

let () =
  let input = read_integers() in
  let arr = init input in
  for _ = 1 to 80 do
    step arr;
  done;
  printf "Answer 1: %i\n" (sum !arr);

  for _ = 81 to 256 do
    step arr;
  done;
  printf "Answer 2: %i\n" (sum !arr);

