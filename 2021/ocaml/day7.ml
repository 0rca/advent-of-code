open Core

let read_integers () =
    In_channel.input_line_exn In_channel.stdin
    |> String.split ~on:','
    |> List.map ~f:Int.of_string

let simple_fuel_metric x y = abs (x - y)

let progressive_fuel_metric x y =
    let x0 = min x y in
    let x1 = max x y in
    (1 + x1 - x0) * (x1 - x0) / 2

let minimal_fuel xs ~metric:fuel_metric =
    let sums = List.mapi xs ~f:(fun i _ ->
        List.fold xs ~init:0 ~f:(fun acc x -> acc + fuel_metric i x)) in
    match List.min_elt sums ~compare:(fun x y -> x - y) with
    | Some(x) -> x
    | None -> 0

let () =
    let ints = read_integers () in
    let an1 = minimal_fuel ~metric:simple_fuel_metric ints in
    let an2 = minimal_fuel ~metric:progressive_fuel_metric ints in
    printf "Answer 1: %i\nAnswer 2: %i" an1 an2
