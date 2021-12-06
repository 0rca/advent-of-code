use std::io::{self, BufRead};
use std::str::FromStr;

// shift all numbers towards 0, and initialize cells 6 and 8 with new spawns
fn step(state: &mut [&mut u64; 9]) {
    let new_spawns = &state[0].clone();
    for i in 0..8 {
        *state[i] = *state[i + 1];
    }
    *state[6] += new_spawns.clone();
    *state[8] = new_spawns.clone();
}

fn read_integers() -> Vec<usize> {
    let mut vec = Vec::new();

    for line in io::stdin().lock().lines() {
        for str in line.unwrap().split(',') {
            vec.push(usize::from_str(str).unwrap());
        }
    }
    vec
}

fn main() {
    let state = &mut [
        &mut 0u64, &mut 0, &mut 0, &mut 0, &mut 0, &mut 0, &mut 0, &mut 0, &mut 0,
    ];
    let input = read_integers();

    // initialize
    input.iter().for_each(|i| *state[*i] += 1);

    // simulate
    for _ in 1..=80 {
        step(state);
        // println!("{} state: {:?}", i, state);
    }

    println!("Answer 1: {}", state.iter().map(|i| **i).sum::<u64>());

    for _ in 81..=256 {
        step(state);
    }

    println!("Answer 2: {}", state.iter().map(|i| **i).sum::<u64>());
}
