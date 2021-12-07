use std::io::{self, BufRead};
use std::str::FromStr;

// Take I : straightforward array copying

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

// Take II: No copying, memory golf

#[derive(Debug)]
struct State {
    array: [u64; 9],
    p0: u8,
    p6: u8,
    p8: u8,
}

impl State {
    fn new() -> Self {
        State {
            array: [0, 0, 0, 0, 0, 0, 0, 0, 0],
            p0: 0,
            p6: 6,
            p8: 8,
        }
    }

    fn init(&mut self, input: &Vec<usize>) {
        input.iter().for_each(|i| self.array[*i] += 1);
    }

    //   0 1 2 3 4 5 6 7 8    =>   0  1 2 3 4 5 6 7 8
    //   |           |   |         |  |           |
    //   p0         p6  p8        p8' p0'        p6' : + *p8'
    fn step(&mut self) {
        let p0_ = (self.p0 + 1) % 9;
        let p6_ = (self.p6 + 1) % 9;
        let p8_ = (self.p8 + 1) % 9;
        self.array[p6_ as usize] += self.array[p8_ as usize];

        self.p0 = p0_;
        self.p6 = p6_;
        self.p8 = p8_;
    }

    fn total(&self) -> u64 {
        self.array.iter().sum()
    }
}

fn main() {
    let state = &mut [
        &mut 0u64, &mut 0, &mut 0, &mut 0, &mut 0, &mut 0, &mut 0, &mut 0, &mut 0,
    ];
    let mut alt_state = State::new();
    let input = read_integers();

    // initialize
    input.iter().for_each(|i| *state[*i] += 1);
    &alt_state.init(&input);

    println!("State {:#3}: {:?}", 0, state);
    println!("State {:#3}: {:?}", 0, alt_state.array);

    // simulate
    for i in 1..=80 {
        step(state);
        alt_state.step();
        println!("State {:#3}: {:?}", i, state);
        println!("State {:#3}: {:?}", i, alt_state.array);
    }

    println!("Answer 1: {}", state.iter().map(|i| **i).sum::<u64>());
    println!("Answer 1 (alt): {}", alt_state.total());

    for _ in 81..=256 {
        step(state);
        alt_state.step();
    }

    println!("Answer 2: {}", state.iter().map(|i| **i).sum::<u64>());
    println!("Answer 2 (alt): {}", alt_state.total());
}
