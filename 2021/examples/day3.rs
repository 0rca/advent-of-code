use std::io::{self, BufRead};
fn read_numbers() -> Vec<u16> {
    io::stdin()
        .lock()
        .lines()
        .map(|line| u16::from_str_radix(&line.unwrap(), 2).unwrap())
        .collect()
}

fn rate(numbers: &Vec<u16>, pred: fn(i32, i32) -> bool) -> u64 {
    let mut acc: u64 = 0;
    for b in 0..15 {
        let mut n1 = 0;
        let mut n0 = 0;
        let mask = 0x1u16.rotate_left(b);
        for n in numbers {
            if *n & mask != 0 {
                n1 += 1;
            } else {
                n0 += 1;
            }
        }
        if pred(n1, n0) {
            acc |= mask as u64;
        }
    }
    acc
}

fn gamma_rate(numbers: &Vec<u16>) -> u64 {
    rate(numbers, |n1, n0| n1 > n0)
}

fn epsilon_rate(numbers: &Vec<u16>) -> u64 {
    rate(numbers, |n1, n0| n1 > 0 && n1 < n0)
}

fn main() {
    let numbers = read_numbers();
    let gamma = gamma_rate(&numbers);
    let epsilon = epsilon_rate(&numbers);

    println!("Gamma rate: {}", gamma);
    println!("Epsilon rate: {}", epsilon);
    println!("Answer 1: {}", gamma * epsilon);
}
