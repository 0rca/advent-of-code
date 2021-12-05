use std::io::{self, BufRead};
use std::str::FromStr;

fn integers() -> Vec<i32> {
    io::stdin()
        .lock()
        .lines()
        .map(|line| i32::from_str(&line.unwrap()).unwrap())
        .collect()
}

fn count_ups(xs: &Vec<i32>) -> i32 {
    let mut counter = 0;
    let mut cur_x = xs[0];

    for x in xs {
        if x > &cur_x {
            counter += 1;
        }
        cur_x = *x;
    }

    counter
}

fn main() {
    let xs = integers();
    println!("Answer 1: {}", count_ups(&xs));

    let ys = xs.windows(3).map(|w| w.iter().sum()).collect();
    println!("Answer 2: {}", count_ups(&ys));
}
