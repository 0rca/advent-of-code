use std::io::{self, BufRead};
use std::str::FromStr;

fn read_integers() -> Vec<usize> {
    let mut vec = Vec::new();

    for line in io::stdin().lock().lines() {
        for str in line.unwrap().split(',') {
            vec.push(usize::from_str(str).unwrap());
        }
    }
    vec
}

fn find_minimum_fuel(vec: &Vec<usize>, fuel_metric: fn(usize, usize) -> usize) -> Option<usize> {
    vec.iter()
        .enumerate()
        .map(|(pos, _)| count_fuel(pos, &vec, fuel_metric))
        .min()
}

fn count_fuel(pos: usize, vec: &Vec<usize>, fuel_metric: fn(usize, usize) -> usize) -> usize {
    vec.iter().map(|x| fuel_metric(pos, *x)).sum()
}

fn simple_fuel_metric(x: usize, y: usize) -> usize {
    if x > y {
        x - y
    } else {
        y - x
    }
}

fn arithmetic_progression_metric(x: usize, y: usize) -> usize {
    let minp = x.min(y);
    let maxp = x.max(y);
    (1 + maxp - minp) * (maxp - minp) / 2
}

fn main() {
    let input = read_integers();
    println!(
        "Minimum fuel 1: {}",
        find_minimum_fuel(&input, simple_fuel_metric).unwrap()
    );
    println!(
        "Minimum fuel 2: {}",
        find_minimum_fuel(&input, arithmetic_progression_metric).unwrap()
    )
}
