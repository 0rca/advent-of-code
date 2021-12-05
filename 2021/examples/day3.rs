use std::io::{self, BufRead};

fn gamma_rate(numbers: &Vec<u16>) -> u64 {
    rate(numbers, |n1, n0| n1 > n0)
}

fn epsilon_rate(numbers: &Vec<u16>) -> u64 {
    rate(numbers, |n1, n0| n1 > 0 && n1 < n0)
}

fn oxygen_rate(numbers: &Vec<u16>) -> Option<u64> {
    second_rate(numbers, |n1, n0| n1 > 0 && n1 >= n0)
}

fn carbon_rate(numbers: &Vec<u16>) -> Option<u64> {
    second_rate(numbers, |n1, n0| n1 > 0 && n1 < n0)
}

fn rate(numbers: &Vec<u16>, pred: fn(i32, i32) -> bool) -> u64 {
    let mut acc: u64 = 0;
    for b in 0..16 {
        let (n1, n0, mask) = count_bits(b, &numbers);
        if pred(n1, n0) {
            acc |= mask as u64;
        }
    }
    acc
}

fn second_rate(numbers: &Vec<u16>, pred: fn(i32, i32) -> bool) -> Option<u64> {
    let mut array = numbers.clone();
    for b in 0..16 {
        let (n1, n0, mask) = count_bits(b, &array);
        // println!("{:#02}(1) {:#02}(0) {:#018b}", n1, n0, mask);
        if pred(n1, n0) {
            // keep numbers with 1's in this bit
            keep_elems(&mut array, mask, |x, mask| x & mask == mask);
        } else {
            // keep numbers with 0's in this bit
            keep_elems(&mut array, mask, |x, mask| x & mask == 0);
        }
        if array.len() == 1 {
            return Some(array[0] as u64);
        }
    }
    None
}

fn keep_elems(vec: &mut Vec<u16>, mask: u16, pred: fn(u16, u16) -> bool) {
    let mut i = 0;
    while i < vec.len() {
        if !pred(vec[i], mask) {
            // println!("x {:#07b} {} from {:?}", vec[i], vec[i], vec);
            vec.remove(i);
        } else {
            i += 1;
        }
    }
}

fn count_bits(bit: u32, numbers: &Vec<u16>) -> (i32, i32, u16) {
    let mut n1 = 0;
    let mut n0 = 0;
    let mask = 0x1u16.rotate_right(bit + 1);
    for n in numbers {
        if *n & mask != 0 {
            n1 += 1;
        } else {
            n0 += 1;
        }
    }
    (n1, n0, mask)
}

fn read_numbers() -> Vec<u16> {
    io::stdin()
        .lock()
        .lines()
        .map(|line| u16::from_str_radix(&line.unwrap(), 2).unwrap())
        .collect()
}

fn main() {
    let numbers = read_numbers();
    let gamma = gamma_rate(&numbers);
    let epsilon = epsilon_rate(&numbers);
    let oxygen = oxygen_rate(&numbers);
    let carbon = carbon_rate(&numbers);

    println!("Gamma rate: {}", gamma);
    println!("Epsilon rate: {}", epsilon);
    println!("Answer 1: {}", gamma * epsilon);
    println!("Oxygen rate: {:?}", oxygen);
    println!("Carbon rate: {:?}", carbon);
    println!("Answer 2: {}", oxygen.unwrap() * carbon.unwrap());
}
