use std::io::{self, BufRead};

fn simple_decode(signal: &str) -> Option<u32> {
    match signal.len() {
        2 => Some(1),
        3 => Some(7),
        4 => Some(4),
        7 => Some(8),
        _ => None,
    }
}

fn ans1(signals: &Vec<&str>) -> u32 {
    signals
        .iter()
        .map(|sig| if simple_decode(*sig).is_some() { 1 } else { 0 })
        .sum()
}

fn read_lines() -> Vec<String> {
    io::stdin().lock().lines().map(|l| l.unwrap()).collect()
}

fn words(input: &str) -> Vec<&str> {
    input.split_whitespace().collect()
}

fn sort_into<'a>(inputs: &Vec<&'a str>, patterns: &mut Vec<&'a str>, signals: &mut Vec<&'a str>) {
    inputs.iter().for_each(|s| {
        let ws = words(s);
        let mut tgt = 0;
        for w in ws {
            match w {
                "|" => tgt = 1,
                _ => {
                    if tgt > 0 {
                        signals.push(w)
                    } else {
                        patterns.push(w)
                    }
                }
            }
        }
    })
}

// ------------------- Part II

//  these outputs are mapped to indicator segments
//  +- a -+      0 1 2 3 4 5 6 7 8 9
//  |     |      a _ a a _ a a a a a  'a' occurs 8 times
//  b     c      b _ _ _ b b b _ b b  'b' occurs 6 times
//  |     |      c c c c c _ _ c c c  'c' occurs 8 times
//  +- d -+      _ _ d d d d d _ d d  'd' occurs 7 times
//  |     |      e _ e _ _ _ e _ e _  'e' occurs 4 times
//  e     f      f f _ f f f f f f f  'f' occurs 9 times
//  |     |      g _ g g _ g g _ g g  'g' occurs 7 times
//  +- g -+      : : : : : : : : : :
//               6 2 5 5 4 5 6 3 7 6 segments

// digits, numbers of segments, and segment signature: sorted list of segment occurences
fn decode(sig: &Vec<i32>) -> i32 {
    match sig[..] {
        // 0 : 6 segments, [4,6,7,8,8,9]
        [4, 6, 7, 8, 8, 9] => 0,
        // 1 : 2 segments, [8,9]
        [8, 9] => 1,
        // 2 : 5 segments, [4,7,7,8,8]
        [4, 7, 7, 8, 8] => 2,
        // 3 : 5 segments, [7,7,8,8,9]
        [7, 7, 8, 8, 9] => 3,
        // 4 : 4 segments, [6,7,8,9]
        [6, 7, 8, 9] => 4,
        // 5 : 5 segments, [6,7,7,8,9]
        [6, 7, 7, 8, 9] => 5,
        // 6 : 6 segments, [4,6,7,7,8,9]
        [4, 6, 7, 7, 8, 9] => 6,
        // 7 : 3 segments, [8,8,9]
        [8, 8, 9] => 7,
        // 8 : 7 segments, [4,6,7,7,8,8,9]
        [4, 6, 7, 7, 8, 8, 9] => 8,
        // 9 : 6 segments, [6,7,7,8,8,9]
        [6, 7, 7, 8, 8, 9] => 9,
        // unknown segment signature
        _ => panic!("Unknown segment signature"),
    }
}

type Board = [i32; 7];

fn compile_board(patterns: &Vec<&str>) -> Box<Board> {
    let mut board = [0, 0, 0, 0, 0, 0, 0];
    for pat in patterns {
        for c in pat.chars() {
            match c {
                'a' => board[0] += 1,
                'b' => board[1] += 1,
                'c' => board[2] += 1,
                'd' => board[3] += 1,
                'e' => board[4] += 1,
                'f' => board[5] += 1,
                'g' => board[6] += 1,
                _ => panic!("invalid input"),
            }
        }
    }
    Box::new(board)
}

fn signature(board: &Board, signal: &str) -> Vec<i32> {
    let mut sig: Vec<i32> = signal
        .chars()
        .map(|c| match c {
            'a' => board[0],
            'b' => board[1],
            'c' => board[2],
            'd' => board[3],
            'e' => board[4],
            'f' => board[5],
            'g' => board[6],
            _ => panic!("invalid input"),
        })
        .collect();
    sig.sort();
    sig
}

enum Select {
    Patterns,
    Signals,
}
use self::Select::*;

fn to_decimal(vec: &Vec<i32>) -> i32 {
    vec.iter().fold(0, |acc, x| 10 * acc + x)
}

fn break_line(line: &str) -> (Vec<&str>, Vec<&str>) {
    let mut patterns: Vec<&str> = Vec::new();
    let mut signals: Vec<&str> = Vec::new();
    let mut destination = Patterns;
    for l in line.split_whitespace() {
        match (l, &destination) {
            ("|", _) => destination = Signals,
            (l, Patterns) => patterns.push(l),
            (l, Signals) => signals.push(l),
        }
    }
    (patterns, signals)
}

fn decrypt(signals: &Vec<&str>, using: Box<[i32; 7]>) -> Vec<i32> {
    signals
        .iter()
        .map(|s| {
            let sig = signature(&using, s);
            decode(&sig)
        })
        .collect()
}

fn main() {
    let lines = read_lines();
    let inputs: Vec<&str> = lines.iter().map(|s| s.as_str()).collect();
    let mut patterns = Vec::new();
    let mut signals = Vec::new();

    sort_into(&inputs, &mut patterns, &mut signals);
    println!("Answer 1: {}", ans1(&signals));

    // Part II
    let mut ans2 = 0;
    for line in inputs {
        let (patterns, signals) = break_line(line);
        let board = compile_board(&patterns);
        let x = decrypt(&signals, board);
        ans2 += to_decimal(&x);
    }
    println!("Answer 2: {}", ans2);
}
