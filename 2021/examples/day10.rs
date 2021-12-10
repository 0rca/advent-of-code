use aoc2021::ioutil::*;

#[derive(Debug)]
enum Action {
    Elim(char),
    Intro(char),
}
use Action::{Elim, Intro};

trait Token {
    fn action(self: Self) -> Action;
    fn reverse(self: Self) -> Self;
}

impl Token for char {
    fn action(self: char) -> Action {
        match self {
            // introducers
            '(' => Intro('('),
            '[' => Intro('['),
            '{' => Intro('{'),
            '<' => Intro('<'),
            // eliminators
            ')' => Elim('('),
            ']' => Elim('['),
            '}' => Elim('{'),
            '>' => Elim('<'),
            _ => panic!("invalid token: {}", &self),
        }
    }

    fn reverse(self: Self) -> Self {
        match self {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            '<' => '>',
            ')' => '(',
            ']' => '[',
            '}' => '{',
            '>' => '<',
            _ => panic!("invalid token: {}", &self),
        }
    }
}

#[derive(Debug)]
enum Outcome {
    Corrupt(char),
    Incomplete(Vec<char>),
    Ok,
}
use Outcome::{Corrupt, Incomplete, Ok};

fn parse(line: &String) -> Outcome {
    let mut st: Vec<char> = Vec::new();
    let actions: Vec<Action> = line.chars().map(|c| c.action()).collect();
    for a in actions {
        match a {
            Intro(c) => {
                st.push(c);
                // println!("{:?}", &st);
            }
            Elim(c) => match st.last() {
                Some(c0) => {
                    if c == *c0 {
                        st.pop();
                    // println!("{:?}", &st);
                    } else {
                        // println!("Corrupt line: {} / {:?} / {}", &line, &st, c);
                        return Corrupt(c.reverse());
                    }
                }
                _ => todo!("empty stack"),
            },
        }
    }
    if st.len() > 0 {
        Incomplete(st)
    } else {
        Ok
    }
}

fn corruption_score(c: char) -> i32 {
    match c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => panic!("invalid corruption char: {}", c),
    }
}

fn completion_score(c: char) -> i64 {
    match c {
        ')' => 1,
        ']' => 2,
        '}' => 3,
        '>' => 4,
        _ => panic!("invalid completion char: {}", c),
    }
}

fn total_completion_score(st: Vec<char>) -> i64 {
    st.iter()
        .rev()
        .fold(0, |acc, c| 5 * acc + completion_score(c.reverse()))
}

fn total_corruption(lines: &Vec<String>) -> i32 {
    let mut sum: i32 = 0;
    for line in lines {
        match parse(&line) {
            Corrupt(c) => sum += corruption_score(c),
            _ => (),
        }
    }
    sum
}

fn total_completion(lines: &Vec<String>) -> i64 {
    let mut vec: Vec<i64> = lines
        .iter()
        .map(|line| parse(line))
        .filter_map(|o| match o {
            Incomplete(st) => Some(total_completion_score(st)),
            _ => None,
        })
        .collect();

    vec.sort();
    vec[vec.len() / 2]
}

fn main() {
    let lines = read_lines();
    println!("Answer 1: {}", total_corruption(&lines));
    println!("Answer 2: {}", total_completion(&lines));
}
