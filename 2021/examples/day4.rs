use std::io::{self, BufRead};
use std::str::FromStr;

// keep the marked flag in sign bit
const MASK: i32 = 0x1i32.rotate_right(1);

fn read_game() -> (Vec<i32>, Vec<Vec<Vec<i32>>>) {
    let mut ls: Vec<String> = io::stdin().lock().lines().map(|x| x.unwrap()).collect();
    let inputs: Vec<i32> = ls
        .remove(0)
        .split(',')
        .map(|str| i32::from_str(str).unwrap())
        .collect();

    ls.remove(0);

    // println!("inputs: {:?}", inputs);
    let mut boards: Vec<Vec<Vec<i32>>> = Vec::new();
    while ls.len() >= 5 {
        let vec: Vec<Vec<i32>> = ls[0..5]
            .iter()
            .map(|x| {
                // println!("{:?}", x);
                x.split(' ')
                    .filter(|c| *c != "")
                    .map(|s| i32::from_str(s).unwrap())
                    .collect()
            })
            .collect();
        boards.push(vec);
        for _ in 0..5 {
            ls.remove(0);
        }
        if ls.len() > 0 {
            ls.remove(0);
        }
    }
    // println!("{:?}, total: {}", boards, boards.len());
    (inputs, boards)
}

fn init_board(vec: Vec<Vec<i32>>) -> Box<[[i32; 5]; 5]> {
    let mut b: Box<[[i32; 5]; 5]> = Box::new([
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
    ]);
    for i in 0..5 {
        for j in 0..5 {
            b[i][j] = vec[i][j];
        }
    }
    b
}

fn is_winner(board: &Box<[[i32; 5]; 5]>) -> bool {
    has_winning_row(board) || has_winning_column(board)
}

fn has_winning_row(board: &Box<[[i32; 5]; 5]>) -> bool {
    board.iter().any(|r| r.iter().all(|x| *x < 0))
}
fn has_winning_column(board: &Box<[[i32; 5]; 5]>) -> bool {
    let mut transposed = board.clone();
    for i in 0..5 {
        for j in 0..5 {
            transposed[i][j] = board[j][i];
        }
    }
    has_winning_row(&transposed)
}

fn final_score(board: &Box<[[i32; 5]; 5]>) -> i32 {
    let mut score = 0;
    for i in 0..5 {
        for j in 0..5 {
            if board[i][j] & MASK == 0 {
                score += board[i][j];
            }
        }
    }
    score
}

fn main() {
    let (input, vectors) = read_game();

    let mut boards: Vec<Box<[[i32; 5]; 5]>> = Vec::new();
    for v in vectors {
        boards.push(init_board(v));
    }

    let mut winners = Vec::new();
    let mut scores = Vec::new();
    for x in input {
        for (n, board) in boards.iter_mut().enumerate() {
            if winners.contains(&n) {
            } else {
                for i in 0..5 {
                    for j in 0..5 {
                        if board[i][j] == x {
                            board[i][j] |= MASK;
                        }
                    }
                }
                if is_winner(&board) {
                    winners.push(n);
                    scores.push(x * final_score(&board));
                }
            }
        }
    }
    println!("First winner score: {}", scores[0]);
    println!("Last winner score: {}", scores[scores.len() - 1]);
}
