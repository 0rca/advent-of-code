use std::io::{self, BufRead};
use std::str::FromStr;

#[derive(Debug)]
enum Command {
    Forward(i32),
    Up(i32),
    Down(i32),
}

trait Interpreter {
    fn interpret(&mut self, cmd: &Command);
}

#[derive(Debug)]
struct Simple {
    x: i32,
    y: i32,
}

impl Interpreter for Simple {
    fn interpret(&mut self, cmd: &Command) {
        match cmd {
            Command::Forward(x) => self.x += x,
            Command::Up(y) => self.y -= y,
            Command::Down(y) => self.y += y,
        }
    }
}

#[derive(Debug)]
struct Complicated {
    x: i32,
    y: i32,
    aim: i32,
}

impl Interpreter for Complicated {
    fn interpret(&mut self, cmd: &Command) {
        match cmd {
            Command::Forward(x) => {
                self.x += x;
                self.y += x * self.aim;
            }
            Command::Up(y) => self.aim -= y,
            Command::Down(y) => self.aim += y,
        }
    }
}

fn read_commands() -> Vec<Command> {
    io::stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap())
        .map(|l| {
            let x: Vec<&str> = l.as_str().split(' ').collect();
            let cmd: Command = match x[0] {
                "forward" => Command::Forward(i32::from_str(x[1]).unwrap()),
                "up" => Command::Up(i32::from_str(x[1]).unwrap()),
                "down" => Command::Down(i32::from_str(x[1]).unwrap()),
                e => panic!("Invalid input: {}", e),
            };
            cmd
        })
        .collect()
}

fn main() {
    let cmds = read_commands();

    let mut simple: Simple = Simple { x: 0, y: 0 };
    cmds.iter().for_each(|cmd| simple.interpret(&cmd));
    println!("Answer 1: {}", simple.x * simple.y);

    let mut complicated: Complicated = Complicated { x: 0, y: 0, aim: 0 };
    cmds.iter().for_each(|cmd| complicated.interpret(cmd));
    println!("Answer 2: {}", complicated.x * complicated.y);
}
