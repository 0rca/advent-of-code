use std::io::{self, BufRead};
use std::num::ParseIntError;
use std::{collections::HashMap, str::FromStr};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl FromStr for Point {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ps: Vec<&str> = s.split(',').collect();
        let x: i32 = ps[0].parse().unwrap();
        let y: i32 = ps[1].parse().unwrap();
        Ok(Point { x, y })
    }
}

#[derive(Debug)]
struct Line {
    p0: Point,
    p1: Point,
}

impl FromStr for Line {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ps: Vec<&str> = s.split(" -> ").collect();
        let p0: Point = ps[0].parse().unwrap();
        let p1: Point = ps[1].parse().unwrap();
        Ok(Line { p0, p1 })
    }
}

impl Line {
    fn into_points(&self, orthogonal: bool) -> Vec<Point> {
        let mut acc: Vec<Point> = Vec::new();
        let p0 = self.p0;
        let p1 = self.p1;
        if orthogonal {
            if p0.x == p1.x {
                let x = p0.x;
                let y0 = p0.y.min(p1.y);
                let y1 = p0.y.max(p1.y);
                for y in y0..=y1 {
                    acc.push(Point { x, y });
                }
            }
            if p0.y == p1.y {
                let y = p0.y;
                let x0 = p0.x.min(p1.x);
                let x1 = p0.x.max(p1.x);
                for x in x0..=x1 {
                    acc.push(Point { x, y });
                }
            }
        } else {
            // println!("? {:?} -> {:?}", p0, p1);
            let x0 = p0.x;
            let y0 = p0.y;
            let x1 = p1.x;
            let y1 = p1.y;

            let dx = (x1 - x0).signum();
            let dy = (y1 - y0).signum();

            if dx != 0 && dy != 0 {
                // println!("+ {:?} -> {:?}, dx: {}, dy: {}", p0, p1, dx, dy);
                let mut x = x0;
                let mut y = y0;

                acc.push(Point { x: x1, y: y1 });
                while x != x1 && y != y1 {
                    acc.push(Point { x, y });
                    // println!("< {:?}", Point { x, y });
                    x += dx;
                    y += dy;
                }
            }
        }
        acc
    }
}

#[derive(Debug)]
struct Map {
    field: HashMap<Point, i32>,
}

impl Map {
    fn new() -> Self {
        Map {
            field: HashMap::new(),
        }
    }
    fn add_point(&mut self, point: Point) {
        match self.field.get_mut(&point) {
            Some(i) => {
                *i += 1;
            }
            None => {
                self.field.insert(point, 1);
            }
        }
    }
    fn add_many(&mut self, points: impl Iterator<Item = Point>) {
        for p in points {
            self.add_point(p.to_owned());
        }
    }
}

fn read_lines() -> Vec<Line> {
    io::stdin()
        .lock()
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect()
}

fn main() {
    let mut map = Map::new();
    let lines = read_lines();

    map.add_many(lines.iter().flat_map(|line| line.into_points(true)));
    let answer1 = map.field.values().filter(|i| **i >= 2).count();
    println!("Answer 1: {}", answer1);

    map.add_many(lines.iter().flat_map(|line| line.into_points(false)));
    let answer2 = map.field.values().filter(|i| **i >= 2).count();
    println!("Answer 2: {}", answer2);
}
