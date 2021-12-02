mod input {
    use std::io;
    use std::io::prelude::*;
    use std::fs::File;
    pub fn read_input(path: &str) -> io::Result<String> {
        let mut buffer = String::new();
        let mut f = File::open(format!("../inputs/{}", path))?;
        f.read_to_string(&mut buffer).unwrap();
        Ok(buffer)
    }
}
mod solutions {
    use std::collections::HashSet;
    use std::collections::HashMap;
    use input;

    // Day 1, part 1

    pub fn day_1_1() {
        let input = input::read_input("input1_1.txt").unwrap();
        let numbers = input.lines().map(|x| x.parse().unwrap());
        let r = numbers.fold(0, |acc, x: i32| {
            acc + x
        });
        println!("day 1, part 1: {}", r);
    }

    // Day 1, part II

    pub fn day_1_2() {
        let input = input::read_input("input1_1.txt").unwrap();
        let numbers = input.lines().map(|x| x.parse().unwrap());
        let r = numbers.cycle().try_fold((0, HashSet::new()), |(freq, mut hist): (i32, HashSet<i32>), x: i32| {
            if !hist.contains(&freq) {
                hist.insert(freq);
                Ok((freq + x, hist))
            }
            else {
                Err(freq)
            }
        });
        match r {
            Err(freq) => println!("day 1, part 2: {}", freq),
            Ok(_) => println!("impossible error")
        };
    }

    // Day 2, part 1

    pub fn day_2_1() {
        let input = input::read_input("input2_1.txt").unwrap();
        let (n,m) = count_ids(input.lines());
        println!("Day 2, part 1: {}", n * m);
    }

    pub fn char_frequencies(word: &str) -> HashMap<char, i32> {
        let mut r = HashMap::new();
        word.chars().for_each(|c| {
            if r.contains_key(&c) {
                *r.get_mut(&c).unwrap() += 1;
            }
            else {
                r.insert(c, 1);
            }
        });
        r
    }

    pub fn contains_n(i: i32, freqs: &HashMap<char, i32>) -> bool {
        freqs.values().find(|&x| *x == i).is_some()
    }

    pub fn count_ids<T>(xs: T) -> (i32,i32)
        where T: IntoIterator,
              T::Item : ToString
    {
        xs.into_iter().fold((0,0), |(n1,n2), x| {
            let freqs = char_frequencies(x.to_string().as_str());
            let fn1 = if contains_n(2, &freqs) { n1 + 1 } else { n1 };
            let fn2 = if contains_n(3, &freqs) { n2 + 1 } else { n2 };
            (fn1, fn2)
        })
    }

    // Day 2, part 2

    fn distance(x: &str, y: &str) -> usize {
        x.chars().zip(y.chars()).filter(|(c1,c2)| c1 != c2).count()
    }

    fn pair_combinations(xs: Vec<&str>) -> Vec<(&str,&str)>
    {
        let mut r = Vec::new();
        for i in 0 .. xs.len() {
            for j in i+1 .. xs.len()  {
                r.push((xs[i],xs[j]));
            }
        }
        r
    }

    fn common_part(w1: &str, w2: &str) -> String {
        w1.chars().zip(w2.chars()).filter_map(|(c1,c2)| {
            if c1 == c2 {
                Some(c1)
            }
            else {
                None
            }
        }).collect()
    }

    fn find_by_distance<'a>(r: usize, input: Vec<(&'a str,&'a str)>) -> (&'a str,&'a str)
    {
        *input.iter().find(|(s1,s2)| distance(s1,s2) == r).unwrap()
    }

    pub fn day_2_2() {
        let input = input::read_input("input2_1.txt").expect("Cannot open input file");
        let cs = pair_combinations(input.lines().collect());
        let (r1,r2) = find_by_distance(1, cs);
        println!("Day 2, part 2: {}", common_part(r1,r2));
    }

    pub fn day_3_1() {
        let input = input::read_input("input3_1.txt").unwrap();
        let lines: Vec<Claim> =
            input.lines().map(|s| s.parse().unwrap()).collect();

        lines.iter().for_each(|c| println!("Claim #{}: {:?}", c.id, c));
    }

    #[derive(Debug, PartialEq)]
    pub struct Claim {
        id: u32,
        x: u32,
        y: u32,
        w: u32,
        h: u32
    }

    #[derive(Debug)]
    struct Parser {
        state: St,
        str_id: String,
        str_x: String,
        str_y: String,
        str_w: String,
        str_h: String
    }

    #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
    enum St {
        StInit,
        StId,
        StSp1,
        StAt,
        StX,
        StY,
        StSp3,
        StW,
        StH
    }

    use self::St::*;

    impl Parser {

        pub fn new() -> Parser {
            Parser{
                state: StInit,
                str_id: String::new(),
                str_x: String::new(),
                str_y: String::new(),
                str_w: String::new(),
                str_h: String::new()
            }
        }

        pub fn char(&mut self, x: char, c: char, next: St) -> Option<Option<char>> {
            if x == c {
                self.state = next;
                Some(None)
            }
            else {
                None
            }
        }

        pub fn digit(&mut self, x: char, stop: char, next: St) -> Option<Option<char>>
        {
            if x.is_digit(10) {
                Some(Some(x))
            }
            else {
                self.char(x, stop, next)
            }
        }

        pub fn digit_(&self, x: char) -> Option<Option<char>> {
            if x.is_digit(10) {
                Some(Some(x))
            }
            else {
                None
            }
        }

        pub fn step(&mut self, c: char) -> Option<()> {
            match self.state {
                StInit =>
                    self.char(c, '#', StId).map(|_| ()),
                StId =>
                    self.digit(c, ' ', StSp1).map(|mc| {
                        mc.map(|c| self.str_id.push(c));
                    }),
                StSp1 =>
                    self.char(c, '@', StAt).map(|_| ()),
                StAt =>
                    self.char(c, ' ', StX).map(|_| ()),
                StX =>
                    self.digit(c, ',', StY).map(|mc| {
                        mc.map(|c| self.str_x.push(c));
                    }),
                StY =>
                    self.digit(c, ':', StSp3).map(|mc| {
                        mc.map(|c| self.str_y.push(c));
                    }),
                StSp3 =>
                    self.char(c, ' ', StW).map(|_| ()),
                StW =>
                    self.digit(c, 'x', StH).map(|mc| {
                        mc.map(|c| self.str_w.push(c));
                    }),
                StH =>
                    self.digit_(c).map(|mc| {
                        mc.map(|c| self.str_h.push(c));
                    })
            }
        }

        fn parse(&mut self, s: &str) -> Option<Claim> {
            s.chars().try_for_each(|c| self.step(c));
            let id: u32 = self.str_id.parse().expect(format!("Id incorrect: {:?}", self).as_str());
            let x: u32 = self.str_x.parse().expect(format!("X incorrect: {:?}", self).as_str());
            let y: u32 = self.str_y.parse().expect(format!("Y incorrect: {:?}", self).as_str());
            let w: u32 = self.str_w.parse().expect(format!("W incorrect: {:?}", self).as_str());
            let h: u32 = self.str_h.parse().expect(format!("H incorrect: {:?}", self).as_str());
            Some(Claim{id: id, x, y, w, h})
        }
    }

    use std::str::FromStr;
    impl FromStr for Claim {
        type Err = &'static str;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut parser = Parser::new();
            match parser.parse(s) {
                Some(x) => Ok(x),
                None => {
                    println!("Parser: {:?}", parser);
                    Err("Error parsing Claim")
                }
            }
        }
    }
}
use solutions::*;

fn main() {
    day_1_1();
    day_1_2();
    day_2_1();
    day_2_2();
    day_3_1();
}
