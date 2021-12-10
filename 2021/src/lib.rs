pub mod ioutil {
    use std::io::{self, BufRead};

    pub fn read_lines() -> Vec<String> {
        io::stdin().lock().lines().map(|l| l.unwrap()).collect()
    }
}
