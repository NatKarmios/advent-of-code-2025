use std::fs::File;
use std::io::{self, BufRead};

fn read_file() -> io::Lines<io::BufReader<File>> {
    let file = File::open("./input.txt").unwrap();
    io::BufReader::new(file).lines()
}

fn read_first_line(line: String) -> Vec<u64> {
    let mut beams = Vec::new();
    line.chars().for_each(|c| match c {
        'S' => beams.push(1),
        '.' => beams.push(0),
        _ => panic!("Invalid char '{}'", c),
    });
    beams
}

fn read_line(line: String, beams: &Vec<u64>) -> (Vec<u64>, u64) {
    let mut new_beams = Vec::new();
    let mut splits = 0;
    (0..line.len()).for_each(|i| {
        let mut count = 0;
        if i > 0 && line.chars().nth(i - 1) == Some('^') && beams[i - 1] > 0 {
            splits += 1;
            count += beams[i - 1];
        }
        if line.chars().nth(i) == Some('.') {
            count += beams[i];
        }
        if line.chars().nth(i + 1) == Some('^') {
            count += beams[i + 1];
        }
        new_beams.push(count);
    });
    (new_beams, splits)
}

fn main() {
    let mut lines = read_file();
    let first_line = lines.nth(0).unwrap().unwrap();
    let mut beams = read_first_line(first_line);
    let mut total_splits = 0;
    lines.skip(1).for_each(|line| {
        let (new_beams, splits) = read_line(line.unwrap(), &beams);
        beams = new_beams;
        total_splits += splits;
    });
    let total_beams: u64 = beams.iter().sum();
    println!("Part 1: {}\nPart 2: {}", total_splits, total_beams);
}
