// Yeah no. Fuck this language. Who the hell would choose this willingly?
use std::fs::File;
use std::collections::HashMap;
use std::io::{BufRead, BufReader};

fn is_safe(numbers: &Vec<i64>) -> bool {
    true
}

fn part_one(input: &String) -> Result<(), crate::common::Error> {
    let valid = BufReader::new(File::open(input)?).lines()
        .map(|line| line?.parse::<i64>()?) // Fuck this, the end of my rope is literally here.
        .filter(|n| is_safe(&vec![*n]))
        .count();
    println!("Valid: {}", valid);
    Ok(())
}

fn part_two(input: &String) -> Result<(), crate::common::Error> {
    Ok(())
}

pub fn run(input: &String) -> Result<(), crate::common::Error> {
    let start = std::time::Instant::now();
    part_one(input)?;
    println!("Part 1 Time: {}µs", start.elapsed().as_micros());
    let start2 = std::time::Instant::now();
    part_two(input)?;
    println!("Part 2 Time: {}µs", start2.elapsed().as_micros());
    Ok(())
}
