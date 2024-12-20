use std::fs::File;
use std::collections::HashMap;
use std::io::{BufRead, BufReader};

fn part_one(input: &String) -> Result<(), crate::common::Error> {
    let mut left_numbers = vec![];
    let mut right_numbers = vec![];
    for line in BufReader::new(File::open(input)?).lines() {
        // FUCK THIS FUCK THIS FUCK THIS FUCK THIS FUCK THIS
        let fuck_rust = line?;
        let parts = fuck_rust.split_whitespace().collect::<Vec<&str>>();
        left_numbers.push(parts[0].parse::<i32>()?);
        right_numbers.push(parts[1].parse::<i32>()?);
    }
    left_numbers.sort();
    right_numbers.sort();
    let mut sum = 0;
    for i in 0..left_numbers.len() {
        sum += (left_numbers[i] - right_numbers[i]).abs();
    }
    println!("{}", sum);
    Ok(())
}

fn part_two(input: &String) -> Result<(), crate::common::Error> {
    let mut left_numbers = vec![];
    let mut right_numbers = HashMap::new();
    for line in BufReader::new(File::open(input)?).lines() {
        // FUCK THIS FUCK THIS FUCK THIS FUCK THIS FUCK THIS
        let fuck_rust = line?;
        let parts = fuck_rust.split_whitespace().collect::<Vec<&str>>();
        left_numbers.push(parts[0].parse::<i32>()?);
        let right_number = parts[1].parse::<i32>()?;
        right_numbers.insert(right_number, right_numbers.get(&right_number).unwrap_or(&0)+1);
    }
    let mut sum = 0;
    for i in 0..left_numbers.len() {
        sum += left_numbers[i] * right_numbers.get(&left_numbers[i]).unwrap_or(&0);
    }
    println!("{}", sum);
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
