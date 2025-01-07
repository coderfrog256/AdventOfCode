use std::fs::File;
use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use anyhow::{Result, Context};

fn part_one(input: &String) -> Result<()> {
    let mut left_numbers = vec![];
    let mut right_numbers = vec![];

    let file = File::open(input).context("Failed to open input file")?;
    for line in BufReader::new(file).lines() {
        let line = line.context("Failed to read line from file")?;
        let parts = line.split_whitespace().collect::<Vec<&str>>();
        left_numbers.push(parts[0].parse::<i32>().context("Failed to parse left number")?);
        right_numbers.push(parts[1].parse::<i32>().context("Failed to parse right number")?);
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

fn part_two(input: &String) -> Result<()> {
    let mut left_numbers = vec![];
    let mut right_numbers = HashMap::new();

    let file = File::open(input).context("Failed to open input file")?;
    for line in BufReader::new(file).lines() {
        let line = line.context("Failed to read line from file")?;
        let parts = line.split_whitespace().collect::<Vec<&str>>();
        left_numbers.push(parts[0].parse::<i32>().context("Failed to parse left number")?);
        let right_number = parts[1].parse::<i32>().context("Failed to parse right number")?;
        *right_numbers.entry(right_number).or_insert(0) += 1;
    }

    let mut sum = 0;
    for i in 0..left_numbers.len() {
        sum += left_numbers[i] * right_numbers.get(&left_numbers[i]).unwrap_or(&0);
    }
    println!("{}", sum);
    Ok(())
}

pub fn run(input: &String) -> Result<()> {
    let start = std::time::Instant::now();
    part_one(input)?;
    println!("Part 1 Time: {}µs", start.elapsed().as_micros());

    let start2 = std::time::Instant::now();
    part_two(input)?;
    println!("Part 2 Time: {}µs", start2.elapsed().as_micros());

    Ok(())
}
