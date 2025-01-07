use anyhow::{Result, Context};

fn is_safe(numbers: &Vec<i64>) -> bool {
    let mut signum: Option<i64> = None;
    for window in numbers.windows(2) {
        let diff = window[1] - window[0];
        if diff.abs() > 3 {
            return false;
        }
        let current_signum = diff.signum();
        if signum.is_some() && current_signum != signum.unwrap() {
            return false;
        }
        signum = Some(current_signum);
    }
    true
}

fn part_one(contents: String) -> Result<()> {
    let numbers: Vec<Result<Vec<i64>>> = contents
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|num| num.parse::<i64>().context("Error parsing int"))
                .collect::<Result<Vec<i64>>>()
        })
        .collect();

    if let Some(err) = numbers.iter().find_map(|res| res.as_ref().err()) {
        return Err(anyhow::Error::msg(err.to_string()));
    }

    let valid = numbers
        .into_iter()
        .filter_map(Result::ok)
        .filter(|line| is_safe(line))
        .count();

    println!("Valid: {}", valid);
    Ok(())
}

fn part_two(contents: String) -> Result<()> {
    let numbers: Vec<Result<Vec<i64>>> = contents
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|num| num.parse::<i64>().context("Error parsing int"))
                .collect::<Result<Vec<i64>>>()
        })
        .collect();

    if let Some(err) = numbers.iter().find_map(|res| res.as_ref().err()) {
        return Err(anyhow::Error::msg(err.to_string()));
    }

    let valid = numbers
        .into_iter()
        .filter_map(Result::ok)
        .filter(|line| {
            if is_safe(line) {
                return true;
            }
            for i in 0..line.len() {
                let mut copy = line.clone();
                copy.remove(i);
                if is_safe(&copy) {
                    return true;
                }
            }
            false
        })
        .count();

    println!("Valid: {}", valid);
    Ok(())
}

pub fn run(input: &String) -> Result<()> {
    let start = std::time::Instant::now();
    part_one(std::fs::read_to_string(input).context("Failed to read input")?)?;
    println!("Part 1 Time: {}µs", start.elapsed().as_micros());
    let start2 = std::time::Instant::now();
    part_two(std::fs::read_to_string(input).context("Failed to read input")?)?;
    println!("Part 2 Time: {}µs", start2.elapsed().as_micros());
    Ok(())
}
