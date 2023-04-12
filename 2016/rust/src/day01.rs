use std::fs;
const DIRECTIONS: &'static [(i32,i32)] = &[(0,1), (1,0), (0,-1), (-1,0)];

fn spin(direction: (i32,i32), turn: &str) -> (i32,i32) {
    let index = DIRECTIONS.iter().position(|&d| d == direction).unwrap();
    let new_index = match turn.chars().nth(0).unwrap() {
        'R' => (index + 1) % 4,
        'L' => (index + 3) % 4,
        _ => panic!("Invalid turn: {}", turn)
    };
    DIRECTIONS[new_index]
}

fn part_one(input: &str) -> i32 {
    let final_state = input.split(", ")
         .fold((DIRECTIONS[0], (0,0)), |state, command| {
             let amount = command[1..].parse::<i32>().unwrap();
             let new_direction = spin(state.0, &command);
             (
                 new_direction,
                 ((state.1).0 + new_direction.0 * amount,
                 (state.1).1 + new_direction.1 * amount)
             )
         }).1;
    (final_state.0 as i32).abs() + (final_state.1 as i32).abs()
}

fn part_two(input: &str) -> i32 {
    let mut visited = vec![(0,0)];
    let mut direction = DIRECTIONS[0];
    let mut position = (0,0);
    for command in input.split(", ")  {
        let amount = command[1..].parse::<i32>().unwrap();
        direction = spin(direction, &command);
        for _ in 0..amount {
            position = (position.0 + direction.0, position.1 + direction.1);
            if visited.contains(&position) {
                return (position.0 as i32).abs() + (position.1 as i32).abs();
            }
            visited.push(position);
        }
    }
    (position.0 as i32).abs() + (position.1 as i32).abs()
}

pub fn run() {
    println!("Day 1, part 1: {}", part_one(fs::read_to_string("../input/day1.txt").unwrap().trim()));
    println!("Day 1, part 2: {}", part_two(fs::read_to_string("../input/day1.txt").unwrap().trim()));
}
